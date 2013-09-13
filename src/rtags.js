// include("esrefactor/lib/esrefactor.js")
var esrefactorContext = new esrefactor.Context();

function indexFile(code, file, verbose)
{
    try {
        var parsed;
        try {
            parsed = esprima.parse(code, { range: true, tolerant: true });
        } catch (err) {
            log("Got error", err);
            return {errors:[err]};
        }

        if (!parsed) {
            throw new Error("Couldn't parse file " + file + ' ' + code.length);
            return undefined;
        }
        esrefactorContext.setCode(parsed);
        if (!esrefactorContext._syntax)
            throw new Error('Unable to identify anything without a syntax tree');

        var parents = [];

        function location(range) { return code.substring(range[0], range[1]); }
        function childKey(child) // slow
        {
            var p = child.parent;
            for (var key in p) {
                if (p[key] === child) {
                    return key;
                }
            }
            return "not found";
        }
        function isChild(key, node)
        {
            if (!node)
                node = parents[parents.length - 1];

            if (node.parent) {
                var p = node.parent[key];
                if (p !== undefined) {
                    if (Array.isArray(p)) {
                        return p.indexOf(node) != -1;
                    } else {
                        return p == node;
                    }
                }
            }
            return false;
        }
        function parentTypeIs(type, node)
        {
            if (!node)
                node = parents[parents.length - 1];
            if (node.parent) {
                var t = node.parent.type;
                if (typeof type == "string") {
                    return type === t;
                } else {
                    return type.indexOf(t) != -1;
                }
            }
            return false;
        }

        var errors = {};
        var scopes = [];
        var scopeStack = [];

        // lowest rank in stack is the declaration if <= 3
        function addSymbol(name, range, declarationRank)
        {
            name += '_';
            var scopeIdx = scopeStack.length - 1;
            var found = false;
            var fitte = undefined;
            while (true) {
                if (scopeStack[scopeIdx].objects[name] !== undefined) {
                    found = true;
                    fitte = scopeStack[scopeIdx].objects[name];
                    break;
                } else if (declarationRank == 0 || scopeIdx == 0) {
                    break;
                }
                --scopeIdx;
            }

            // log("Adding symbol", name, range, location(range), found, declarationRank, parents[parents.length - 2].type, parents[parents.length - 1].type);
            range.push(declarationRank);
            if (found) {
                scopeStack[scopeIdx].objects[name].push(range);
            } else {
                scopeStack[scopeIdx].objects[name] = [range];
            }
            ++scopeStack[scopeIdx].count;
        }

        function qualifiedName(node)
        {
            if (!node)
                node = parents[parents.length - 1];
            var orig = node;
            var seen = [];
            function resolveName(n)
            {
                if (seen.indexOf(n) != -1)
                    return undefined;
                seen.push(n);
                if (n) {
                    switch (n.type) {
                    case esprima.Syntax.Identifier:
                        return n.name;
                    case esprima.Syntax.Literal:
                        return n.value;
                    case esprima.Syntax.MemberExpression:
                        if (seen.indexOf(n.object) != -1 || seen.indexOf(n.property) != -1)
                            break;
                        return resolveName(n.object) + "." + resolveName(n.property);
                    case esprima.Syntax.ObjectExpression:
                    case esprima.Syntax.Property:
                        return resolveName(n.key);
                    case esprima.Syntax.VariableDeclarator:
                    case esprima.Syntax.FunctionDeclaration:
                        return resolveName(n.id);
                    case esprima.Syntax.AssignmentExpression:
                        return resolveName(n.left);
                    case esprima.Syntax.CallExpression:
                        return resolveName(n.callee);
                    default:
                        break;
                        // log("Not sure how to resolve", node.type, node.range);
                    }
                }
                return undefined;
            }

            var name = undefined;
            var prev = undefined;
            while (node) {
                if (node.type) {
                    var done = false;
                    switch (node.type) {
                    case esprima.Syntax.FunctionExpression:
                    case esprima.Syntax.FunctionDeclaration:
                    case esprima.Syntax.CallExpression:
                        done = prev;
                        break;
                    case esprima.Syntax.AssignmentExpression:
                        done = prev && (prev.type != esprima.Syntax.ObjectExpression || node.right != prev);
                        // if (done)
                        //     log("Stopping", orig.range, "at", name, "because of", node.type);
                        break;
                    case esprima.Syntax.VariableDeclarator:
                        done = prev && prev.type != esprima.Syntax.ObjectExpression && isChild("init", prev);
                        // if (done)
                        //     log("Stopping", orig.range, "at", name, "because of VariableDeclarator");
                        break;
                    default:
                        break;
                    }
                    if (done)
                        break;

                    var n = resolveName(node);
                    if (n) {
                        if (!name) {
                            name = n;
                        } else {
                            name = n + "." + name;
                        }
                    }
                    prev = node;
                    node = node.parent;
                }
            }
            return name;
        }

        function indexIdentifier(node)
        {
            node.indexed = true;
            var rank = 3;
            var name = undefined;
            switch (node.parent.type) {
            case esprima.Syntax.FunctionDeclaration:
            case esprima.Syntax.FunctionExpression:
            case esprima.Syntax.VariableDeclarator:
            case esprima.Syntax.Property:
                rank = 0;
                break;
            case esprima.Syntax.AssignmentExpression:
                rank = 3;
                break;
            case esprima.Syntax.CallExpression:
            case esprima.Syntax.UnaryExpression:
            case esprima.Syntax.BinaryExpression: // ### ???
            case esprima.Syntax.ReturnStatement:
            case esprima.Syntax.NewExpression:
            case esprima.Syntax.UpdateExpression:
            case esprima.Syntax.ForInStatement:
            case esprima.Syntax.IfStatement:
            case esprima.Syntax.WhileStatement:
            case esprima.Syntax.DoWhileStatement:
            case esprima.Syntax.ForStatement:
            case esprima.Syntax.LogicalExpression:
            case esprima.Syntax.ConditionalExpression:
            case esprima.Syntax.ArrayExpression:
                rank = 5;
                break;
            case esprima.Syntax.MemberExpression:
                if (isChild("property", node))
                    name = qualifiedName(node.parent);
                rank = (node.parent.parent.type == esprima.Syntax.AssignmentExpression ? 3 : 5);
                break;
            default:
                // log("Shit", node.type);
                log("Unhandled parent", node.parent.type, node.parent.range, location(parent.range),
                    node.range, location(node.range));
                break;
            }
            if (name === undefined)
                name = qualifiedName();
            // log("Found identifier", name, node.range, parent.type, rank);
            addSymbol(name, node.range, rank);
        }

        estraverse.traverse(esrefactorContext._syntax, {
            enter: function (node) {
                if (parents.length)
                    node.parent = parents[parents.length - 1];
                parents.push(node);
                var name = undefined;
                var declaration = 0;
                // log("entering", node.type, childKey(node));
                switch (node.type) {
                case esprima.Syntax.Program:
                    node.scope = true;
                    break;
                case esprima.Syntax.FunctionDeclaration:
                    node.id.parent = node;
                    indexIdentifier(node.id);
                    node.scope = true;
                    break;
                case esprima.Syntax.FunctionExpression:
                    node.scope = true;
                    break;
                case esprima.Syntax.Identifier:
                    if (!node.indexed) {
                        // we need to handle the function itself as part of the previous scope
                        indexIdentifier(node);
                    }
                    break;
                default:
                    break;
                }
                if (node.scope) {
                    var scope = { objects:{}, count:0, type:node.type };
                    scopeStack.push(scope);
                    scopes.push(scope);
                }
            },
            leave: function (node) {
                parents.pop();
                if (node.scope)
                    scopeStack.pop();
            }
        });


        var ret = { objects:[] };
        // log(scopes.length);
        // return ret;
        for (var s=0; s<scopes.length; ++s) {
            var scope = scopes[s];
            if (scope.count) {
                for (var name in scope.objects) {
                    scope.objects[name].sort(function(l, r) { var ret = l[2] - r[2]; if (!ret) { ret = l[0] - r[0]; } return ret; });
                }

                ret.objects.push(scopes[s].objects);
                if (scopes.length < 5)
                    log(s, scopes[s].objects);
            }
        }

        if (verbose) {
            estraverse.traverse(esrefactorContext._syntax, {
                enter: function(node) {
                    delete node.parent;
                    delete node.indexed;
                }
            });
            ret.ast = esrefactorContext._syntax;
        }
        if (errors)
            ret.errors = errors;
    } catch(err) {
        log("Got an error", err.toString(), "\n", err.stack);
        ret = {objects:[]};
    }

    return ret;
}

