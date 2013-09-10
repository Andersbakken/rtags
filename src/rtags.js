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
        function childKey(child, offset) // slow
        {
            if (typeof offset === 'undefined') {
                offset = parents.length - 1;
            } else if (typeof offset == 'object') {
                offset = parents.indexOf(offset);;
            }
            var p = parents[offset];
            for (var key in p) {
                if (p[key] === child) {
                    return key;
                }
            }
            return "not found";
        }
        function isChild(key, offset)
        {
            if (typeof offset == 'undefined') {
                offset = parents.length - 1;
            } else if (typeof offset == 'object') {
                offset = parents.indexOf(offset);
            }

            if (offset > 0 && parents[offset - 1]) {
                var p = parents[offset - 1][key];
                if (p !== undefined) {
                    if (Array.isArray(p)) {
                        return p.indexOf(parents[offset]) != -1;
                    } else {
                        return p == parents[offset];
                    }
                }
            }
            return false;
        }
        function parentTypeIs(type, offset)
        {
            if (typeof offset == 'undefined')
                offset = parents.length - 1;
            if (offset > 0 && parents[offset - 1]) {
                var t = parents[offset - 1].type;
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
            var scopeIdx = scopeStack.length - 1;
            var found = false;
            while (true) {
                if (scopeStack[scopeIdx].objects[name]) {
                    found = true;
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

        function qualifiedName(offset, walk)
        {
            if (offset === undefined)
                offset = -1;
            if (walk === undefined)
                walk = true;
            var seen = [];
            function resolveName(node)
            {
                if (seen.indexOf(node) != -1)
                    return undefined;
                seen.push(node);
                if (node) {
                    switch (node.type) {
                    case esprima.Syntax.Identifier:
                        return node.name;
                    case esprima.Syntax.Literal:
                        return node.value;
                    case esprima.Syntax.MemberExpression:
                        if (seen.indexOf(node.object) != -1 || seen.indexOf(node.property) != -1)
                            break;
                        return resolveName(node.object) + "." + resolveName(node.property);
                    case esprima.Syntax.ObjectExpression:
                    case esprima.Syntax.Property:
                        return resolveName(node.key);
                    case esprima.Syntax.VariableDeclarator:
                    case esprima.Syntax.FunctionDeclaration:
                        return resolveName(node.id);
                    case esprima.Syntax.AssignmentExpression:
                        return resolveName(node.left);
                    case esprima.Syntax.CallExpression:
                        return resolveName(node.callee);
                    default:
                        break;
                        // log("Not sure how to resolve", node.type, node.range);
                    }
                }
                return undefined;
            }

            var name = undefined;
            var end = walk ? 0 : parents.length + offset;
            for (var i=parents.length + offset; i>=end; --i) {
                if (parents[i].type) {
                    var done = false;
                    switch (parents[i].type) {
                    case esprima.Syntax.FunctionExpression:
                    case esprima.Syntax.FunctionDeclaration:
                    case esprima.Syntax.CallExpression:
                    case esprima.Syntax.AssignmentExpression:
                        done = i < parents.length - 1;
                        break;
                        // case esprima.Syntax.MemberExpression:
                        //     // if (i < parents.length - 1 && parents[i + 1] == parents[i].object) {
                        //     //     log("Fisj?");
                        //     //     continue;
                        //     // }
                        //     break;
                        // case esprima.Syntax.AssignmentExpression:
                        //     // if (i < parents.length - 1 && parents[i + 1] == parents[i].left)
                        //     //     continue;
                        //     break;
                    default:
                        break;
                    }
                    if (done)
                        break;

                    var n = resolveName(parents[i]);
                    if (n) {
                        if (!name) {
                            name = n;
                        } else {
                            name = n + "." + name;
                        }
                    }
                    // log(i, parents[i].type, done, n, name);
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
                    name = qualifiedName(-2);
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
                    scope.objects[name].sort(function(l, r) { return l[2] - r[2]; });
                }

                ret.objects.push(scopes[s].objects);
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

