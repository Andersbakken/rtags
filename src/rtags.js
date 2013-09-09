// include("esrefactor/lib/esrefactor.js")
var esrefactorContext = new esrefactor.Context();

function indexFile(code, file, verbose)
{
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
    if (!esrefactorContext._scopeManager)
        throw new Error('Unable to identify anything without a valid scope manager');

    var scopeManager = esrefactorContext._scopeManager;
    // var lookup = esrefactorContext._lookup;
    // log(esrefactorContext._syntax);

    scopeManager.attach();
    var parents = [];

    function location(range)
    {
        return code.substring(range[0], range[1]);
    }

    function childKey(child, offset) // slow
    {
        if (typeof offset === 'undefined')
            offset = parents.length - 1;
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
        if (typeof offset == 'undefined')
            offset = parents.length - 1;

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

    // lowest rank in stack is the declaration
    function addSymbol(name, range, declarationRank)
    {
        var scopeIdx = scopeStack.length - 1;
        var found = false;
        if (declarationRank == 0) {
            found = scopeStack[scopeIdx].objects[name] !== undefined;
        } else {
            var object;
            var idx = name.indexOf('.');
            if (idx != -1) {
                object = name.substring(0, idx);
            } else {
                object = name;
            }
            for (var i=scopeStack.length - 1; i>=0; --i) {
                if (scopeStack[i].objects[name]) {
                    found = true;
                    // log("Found", name, "in a scope", i, scopeStack.length);
                    scopeIdx = i;
                    break;
                } else if (scopeStack[i].objects[object]) {
                    log("Found", name, "in a scope", i, scopeStack.length);
                    found = true;
                    scopeIdx = i;
                    break;
                }
            }
         }

        log("Adding symbol", name, range, location(range), found, declarationRank, parents[parents.length - 2].type, parents[parents.length - 1].type);
        if (range.length != 2) {
            throw new Error("Something wrong here " + name + " " + range);
        }
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
                    node.handled = true;
                    return node.name;
                case esprima.Syntax.Literal:
                    node.handled = true;
                    return node.value;
                case esprima.Syntax.MemberExpression:
                    if (seen.indexOf(node.object) != -1 || seen.indexOf(node.property) != -1)
                        break;
                    return resolveName(node.object) + "." + resolveName(node.property);
                case esprima.Syntax.ObjectExpression:
                case esprima.Syntax.Property:
                    return resolveName(node.key);
                    // break;
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
                // case esprima.Syntax.VariableDeclarator:
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

    function range(node, key)
    {
        if (!node)
            node = parents[parents.length - 1];

        switch (node.type) {
        case esprima.Syntax.FunctionDeclaration:
        case esprima.Syntax.VariableDeclarator:
            if (node.id)
                return range(node.id, key);
            break;
        case esprima.Syntax.Property:
            if (key) {
                if (node.key)
                    return range(node.key);
            } else if (node.value) {
                return range(node.value);
            }
            break;
        case esprima.Syntax.MemberExpression:
            if (key) {
                if (node.object)
                    return range(node.object, key);
            } else if (node.property) {
                return range(node.property, key);
            }
            break;
        case esprima.Syntax.CallExpression:
            if (node.callee)
                return range(node.callee, false);
            break;
        default:
            break;
            // log("Not sure how to resolve", node.type, node.range);
        }
        return node.range;
   }

    estraverse.traverse(esrefactorContext._syntax, {
        enter: function (node) {
            var ret = undefined;
            parents.push(node);
            var name = undefined;
            var declaration = 0;
            // log("entering", node.type, childKey(node));
            switch (node.type) {
            case esprima.Syntax.Program:
                node.scope = true;
                break;
            case esprima.Syntax.VariableDeclarator:
            case esprima.Syntax.FunctionDeclaration:
                node.scope = true;
                addSymbol(qualifiedName(), range(), 0);
                break;
            case esprima.Syntax.CallExpression:
                addSymbol(qualifiedName(), range(), 5);
                ret = estraverse.VisitorOption.Skip;
                break;
            case esprima.Syntax.Property:
                addSymbol(qualifiedName(), range(undefined, true), 0);
                break;
            case esprima.Syntax.Identifier:
                if (!node.handled) {
                    if (parentTypeIs(esprima.Syntax.MemberExpression)) {
                        if (isChild("object")) {
                            addSymbol(qualifiedName(undefined, false), range(), 5);
                        } else if (isChild("property")) {
                            addSymbol(qualifiedName(-2, false), range(), 3);
                        }
                    } else if (parentTypeIs(esprima.Syntax.Property)  && isChild("value")) {
                        addSymbol(qualifiedName(undefined, false), range(), 5);
                    } else if (parentTypeIs(esprima.Syntax.AssignmentExpression)  && isChild("left")) {
                        addSymbol(qualifiedName(-2, false), range(), 3);
                    } else if (parentTypeIs([esprima.Syntax.FunctionDeclaration, esprima.Syntax.FunctionExpression]) && isChild("params")) {
                        addSymbol(qualifiedName(undefined, false), range(), 0);
                    } else if (parentTypeIs([esprima.Syntax.UnaryExpression, esprima.Syntax.BinaryExpression,
                                             esprima.Syntax.AssignmentExpression, esprima.Syntax.ReturnStatement,
                                             esprima.Syntax.NewExpression, esprima.Syntax.UpdateExpression,
                                             esprima.Syntax.ForInStatement])) {
                        addSymbol(qualifiedName(undefined, false), range(), 5);
                    } else if (parentTypeIs([esprima.Syntax.IfStatement, esprima.Syntax.WhileStatement,
                                             esprima.Syntax.DoWhileStatement, esprima.Syntax.ForStatement]) && isChild("test")) {
                        addSymbol(qualifiedName(undefined, false), range(), 5);
                    } else if (parentTypeIs(esprima.Syntax.FunctionExpression)) {
                        node.handled = true;
                    }

                    if (!node.handled) {
                        log("Unhandled identifier", node.name, node.range, parents[parents.length - 2].type);
                    }
                }
                break;
            default:
                break;
            }
            if (node.scope) {
                var scope = {objects:{}, count:0, type:node.type};
                scopeStack.push(scope);
                scopes.push(scope);
            }
            return ret;
        },
        leave: function (node) {
            parents.pop();
        }
    });

    scopeManager.detach();

    var ret = { objects:[] };
    for (var s=0; s<scopes.length; ++s) {
        var scope = scopes[s];
        if (scope.count) {
            for (var name in scope.objects) {
                scope.objects[name].sort(function(l, r) { return r[2] - l[2]; });
            }

            ret.objects.push(scopes[s].objects);
            log(s, scopes[s].objects);
        }
    }

    if (verbose)
        ret.ast = esrefactorContext._syntax;
    if (errors)
        ret.errors = errors;

    return ret;
}

