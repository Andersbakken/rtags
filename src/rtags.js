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
        return code.substr(range[0], range[1]);
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
        return (offset > 0
                && parents[offset - 1]
                && typeof parents[offset - 1][key] != 'undefined'
                && offset <= parents.length
                && parents[offset - 1][key] == parents[offset]);
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

    // 0, only-if-found
    // 1, declaration,
    // 2, declaration if unique in current scope

    function addSymbol(name, range, declaration)
    {
        var scopeIdx = scopeStack.length - 1;
        var found = false;
        if (declaration) {
            found = scopeStack[scopeIdx].objects[name] !== undefined;
        } else {
            var object;
            var idx = name.indexOf('.');
            if (idx != -1) {
                object = name.substr(0, idx);
            } else {
                object = name;
            }
            for (var i=scopeStack.length - 1; i>=0; --i) {
                // log("Looking for " + name + " " + object + " " + i + " " + JSON.stringify(scopeStack[i].objects));
                if (scopeStack[i].objects[name]) {
                    found = true;
                    // log("Found", name, "in a scope", i, scopeStack.length);
                    scopeIdx = i;
                    break;
                } else if (scopeStack[i].objects[object]) {
                    // log("Found", name, "in a scope", i, scopeStack.length);
                    scopeIdx = i;
                    break;
                }
            }
        }

        log("Adding symbol", name, range, location(range), declaration, found, arguments[3], arguments[4], arguments[5]);
        if (!found) {
            // log("Didn't find " + name + " " + declaration);
            if (declaration == 0) {
                log("Returning false for ", name, location(range), range);
                return false;
            }
            log("Forcing " + name + " " + declaration);
            range.push(true);
            scopeStack[scopeIdx].objects[name] = [range];
        } else {
            // log("Found " + name + " " + declaration);
            scopeStack[scopeIdx].objects[name].push(range);
        }
        ++scopeStack[scopeIdx].count;
        return true;
    }

    function resolveNames()
    {
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
                case esprima.Syntax.MemberExpression:
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
        // if (parents[parents.length - 1].id)
        //     name = resolveName(parents[parents.length - 1].id);
       // log("resolveNames");
        for (var i=parents.length - 1; i>=0; --i) {
            if (parents[i].type) {
                var done = false;
                switch (parents[i].type) {
                case esprima.Syntax.FunctionExpression:
                case esprima.Syntax.FunctionDeclaration:
                // case esprima.Syntax.VariableDeclarator:
                    done = i < parents.length - 1;
                    break;
                case esprima.Syntax.MemberExpression:
                    if (i < parents.length - 1 && parents[i + 1] == parents[i].object)
                        continue;
                    break;
                case esprima.Syntax.AssignmentExpression:
                    if (i < parents.length - 1 && parents[i + 1] == parents[i].left)
                        continue;
                    break;
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

    estraverse.traverse(esrefactorContext._syntax, {
        enter: function (node) {
            parents.push(node);
            var name = undefined;
            var declaration = 0;
            var range = node.range;
            // log("entering", node.type, childKey(node));
            switch (node.type) {
            case esprima.Syntax.VariableDeclarator:
                range = node.id.range;
                name = resolveNames();
                declaration = 1;
                break;
            case esprima.Syntax.FunctionExpression:
            case esprima.Syntax.FunctionDeclaration:
                name = resolveNames();
                declaration = 1;
                break;
            case esprima.Syntax.CallExpression:
            // case esprima.Syntax.AssignmentExpression:
                name = resolveNames();
                declaration = 0;
                break;
            case esprima.Syntax.Property:
                name = resolveNames();
                declaration = 2;
                range = node.key.range;
                break;
            case esprima.Syntax.MemberExpression:
                name = resolveNames();
                range = node.property.range;
                declaration = 2;
                break;
            case esprima.Syntax.Identifier:
                if (parentTypeIs(esprima.Syntax.MemberExpression) && isChild("object")) {
                    name = resolveNames();
                    declaration = 0;
                }
                break;
            default:
                break;
            }
            if (name) {
                if (!addSymbol(name, range, declaration, node.type)) {
                    // log("Failed to add", node.type);
                    scopeStack[scopeStack.length - 1].pending.push({name:name, range:range});
                }
            } else {
                log("ignored", node.type, node.range);
            }
            var scope = scopeManager.acquire(node);
            if (scope) {
                scope.objects = {};
                scope.pending = [];
                scope.count = 0;
                scopes.push(scope);
                scopeStack.push(scope);
                node.scope = true;
                // log("Pushing a scope " + node.type);
            }
        },
        leave: function (node) {
            // log("leaving", node.type);
            parents.pop();
            if (node.scope) {
                scopeManager.release(node);
                // log("Popping a scope");
                var pending = scopeStack[scopeStack.length - 1].pending;
                // log("Trying out pending", pending);
                for (var i=0; i<pending.length; ++i) {
                    addSymbol(pending[i].name, pending[i].range, 2);
                }
                scopeStack.pop();
            }
        }
    });

    scopeManager.detach();

    var ret = { objects:[] };
    for (var s=0; s<scopes.length; ++s) {
        if (scopes[s].count) {
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

