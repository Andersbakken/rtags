// include("esrefactor/lib/esrefactor.js")
var esrefactorContext = new esrefactor.Context();

function resolveName(node)
{
    if (node.type == "Identifier") {
        return node.name;
    } else if (node.type == "MemberExpression") {
        return resolveName(node.object) + "." + resolveName(node.property);
    } else if (node.type == "Literal") {
        return node.value;
    } else {
        log("Unhandled node type", node);
    }
    return "";
}

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
        return (offset > 0 && parents[offset - 1] && parents[offset - 1].type == type);
    }

    var scopes = [];
    var scopeStack = [];

    // 0, reference
    // 1, declaration,
    // 2, declaration if unique in current scope
    function add(path, range, declaration) {
        if ({}[path])
            path += ' ';
        var scope = scopeStack[scopeStack.length - 1];
        var found = false;
        if (declaration) {
            if (scope.objects[path])
                found = true;
        } else {
            var object;
            var idx = path.indexOf('.');
            if (idx != -1) {
                object = path.substr(0, idx);
            } else {
                object = path;
            }
            for (var i=scopeStack.length - 1; i>=0; --i) {
                if (scopeStack[i].objects[path]) {
                    found = true;
                    // log("Found", path, "in a scope", i, scopeStack.length);
                    scope = scopeStack[i];
                    break;
                } else if (scopeStack[i].objects[object]) {
                    // log("Found", path, "in a scope", i, scopeStack.length);
                    scope = scopeStack[i];
                    break;
                }
            }
        }

        if (!found) {
            range.push(true);
            scope.objects[path] = [range];
        } else {
            scope.objects[path].push(range);
        }
        ++scope.count;
    }

    var byName = {};
    var errors = null;
    estraverse.traverse(esrefactorContext._syntax, {
        enter: function (node) {
            var path;
            parents.push(node);
            var s = scopeManager.acquire(node);
            if (s) {
                s.objects = {};
                s.count = 0;
                s.objectScope = [];
                scopes.push(s);
                scopeStack.push(s);
            }
            if (node.type == esprima.Syntax.Program) {
                errors = node.errors;
            } else if (node.type == esprima.Syntax.ObjectExpression) {
                if (isChild("init") && parentTypeIs(esprima.Syntax.VariableDeclarator)) {
                    node.addedObjectScope = true;
                    scopeStack[scopeStack.length - 1].objectScope.push(parents[parents.length - 2].id.name);
                } else if (isChild("value") && parentTypeIs(esprima.Syntax.Property)) {
                    node.addedObjectScope = true;
                    scopeStack[scopeStack.length - 1].objectScope.push(parents[parents.length - 2].key.name);
                }
            } else if (node.type == esprima.Syntax.MemberExpression) {
                node.name = resolveName(node);
                if (node.property.type == esprima.Syntax.Literal) {
                    // this one will not show up as an Identifier so we have to add it here
                    path = scopeStack[scopeStack.length - 1].objectScope.join(".");
                    if (path)
                        path += ".";
                    path += node.name;
                    add(path, node.property.range);
                }
                // log("got member expression", node.name, node.range);
            } else if (node.type == esprima.Syntax.Identifier) {
                // log("Got an identifier", node, scopeStack.length);
                path = scopeStack[scopeStack.length - 1].objectScope.join(".");
                if (path)
                    path += ".";
                var decl = false;
                if (parentTypeIs(esprima.Syntax.MemberExpression) && isChild("property")) {
                    path += parents[parents.length - 2].name;
                } else {
                    if (parentTypeIs(esprima.Syntax.Property) && parentTypeIs(esprima.Syntax.ObjectExpression, parents.length - 2)
                        && isChild("init", parents.length - 2)) {
                        decl = true;
                    } else if (parentTypeIs(esprima.Syntax.VariableDeclarator) && isChild("id")) {
                        decl = true;
                    } else if (parentTypeIs(esprima.Syntax.FunctionDeclaration)) { // it's either a parameter or the id of the function
                        decl = true;
                    } else {
                        // log("reference it seems", node.name, node.type, node.range, parents[parents.length - 2].name,
                        //     parents[parents.length - 2].type);

                    }
                    path += node.name;
                }
                // log("Adding an identifier", path, node.range, decl);
                add(path, node.range, decl); // probably more of them that should pass true
                // log("identifier", path, JSON.stringify(node.range));
            }
        },
        leave: function (node) {
            parents.pop();
            if (node.addedObjectScope)
                scopeStack[scopeStack.length - 1].objectScope.pop();
            if (scopeManager.release(node)) // the scopeManager probably knows enough about this to provide the scopeStack
                scopeStack.pop();

        }
    });
    scopeManager.detach();

    var ret = { objects:[] };
    for (var s=0; s<scopes.length; ++s) {
        if (scopes[s].count) {
            ret.objects.push(scopes[s].objects);
            // log(s, scopes[s].objects);
        }
    }
    if (verbose)
        ret.ast = esrefactorContext._syntax;
    if (errors)
        ret.errors = errors;

    return ret;
}

