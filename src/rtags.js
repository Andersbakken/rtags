function findChildren(cursor, kind, depth, max)
{
    let children = cursor.children();
    let size = children.size;
    for (let i=0; i<size; ++i) {
        let child = children[i];
        let childKind = child.kind;
        if (kind == childKind) {
            return child;
        }
        if (depth == undefined || depth > 0) {
            let ret = findChildren(child, kind, depth - 1);
            if (ret)
                return ret;
        }
    }
    return undefined;
}
