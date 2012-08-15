#include "Project.h"
#include "Indexer.h"
#include "GRTags.h"
#include "Server.h"

Project::Project()
    : indexer(0), grtags(0)
{
    memset(databases, 0, sizeof(databases));
}

Project::~Project()
{
    for (int i=0; i<DatabaseTypeCount; ++i) {
        delete databases[i];
    }
}

ScopedDB Project::db(DatabaseType type, ReadWriteLock::LockType lockType) const
{
    return ScopedDB(databases[type], lockType);
}

Path Project::databaseDir(DatabaseType type) const
{
    const char *dbNames[] = {
        "symbols.db",
        "symbolnames.db",
        "dependencies.db",
        "fileinfos.db",
        "grfiles.db",
        "gr.db"
    };

    char ret[PATH_MAX];
    const int w = snprintf(ret, sizeof(ret), "%s%s", projectPath.constData(), dbNames[type]);
    return Path(ret, w);
}

