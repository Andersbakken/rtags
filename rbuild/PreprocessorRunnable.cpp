#include "PreprocessorRunnable.h"

QList<Path> PreprocessorRunnable::sStdIncludePaths;
void PreprocessorRunnable::init(const QList<Path> &stdIncludePaths)
{
    sStdIncludePaths = stdIncludePaths;
}

PreprocessorRunnable::PreprocessorRunnable(const Path &sourceFile, const GccArguments &args)
    : mSourceFile(sourceFile), mArgs(args)
{
    setAutoDelete(true);
}

void PreprocessorRunnable::run()
{
    QElapsedTimer timer;
    timer.start();
    QList<Path> includes;
    QFile f(mSourceFile);
    if (!f.open(QIODevice::ReadOnly)) {
        qWarning("Can't open %s", mSourceFile.constData());
        emit error(mSourceFile, mArgs, "Can't open: " + mSourceFile);
    }
    // qDebug() << __LINE__ << timer.restart() << path;

    QByteArray unsaved;
    unsaved.reserve(f.size() / 2);
    const QList<QByteArray> lines = f.readAll().split('\n');
    for (int i=0; i<lines.size(); ++i) {
        QByteArray line = lines.at(i).trimmed();
        if (line.startsWith('#')) {
            while (line.endsWith('\\') && i + 1 < lines.size()) {
                line.chop(1);
                line += lines.at(++i).trimmed();
            }
            if (line.startsWith("#error")) {
// #warning hack for some Qt things where defines arent defined we have to fix this properly
                continue;
            }
            if (line.startsWith("#include ")) {
                line.remove(0, 9);
            }


            unsaved.append(line);
            unsaved.append('\n');
        }
    }
    // qDebug() << mSourceFile << unsaved;
    QProcess process;
    const char *clangPaths[] = {
        "/usr/local/llvm/bin/clang",
        "/usr/local/bin/clang",
        "/usr/bin/clang",
        0
    };
    const char *clang = 0;
    for (int i=0; clangPaths[i]; ++i) {
        struct stat st;
        if (!stat(clangPaths[i], &st)) {
            clang = clangPaths[i];
            break;
        }
    }
    Q_ASSERT_X(clang, __FUNCTION__, "Can't find clang executable");

    process.start(clang, QStringList() << "-E" << "-");
    process.write(unsaved);
    process.closeWriteChannel();
    process.waitForFinished();
    const Path sourceFileDir = mSourceFile.parentDir();
    const QList<Path> includePaths = mArgs.includePaths();
    const QList<Path> *lists[] = { &includePaths, &PreprocessorRunnable::sStdIncludePaths };
    foreach(QByteArray line, process.readAllStandardOutput().split('\n')) {
        // qDebug() << mSourceFile << unsaved << line;
        if (!line.isEmpty()) {
            bool quote = true;
            switch (line.at(0)) {
            case '"':
                if (line.at(line.size() - 1) != '"') { // flag error?
                    qWarning() << "Weird include" << line;
                    continue;
                }
                break;
            case '<':
                if (line.at(line.size() - 1) != '>') { // flag error?
                    qWarning() << "Weird include" << line;
                    continue;
                }
                quote = false;
                break;
            default:
                continue;
            }
            line.remove(0, 1);
            line.chop(1);
            // qWarning() << "looking for" << line;
            if (quote) {
                const Path resolved = Path::resolved(line, sourceFileDir);
                if (resolved.isHeader()) {
                    includes.append(resolved);
                    continue;
                }
            }
            enum { Found, DidntFind, DidntWant } state = DidntWant;
            for (int i=0; i<2 && state == DidntWant; ++i) {
                foreach(const Path &dir, *lists[i]) {
                    const Path resolved = Path::resolved(line, dir);
                    switch (resolved.magicType()) {
                    case Path::Header:
                        includes.append(resolved);
                        state = Found;
                        break;
                    case Path::Source:
                        state = DidntWant;
                    default:
                        break;
                    }
                }
            }
            if (state == DidntFind) {
                qWarning() << "Couldn't resolve" << line << includePaths << sStdIncludePaths;
            }
        }
    }
    emit headersFound(mSourceFile, mArgs, includes);
}
