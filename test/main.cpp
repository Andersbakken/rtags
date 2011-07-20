#include <QtCore>
#include <CppDocument.h>
#include <TypeOfExpression.h>
#include <Symbol.h>
#include <Name.h>
#include <Literals.h>

using namespace CPlusPlus;

static bool isWordChar(const QChar &ch)
{
    return (ch.isLetterOrNumber() || ch == '_' || ch == ' ');
}

static inline QString wordAt(const QString &line, int idx)
{
    int left = idx;
    while (left > 0 && isWordChar(line.at(left - 1)))
        --left;
    while (idx + 1 < line.size() && isWordChar(line.at(idx + 1)))
        ++idx;
    return line.mid(left, idx + 1 - left);
}

static void checkLine(Document::Ptr& ptr, Snapshot& snapshot, Scope* scope, const QString& line)
{
    QString linecopy = line.simplified();
    qDebug() << "checking line" << linecopy;

    TypeOfExpression typeOfExpression;
    typeOfExpression.init(ptr, snapshot);
    QList<LookupItem> items = typeOfExpression.reference(linecopy, scope, TypeOfExpression::Preprocess);

    qDebug() << "items looked up" << items.size();
    foreach(const LookupItem& item, items) {
        qDebug() << item.declaration() << item.declaration()->line() << item.declaration()->column();
    }
}

int main(int argc, char **argv)
{
    QString file = "./test.cpp";
    for (int i=1; i<argc; ++i) {
        file = argv[i];
    }

    Snapshot snapshot;
    Document::Ptr doc = Document::create(file);
    if (doc) {
        snapshot.insert(doc);

        QFile f(file);
        if (f.open(QIODevice::ReadOnly)) {
            QByteArray source = f.readAll();
            doc->setSource(source);
            doc->check();

            QStringList lines = QString::fromLocal8Bit(source).split('\n');
            //qDebug() << lines;
            const int count = lines.size();
            QSet<Symbol*> seen;
            for (int i=0; i<count; ++i) {
                const QString &line = lines.at(i);
                Scope* scope = doc->scopeAt(i);
                if (scope) {
                    checkLine(doc, snapshot, scope, line);
                }
                continue;

                // Symbol *lastVisibleSymbolAt(unsigned line, unsigned column = 0) const;
                for (int j=0; j<line.size(); ++j) {
                    Symbol *s = doc->lastVisibleSymbolAt(i + 1, j);
                    if (s && !seen.contains(s)) {
                        seen.insert(s);
                        if (s->name() && s->name()->identifier()) {
                            printf("%d %d => %s (%s)\n", i + 1, j, s->name()->identifier()->chars(),
                                   qPrintable(wordAt(line, j)));
                        }
                    } else if (s) {
                        // seen
                        printf("seen symbol %p at %d.%d\n", s, i + 1, j);
                        if (s->name() && s->name()->identifier())
                            printf("  which is %s\n", s->name()->identifier()->chars());
                    }
                }
            }

        }
        printf("%s %d: if (doc) {\n", __FILE__, __LINE__);
    } else {
        printf("%s %d: } else {\n", __FILE__, __LINE__);
    }
    return 0;
}
