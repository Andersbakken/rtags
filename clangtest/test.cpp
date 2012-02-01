#include <QByteArray>
#include <QList>
#include <qglobal.h>

int main()
{
    QList<QByteArray> foo;
    foreach(const QByteArray &f, foo) {
        return f.isEmpty() ? 0 : 1;
    }
    return 0;
}
