#include <QCoreApplication>
#include "Rdm.h"

int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);

    Rdm rdm(argc, argv);

    return app.exec();
}
