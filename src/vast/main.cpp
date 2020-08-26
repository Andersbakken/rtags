#include <qapplication.h>
#include <qobjectdefs.h>
#include <qprogressdialog.h>
#include <qsettings.h>
#include <qvariant.h>
#include <stdio.h>
#include <memory>
#include <thread>
#include <utility>

#include "TranslationUnit.h"
#include "Window.h"

int main(int argc, char **argv)
{
    QApplication a(argc, argv);
    a.setApplicationName("vast");
    a.setOrganizationName("rtags");

    unsigned int flags = 0;
    {
        QSettings settings;
        if (settings.value("showDefines").value<bool>())
            flags |= TranslationUnit::ShowDefines;
        if (settings.value("showIncludes").value<bool>())
            flags |= TranslationUnit::ShowIncludes;
        if (settings.value("showTypedefs").value<bool>())
            flags |= TranslationUnit::ShowTypedefs;
    }

    std::unique_ptr<TranslationUnit> translationUnit;
    QProgressDialog dialog("Parsing translation unit...", "&Abort", 0, 0);
    std::thread t([&dialog, &translationUnit, argc, argv, flags]() {
        translationUnit = TranslationUnit::create(argv, argc, flags);
        QMetaObject::invokeMethod(&dialog, "accept");
    });
    dialog.exec();
    t.join();
    if (dialog.wasCanceled())
        return 0;
    if (!translationUnit) {
        fprintf(stderr, "Failed to parse translation unit:\n");
        for (int i=1; i<argc; ++i) {
            if (i)
                fprintf(stderr, " ");
            fprintf(stderr, "%s", argv[i]);
        }
        fprintf(stderr, "\n");
        return 1;
    }

    Window window(std::move(translationUnit));
    window.show();
    return a.exec();
}
