#qmake INCLUDEPATH+=/usr/local/llvm/include/ "LIBS+=-L/usr/local/llvm/lib -Wl,-rpath,/usr/local/llvm/lib"
TEMPLATE = app
TARGET = 
DEPENDPATH += .
INCLUDEPATH += .
CONFIG += debug

macx {
    CONFIG -= app_bundle
    !dbus:CONFIG += ebus
} else {
    !dbus:CONFIG += ebus
    #!ebus:CONFIG += dbus
}

QT = core
dbus {
    message("Using DBus. Run qmake -config ebus to use ebus")
    QT += dbus
    SOURCES += DaemonAdaptor.cpp \
               DaemonInterface.cpp
    HEADERS += DaemonAdaptor.h \
               DaemonInterface.h
    DEFINES += DBUS_ENABLED
} else {
    message("Using EBus. Run qmake -config dbus to use dbus")
    QT += network
    DEFINES += EBUS_ENABLED
}


# Input
SOURCES += \
    main.cpp \
    Daemon.cpp \
    Client.cpp \
    GccArguments.cpp \
    Utils.cpp \
    PreCompile.cpp \
    EBus.cpp \
    Node.cpp \
    Path.cpp \
    ParseThread.cpp \
    VisitThread.cpp \
    FileManager.cpp \
    ArgParser.cpp

HEADERS += \
    Daemon.h \
    Client.h \
    GccArguments.h \
    Utils.h \
    PreCompile.h \
    EBus.h \
    Path.h \
    Node.h \
    Location.h \
    ParseThread.h \
    VisitThread.h \
    FileManager.h \
    ArgParser.h

OTHER_FILES += \
    gccopts.gperf

gccopts_gperf.commands = gperf -I -C -l -L C++ gccopts.gperf --output-file gccopts_gperf.cpp -Z gccopts_gperf
gccopts_gperf.target = gccopts_gperf.cpp
gccopts_gperf.depends = gccopts.gperf
QMAKE_EXTRA_TARGETS += gccopts_gperf

PRE_TARGETDEPS += gccopts_gperf.cpp

LIBS += -lclang

unix {
    debug:OBJECTS_DIR = $${OUT_PWD}/.obj/debug-shared
    release:OBJECTS_DIR = $${OUT_PWD}/.obj/release-shared

    debug:MOC_DIR = $${OUT_PWD}/.moc/debug-shared
    release:MOC_DIR = $${OUT_PWD}/.moc/release-shared

    RCC_DIR = $${OUT_PWD}/.rcc/
    UI_DIR = $${OUT_PWD}/.uic/
}
