#qmake INCLUDEPATH+=/usr/local/llvm/include/ "LIBS+=-L/usr/local/llvm/lib -Wl,-rpath,/usr/local/llvm/lib"
TEMPLATE = app
TARGET = rb
DESTDIR = ..
DEPENDPATH += .
INCLUDEPATH += .

macx {
    CONFIG -= app_bundle
    INCLUDEPATH += /opt/local/include
    LIBS += -L/opt/local/lib
}

QT = core network

DEFINES += QT
# Input
SOURCES += \
    main.cpp \
    RBuild.cpp \
    GccArguments.cpp \
    Utils.cpp \
    Node.cpp \
    Path.cpp \
    ClangRunnable.cpp \
    PreprocessorRunnable.cpp

HEADERS += \
    RBuild.h \
    GccArguments.h \
    Utils.h \
    Path.h \
    Node.h \
    Location.h \
    ClangRunnable.h \
    PreprocessorRunnable.h

OTHER_FILES += gccopts.gperf

include(../shared/shared.pri)

CLANG_PREFIX=$$(CLANG_PREFIX)
isEmpty(CLANG_PREFIX):CLANG_PREFIX=/usr/local/
DEFINES += CLANG_PREFIX=$$CLANG_PREFIX
release {
    QMAKE_CXXFLAGS += -g
    QMAKE_CFLAGS += -g
}

gccopts_gperf.commands = gperf -I -C -l -L C++ gccopts.gperf --output-file gccopts_gperf.cpp -Z gccopts_gperf
gccopts_gperf.target = gccopts_gperf.cpp
gccopts_gperf.depends = gccopts.gperf
QMAKE_EXTRA_TARGETS += gccopts_gperf

PRE_TARGETDEPS += gccopts_gperf.cpp

### If you don't have libmagic on mac you can install the file package in ports
### or something along the lines of file-shlibs and file-dev on fink

LIBS += -lclang -lmagic 

unix {
    debug:OBJECTS_DIR = $${OUT_PWD}/.obj/debug-shared
    release:OBJECTS_DIR = $${OUT_PWD}/.obj/release-shared

    debug:MOC_DIR = $${OUT_PWD}/.moc/debug-shared
    release:MOC_DIR = $${OUT_PWD}/.moc/release-shared

    RCC_DIR = $${OUT_PWD}/.rcc/
    UI_DIR = $${OUT_PWD}/.uic/
}
