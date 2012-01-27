INCLUDEPATH += $$PWD
DEPENDPATH += $$PWD
gccopts_gperf.commands = gperf -I -C -l -L C++ $$PWD/gccopts.gperf --output-file $$PWD/gccopts_gperf.h -Z gccopts_gperf
gccopts_gperf.target = $$PWD/gccopts_gperf.h
gccopts_gperf.depends = $$PWD/gccopts.gperf
QMAKE_EXTRA_TARGETS += gccopts_gperf

PRE_TARGETDEPS += $$PWD/gccopts_gperf.h
PRECOMPILED_HEADER = $$PWD/pch.h
CONFIG += precompile_header
PRECOMPILED_DIR = .pch
SOURCES += $$PWD/GccArguments.cpp \
           $$PWD/Path.cpp \
           $$PWD/RTags.cpp \
           $$PWD/Database.cpp \
           $$PWD/Mmap.cpp \
           $$PWD/FileDB.cpp \
           $$PWD/MmapDevice.cpp
HEADERS += $$PWD/GccArguments.h \
           $$PWD/Path.h \
           $$PWD/RTags.h \
           $$PWD/Location.h \
           $$PWD/Database.h \
           $$PWD/LevelDB.h \
           $$PWD/Mmap.h \
           $$PWD/FileDB.h \
           $$PWD/MmapDevice.h
LIBS += -lclang
include($$PWD/../3rdparty/leveldb.pri)
mac {
    LIBS += -L/opt/local/lib
    INCLUDEPATH += /opt/local/include
}

QMAKE_CXXFLAGS += -O2

valgrind {
    CONFIG += debug
    QMAKE_CXXFLAGS -= -O2
    QMAKE_CXXFLAGS += -fno-inline
}

unix {
    MOC_DIR = .moc
    OBJECTS_DIR = .obj
}

QT -= gui
macx {
    CONFIG -= app_bundle
    LIBS += -L/opt/local/lib
    INCLUDEPATH += /opt/local/include
}

