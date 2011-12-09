INCLUDEPATH += $$PWD
DEPENDPATH += $$PWD
SOURCES += $$PWD/GccArguments.cpp \
           $$PWD/Path.cpp \
           $$PWD/RTags.cpp \
           $$PWD/AtomicString.cpp \
           $$PWD/Database.cpp \
           $$PWD/Mmap.cpp \
           $$PWD/FileDB.cpp \
           $$PWD/MmapDevice.cpp
HEADERS += $$PWD/GccArguments.h \
           $$PWD/Path.h \
           $$PWD/RTags.h \
           $$PWD/AtomicString.h \
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
