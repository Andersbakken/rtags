INCLUDEPATH += $$PWD
DEPENDPATH += $$PWD
SOURCES += $$PWD/GccArguments.cpp \
           $$PWD/Path.cpp \
           $$PWD/RTags.cpp \
           $$PWD/AtomicString.cpp \
           $$PWD/Database.cpp
HEADERS += $$PWD/GccArguments.h \
           $$PWD/Path.h \
           $$PWD/RTags.h \
           $$PWD/AtomicString.h \
           $$PWD/Location.h \
           $$PWD/Database.h \
           $$PWD/LevelDB.h
LIBS += -lclang
include($$PWD/../3rdparty/leveldb.pri)
mac {
    LIBS += -L/opt/local/lib
    INCLUDEPATH += /opt/local/include
}
