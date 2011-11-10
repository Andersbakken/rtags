INCLUDEPATH += $$PWD
DEPENDPATH += $$PWD
SOURCES += $$PWD/GccArguments.cpp \
           $$PWD/Path.cpp \
           $$PWD/RTags.cpp \
           $$PWD/AtomicString.cpp 
HEADERS += $$PWD/GccArguments.h \
           $$PWD/Path.h \
           $$PWD/RTags.h \
           $$PWD/CursorKey.h \
           $$PWD/AtomicString.h
LIBS += -lleveldb -lclang
include($$PWD/../3rdparty/leveldb.pri)
mac {
    LIBS += -L/opt/local/lib
    INCLUDEPATH += /opt/local/include
}
