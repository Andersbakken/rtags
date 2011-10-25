INCLUDEPATH += $$PWD
SOURCES += $$PWD/GccArguments.cpp $$PWD/Path.cpp $$PWD/RTags.cpp
HEADERS += $$PWD/GccArguments.h $$PWD/Path.h $$PWD/RTags.h
LIBS += -lmagic -lleveldb
include($$PWD/../3rdparty/leveldb.pri)
