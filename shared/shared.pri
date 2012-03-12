INCLUDEPATH += $$PWD $$PWD/messages
DEPENDPATH += $$PWD $$PWD/messages

OBJECTS_DIR = .obj
MOC_DIR = .moc

QT = core network

SOURCES += \
    $$PWD/Connection.cpp \
    $$PWD/Path.cpp \
    $$PWD/Messages.cpp \
    $$PWD/messages/AddMessage.cpp \
    $$PWD/messages/QueryMessage.cpp \
    $$PWD/messages/ErrorMessage.cpp

HEADERS += \
    $$PWD/Connection.h \
    $$PWD/Path.h \
    $$PWD/Message.h \
    $$PWD/Messages.h \
    $$PWD/messages/AddMessage.h \
    $$PWD/messages/QueryMessage.h \
    $$PWD/messages/ErrorMessage.h \
    $$PWD/Shared.h

PRECOMPILED_HEADER = $$PWD/Pch.h
CONFIG += precompile_header
PRECOMPILED_DIR = .pch

noinline {
    QMAKE_CXXFLAGS += -fno-inline
    QMAKE_CFLAGS += -fno-inline
}
CLANG_LIBS = $(CLANG_ROOT)/lib
CLANG_INCLUDE = $(CLANG_ROOT)/include
LIBS += -lclang -lcrypto -L../3rdparty -lleveldb -lz -llzma -L$${CLANG_LIBS} -Wl,-rpath,$${CLANG_LIBS}
INCLUDEPATH += $${CLANG_INCLUDE}
