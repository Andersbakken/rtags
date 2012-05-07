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
    $$PWD/messages/ErrorMessage.cpp \
    $$PWD/Log.cpp \
    $$PWD/RTags.cpp

HEADERS += \
    $$PWD/Connection.h \
    $$PWD/Path.h \
    $$PWD/Message.h \
    $$PWD/Messages.h \
    $$PWD/messages/AddMessage.h \
    $$PWD/messages/QueryMessage.h \
    $$PWD/messages/ErrorMessage.h \
    $$PWD/RTags.h \
    $$PWD/Log.h

PRECOMPILED_HEADER = Pch.h
CONFIG += precompile_header
PRECOMPILED_DIR = .pch

noinline {
    QMAKE_CXXFLAGS += -fno-inline
    QMAKE_CFLAGS += -fno-inline
}
CONFIG -= app_bundle
