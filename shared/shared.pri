INCLUDEPATH += $$PWD $$PWD/messages
DEPENDPATH += $$PWD

OBJECTS_DIR = .obj
MOC_DIR = .moc

QT -= gui
QT += network

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
    $$PWD/Tools.h

noinline {
    QMAKE_CXXFLAGS += -fno-inline
    QMAKE_CFLAGS += -fno-inline
}
