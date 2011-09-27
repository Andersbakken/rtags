TEMPLATE = app
TARGET = rd
DESTDIR = ..
DEPENDPATH += .
INCLUDEPATH += .

macx {
    CONFIG -= app_bundle
    INCLUDEPATH += /opt/local/include
    LIBS += -L/opt/local/lib
}

QT =

LIBS += -lclang
SOURCES += main.cpp

release {
    QMAKE_CXXFLAGS += -g
    QMAKE_CFLAGS += -g
}

unix {
    debug:OBJECTS_DIR = $${OUT_PWD}/.obj/debug-shared
    release:OBJECTS_DIR = $${OUT_PWD}/.obj/release-shared

    debug:MOC_DIR = $${OUT_PWD}/.moc/debug-shared
    release:MOC_DIR = $${OUT_PWD}/.moc/release-shared

    RCC_DIR = $${OUT_PWD}/.rcc/
    UI_DIR = $${OUT_PWD}/.uic/
}
