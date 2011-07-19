INCLUDEPATH += $$PWD
LIBS += -lcplusplus -L$$PWD
unix {
    POST_TARGETDEPS += $$PWD/libcplusplus.a
}
