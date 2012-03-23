TEMPLATE = app

libleveldb.commands = \$(MAKE) -C src
libleveldb.target = src/libleveldb.a

QMAKE_EXTRA_TARGETS += libleveldb
PRE_TARGETDEPS += src/libleveldb.a
QMAKE_POST_LINK = cp src/libleveldb.a ..

# nasty hack, not sure how else to prevent the link from being attempted
QMAKE_LINK = echo
