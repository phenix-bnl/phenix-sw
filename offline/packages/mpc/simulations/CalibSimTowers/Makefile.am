AUTOMAKE_OPTIONS = foreign

ACLOCAL_AMFLAGS = -I m4

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libanaCalibSimTowers.la

libanaCalibSimTowers_la_SOURCES = \
  CalibSimTowers.C \
  CalibSimTowersDict.C

libanaCalibSimTowers_la_LIBADD = \
 -L$(libdir) \
 -L${ROOTSYS}/lib \
 -L$(OFFLINE_MAIN)/lib \
 -lSubsysReco \
 -lmpc \
 -lbbc


CalibSimTowersDict.C: \
  CalibSimTowers.h \
  CalibSimTowersLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

