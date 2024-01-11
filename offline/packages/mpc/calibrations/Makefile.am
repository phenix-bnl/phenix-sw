AUTOMAKE_OPTIONS = foreign

ACLOCAL_AMFLAGS = -I m4

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

lib_LTLIBRARIES = libmpccalib.la   

noinst_HEADERS = \
  MpcCalibDict.h \
  MpcCalibLinkDef.h

include_HEADERS = \
  MpcPed.h \
  MpcInvSlope.h \
  MpcTime.h \
  MpcHiLo.h

libmpccalib_la_SOURCES = \
  MpcCalibDict.C \
  MpcPed.cc \
  MpcInvSlope.C \
  MpcTime.C \
  MpcHiLo.C

libmpccalib_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lSubsysReco \
  -lmpc 

bin_PROGRAMS = mpcped mpcled mpcdbase

mpcped_SOURCES = mpcped.cc

mpcped_LDADD = @ROOTGLIBS@ -lEvent libmpccalib.la

mpcled_SOURCES = mpcled.cc

mpcled_LDADD = @ROOTGLIBS@ -lEvent -lmpc

mpcdbase_SOURCES = mpcdbase.cc

mpcdbase_LDADD = -lmpc -lphtime -lpdbcalBase -lPgCal

MpcCalibDict.C: MpcTime.h MpcHiLo.h MpcInvSlope.h MpcCalibLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f $(BUILT_SOURCES) *Dict*

