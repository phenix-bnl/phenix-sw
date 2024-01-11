AUTOMAKE_OPTIONS = foreign

CXXFLAGS = -g

# List of shared libraries to produce
lib_LTLIBRARIES = libCrkCalib.la

noinst_HEADERS = CrkCalib.h

OFFLINE_INC = -I$(OFFLINE_MAIN)/include

INCLUDES  = -I. -I$(includedir) $(OFFLINE_INC) -I$(ROOTSYS)/include

SYSLIBS   = -L/usr/X11R6/lib -lX11 -lXpm -ldl

BUILT_SOURCES = CrkCalibdict.cc CrkCalib.h

libCrkCalib_la_SOURCES = CrkCalib.h CrkCalibdict.cc

# Explicit rule to make dictionary

CrkCalibdict.h CrkCalibdict.cc: CrkCalib.h CrkCalibLinkDef.h
	rootcint -f CrkCalibdict.cc -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) CrkCalib.h CrkCalibLinkDef.h

clean-local:
	 rm -f $(BUILT_SOURCES)




