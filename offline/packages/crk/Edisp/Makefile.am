AUTOMAKE_OPTIONS = foreign

CXXFLAGS = -g

# List of shared libraries to produce
lib_LTLIBRARIES = libEdisp.la

noinst_HEADERS = Edisp.h

OFFLINE_INC = -I$(OFFLINE_MAIN)/include

INCLUDES  = -I. -I$(includedir) $(OFFLINE_INC) -I$(EVT_INC) -I$(ROOTSYS)/include

SYSLIBS   = -L/usr/X11R6/lib -lX11 -lXpm -ldl

BUILT_SOURCES = Edispdict.cc Edispdict.h

libEdisp_la_SOURCES = Edisp.cc Edispdict.cc

# Explicit rule to make dictionary

Edispdict.h Edispdict.cc: Edisp.h EdispLinkDef.h
	rootcint -f Edispdict.cc -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) Edisp.h EdispLinkDef.h

clean-local:
	 rm -f $(BUILT_SOURCES)




