AUTOMAKE_OPTIONS = foreign

PACKAGE = pmonitor

include_HEADERS = \
  pmonitor.h \
  pmonstate.h \
  pmondisplay.h \
  pmongui.h \
  pMutex.h

LINKFILE = pmonitorLinkDef.h


noinst_HEADERS = $(LINKFILE)

BUILT_SOURCES = pmonitor_dict.C

ROOTCINT =  rootcint

INCLUDES =  -I$(includedir) -I$(ONLINE_MAIN)/include -I$(OFFLINE_MAIN)/include -I$(shell root-config --incdir)

lib_LTLIBRARIES = libpmonitor.la

libpmonitor_la_SOURCES = \
  pmonitor.cc \
  pmongui.cc \
  pmonitor.h \
  pmonstate.h \
  pmondisplay.h \
  pmongui.h \
  pMutex.cc \
  pmonitor_dict.C


bin_SCRIPTS = writePmonProject.pl

if FROG

libpmonitor_la_LIBADD = \
  -L$(libdir) \
  -L$(ONLINE_MAIN)/lib \
  -lEvent -let -lmessage \
  -L$(ROOTSYS)/lib -lThread -lpthread \
  -L$(OFFLINE_MAIN)/lib -lFROG

else

libpmonitor_la_LIBADD = \
  -L$(libdir) \
  -L$(ONLINE_MAIN)/lib \
  -lEvent -let -lmessage \
  -L$(ROOTSYS)/lib -lThread -lpthread \
  -L$(OFFLINE_MAIN)/lib 

endif

pmonitor_dict.C: pmonitor.h pmonstate.h pmondisplay.h pmongui.h $(LINKFILE)
	$(ROOTCINT) -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

CLEANFILES = *_dict.*
