AUTOMAKE_OPTIONS = foreign

ACLOCAL_AMFLAGS = -I m4

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libmpconline.la 

noinst_HEADERS = \
  MpcOnlineLinkDef.h 

include_HEADERS = \
  MpcSimpleEvtDisp.h \
  MpcTriggerStudy.h \
  MpcMip.h \
  MpcCheck.h

libmpconline_la_SOURCES = \
  MpcSimpleEvtDisp.C \
  MpcTriggerStudy.C \
  MpcMip.C \
  MpcCheck.C \
  MpcOnline_Dict.C

libmpconline_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lSubsysReco

# Rule for generating CINT dictionaries from class headers.
MpcOnline_Dict.C: MpcSimpleEvtDisp.h MpcTriggerStudy.h MpcMip.h MpcCheck.h MpcOnlineLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

