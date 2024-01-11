AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libMpcEx.la \
  libMpcExReco.la 

noinst_HEADERS = \
  MpcExRecoLinkDef.h  \
  MpcExLinkDef.h 

include_HEADERS = \
  MpcExEventHeader.h \
  MpcExEventHeaderv1.h \
  MpcExEventHeaderv2.h \
  MpcExRawHit.h \
  MpcExRawHitv1.h


libMpcEx_la_SOURCES = \
  MpcExRawHitv1.C \
  MpcExEventHeaderv1.C \
  MpcExEventHeaderv2.C \
  MpcEx_Dict.C

libMpcExReco_la_SOURCES = \
  MpcExCompactReco.C \
  MpcExReco_Dict.C

libMpcEx_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lphool

libMpcExReco_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  libMpcEx.la \
  -lSubsysReco

# Rule for generating CINT dictionaries from class headers.
MpcEx_Dict.C: \
  MpcExRawHit.h \
  MpcExRawHitv1.h \
  MpcExEventHeader.h \
  MpcExEventHeaderv1.h \
  MpcExEventHeaderv2.h \
  MpcExLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

# Rule for generating CINT dictionaries from class headers.
MpcExReco_Dict.C: \
  MpcExCompactReco.h \
  MpcExRecoLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

