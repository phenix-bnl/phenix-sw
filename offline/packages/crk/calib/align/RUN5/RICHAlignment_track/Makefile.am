AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  librichalignment_track.la 

noinst_HEADERS = \
  RICHAlignment_trackLinkDef.h 

include_HEADERS = \
  RICHAlignment_track.h

librichalignment_track_la_SOURCES = \
  RICHAlignment_track.C \
  RICHAlignment_track_Dict.C

librichalignment_track_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lSubsysReco

# Rule for generating CINT dictionaries from class headers.
RICHAlignment_track_Dict.C: \
  RICHAlignment_track.h \
  RICHAlignment_trackLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

