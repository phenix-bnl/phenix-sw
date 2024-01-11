AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libRICH_Alignment.la 

noinst_HEADERS = \
  RICH_AlignmentLinkDef.h \
  CrkGeometryObject_newLinkDef.hh

include_HEADERS = \
  RICH_Alignment.h \
  CrkGeometryObject_new.hh

libRICH_Alignment_la_SOURCES = \
  RICH_Alignment.cc \
  RICH_Alignment_Dict.C \
  CrkGeometryObject_new.cc \
  CrkGeometryObject_new_Dict.C 

libRICH_Alignment_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lSubsysReco \
  -lpdbcalBase \
  -lphgeo \
  -ldgo \
  -ldch

# Rule for generating CINT dictionaries from class headers.
RICH_Alignment_Dict.C: \
  RICH_Alignment.h \
  RICH_AlignmentLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

CrkGeometryObject_new_Dict.C: \
  CrkGeometryObject_new.hh \
  CrkGeometryObject_newLinkDef.hh
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local: 
	rm -f *Dict*

