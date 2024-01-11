AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libmpcpi0ttree.la  

noinst_HEADERS = \
  mpcPi0TTreeLinkDef.h

include_HEADERS = \
  mpcPi0TTree.h

libmpcpi0ttree_la_SOURCES = \
  mpcPi0TTree_Dict.C\
  mpcPi0TTree.C

libmpcpi0ttree_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lSubsysReco \
  -lmpc

# Rule for generating CINT dictionaries from class headers.
mpcPi0TTree_Dict.C: mpcPi0TTree.h mpcPi0TTreeLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

