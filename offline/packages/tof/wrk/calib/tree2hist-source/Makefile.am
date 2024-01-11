AUTOMAKE_OPTIONS = foreign

INCLUDES = -I. -I$(srcdir) -I$(includedir) -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include


lib_LTLIBRARIES = libtof_calib.la

libtof_calib_la_LIBADD = -L$(OFFLINE_MAIN)/lib -lphool

noinst_HEADERS = \
	TofTrkTree.h      TofTrkTreeLinkDef.h     TofTrkTreeDict.h \
	TreeToHist.hh     TreeToHistLinkDef.h     TreeToHistDict.h \
	TreeToNtpl.hh     TreeToNtplLinkDef.h     TreeToNtplDict.h \
	Slewing.hh        SlewingLinkDef.h        SlewingDict.h

include_HEADERS = 

libtof_calib_la_SOURCES = \
	TofTrkTree.C      TofTrkTreeDict.C \
	TreeToHist.cc     TreeToHistDict.C \
	TreeToNtpl.cc     TreeToNtplDict.C \
	Slewing.cc        SlewingDict.C

# Rule for generating CINT dictionaries from class headers.
%Dict.C %Dict.h: %.hh
	rootcint -f $*Dict.C -c $(DEFS) $(INCLUDES) $*.hh
%Dict.C %Dict.h: %.h %LinkDef.h
	rootcint -f $*Dict.C -c $(DEFS) $(INCLUDES) $*.h $*LinkDef.h

clean-local:
	 rm -f $(BUILT_SOURCES)

