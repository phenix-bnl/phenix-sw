AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(ROOTSYS)/include -I$(OFFLINE_MAIN)/include

lib_LTLIBRARIES = libmutwrk.la 

libmutwrk_la_LIBADD = -L$(OFFLINE_MAIN)/lib -lphool -lmutgeom

libmutwrk_la_SOURCES =  updateDCMChannels.C  updateDCMChannelsDict.C 
  
include_HEADERS =  updateDCMChannels.h 

noinst_HEADERS=  updateDCMChannelsLinkDef.h 

# Rule for generating CINT dictionaries from class headers.
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f $(BUILT_SOURCES)
