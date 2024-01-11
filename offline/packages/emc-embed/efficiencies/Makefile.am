AUTOMAKE_OPTIONS = foreign

ROOTCINT = rootcint

#bin_PROGRAMS = efficiencies hmerge2

#hmerge2_SOURCES = hmerge2.C
#hmerge2_LDADD = libemcEfficiency.la

#efficiencies_SOURCES = efficiencies.C
#efficiencies_LDADD = libemcEfficiency.la

lib_LTLIBRARIES = libemcEfficiency.la

libemcEfficiency_la_SOURCES = emcEfficiency.C emcEfficiencyDict.C PHNodeHelper.C utils.C HistogramCollection.C effParticle.C effPair.C EventSorting.C EventSortingDict.C

libemcEfficiency_la_LIBADD = -L$(libdir) -L$(OFFLINE_MAIN)/lib -lemc -L$(ROOTSYS)/lib -lEG

INCLUDES = -I. -I$(includedir) -I$(ROOTSYS)/include -I$(OFFLINE_MAIN)/include

EventSortingDict.C: EventSorting.h EventSortingLinkDef.h
	$(ROOTCINT) -f $@ -c $(CINTFLAGS) $(INCLUDES) $^

emcEfficiencyDict.C: emcEfficiency.h effParticle.h effPair.h emcEfficiencyLinkDef.h
	$(ROOTCINT) -f $@ -c $(CINTFLAGS) $(INCLUDES) $^

clean-local:
	rm -f *_dict*
