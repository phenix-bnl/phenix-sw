AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libemcAnalyzer.la

noinst_HEADERS =  emcAnalyzerUtilsLinkDef.h emcAnalyzerCorrFactorsLinkDef.h emcAnalyzerLinkDef.h

include_HEADERS = emcAnalyzerUtils.h emcAnalyzerCorrFactors.h emcEfficiency_7percent_counts.h emcReadOtherRAA.h emcAnalyzer.h 

libemcAnalyzer_la_SOURCES = \
	emcAnalyzerUtils.C \
	emcAnalyzerUtilsDict.C \
	emcAnalyzerCorrFactors.C \
	emcAnalyzerCorrFactorsDict.C \
	emcAnalyzer.C \
	emcAnalyzerDict.C

%Dict.C: %.h %LinkDef.h
	rootcint -v -f $@ -c $(DEFS) $(INCLUDES) $(INCLUDES) $^

clean:
	rm -rf *Dict*
	rm -rf *dict*
