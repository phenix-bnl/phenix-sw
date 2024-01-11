AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

lib_LTLIBRARIES = libzutilities.la

include_HEADERS = \
  zutility.h

libzutilities_la_SOURCES = \
  zutility.cc

dnl bin_PROGRAMS = tdcoverflow adcgain tdcgain zdcdbase slewfit
bin_PROGRAMS = tdcoverflow adcgain tdcgain zdcdbase slewfit

tdcoverflow_SOURCES = \
  tdcoverflow.cc

tdcoverflow_LDADD = @ROOTGLIBS@ -lEvent -lzutilities

adcgain_SOURCES = \
  adcgain.cc

adcgain_LDADD = @ROOTGLIBS@ -lEvent -lzutilities

tdcgain_SOURCES = \
  tdcgain.cc

tdcgain_LDADD = @ROOTGLIBS@ -lEvent -lzutilities

slewfit_SOURCES = \
  slewfit.cc

slewfit_LDADD = @ROOTGLIBS@ -lzutilities

zdcdbase_SOURCES = \
  zdcdbase.cc

zdcdbase_LDADD = @ROOTGLIBS@ -lzdc -lphtime -lpdbcalBase -lPgCal

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f $(BUILT_SOURCES) *Dict*

