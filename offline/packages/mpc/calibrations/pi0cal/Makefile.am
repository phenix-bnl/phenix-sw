AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libMpcPi0Cal.la   

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libMpcPi0Cal_la_LIBADD = \
  -L$(libdir) \
  -L${ROOTSYS}/lib \
  -L$(OFFLINE_MAIN)/lib \
  -lphool \
  -lrecal \
  -luti \
  -ltrigger \
  -lPISARoot \
  -lnanoDST \
  -lPHPythia \
  -lmpc

noinst_HEADERS = \
  MpcPi0CalLinkDef.h

include_HEADERS = \
  MpcPi0Mass.h \
  MpcPi0MassContainer.h \
  MpcPi0Cal.h \
  pi0base.h \
  pi0mixer.h

libMpcPi0Cal_la_SOURCES = \
  MpcPi0Mass.C \
  MpcPi0Cal.C \
  pi0base.C \
  MpcPi0CalCuts.C \
  MpcPi0CalSet.C \
  pi0mixer.C \
  MpcPi0Cal_Dict.C

# Rule for generating CINT dictionaries from class headers.
MpcPi0Cal_Dict.C: MpcPi0Mass.h MpcPi0Cal.h pi0base.h pi0mixer.h MpcPi0MassContainer.h \
            MpcPi0CalLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libMpcPi0Cal.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict*
