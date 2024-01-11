AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

# List of shared libraries to produce
lib_LTLIBRARIES = \
  libuti.la \
  libutiMem.la \
  libTHmul.la

bin_PROGRAMS = hmerge haddPhenix

include_HEADERS = \
  CentralityParameters.h \
  reactionPlane.hh \
  THmulf.h \
  PHAverager.h \
  utiMatch.h \
  utiCentrality.h \
  utiPrototype.hh \
  utiHadPid.h \
  utiMemoryWatcher.h

libuti_la_SOURCES = \
  CentralityParameters.C \
  reactionPlane.C \
  utiMatch.cc \
  utiCentrality.C \
  utiGaussian.c \
  utiLandau.c \
  utiLineCircle.c \
  utiLocaltoGlobal.c \
  utiPolya.c \
  utiRPhitoXY.c \
  utiRandom.c \
  utiXYtoRPhi.c \
  utiPolyint.C \
  utiHadPid.cc


AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libuti_la_LIBADD = \
  -lbbc \
  -lheader \
  -lnanoDST \
  -lphool \
  -lzdc 

libutiMem_la_SOURCES = \
  utiMemoryWatcher.C \
  utiMemoryWatcherDict.C

libTHmul_la_SOURCES = \
  THmulf.C \
  THmul_Dict.C

libTHmul_la_LDFLAGS = `root-config --libs`
libTHmul_la_LIBADD = -lTreePlayer

hmerge_SOURCES = hmerge.C

hmerge_LDADD = libTHmul.la

haddPhenix_SOURCES = haddPhenix.C

haddPhenix_LDADD = \
  libTHmul.la

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libuti.la \
  libutiMem.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%Dict.C: %.hh %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

THmul_Dict.C : THmulf.h THmulLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
