AUTOMAKE_OPTIONS = foreign

SUBDIRS = . custom_gen

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

lib_LTLIBRARIES = libPHParticleGen.la

noinst_HEADERS = 

include_HEADERS = \
  TSingleParticleGenerator.h \
  TStarlightGenerator.h

libPHParticleGen_la_SOURCES = \
  PHFvtxTrigger.C \
  PHFvtxTrigger_Dict.C \
  PHParticleGen.C \
  PHParticleGen_Dict.C \
  TSingleParticleGenerator.C \
  TSingleParticleGenerator_Dict.C \
  TStarlightGenerator.C \
  TStarlightGenerator_Dict.C

libPHParticleGen_la_LIBADD = \
  -lPHPythia -lPHPyTrigger -lgsl -lgslcblas

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libPHParticleGen.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

%_Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

