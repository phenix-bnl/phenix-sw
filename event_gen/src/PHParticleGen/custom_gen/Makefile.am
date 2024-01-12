AUTOMAKE_OPTIONS = foreign

AM_CPPFLAGS = -I${top_srcdir} -I$(includedir) -I$(OFFLINE_MAIN)/include -I@ROOTINC@
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

lib_LTLIBRARIES = libPHParticleCustomGen.la

noinst_HEADERS = \
  TSingleParticleCustomGeneratorLinkDef.h

include_HEADERS = \
  TSingleParticleCustomGenerator.h \
  MpcSingleParticleGen.h \
  TSingleDYGenerator.h

libPHParticleCustomGen_la_SOURCES = \
  TSingleParticleCustomGenerator_Dict.C \
  TSingleParticleCustomGenerator.C \
  MpcSingleParticleGen.C \
  TSingleDYGenerator.C

libPHParticleCustomGen_la_LIBADD = \
  ${top_builddir}/libPHParticleGen.la

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libPHParticleCustomGen.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

TSingleParticleCustomGenerator_Dict.C: TSingleParticleCustomGenerator.h MpcSingleParticleGen.h TSingleDYGenerator.h TSingleParticleCustomGeneratorLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict*

