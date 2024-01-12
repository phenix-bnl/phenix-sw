AUTOMAKE_OPTIONS = foreign

AM_CPPFLAGS = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib
AM_CXXFLAGS = -Wall -Werror

lib_LTLIBRARIES = libPHPyParticleSelect.la

libPHPyParticleSelect_la_LIBADD = \
  -lPHPythia \
  -lPHHepMCNode

include_HEADERS = \
  PHPyParticleSelect.h

libPHPyParticleSelect_la_SOURCES = \
  PHPyParticleSelect_Dict.C \
  PHPyParticleSelect.C \
  PHPySelectUpsilon.C \
  PHPySelectChic.C \
  PHPySelectOnia.C \
  PHPySelectDY.C \
  PHPySelectOpen.C \
  PHPySelectD0Kpi.C \
  PHPySelectEfromCharm.C \
  PHPySelectDstar.C \
  PHPySelectChiC2.C \
  PHPySelectEfromBottom.C \
  PHPySelectStable.C

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libPHPyParticleSelect.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

PHPyParticleSelect_Dict.C: \
  PHPyParticleSelect.h \
  PHPySelectUpsilon.h \
  PHPySelectChic.h \
  PHPySelectOnia.h \
  PHPySelectDY.h \
  PHPySelectOpen.h \
  PHPySelectD0Kpi.h \
  PHPySelectEfromCharm.h \
  PHPySelectDstar.h \
  PHPySelectChiC2.h \
  PHPySelectEfromBottom.h \
  PHPySelectStable.h \
  PHPyParticleSelectLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict*
	rm -f testexternals*
