AUTOMAKE_OPTIONS = foreign

AM_CPPFLAGS = -I$(includedir) -I$(OFFLINE_MAIN)/include -I@ROOTINC@
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib @ROOTLIBS@

lib_LTLIBRARIES = \
  libQPythiaAna.la

noinst_headers = \
  QPythiaAna.h

libQPythiaAna_la_LIBADD = \
  -lPHPythia

libQPythiaAna_la_SOURCES = \
  QPythiaAna.C \
  QPythiaAnaDict.C

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libQPythiaAna.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

QPythiaAnaDict.C: \
  QPythiaAna.h \
  QPythiaAnaLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict*
