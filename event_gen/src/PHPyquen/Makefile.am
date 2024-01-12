AUTOMAKE_OPTIONS = foreign

# The flags -fno-automatic -finit-local-zero are taken from
# event_gen/lib/flags*mak -ffloat-store and/or -mfpmath=sse were
# suggested by Yue Shi Lai to address a possible g77 compiler bug
# where the RHS of a logical comparison is not precalculated.
# We use -msee instead of -msse2 in case there are PIII's floating
# around.
AM_FFLAGS = -Wall -fno-automatic -msse -mfpmath=sse -ffixed-line-length-none -fsecond-underscore

AM_CPPFLAGS = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(OPT_PHENIX)/lib
AM_CXXFLAGS = -Wall

lib_LTLIBRARIES = libPHPyquen.la

include_HEADERS = \
  PHPyquen.h

libPHPyquen_la_LDFLAGS = ${AM_LDFLAGS} `root-config --libs`
libPHPyquen_la_LIBADD = \
  -lphool -lSubsysReco -lfun4all -lPythia6 -lEG -lEGPythia6 -lPHPythia @FLIBS@

libPHPyquen_la_SOURCES = \
  pyquen.f \
  PHPyquen.C \
  PHPyquenDict.C

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libPHPyquen.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

PHPyquenDict.C: \
  PHPyquen.h \
  PHPyquenLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict*
