AUTOMAKE_OPTIONS = foreign

# The flags -fno-automatic -finit-local-zero are taken from
# event_gen/lib/flags*mak -ffloat-store and/or -mfpmath=sse were
# suggested by Yue Shi Lai to address a possible g77 compiler bug
# where the RHS of a logical comparison is not precalculated.
# We use -msee instead of -msse2 in case there are PIII's floating
# around.
AM_FFLAGS = -Wall -fno-automatic -msse -mfpmath=sse -ffixed-line-length-none -fsecond-underscore


AM_CPPFLAGS = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(OPT_PHENIX)/lib `root-config --libs`
AM_CXXFLAGS = -Wall

lib_LTLIBRARIES = \
  libTQPythia6.la \
  libPHQPythia.la

include_HEADERS = \
  TQPythia6.h \
  PHQPythia.h

libTQPythia6_la_LIBADD = \
  -lPythia6 \
  -lEG \
  -lEGPythia6 \
  $(FLIBS)

libTQPythia6_la_SOURCES = \
  q-pyshow.1.0.2.f \
  TQPythia6.C \
  TQPythia6Dict.C

libPHQPythia_la_LIBADD = \
  libTQPythia6.la \
  -lphool \
  -lSubsysReco \
  -lPHPythia \
  -lfun4all

libPHQPythia_la_SOURCES = \
  PHQPythia.C \
  PHQPythiaDict.C

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libTQPythia6.la \
  libPHQPythia.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

TQPythia6Dict.C: \
  TQPythia6.h \
  TQPythia6LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

PHQPythiaDict.C: \
  PHQPythia.h \
  PHQPythiaLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict*
