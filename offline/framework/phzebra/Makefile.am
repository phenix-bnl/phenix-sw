AUTOMAKE_OPTIONS = foreign

# enable all warnings -Wall
# cfortran.h has a few unused functions, suppress unused warnings: -Wno-unused
# treat warnings as errors: -Werror
AM_CXXFLAGS = -Wall -Df2cFortran -Wno-unused -Werror
AM_CFLAGS = -Wall -Werror

AM_FFLAGS = -Wall #gfortran

AM_CPPFLAGS = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include \
  -I$(CERN)/$(CERN_LEVEL)/include

lib_LTLIBRARIES = libPHGeant.la

libPHGeant_la_SOURCES = \
   staf_attach_geant_.F \
   PHGeant.C \
   PHGeantDict.C \
   glue.c \
   zebra_inline_compress.F \
   zebra_init.F

libPHGeant_la_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  `$(OPT_PHENIX)/bin/phenix_cernlib.pl` \
  `root-config --libs`

#  -lgfortran

include_HEADERS = \
  cfortran.h \
  PHGeant.hh

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libPHGeant.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionary for PHGeant.hh
PHGeantDict.C : PHGeant.hh PHGeantLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	 rm -f *Dict*
