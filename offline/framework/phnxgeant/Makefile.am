AUTOMAKE_OPTIONS = foreign

AM_FFLAGS = -funroll-loops -fomit-frame-pointer -fPIC -fno-second-underscore -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_GFORTRAN -DCERNLIB_BLDLIB

AM_CPPFLAGS = \
  -I$(includedir) \
  -I$(CERN)/$(CERN_LEVEL)/include

lib_LTLIBRARIES = libphnxgeant.la

libphnxgeant_la_SOURCES = \
   ggclos.F \
   gtnext.F \
   bimsel.F \
   gskine.F \
   gfkine.F \
   gltrac.F \
   gpkine.F \
   gsvert.F \
   gfvert.F \
   gpvert.F \
   gtreve.F \
   glund.F  \
   gpjxyz.F \
   gsveru.F \
   lgkine.F \
   lgvert.F \
   lucomp.F \
   lueevt.F

libphnxgeant_la_LDFLAGS = \
  $(FLIBS)

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.cc

testexternals_LDADD = \
  libphnxgeant.la

testexternals_LDFLAGS = \
  `$(OPT_PHENIX)/bin/phenix_cernlib.pl`


testexternals.cc:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm $(testexternals_SOURCES)
