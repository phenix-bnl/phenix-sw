
AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libmuioo_classes.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libmuioo_classes_la_LIBADD = \
 -lmuioo_interface

include_HEADERS = \
  TMuiErrorStats.h \
  TMuiRoadFinder.h \
  TMuiRoadOGroup.h

libmuioo_classes_la_SOURCES = \
  TMuiErrorStats.cxx \
  TMuiRoadFinder.cxx \
  TMuiRoadOGroup.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmuioo_classes.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)
