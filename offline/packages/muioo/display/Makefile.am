AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = libmuioo_display.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

libmuioo_display_la_LIBADD = \
 -lmutoo_display

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  TMuiCluster3D.h \
  TMuiIndex.h \
  TMuiPanel3D.h \
  TMuiRoad3D.h

##############################################
# please add new classes in alphabetical order

libmuioo_display_la_SOURCES = \
  TMuiCluster3D.cxx \
  TMuiIndex.cxx \
  TMuiPanel3D.cxx \
  TMuiRoad3D.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals
  
testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmuioo_display.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f $(BUILT_SOURCES)
