AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include 
  
lib_LTLIBRARIES = libcglVtx.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libcglVtx_la_LIBADD = \
  -lcgl \
  -ldch \
  -lgsl \
  -lpad \
  -lphool

include_HEADERS = cglVtx.h cglVtxv1.h

libcglVtx_la_SOURCES = cglVtx.C cglVtxv1.C cglVtxDict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libcglVtx.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

cglVtxDict.C: cglVtx.h cglVtxv1.h cglVtxLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^




testexternals_SOURCES = testexternals.C
