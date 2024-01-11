AUTOMAKE_OPTIONS = foreign

# List of shared libraries to produce

lib_LTLIBRARIES = librpcoo_display.la

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

librpcoo_display_la_LIBADD = \
 -lmutoo_display \
 -lrpcoo_base \
 -lrpcgeom

##############################################
# please add new classes in alphabetical order

include_HEADERS = \
  TRpcIndex.h \
  TRpcStrip3D.h	

##############################################
# please add new classes in alphabetical order

librpcoo_display_la_SOURCES = \
  TRpcIndex.cxx \
  TRpcStrip3D.cxx

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals
  
testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  librpcoo_display.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f $(BUILT_SOURCES)
