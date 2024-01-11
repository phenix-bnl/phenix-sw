AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libHardpDST.la

noinst_HEADERS = \
  PhCglLinkDef.h \
  PhPhotonLinkDef.h 

include_HEADERS = \
  PhCglList.h \
  PhCglListv2.h \
  PhCglListv3.h \
  PhCglListv4_Run7a.h \
  PhCglSngl.h \
  PhCglSnglv2.h \
  PhCglSnglv3.h \
  PhCglSnglv4_Run7a.h \
  PhPhotonList.h \
  PhPhotonListv2.h \
  PhPhotonListv4_Run7a.h \
  PhPhotonSngl.h \
  PhPhotonSnglv2.h 

libHardpDST_la_SOURCES = \
  PhCglList.C \
  PhCglListv2.C \
  PhCglListv3.C \
  PhCglListv4_Run7a.C \
  PhCglSngl.C \
  PhCglSnglv2.C \
  PhCglSnglv3.C \
  PhCglSnglv4_Run7a.C \
  PhPhotonList.C \
  PhPhotonListv2.C \
  PhPhotonListv4_Run7a.C \
  PhPhotonSngl.C \
  PhPhotonSnglv2.C \
  PhCglList_Dict.C \
  PhPhotonList_Dict.C 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libHardpDST_la_LIBADD = \
  -lnanoDST \
  -lemc \
  -lphool 

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libHardpDST.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.

PhCglList_Dict.C: PhCglList.h PhCglListv2.h PhCglListv3.h PhCglListv4_Run7a.h  PhCglSngl.h  PhCglSnglv4_Run7a.h PhCglSnglv2.h PhCglSnglv3.h PhCglLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

PhPhotonList_Dict.C: PhPhotonList.h PhPhotonListv2.h PhPhotonSngl.h PhPhotonSnglv2.h PhPhotonListv4_Run7a.h PhPhotonLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

clean-base:
	rm -f aclocal.m4 config.sub ltconfig mkinstalldirs configure ltmain.sh \
		Makefile config.guess depcomp config.log install-sh \
		Makefile.in config.status libtool missing

testexternals_SOURCES = testexternals.C
