AUTOMAKE_OPTIONS = foreign

######################################
# this is the generic include path to be used
AM_CPPFLAGS = \
  -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

lib_LTLIBRARIES = libsvxgeo.la

AM_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  `root-config --libs` \
  `root-config --evelibs`

##############################################
# please add new classes in alphabetical order

noinst_HEADERS = \
  SvxGeoLinkDef.h

include_HEADERS = \
  SvxGeoTrack.h \
  SvxProj.h \
  SvxTGeo.h

##############################################
# please add new classes in alphabetical order

libsvxgeo_la_SOURCES = \
  SvxGeoTrack.C \
  SvxProj.C \
  SvxTGeo.C \
  SvxGeoDict.C

BUILT_SOURCES = testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C

testexternals_LDADD = \
  libsvxgeo.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

SvxGeoDict.C: \
	SvxGeoTrack.h \
	SvxProj.h \
	SvxTGeo.h \
	SvxGeoLinkDef.h
	rootcint -f $@ -c $(CINTFLAGS) $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict* $(BUILT_SOURCES)

