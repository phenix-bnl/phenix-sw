AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES =  -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

lib_LTLIBRARIES = \
  libutireactionplane.la

libutireactionplane_la_LIBADD = \
  -lbbc \
  -lcgl \
  -ldch \
  -lheader \
  -llvl2dstobjects \
  -lnanoDST \
  -lpdbcalBase \
  -ltrigger \
  -luti \
  -lvtx

noinst_HEADERS = \
  utiReactionPlaneLinkDef.h

include_HEADERS = \
  reactionPlane.hh \
  utiReactionPlane.h

libutireactionplane_la_SOURCES = \
  reactionPlane.C \
  utiReactionPlane.C \
  utiReactionPlane_Dict.C

utiReactionPlane_Dict.C: \
  reactionPlane.hh \
  utiReactionPlane.h \
  utiReactionPlaneLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libutireactionplane.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
