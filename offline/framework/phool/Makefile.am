AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libphool.la

BUILT_SOURCES = \
  testexternals.C

INCLUDES := -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`

libphool_la_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  `root-config --libs`
libphool_la_LIBADD = \
  -lEvent

libphool_la_SOURCES = \
  PHObject.C \
  PHNode.C \
  PHTimer.C \
  PHTimeServer.C \
  PHCompositeNode.C \
  PHRawDataNode.C \
  PHNodeIterator.C \
  PHMessage.C \
  PHIOManager.C \
  PHNodeIOManager.C \
  PHRawOManager.C \
  PHOperation.C \
  PHNodeReset.C \
  PHBase_dict.C \
  PHString.C

include_HEADERS =  \
  PHObject.h \
  PHCompositeNode.h \
  PHDataNode.h \
  PHIODataNode.h \
  PHRawDataNode.h \
  PHNode.h \
  PHIOManager.h \
  PHNodeIOManager.h \
  PHRawOManager.h \
  PHOperation.h \
  PHNodeOperation.h \
  PHNodeReset.h \
  PHNodeIterator.h \
  PHPointerList.h \
  PHPointerListIterator.h \
  PHString.h \
  PHTimer.h \
  PHTimeServer.h \
  phool.h \
  PHDataNodeIterator.h \
  PHTypedNodeIterator.h

# for compatibility for sPHENIX, install headers in subdir
pkginclude_HEADERS =  $(include_HEADERS)

noinst_HEADERS = \
  PHBase_LinkDef.h

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C

testexternals_LDADD = \
  libphool.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@


PHBase_dict.C : PHString.h phool.h PHObject.h PHTimeServer.h PHBase_LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *_dict*
	rm -rf cxx_repository
	rm -f testexternals.C
