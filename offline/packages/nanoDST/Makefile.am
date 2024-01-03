AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libnanoDST.la

noinst_HEADERS = \
  PHParticleLinkDef.h \
  PHGlobalLinkDef.h \
  PHnanoCutsLinkDef.h \
  DepObjLinkDef.h

include_HEADERS = \
  MvdRpParams.h \
  DepObj.h \
  DepObjv1.h \
  DepObjv2.h \
  PHParticle.h \
  PHCentralTrack.h \
  PHSnglCentralTrack.h \
  PHGlobal.h	\
  PHGlobalv1.h  \
  PHGlobalv2.h  \
  PHGlobalv3.h  \
  PHGlobalv4.h  \
  PHGlobalv5.h  \
  PHGlobalv6.h  \
  PHGlobalv7.h  \
  PHGlobalv8.h  \
  PHGlobalv9.h  \
  PHGlobalv10.h  \
  PHGlobalv11.h  \
  PHGlobal_Central.h  \
  PHGlobal_Centralv1.h  \
  PHGlobal_Muon.h  \
  PHGlobal_Muonv1.h  \
  PHInclusiveNanoCuts.h \
  PHnanoCuts.h \
  PHMuoTracksOut.h

libnanoDST_la_SOURCES = \
 MvdRpParams.C \
 DepObj.C \
 DepObjv1.C \
 DepObjv2.C \
 PHParticle.C \
 PHCentralTrack.C \
 PHSnglCentralTrack.C \
 PHParticle_Dict.C \
 PHGlobal.C \
 PHGlobalv1.C \
 PHGlobalv2.C \
 PHGlobalv3.C \
 PHGlobalv4.C \
 PHGlobalv5.C \
 PHGlobalv6.C \
 PHGlobalv7.C \
 PHGlobalv8.C \
 PHGlobalv9.C \
 PHGlobalv10.C \
 PHGlobalv11.C \
 PHGlobal_Central.C  \
 PHGlobal_Centralv1.C  \
 PHGlobal_Muon.C  \
 PHGlobal_Muonv1.C  \
 PHGlobal_Dict.C \
 PHInclusiveNanoCuts.C \
 PHnanoCuts.C \
 PHnanoCuts_Dict.C \
 PHMuoTracksOut.C \
 DepObj_Dict.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libnanoDST_la_LIBADD = -lfun4all

noinst_PROGRAMS = testexternals
testexternals_SOURCES = testexternals.C
testexternals_LDADD = libnanoDST.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

PHParticle_Dict.C: \
  PHParticle.h \
  PHCentralTrack.h \
  PHSnglCentralTrack.h \
  PHMuoTracksOut.h \
  PHParticleLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

PHGlobal_Dict.C:     \
  PHGlobal.h   \
  PHGlobalv1.h \
  PHGlobalv2.h \
  PHGlobalv3.h \
  PHGlobalv4.h \
  PHGlobalv5.h \
  PHGlobalv6.h \
  PHGlobalv7.h \
  PHGlobalv8.h \
  PHGlobalv9.h \
  PHGlobalv10.h \
  PHGlobalv11.h \
  PHGlobal_Central.h \
  PHGlobal_Centralv1.h \
  PHGlobal_Muon.h \
  PHGlobal_Muonv1.h \
  PHGlobalLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

PHnanoCuts_Dict.C:  PHnanoCuts.h PHInclusiveNanoCuts.h PHnanoCutsLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

DepObj_Dict.C:        \
	DepObj.h \
	DepObjv1.h \
	DepObjv2.h \
	DepObjLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*
