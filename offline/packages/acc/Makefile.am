AUTOMAKE_OPTIONS = foreign

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include

lib_LTLIBRARIES = \
  libacc.la 

BUILT_SOURCES = \
  testexternals.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libacc_la_LIBADD = \
  -lheader \
  -lpdbcalBase \
  -lphgeo \
  -lphool \
  -lphtime \
  -lPISARoot

noinst_HEADERS = \
  AccClassLinkDef.h

include_HEADERS = \
  Acc.h  \
  AccGeometry.h \
  AccSnglRaw.h \
  AccSnglRawv1.h \
  AccSnglRawv2.h \
  AccRaw.h \
  AccRawv1.h \
  AccRawv2.h \
  AccSnglHit.h \
  AccSnglHitv1.h \
  AccHit.h \
  AccHitv1.h \
  AccSim.h \
  AerGeaHits.h \
  AerGeaHitsv1.h \
  AccSnglCluster.h \
  AccSnglClusterv1.h \
  AccCluster.h \
  AccClusterv1.h \
  AccCalibPar.h \
  AccCalib.h \
  AccEvent.h \
  AccProj.h 

libacc_la_SOURCES = \
  Acc.C \
  AccGeometry.C \
  AccSnglRaw.C \
  AccSnglRawv1.C \
  AccSnglRawv2.C \
  AccRaw.C \
  AccRawv1.C \
  AccRawv2.C \
  AccSnglHit.C \
  AccSnglHitv1.C \
  AccHit.C \
  AccHitv1.C \
  AccSim.C \
  AerGeaHits.C \
  AerGeaHitsv1.C \
  AccSnglCluster.C \
  AccSnglClusterv1.C \
  AccCluster.C \
  AccClusterv1.C \
  AccClass_Dict.C \
  AccCalibPar.C \
  AccCalib.C \
  AccEvent.C \
  AccProj.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libacc.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

AccClass_Dict.C: \
   AccSnglRaw.h \
   AccSnglRawv1.h \
   AccSnglRawv2.h \
   AccRaw.h \
   AccRawv1.h \
   AccRawv2.h \
   AccSnglHit.h \
   AccSnglHitv1.h \
   AccHit.h \
   AccHitv1.h \
   AerGeaHits.h \
   AerGeaHitsv1.h \
   AccSnglCluster.h \
   AccSnglClusterv1.h \
   AccCluster.h \
   AccClusterv1.h \
AccClassLinkDef.h 	 
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^ 	 

clean-local:
	rm -f *Dict*
