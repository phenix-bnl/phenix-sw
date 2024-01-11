AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libcrk.la

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib
AM_CXXFLAGS = -Wall -Werror

libcrk_la_LIBADD = \
  -lcgl \
  -lWrappers \
  -ldch \
  -ldgo \
  -lgea \
  -lgsl \
  -lpad \
  -lpdbcalBase \
  -lphflag \
  -lphgeo \
  -lphool \
  -lphtime

include_HEADERS = \
  CrkCal.h \
  CrkDAO.h \
  CrkDisp.h \
  CrkGeometryObject.hh \
  CrkHit.h \
  CrkHitv1.h \
  CrkHitExt.h \
  CrkHitExtv1.h \
  CrkPutDCM.h \
  CrkPutDCMReCal.h \
  CrkSimuRawReCal.h \
  CrkUtil.h \
  dCrkCal.h \
  dCrkCalWrapper.h \
  dCrkDCM.h \
  dCrkDCMWrapper.h \
  dCrkDCMpar.h \
  dCrkDCMparWrapper.h \
  dCrkDCMRawPar.h \
  dCrkDCMRawParWrapper.h \
  dCrkFEM.h \
  dCrkFEMWrapper.h \
  dCrkGeo.h \
  dCrkGeoWrapper.h \
  dCrkGhitRawPar.h \
  dCrkGhitRawParWrapper.h \
  dCrkHit.h \
  dCrkHitWrapper.h \
  dCrkPid.h \
  dCrkPidWrapper.h \
  dCrkProjPidPar.h \
  dCrkProjPidParWrapper.h \
  dCrkRaw.h \
  dCrkRawWrapper.h \
  dCrkRawFEMpar.h \
  dCrkRawFEMparWrapper.h \
  dCrkRawHitPar.h \
  dCrkRawHitParWrapper.h \
  dCrkRel2s.h \
  dCrkRel2sWrapper.h \
  dCrkUcal.h \
  dCrkUcalWrapper.h \
  find_Wrapper.h \
  pidcode.h \
  TCrkDcFitter.h \
  TCrkModule.h

noinst_HEADERS = \
  CrkSnglHitExt.h \
  CrkSnglHitExtv1.h \
  CrkSnglHit.h \
  CrkSnglHitv1.h \
  TCrkProjector.h \
  libcrk_LinkDef.h

libcrk_la_SOURCES = \
  CrkCal.cc \
  CrkDisp.cc \
  CrkDAO.cc \
  CrkGeometryObject.cc \
  CrkGetDCM.cc \
  CrkHit.C \
  CrkHitExt.C \
  CrkHitv1.C \
  CrkHitExtv1.C \
  CrkPutDCM.cc \
  CrkPutDCMReCal.cc \
  CrkSimuRawReCal.C \
  CrkSnglHit.C \
  CrkSnglHitv1.C \
  CrkSnglHitExt.C \
  CrkSnglHitExtv1.C \
  CrkUtil.cc \
  dCrkCalWrapper.C \
  dCrkDCMWrapper.C \
  dCrkDCMRawParWrapper.C \
  dCrkDCMparWrapper.C \
  dCrkFEMWrapper.C \
  dCrkGeoWrapper.C \
  dCrkGhitRawParWrapper.C \
  dCrkHitWrapper.C \
  dCrkPidWrapper.C \
  dCrkProjPidParWrapper.C \
  dCrkRawWrapper.C \
  dCrkRawFEMparWrapper.C \
  dCrkRawHitParWrapper.C \
  dCrkRel2sWrapper.C \
  dCrkUcalWrapper.C \
  TCrkModule.cc \
  TCrkProjector.cc

nodist_libcrk_la_SOURCES = libcrk_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libcrk.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libcrk_Dict.C: \
  CrkDAO.h \
  CrkDisp.h \
  CrkGeometryObject.hh \
  CrkHitExt.h \
  CrkHitExtv1.h \
  CrkHit.h \
  CrkHitv1.h \
  CrkSimuRawReCal.h \
  CrkSnglHitExt.h \
  CrkSnglHitExtv1.h \
  CrkSnglHit.h \
  CrkSnglHitv1.h \
  CrkUtil.h \
  dCrkCalWrapper.h \
  dCrkDCMparWrapper.h \
  dCrkDCMRawParWrapper.h \
  dCrkDCMWrapper.h \
  dCrkFEMWrapper.h \
  dCrkGeoWrapper.h \
  dCrkGhitRawParWrapper.h \
  dCrkHitWrapper.h \
  dCrkPidWrapper.h \
  dCrkProjPidParWrapper.h \
  dCrkRawFEMparWrapper.h \
  dCrkRawHitParWrapper.h \
  dCrkRawWrapper.h \
  dCrkRel2sWrapper.h \
  dCrkUcalWrapper.h \
  TCrkModule.h \
  libcrk_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^
  
clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
