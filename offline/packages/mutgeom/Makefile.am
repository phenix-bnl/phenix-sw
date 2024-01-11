AUTOMAKE_OPTIONS = foreign

AM_CPPFLAGS = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(CERN)/$(CERN_LEVEL)/include \
  -I$(CERN)/$(CERN_LEVEL)/include/geant321 \
  -I$(ROOTSYS)/include

lib_LTLIBRARIES = libmutgeom_fortran.la libmutgeom.la

AM_FFLAGS = -fno-automatic -ffixed-line-length-none -fsecond-underscore -Werror

AM_CXXFLAGS = -Wall -Werror
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

# libmfm must be loaded before CERNLIB because it re-implements, notably, gufld,
# the method responsible (in GEANT) for providing the magnetic field at a given
# point in space
libmutgeom_fortran_la_LDFLAGS = \
  -lmfm \
  `$(OPT_PHENIX)/bin/phenix_cernlib.pl`

libmutgeom_fortran_la_LIBADD = \
  -lpdbcalBase

libmutgeom_la_LIBADD = \
  -lpdbcalBase \
  -lPHGeant \
  libmutgeom_fortran.la \
  -lphflag \
  -lPISARoot

#
# the source code here is needed for the geant extrapolator PHGeantExtrap
# and compiled directly from GEANT source code
# this might need to be revisited whenever the geant version changes
#
libmutgeom_fortran_la_SOURCES = \
  $(CERN)/$(CERN_LEVEL)/src/geant321/matx55/dmm55.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/matx55/ssmt5t.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/matx55/xmm55.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/erbcer.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/erbctr.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/erland.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/ermcsc.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/erpini.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/erprop.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/erstor.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/ertrak.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/ertrch.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/ertrnt.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/ertrgo.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/erxyzc.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/eufilp.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/eufilv.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/eustep.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erdecks/evolio.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erpremc/trprfn.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erpremc/trprop.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erpremc/trscsd.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erpremc/trscsp.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erpremc/trsdsc.F \
  $(CERN)/$(CERN_LEVEL)/src/geant321/erpremc/trspsc.F \
  fortran/PHGeantExtrap.F

libmutgeom_la_SOURCES = \
  MUTGEOM.cxx \
  MutAnodeMap.cxx \
  MutArm.C \
  MutArmDict.C \
  MutCalib.cc \
  MutCalibDict.C \
  MutCalibStrip.cc \
  MutCalibStripDict.C \
  MutDCMChannelMap.C \
  MutDCMChannelMapDict.C \
  MutGap.C \
  MutGeom.cc \
  MutGeomL2Util.cc \
  MutGeomL2UtilDict.C \
  MutGeomObject.C \
  MutHalfOctant.C \
  MutOctant.C \
  MutPlane.C \
  MutStation.C \
  MutStrip.C \
  MutWire.C \
  MutChamberPar.cc \
  MutChamberParDict.C \
  TMutAlign.cxx \
  TMutDatabaseCntrl.cxx \
  TMutDatabaseCntrlDict.C \
  TMutDatabaseInit.cxx \
  TMutDatabaseInitDict.C \
  TMutUnstable.cxx \
  TMutUnstableDict.C

include_HEADERS = \
  $(srcdir)/fortran/PHGeantExtrapP.h \
  gconst.h \
  gufld.h \
  MUTGEOM.h \
  MutAnodeMap.h \
  MutArm.h \
  MutCalib.h \
  MutCalibStrip.hh \
  MutDCMChannelMap.h \
  MutGap.h \
  MutGeom.h \
  MutGeomObject.h \
  MutHalfOctant.h \
  MutOctant.h \
  MutPlane.h \
  MutStation.h \
  MutStrip.h \
  MutWire.h \
  MutChamberPar.hh \
  TMutAlign.h \
  TMutDatabaseCntrl.h \
  TMutDatabaseInit.h \
  TMutUnstable.h

BUILT_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals_fortran \
  testexternals

testexternals_fortran_SOURCES = \
  testexternals.C

testexternals_fortran_LDADD = \
  libmutgeom_fortran.la

testexternals_SOURCES = \
  testexternals.C

testexternals_LDADD = \
  libmutgeom.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

# Rule for generating CINT dictionaries from class headers.
%Dict.C: %.h %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

# Rule for generating CINT dictionaries from class headers.
%Dict.C: %.hh %LinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

# Rule for generating CINT dictionaries from class headers.
%Dict.C: %.hh
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	 rm -f *Dict* $(BUILT_SOURCES)
