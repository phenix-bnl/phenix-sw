AUTOMAKE_OPTIONS = foreign

#INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

AM_CXXFLAGS = -Wall -Werror
AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

RINCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I`root-config --incdir`
AM_CPPFLAGS = -isystem $(includedir) -isystem $(OFFLINE_MAIN)/include -isystem `root-config --incdir`

lib_LTLIBRARIES = libreactionplane.la

noinst_HEADERS = \
  RpSnglSumXYv1.h \
  ReactionPlaneSnglv1.h \
  MvdGeoParameter.hh \
  MvdGeometry.hh \
  MvdParameter.hh \
  MvdSnglRPhiZ.h \
  MvdSnglRPhiZv1.h \
  MvdRPhiZOutv1.h \
  rpLinkDef.h

include_HEADERS = \
  MvdRPhiZOut.h \
  RpConst.h \
  ReactionPlaneCalib.h \
  ReactionPlaneCalibv1.h \
  ReactionPlaneCalibv2.h \
  ReactionPlaneObject.h \
  ReactionPlaneObjectv1.h \
  ReactionPlaneObjectv2.h \
  ReactionPlaneObjectv3.h \
  ReactionPlaneObjectv4.h \
  RpSumXYObject.h \
  RpSnglSumXY.h \
  ReactionPlaneSngl.h \
  RpSumXYObjectv1.h \
  RpSumXYObjectv2.h \
  RpSumXYObjectv3.h \
  RpSumXY.h \
  SvxRpSumXYReco.h \
  RPCalib_priv.h \
  RPCalibrator.h \
  RpRecoConst.h

libreactionplane_la_SOURCES = \
  MvdGeoParameter.cc \
  MvdGeometry.cc \
  MvdParameter.cc \
  MvdRPhiZOut.C \
  MvdSnglRPhiZ.C\
  MvdRPhiZOutv1.C \
  MvdSnglRPhiZv1.C \
  RpConst.C \
  RpSumXY.C \
  ReactionPlaneCalib.C \
  ReactionPlaneCalibv1.C \
  ReactionPlaneCalibv2.C \
  ReactionPlaneObject.C \
  ReactionPlaneObjectv1.C \
  ReactionPlaneObjectv2.C \
  ReactionPlaneObjectv3.C \
  ReactionPlaneObjectv4.C \
  RpSnglSumXY.C \
  RpSnglSumXYv1.C \
  ReactionPlaneSngl.C \
  ReactionPlaneSnglv1.C \
  RpSumXYObject.C \
  RpSumXYObjectv1.C \
  RpSumXYObjectv2.C \
  RpSumXYObjectv3.C \
  SvxRpSumXYReco.C \
  RPCalib_priv.C \
  RPCalibrator.C

nodist_libreactionplane_la_SOURCES = Rp_Dict.C 

libreactionplane_la_LIBADD = \
  -ldch\
  -lzdc\
  -lmpc \
  -lfcl \
  -luti \
  -lsvx \
  -lrxnp_interface \
  -lfvtxoo_interface 

noinst_PROGRAMS = testexternals
testexternals_SOURCES = testexternals.C
testexternals_LDADD = libreactionplane.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

Rp_Dict.C: \
  MvdRPhiZOut.h \
  MvdSnglRPhiZ.h \
  MvdRPhiZOutv1.h \
  MvdSnglRPhiZv1.h \
  RpConst.h \
  ReactionPlaneCalib.h \
  ReactionPlaneCalibv1.h \
  ReactionPlaneCalibv2.h \
  ReactionPlaneObject.h \
  ReactionPlaneObjectv1.h \
  ReactionPlaneObjectv2.h \
  ReactionPlaneObjectv3.h \
  ReactionPlaneObjectv4.h \
  RpSnglSumXY.h \
  RpSnglSumXYv1.h \
  ReactionPlaneSngl.h \
  ReactionPlaneSnglv1.h \
  RpSumXYObject.h \
  RpSumXYObjectv1.h \
  RpSumXYObjectv2.h \
  RpSumXYObjectv3.h \
  RpSumXY.h \
  SvxRpSumXYReco.h \
  RPCalib_priv.h \
  RPCalibrator.h \
  RpRecoConst.h \
  rpLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(RINCLUDES) $^

clean-local:
	rm -f *Dict*
