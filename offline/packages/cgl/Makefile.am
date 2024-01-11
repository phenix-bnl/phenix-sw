AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libcgl.la

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include  -I$(ROOTSYS)/include 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libcgl_la_LIBADD = \
  -lemc-common \
  -lemc-datatypes \
  -ltec \
  -lWrappers \
  -lacc \
  -ldch \
  -lhbd \
  -ltofw \
  -lpad \
  -lphgeo \
  -lphool \
  -lsvx \
  -ltof \
  -ltrigger \
  -luti \
  -lvtx

noinst_HEADERS = \
  CglEmbedreco.h \
  CglReco.h \
  CglSnglTrackv1.h \
  CglSnglTrackv2.h \
  CglSnglTrackv3.h \
  CglSnglTrackv4.h \
  CglSnglTrackv5.h \
  CglSnglTrackv6.h \
  CglSnglTrackv7.h \
  CglSnglTrackv8.h \
  CglSnglTrackv9.h \
  PHDchSnglTrackv1.h \
  PHSnglTrackv1.h \
  PHSnglTrackv2.h \
  PHSnglTrackv3.h \
  PHSnglTrackv4.h \
  PHSnglTrackv5.h \
  PHSnglTrackv6.h \
  PHSnglTrackv7.h \
  PHSnglTrackv8.h \
  libcgl_LinkDef.h

include_HEADERS = \
  PHDchTrack.hh \
  PHDchTrackFitter.hh \
  PHDchTrackFitterPar.hh \
  PHLineTrack.hh \
  PHTrack.h \
  cglDetectorGeo.hh \
  cglHitAssociate.hh \
  cglHitList.hh \
  dCglTrack.h \
  dCglTrackWrapper.h \
  dPHDchTrack.h \
  dPHDchTrackWrapper.h \
  dPHTrack.h \
  dPHTrackWrapper.h \
  mPHDchTrackModel.hh \
  mPHLineTrack.hh \
  CglTrack.h \
  CglTrackv1.h \
  CglTrackv2.h \
  CglTrackv3.h \
  CglTrackv4.h \
  CglTrackv5.h \
  CglTrackv6.h \
  CglTrackv7.h \
  CglTrackv8.h \
  CglTrackv9.h \
  CglSnglTrack.h \
  CglRecoDefs.h \
  PHSnglTrack.h \
  PHTrackOut.h \
  PHTrackOutv1.h \
  PHTrackOutv2.h \
  PHTrackOutv3.h \
  PHTrackOutv4.h \
  PHTrackOutv5.h \
  PHTrackOutv6.h \
  PHTrackOutv7.h \
  PHTrackOutv8.h \
  PHDchTrackOut.h \
  PHDchTrackOutv1.h \
  PHDetectorGeometry.h \
  DetectorGeometry.h \
  DetectorGeometryv1.h \
  SavePHPanel.h \
  SavePHPanelv1.h 

libcgl_la_SOURCES = \
  CglEmbedreco.C \
  PHDchTrack.C \
  PHDchTrackFitter.C \
  PHLineTrack.C \
  PHTrack.C \
  cglDetectorGeo.cc \
  cglHitAssociate.cc \
  cglHitList.cc \
  dCglTrackWrapper.C \
  dPHDchTrackWrapper.C \
  dPHTrackWrapper.C \
  mPHDchTrackModel.C \
  mPHLineTrack.C \
  CglReco.C \
  CglTrack.C \
  CglTrackv1.C \
  CglTrackv2.C \
  CglTrackv3.C \
  CglTrackv4.C \
  CglTrackv5.C \
  CglTrackv6.C \
  CglTrackv7.C \
  CglTrackv8.C \
  CglTrackv9.C \
  CglSnglTrack.C \
  CglSnglTrackv1.C \
  CglSnglTrackv2.C \
  CglSnglTrackv3.C \
  CglSnglTrackv4.C \
  CglSnglTrackv5.C \
  CglSnglTrackv6.C \
  CglSnglTrackv7.C \
  CglSnglTrackv8.C \
  CglSnglTrackv9.C \
  PHSnglTrack.C \
  PHSnglTrackv1.C \
  PHSnglTrackv2.C \
  PHSnglTrackv3.C \
  PHSnglTrackv4.C \
  PHSnglTrackv5.C \
  PHSnglTrackv6.C \
  PHSnglTrackv7.C \
  PHSnglTrackv8.C \
  PHTrackOut.C \
  PHTrackOutv1.C \
  PHTrackOutv2.C \
  PHTrackOutv3.C \
  PHTrackOutv4.C \
  PHTrackOutv5.C \
  PHTrackOutv6.C \
  PHTrackOutv7.C \
  PHTrackOutv8.C \
  PHDchTrackOut.C \
  PHDchTrackOutv1.C \
  PHDchSnglTrackv1.C \
  PHDetectorGeometry.C \
  DetectorGeometry.C \
  DetectorGeometryv1.C \
  SavePHPanel.C \
  SavePHPanelv1.C

nodist_libcgl_la_SOURCES = libcgl_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libcgl.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libcgl_Dict.C: \
  CglEmbedreco.h \
  CglEmbedreco.h \
  CglReco.h \
  CglReco.h \
  CglSnglTrack.h \
  CglSnglTrack.h \
  CglSnglTrackv1.h \
  CglSnglTrackv1.h \
  CglSnglTrackv2.h \
  CglSnglTrackv2.h \
  CglSnglTrackv3.h \
  CglSnglTrackv3.h \
  CglSnglTrackv4.h \
  CglSnglTrackv4.h \
  CglSnglTrackv5.h \
  CglSnglTrackv5.h \
  CglSnglTrackv6.h \
  CglSnglTrackv6.h \
  CglSnglTrackv7.h \
  CglSnglTrackv7.h \
  CglSnglTrackv8.h \
  CglSnglTrackv8.h \
  CglSnglTrackv9.h \
  CglSnglTrackv9.h \
  CglTrack.h \
  CglTrack.h \
  CglTrackv1.h \
  CglTrackv1.h \
  CglTrackv2.h \
  CglTrackv2.h \
  CglTrackv3.h \
  CglTrackv3.h \
  CglTrackv4.h \
  CglTrackv4.h \
  CglTrackv5.h \
  CglTrackv5.h \
  CglTrackv6.h \
  CglTrackv6.h \
  CglTrackv7.h \
  CglTrackv7.h \
  CglTrackv8.h \
  CglTrackv8.h \
  CglTrackv9.h \
  CglTrackv9.h \
  dCglTrackWrapper.h \
  DetectorGeometry.h \
  DetectorGeometry.h \
  DetectorGeometryv1.h \
  DetectorGeometryv1.h \
  dPHDchTrackWrapper.h \
  dPHTrackWrapper.h \
  PHDchSnglTrackv1.h \
  PHDchSnglTrackv1.h \
  PHDchTrackOut.h \
  PHDchTrackOut.h \
  PHDchTrackOutv1.h \
  PHDchTrackOutv1.h \
  PHDetectorGeometry.h \
  PHDetectorGeometry.h \
  PHSnglTrack.h \
  PHSnglTrack.h \
  PHSnglTrackv1.h \
  PHSnglTrackv1.h \
  PHSnglTrackv2.h \
  PHSnglTrackv2.h \
  PHSnglTrackv3.h \
  PHSnglTrackv3.h \
  PHSnglTrackv4.h \
  PHSnglTrackv4.h \
  PHSnglTrackv5.h \
  PHSnglTrackv5.h \
  PHSnglTrackv6.h \
  PHSnglTrackv6.h \
  PHSnglTrackv7.h \
  PHSnglTrackv7.h \
  PHSnglTrackv8.h \
  PHSnglTrackv8.h \
  PHTrackOut.h \
  PHTrackOut.h \
  PHTrackOutv1.h \
  PHTrackOutv1.h \
  PHTrackOutv2.h \
  PHTrackOutv2.h \
  PHTrackOutv3.h \
  PHTrackOutv3.h \
  PHTrackOutv4.h \
  PHTrackOutv4.h \
  PHTrackOutv5.h \
  PHTrackOutv5.h \
  PHTrackOutv6.h \
  PHTrackOutv6.h \
  PHTrackOutv7.h \
  PHTrackOutv7.h \
  PHTrackOutv8.h \
  PHTrackOutv8.h \
  SavePHPanel.h \
  SavePHPanel.h \
  SavePHPanelv1.h \
  SavePHPanelv1.h \
  libcgl_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict.*

testexternals_SOURCES = testexternals.C
