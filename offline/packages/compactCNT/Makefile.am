AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libcompactCNT.la

# Normaly one would use pkginclude_HEADERS here to get a directory name, but
# this lowercases the package name resulting in a wrong name.
# Set it by hand here.
libcompactCNT_la_includedir=$(includedir)/compactCNT
libcompactCNT_la_include_HEADERS = \
  RecoverDchHits.h \
  RecoverEmcHits.h \
  RecoverPadHits.h \
  RecoverTofeHits.h \
  RecoverTofwHits.h \
  RecoverTecHits.h \
  RecoverCrkHits.h \
  RecoverAccHits.h \
  RecoverSvxHits.h \
  RecoverSvxRawHits.h \
  RecoverHbdHits.h \
  RecoverTrackHits.h \
  RecoverTrackPathLengths.h \
  RecoverTrackProjections.h \
  RecoverTrackLineProjections.h \
  CreateCNT.h \
  FillCNT_DchHits.h \
  FillCNT_EmcHits.h \
  FillCNT_PadHits.h \
  FillCNT_EmcPc3.h \
  FillCNT_TofeHits.h \
  FillCNT_TofwHits.h \
  FillCNT_CrkHits.h \
  FillCNT_TecHits.h \
  FillCNT_AccHits.h \
  FillCNT_TrackPathLengths.h \
  FillCNT_TrackProjections.h \
  FillCNT_TrackHits.h \
  dumpflag.h \
  setIntflag.h \
  PadHitMap.h \
  PadHitMapEntry.h \
  AccHitMap.h \
  AccHitMapEntry.h \
  HbdHitMap.h \
  HbdHitMapEntry.h \
  VariableArrayInt.h \
  SvxHitMap.h \
  SvxHitMapEntry.h \
  SvxTrackMap.h \
  SvxTrackMapEntry.h \
  SvxCentralTrackMapEntry.h \
  SvxCentralTrackMap.h \
  SvxCompactToDST.h \
  CrkHitMap.h \
  CrkHitMapEntry.h \
  CrkAssocHits.h  \
  CrkAssocHitsEntry.h \
  TrackLineProjectionMap.h \
  TrackLineProjectionMapEntry.h \
  id_detector.h

noinst_HEADERS = \
  DchHitMap.h \
  DchHitMapEntry.h \
  EmcHitMap.h \
  EmcHitMapEntry.h \
  FillDchHits.h \
  FillEmcHits.h \
  FillPadHits.h \
  FillTofeHits.h \
  FillTofwHits.h \
  FillTecHits.h \
  FillCrkHits.h \
  FillAccHits.h \
  FillSvxHits.h \
  FillSvxRawHits.h \
  FillHbdHits.h \
  FillTrackHits.h \
  FillTrackHits.h \
  FillTrackPathLengths.h \
  FillTrackProjections.h \
  FillTrackLineProjections.h \
  TestNewOut.h \
  TestVariableArray.h \
  TofeHitMap.h \
  TofeHitMapEntry.h \
  TofwHitMap.h \
  TofwHitMapEntry.h \
  TecHitMap.h \
  TecHitMapEntry.h \
  CrkHitMap.h \
  CrkHitMapEntry.h \
  AccHitMap.h \
  AccHitMapEntry.h \
  SvxHitMap.h \
  SvxHitMapEntry.h \
  SvxTrackMap.h \
  SvxTrackMapEntry.h \
  SvxCentralTrackMap.h \
  SvxCentralTrackMapEntry.h \
  HbdHitMap.h \
  HbdHitMapEntry.h \
  TrackHitsMap.h \
  TrackHitsMapEntry.h \
  TrackProjectionMap.h \
  TrackProjectionMapEntry.h \
  TrackPathLengthMap.h \
  TrackPathLengthMapEntry.h \
  CrkAssocHits.h \
  CrkAssocHitsEntry.h \
  VariableArrayInt.h \
  CopySvxHits.h \
  OutLinkDef.h

INCLUDES = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include  \
  -I$(ROOTSYS)/include

AM_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib

libcompactCNT_la_LIBADD = \
  -ltec \
  -lCNT \
  -lcgl \
  -lhalf \
  -ltecpid \
  -lt0 \
  -lphool \
  -lvararray


libcompactCNT_la_SOURCES = \
  CreateCNT.C \
  DchHitMap.C \
  DchHitMapEntry.C \
  EmcHitMap.C \
  EmcHitMapEntry.C \
  FillCNT_DchHits.C \
  FillCNT_EmcHits.C \
  FillCNT_PadHits.C \
  FillCNT_EmcPc3.C \
  FillCNT_TofeHits.C \
  FillCNT_TofwHits.C \
  FillCNT_CrkHits.C \
  FillCNT_TecHits.C \
  FillCNT_AccHits.C \
  FillCNT_TrackPathLengths.C \
  FillCNT_TrackProjections.C \
  FillCNT_TrackHits.C \
  FillDchHits.C \
  FillEmcHits.C \
  FillPadHits.C \
  FillTofeHits.C \
  FillTofwHits.C \
  FillTecHits.C \
  FillCrkHits.C \
  FillAccHits.C \
  FillSvxHits.C \
  FillSvxRawHits.C \
  FillHbdHits.C \
  FillTrackHits.C \
  FillTrackPathLengths.C \
  FillTrackProjections.C \
  FillTrackLineProjections.C \
  TestNewOut.C \
  PadHitMap.C \
  PadHitMapEntry.C \
  RecoverDchHits.C \
  RecoverEmcHits.C \
  RecoverPadHits.C \
  RecoverTofeHits.C \
  RecoverTofwHits.C \
  RecoverTecHits.C \
  RecoverCrkHits.C \
  RecoverAccHits.C \
  RecoverSvxHits.C \
  RecoverSvxRawHits.C \
  RecoverHbdHits.C \
  RecoverTrackHits.C \
  RecoverTrackPathLengths.C \
  RecoverTrackProjections.C \
  RecoverTrackLineProjections.C \
  TestVariableArray.C \
  TofeHitMap.C \
  TofeHitMapEntry.C \
  TofwHitMap.C \
  TofwHitMapEntry.C \
  TecHitMap.C \
  TecHitMapEntry.C \
  CrkHitMap.C \
  CrkHitMapEntry.C \
  AccHitMap.C \
  AccHitMapEntry.C \
  SvxHitMap.C \
  SvxHitMapEntry.C \
  SvxTrackMap.C \
  SvxTrackMapEntry.C \
  SvxCentralTrackMap.C \
  SvxCentralTrackMapEntry.C \
  SvxCompactToDST.C \
  HbdHitMap.C \
  HbdHitMapEntry.C \
  TrackHitsMap.C \
  TrackHitsMapEntry.C \
  TrackProjectionMap.C \
  TrackProjectionMapEntry.C \
  TrackLineProjectionMap.C \
  TrackLineProjectionMapEntry.C \
  TrackPathLengthMap.C \
  TrackPathLengthMapEntry.C \
  CrkAssocHits.C  \
  CrkAssocHitsEntry.C  \
  VariableArrayInt.C \
  CopySvxHits.C \
  useInt.h

nodist_libcompactCNT_la_SOURCES = Out_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_SOURCES = testexternals.C
testexternals_LDADD = libcompactCNT.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

Out_Dict.C: \
  CreateCNT.h \
  DchHitMap.h \
  DchHitMapEntry.h \
  EmcHitMap.h \
  EmcHitMapEntry.h \
  FillCNT_DchHits.h \
  FillCNT_EmcHits.h \
  FillCNT_PadHits.h \
  FillCNT_EmcPc3.h \
  FillCNT_TofeHits.h \
  FillCNT_TofwHits.h \
  FillCNT_CrkHits.h \
  FillCNT_TecHits.h \
  FillCNT_AccHits.h \
  FillCNT_TrackHits.h \
  FillCNT_TrackPathLengths.h \
  FillCNT_TrackProjections.h \
  FillDchHits.h \
  FillEmcHits.h \
  FillPadHits.h \
  FillTofeHits.h \
  FillTofwHits.h \
  FillTecHits.h \
  FillCrkHits.h \
  FillAccHits.h \
  FillSvxHits.h \
  FillSvxRawHits.h \
  FillHbdHits.h \
  FillTrackHits.h \
  FillTrackPathLengths.h \
  FillTrackProjections.h \
  FillTrackLineProjections.h \
  TestNewOut.h \
  PadHitMap.h \
  PadHitMapEntry.h \
  RecoverDchHits.h \
  RecoverEmcHits.h \
  RecoverPadHits.h \
  RecoverTofeHits.h \
  RecoverTofwHits.h \
  RecoverTecHits.h \
  RecoverCrkHits.h \
  RecoverAccHits.h \
  RecoverSvxHits.h \
  RecoverSvxRawHits.h \
  RecoverHbdHits.h \
  RecoverTrackHits.h \
  RecoverTrackPathLengths.h \
  RecoverTrackProjections.h \
  RecoverTrackLineProjections.h \
  TestVariableArray.h \
  TofeHitMap.h \
  TofeHitMapEntry.h \
  TofwHitMap.h \
  TofwHitMapEntry.h \
  TecHitMap.h \
  TecHitMapEntry.h \
  CrkHitMap.h \
  CrkHitMapEntry.h \
  AccHitMap.h \
  AccHitMapEntry.h \
  SvxHitMap.h \
  SvxHitMapEntry.h \
  SvxTrackMap.h \
  SvxTrackMapEntry.h \
  SvxCentralTrackMap.h \
  SvxCentralTrackMapEntry.h \
  SvxCompactToDST.h \
  HbdHitMap.h \
  HbdHitMapEntry.h \
  TrackHitsMap.h \
  TrackHitsMapEntry.h \
  TrackProjectionMap.h \
  TrackProjectionMapEntry.h \
  TrackPathLengthMap.h \
  TrackPathLengthMapEntry.h \
  CrkAssocHits.h \
  CrkAssocHitsEntry.h \
  VariableArrayInt.h \
  CopySvxHits.h \
  OutLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local:
	rm -f *Dict.*
