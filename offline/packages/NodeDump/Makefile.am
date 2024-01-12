AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

lib_LTLIBRARIES = libphnodedump.la

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

include_HEADERS = \
  PHNodeDump.h \
  DumpObject.h

#  DumpKalFitOut.C 

libphnodedump_la_SOURCES = \
  DumpAccCluster.C \
  DumpAccHit.C \
  DumpAccRaw.C \
  DumpAerGeaHits.C \
  Dumpbbcghit.C \
  DumpBbcOut.C \
  DumpBbcRaw.C \
  DumpCglTrack.C \
  Dumpcrkghit.C \
  DumpCrkHit.C \
  DumpCrkProj.C \
  DumpCrkRing.C \
  DumpdBbcGhitRaw.C \
  DumpdBbcRaw.C \
  DumpDchHitLineTable.C \
  DumpDchTrack.C \
  DumpdCrkHit.C \
  DumpdCrkRel2s.C \
  Dumpdcghit.C \
  DumpdDchGhitHits.C \
  DumpdDchHit.C \
  DumpdDchNibbleGhit.C \
  DumpdDchTracks.C \
  DumpdDchTracksExt.C \
  DumpdEmcCalibTower.C \
  DumpdEmcClusterLocalExt.C \
  DumpdEmcGeaClusterTrack.C \
  DumpdEmcGeaHit.C \
  DumpdEmcGeaTowerTrack.C \
  DumpdEmcGeaTrack.C \
  DumpdEmcGeaTrackCluster.C \
  DumpdEmcGeaTrackTower.C \
  DumpDetectorGeometry.C \
  DumpdPadCluster.C \
  DumpdPadGhitClus.C \
  DumpdPadGhitRaw.C \
  DumpdPadNibbleGhit.C \
  DumpdPadRaw.C \
  DumpdPHDchTrack.C \
  DumpdTecGhitRaw.C \
  DumpdTofFEMhitGhit.C \
  DumpdTofGdigi.C \
  DumpdTofGhitGdigi.C \
  DumpdTofGdigiRec.C \
  DumpdTofGhitRaw.C \
  DumpdTofRaw.C \
  DumpdTofRawRec.C \
  DumpdTofReconstructed.C \
  DumpemcClusterAuxInfoContainer.C \
  DumpemcClusterContainer.C \
  DumpemcTowerContainer.C \
  Dumper.C \
  DumperDict.C \
  DumpErtOut.C \
  DumpEventHeader.C \
  DumpFclOut.C \
  DumpFclRaw.C \
  Dumpfkin.C \
  DumpFlags.C \
  DumpHbdBlobList.C \
  DumpHbdCellList.C \
  DumpHbdMiniCellList.C \
  DumpHbdRawList.C \
  Dumpheader.C \
  DumplpcRaw.C \
  DumpLvl2DecisionOut.C \
  DumpLvl2OutArray.C \
  DumpMcEvalSingleList.C \
  DumpmpcClusterContainer.C \
  DumpMpcExEventHeader.C \
  DumpMpcExRawHit.C \
  DumpmpcRawContainer.C \
  DumpmpcSampleContainer.C \
  DumpmpcTowerContainer.C \
  DumpMutrgHeaderArray.C \
  DumpMutrgHitArray.C \
  DumpMutrgTrkArray.C \
  DumpMvdRPhiZOut.C \
  DumpPadCluster.C \
  DumpPadRaw.C \
  Dumppcghit.C \
  DumpPdbCalBankSave.C \
  DumpPHCentralTrack.C \
  DumpPHCentralTrackv24.C \
  DumpPHDchTrackOut.C \
  DumpPHGlobal.C \
  DumpPHMuoTracksAdc.C \
  DumpPHMuoTracksOut.C \
  DumpPHTrackOut.C \
  DumpPreviousEvent.C \
  Dumpprimary.C \
  Dumppythia.C \
  DumpRpSumXYObject.C \
  DumpRunHeader.C \
  DumpSmdOut.C \
  DumpSpinDataEventOut.C \
  DumpSvxCentralTrackList.C \
  DumpSvxClusterList.C \
  DumpSvxEventInfo.C \
  DumpSvxQAInfo.C \
  DumpSvxPixelRawHitList.C \
  DumpSvxRawhitClusterList.C \
  DumpSvxRawhitList.C \
  DumpSvxSegmentList.C \
  DumpSyncObject.C \
  DumpT0Out.C \
  DumpTecClusterContainer.C \
  DumpTecOut.C \
  Dumptecghit.C \
  Dumptofghit.C \
  DumpTofOut.C \
  DumpTofwHit.C \
  DumpTofwRaw.C \
  DumpTrigLvl1.C \
  DumpTrigRunLvl1.C \
  DumpTrigRunLvl2.C \
  DumpVariableArray.C \
  DumpVariableArrayInt.C \
  DumpVtxOut.C \
  DumpZdcOut.C \
  DumpZdcRaw.C \
  DumpObject.C \
  PHNodeDump.C \
  PHNodeDumpDict.C

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

#  -lKalFit 

libphnodedump_la_LIBADD = \
  -lacc \
  -lbbc \
  -lcgl \
  -lCNT \
  -lcompactCNT \
  -lCrkPID \
  -ldch \
  -lemc \
  -lert \
  -lfcl \
  -lHardpDST \
  -lhbd \
  -lheader \
  -llpc \
  -llvl2 \
  -lmpc \
  -lMpcEx \
  -lmuioo_classes \
  -lmutoo_classes \
  -lnanoDST \
  -lpad \
  -lreactionplane \
  -lrxnp_interface \
  -lspin \
  -lsvx \
  -lt0 \
  -ltec \
  -ltecpid \
  -ltof \
  -ltofw \
  -lvtx \
  -lzdc

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libphnodedump.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

PHNodeDumpDict.C : PHNodeDump.h PHNodeDumpLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

DumperDict.C : Dumper.h DumperLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
