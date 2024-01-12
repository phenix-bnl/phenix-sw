AUTOMAKE_OPTIONS = foreign

INCLUDES = \
 -I$(includedir) \
 -I$(OFFLINE_MAIN)/include \
 -I$(ROOTSYS)/include \
 -I$(OPT_PHENIX)/include

pkginclude_HEADERS = \
  HeadReco.h \
  TrigReco.h \
  T0Reco.h \
  VtxReco.h

# build the libs in the order in which they depend on each other
lib_LTLIBRARIES = \
  libfun4allfuncs_common.la \
  libfun4allfuncs_muons.la \
  libfun4allfuncs_central.la \
  libfun4allfuncs.la

noinst_HEADERS = \
  AccclusterReco.h \
  AccReco.h \
  AllSysReco_centralLinkDef.h \
  AllSysReco_commonLinkDef.h \
  AllSysReco_muonsLinkDef.h \
  BbcReco.h \
  BeamShiftReco.h \
  ChargedvetoReco.h \
  CentralityCalibrator.h \
  CentraltrackReco.h \
  CrkReco.h \
  DchhistReco.h \
  DchReco.h \
  EmcAbsEScaleRecalReco.h \
  EmcRecalReco.h \
  EmcReco3.h \
  EmcSimTowerSmearer.h \
  EmcT0RecalReco.h \
  ErtReco.h \
  EventTypeSelect.h \
  FlagReadBack.h \
  GlobalReco.h \
  GlobalReco_central.h \
  GlobalReco_muons.h \
  HeadReco.h \
  MomentumscaleReco.h \
  OutputAccounting.h \
  PadReco.h \
  PadVtxReco.h \
  PdbBankAddNew.h \
  PdbCalFlagSaveReco.h \
  PreviousEventReco.h \
  RandomTracks.h \
  RpSumXYReco.h \
  SegmentSelect.h \
  SpinReco.h \
  SyncReco.h \
  T0Reco.h \
  TecPidReco.h \
  TecReco.h \
  TofReco.h \
  TriggerAccounting.h \
  TrigReco.h \
  TrigSelect.h \
  VtxReco.h \
  VtxReject.h

nodist_libfun4allfuncs_common_la_SOURCES = AllSysReco_common_Dict.C
libfun4allfuncs_common_la_SOURCES = \
  BbcReco.C \
  FlagReadBack.C \
  GlobalReco.C \
  HeadReco.C \
  OutputAccounting.C \
  PdbBankAddNew.C \
  PdbCalFlagSaveReco.C \
  PreviousEventReco.C \
  RpSumXYReco.C \
  SyncReco.C \
  T0Reco.C \
  TrigReco.C \
  TrigSelect.C \
  TriggerAccounting.C \
  VtxReco.C

nodist_libfun4allfuncs_central_la_SOURCES = AllSysReco_central_Dict.C
libfun4allfuncs_central_la_SOURCES = \
  AccReco.C \
  AccclusterReco.C \
  BeamShiftReco.C \
  ChargedvetoReco.C \
  CentralityCalibrator.C \
  CentraltrackReco.C \
  CrkReco.C \
  DchReco.C \
  DchhistReco.C \
  EmcReco3.C \
  EmcRecalReco.C \
  EmcAbsEScaleRecalReco.C \
  EmcT0RecalReco.C \
  EmcSimTowerSmearer.C \
  ErtReco.C \
  EventTypeSelect.C \
  GlobalReco_central.C \
  MomentumscaleReco.C \
  PadReco.C \
  PadVtxReco.C \
  RandomTracks.C \
  SegmentSelect.C \
  SpinReco.C \
  TecReco.C \
  TecPidReco.C \
  TofReco.C \
  VtxReject.C

nodist_libfun4allfuncs_muons_la_SOURCES = AllSysReco_muons_Dict.C
libfun4allfuncs_muons_la_SOURCES = \
  GlobalReco_muons.C

# note fun4allfuncs has no sources and should be left so.
# this is a "meta" library, that loads all specialization
# namely fun4allfuncs_common, fun4allfuncs_central and fun4allfuncs_muons
# please put new sources under the relevant library amongst the above.
libfun4allfuncs_la_SOURCES =

libfun4allfuncs_common_la_LIBADD = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lreactionplane \
  -lt0 \
  -lvtx \
  -ltrigger \
  -lRawDataCheck \
  -lboost_filesystem \
  -L$(OPT_PHENIX)/lib -lodbc++

libfun4allfuncs_central_la_LIBADD = \
  libfun4allfuncs_common.la \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lpdbcalBase \
  -lPHGeant \
  -lt0 \
  -lRawDataCheck \
  -ltecpid \
  -lCNT \
  -lspin \
  -lemc \
  -lert \
  -L$(OPT_PHENIX)/lib -lodbc++

libfun4allfuncs_muons_la_LIBADD = \
  libfun4allfuncs_common.la \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lpdbcalBase \
  -lPHGeant \
  -lnanoDST \
  -lmutoo_interface \
  -lmuioo_interface

libfun4allfuncs_la_LIBADD = \
  libfun4allfuncs_common.la \
  libfun4allfuncs_central.la \
  libfun4allfuncs_muons.la

noinst_PROGRAMS = \
  testexternals_common \
  testexternals_central \
  testexternals_muons

testexternals_common_SOURCES = testexternals.C
testexternals_common_LDADD = libfun4allfuncs_common.la

testexternals_central_SOURCES = testexternals.C
testexternals_central_LDADD = libfun4allfuncs_central.la

testexternals_muons_SOURCES = testexternals.C
testexternals_muons_LDADD = libfun4allfuncs_muons.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

AllSysReco_common_Dict.C: \
  BbcReco.h \
  FlagReadBack.h \
  GlobalReco.h \
  HeadReco.h \
  OutputAccounting.h \
  PdbBankAddNew.h \
  PdbCalFlagSaveReco.h \
  PreviousEventReco.h \
  RpSumXYReco.h \
  SyncReco.h \
  T0Reco.h \
  TrigReco.h \
  TrigSelect.h \
  TriggerAccounting.h \
  VtxReco.h \
  AllSysReco_commonLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

AllSysReco_central_Dict.C: \
  AccReco.h \
  AccclusterReco.h \
  BeamShiftReco.h \
  ChargedvetoReco.h \
  CentralityCalibrator.h \
  CentraltrackReco.h \
  CrkReco.h \
  DchReco.h \
  DchhistReco.h \
  EmcReco3.h \
  EmcRecalReco.h \
  EmcSimTowerSmearer.h \
  EmcAbsEScaleRecalReco.h \
  EmcT0RecalReco.h \
  ErtReco.h \
  EventTypeSelect.h \
  GlobalReco_central.h \
  MomentumscaleReco.h \
  PadReco.h \
  PadVtxReco.h \
  RandomTracks.h \
  SegmentSelect.h \
  SpinReco.h \
  TecReco.h \
  TecPidReco.h \
  TofReco.h \
  VtxReject.h \
  AllSysReco_centralLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^

AllSysReco_muons_Dict.C: \
  GlobalReco_muons.h \
  AllSysReco_muonsLinkDef.h
	rootcint -f $@ -c $(DEFAULT_INCLUDES) $(INCLUDES) $^


clean-local:
	rm -f *Dict*
