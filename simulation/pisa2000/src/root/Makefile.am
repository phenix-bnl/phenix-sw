AUTOMAKE_OPTIONS = foreign

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib -L$(ROOTSYS)/lib
AM_CXXFLAGS = -Wall -Werror
INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

bin_PROGRAMS = pisaRootRead pisaRootOSCAR pisaRootIdentify

pisaRootRead_SOURCES = pisaRootRead.cc

pisaRootRead_LDADD =  libPISARoot.la

pisaRootOSCAR_SOURCES = pisaRootOSCAR.cc

pisaRootOSCAR_LDADD =  libPISARoot.la

pisaRootIdentify_SOURCES = pisaRootIdentify.cc

pisaRootIdentify_LDADD =  libPISARoot.la

lib_LTLIBRARIES = libPISARoot.la

libPISARoot_la_LIBADD =  \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -lphool \
  @ROOTLIBS@

libPISARoot_la_SOURCES = \
  AerPISAHit.cc \
  BbcPISAHit.cc \
  BbcPISAPara.cc \
  SvxPISAPara.cc \
  SvxPISAParav1.cc \
  ZdcPISAHit.cc \
  CrkPISAHit.cc \
  CtrPISAHit.cc \
  DchPISAHit.cc \
  DchPISAPara.cc \
  EmcPISAHit.cc \
  EmcPISAPara.cc \
  FclPISAHit.cc \
  HbdPISAHit.cc \
  KinPISAHit.cc \
  KinPISAHitHelper.cc \
  MuiPISAHit.cc \
  MuiPISAPara.cc \
  MutPISAHit.cc \
  MutPISAPara.cc \
  RxnPISAHit.cc \
  SvxPISAHit.cc \
  PISAEvent.cc \
  PISAEventHeader.cc \
  PISAHistogramManager.cc \
  PISARun.cc \
  PISARun_AddFile.cc \
  PISARun_GetOneEvent.cc \
  PadPISAHit.cc \
  MuPCPISAHit.cc \
  NCCPISAHit.cc \
  MPCEXABSPISAHit.cc \
  MPCFPLTPISAHit.cc \
  MPCEXEntryPISAHit.cc \
  VncPISAHit.cc \
  MpcPISAHit.cc \
  RLTPISAHit.cc \
  PadPISAPara.cc \
  PriPISAHit.cc \
  PriPISAHitHelper.cc \
  TecPISAHit.cc \
  TfwPISAHit.cc \
  TofPISAHit.cc \
  TofPISAPara.cc \
  rootAncAer.cc \
  rootAncBbc.cc \
  rootAncZdc.cc \
  rootAncPythia.cc \
  rootAncPri.cc \
  rootAncCrk.cc \
  rootAncDch.cc \
  rootAncEmcPad.cc \
  rootAncFcl.cc \
  rootAncHbd.cc \
  rootAncSvx.cc \
  rootAncMuPC.cc \
  rootAncRLT.cc \
  rootAncNCC.cc \
  rootAncVnc.cc \
  rootAncMpc.cc \
  rootAncMui.cc \
  rootAncMut.cc \
  rootAncPad.cc \
  rootAncTec.cc \
  rootAncTfw.cc \
  rootAncTof.cc \
  rootAncRxn.cc \
  root_ptrk.cc

nodist_libPISARoot_la_SOURCES = libPISARoot_Dict.C

include_HEADERS = \
  AerPISAHit.h \
  BbcPISAHit.h \
  BbcPISAPara.h \
  SvxPISAPara.h \
  SvxPISAParav1.h \
  ZdcPISAHit.h \
  CrkPISAHit.h \
  CtrPISAHit.h \
  DchPISAHit.h \
  DchPISAPara.h \
  EmcPISAHit.h \
  EmcPISAPara.h \
  FclPISAHit.h \
  HbdPISAHit.h \
  SvxPISAHit.h \
  KinPISAHit.h \
  KinPISAHitHelper.h \
  MuiPISAHit.h \
  MuiPISAPara.h \
  MutPISAHit.h \
  MutPISAPara.h \
  RxnPISAHit.h \
  PISAEvent.h \
  PISAEventHeader.h \
  PISAHistogramManager.h \
  PISAHitCounter.h \
  PISARun.h \
  PadPISAHit.h \
  MuPCPISAHit.h \
  RLTPISAHit.h \
  NCCPISAHit.h \
  MPCEXABSPISAHit.h \
  MPCFPLTPISAHit.h \
  MPCEXEntryPISAHit.h \
  VncPISAHit.h \
  MpcPISAHit.h \
  PadPISAPara.h \
  PriPISAHit.h \
  PriPISAHitHelper.h \
  TecPISAHit.h \
  TfwPISAHit.h \
  TofPISAPara.h \
  TofPISAHit.h \
  geantMass.h \
  geantToOSCAR.h \
  pisaMass.h \
  rootAnc.h \
  root_ptrk.h

libPISARoot_Dict.C: \
  AerPISAHit.h \
  BbcPISAHit.h \
  BbcPISAPara.h \
  SvxPISAPara.h \
  SvxPISAParav1.h \
  ZdcPISAHit.h \
  CrkPISAHit.h \
  CtrPISAHit.h \
  DchPISAHit.h \
  DchPISAPara.h \
  EmcPISAHit.h \
  EmcPISAPara.h \
  FclPISAHit.h \
  HbdPISAHit.h \
  SvxPISAHit.h \
  KinPISAHit.h \
  MuiPISAHit.h \
  MuiPISAPara.h \
  MutPISAHit.h \
  MutPISAPara.h \
  RxnPISAHit.h \
  PISAEvent.h \
  PISAEventHeader.h \
  PISAHistogramManager.h \
  PadPISAHit.h \
  MuPCPISAHit.h \
  RLTPISAHit.h \
  NCCPISAHit.h \
  MPCEXABSPISAHit.h \
  MPCFPLTPISAHit.h \
  MPCEXEntryPISAHit.h \
  VncPISAHit.h \
  MpcPISAHit.h \
  PadPISAPara.h \
  PriPISAHit.h \
  TecPISAHit.h \
  TfwPISAHit.h \
  TofPISAHit.h \
  TofPISAPara.h
	rootcint -f $@ -c $(INCLUDES) $^

BUILT_SOURCES = \
  testexternals.C

testexternals_SOURCES = \
  testexternals.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libPISARoot.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

clean-local:
	rm -f *Dict*
