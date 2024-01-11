AUTOMAKE_OPTIONS = foreign

BUILT_SOURCES = \
  testexternals.C

INCLUDES = -I$(includedir) -I$(OFFLINE_MAIN)/include -I$(ROOTSYS)/include

lib_LTLIBRARIES = libgea.la 

AM_LDFLAGS = -L$(libdir) -L$(OFFLINE_MAIN)/lib

libgea_la_LIBADD = \
  -lPISARoot \
  -lWrappers

noinst_HEADERS = libgea_LinkDef.h

include_HEADERS = \
  bbcgeo.h \
  bbcgeoWrapper.h \
  bbcghit.h \
  bbcghitWrapper.h \
  crkghit.h \
  crkghitWrapper.h \
  crkpar.h \
  crkparWrapper.h \
  crkparu.h \
  crkparuWrapper.h \
  ctra.h \
  ctraWrapper.h \
  dcgeo.h \
  dcgeoWrapper.h \
  dcghit.h \
  dcghitWrapper.h \
  dio_trk.hh \
  emcgeo.h \
  emcgeoWrapper.h \
  emcghit.h \
  emcghitWrapper.h \
  emcpar.h \
  emcparWrapper.h \
  encodeString.h \
  fkin.h \
  fkinWrapper.h \
  hbdghit.h \
  hbdghitWrapper.h \
  header.h \
  headerWrapper.h \
  itrpar.h \
  itrparWrapper.h \
  mumgeo.h \
  mumgeoWrapper.h \
  mumhits.h \
  mumhitsWrapper.h \
  mungeo.h \
  mungeoWrapper.h \
  munhits.h \
  munhitsWrapper.h \
  pc23par.h \
  pc23parWrapper.h \
  pcghit.h \
  pcghitWrapper.h \
  mupcghit.h \
  mupcghitWrapper.h \
  primary.h \
  primaryWrapper.h \
  pythia.h \
  pythiaWrapper.h \
  tecghit.h \
  tecghitWrapper.h \
  tecpar.h \
  tecparWrapper.h \
  tfwghit.h \
  tfwghitWrapper.h \
  tofghit.h \
  tofghitWrapper.h \
  tofpara.h \
  tofparaWrapper.h \
  tofparu.h \
  tofparuWrapper.h \
  AerGetGEA.h \
  BbcGetGEA.h \
  CrkGetGEA.h \
  DchGetGEA.h \
  GeaTrkStack.h \
  HbdGetGEA.h \
  KinGetGEA.h \
  MuiGetGEA.h  \
  MuPCGetGEA.h \
  MutGetGEA.h  \
  NCCGetGEA.h \
  NCCPisaHit.h \
  NCCPisaHitv1.h \
  NCCPisaHitv2.h \
  NCCSnglPisaHit.h \
  NCCSnglPisaHitv1.h \
  NCCSnglPisaHitv2.h \
  MPCEXABSPisaHit.h \
  MPCEXABSPisaHitv1.h \
  MPCEXABSSnglPisaHit.h \
  MPCEXABSSnglPisaHitv1.h \
  MPCFPLTPisaHit.h \
  MPCFPLTPisaHitv1.h \
  MPCFPLTSnglPisaHit.h \
  MPCFPLTSnglPisaHitv1.h \
  MPCEXENTPisaHit.h \
  MPCEXENTPisaHitv1.h \
  MPCEXENTSnglPisaHit.h \
  MPCEXENTSnglPisaHitv1.h \
  PadGetGEA.h \
  TecGetGEA.h \
  TfwGetGEA.h \
  TofGetGEA.h \
  ZdcGetGEA.h

libgea_la_SOURCES = \
  AerGetGEA.cc \
  BbcGetGEA.cc \
  CrkGetGEA.cc \
  DchGetGEA.cc \
  GeaTrkStack.cc \
  HbdGetGEA.cc \
  KinGetGEA.cc \
  MuiGetGEA.cc  \
  MuPCGetGEA.cc \
  MutGetGEA.cc  \
  NCCGetGEA.cc \
  NCCPisaHit.C \
  NCCPisaHitv1.C \
  NCCPisaHitv2.C \
  NCCSnglPisaHit.C \
  NCCSnglPisaHitv1.C \
  NCCSnglPisaHitv2.C \
  MPCEXABSPisaHit.C \
  MPCEXABSPisaHitv1.C \
  MPCEXABSSnglPisaHit.C \
  MPCEXABSSnglPisaHitv1.C \
  MPCFPLTPisaHit.C \
  MPCFPLTPisaHitv1.C \
  MPCFPLTSnglPisaHit.C \
  MPCFPLTSnglPisaHitv1.C \
  MPCEXENTPisaHit.C \
  MPCEXENTPisaHitv1.C \
  MPCEXENTSnglPisaHit.C \
  MPCEXENTSnglPisaHitv1.C \
  PadGetGEA.cc \
  TecGetGEA.cc \
  TfwGetGEA.cc \
  TofGetGEA.cc \
  ZdcGetGEA.cc \
  bbcgeoWrapper.C \
  bbcghitWrapper.C \
  crkghitWrapper.C \
  crkparWrapper.C \
  crkparuWrapper.C \
  ctraWrapper.C \
  dcgeoWrapper.C \
  dcghitWrapper.C \
  emcgeoWrapper.C \
  emcghitWrapper.C \
  emcparWrapper.C \
  encodeString.C \
  fkinWrapper.C \
  hbdghitWrapper.C \
  headerWrapper.C \
  itrparWrapper.C \
  mumgeoWrapper.C \
  mumhitsWrapper.C \
  mungeoWrapper.C \
  munhitsWrapper.C \
  pc23parWrapper.C \
  pcghitWrapper.C \
  mupcghitWrapper.C \
  primaryWrapper.C \
  pythiaWrapper.C \
  tecghitWrapper.C \
  tecparWrapper.C \
  tfwghitWrapper.C \
  tofghitWrapper.C \
  tofparaWrapper.C \
  tofparuWrapper.C

nodist_libgea_la_SOURCES = libgea_Dict.C

noinst_PROGRAMS = \
  testexternals

testexternals_LDADD = \
  libgea.la

testexternals.C:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

libgea_Dict.C: \
  bbcgeoWrapper.h \
  bbcghitWrapper.h \
  crkghitWrapper.h \
  crkparuWrapper.h \
  crkparWrapper.h \
  ctraWrapper.h \
  dcgeoWrapper.h \
  dcghitWrapper.h \
  emcgeoWrapper.h \
  emcghitWrapper.h \
  emcparWrapper.h \
  fkinWrapper.h \
  hbdghitWrapper.h \
  headerWrapper.h \
  itrparWrapper.h \
  mumgeoWrapper.h \
  mumhitsWrapper.h \
  mungeoWrapper.h \
  munhitsWrapper.h \
  mupcghitWrapper.h \
  NCCPisaHit.h \
  NCCPisaHitv1.h \
  NCCPisaHitv2.h \
  NCCSnglPisaHit.h \
  NCCSnglPisaHitv1.h \
  NCCSnglPisaHitv2.h \
  MPCEXABSPisaHit.h \
  MPCEXABSPisaHitv1.h \
  MPCEXABSSnglPisaHit.h \
  MPCEXABSSnglPisaHitv1.h \
  MPCFPLTPisaHit.h \
  MPCFPLTPisaHitv1.h \
  MPCFPLTSnglPisaHit.h \
  MPCFPLTSnglPisaHitv1.h \
  MPCEXENTPisaHit.h \
  MPCEXENTPisaHitv1.h \
  MPCEXENTSnglPisaHit.h \
  MPCEXENTSnglPisaHitv1.h \
  pc23parWrapper.h \
  pcghitWrapper.h \
  primaryWrapper.h \
  pythiaWrapper.h \
  tecghitWrapper.h \
  tecparWrapper.h \
  tfwghitWrapper.h \
  tofghitWrapper.h \
  tofparaWrapper.h \
  tofparuWrapper.h \
  libgea_LinkDef.h
	rootcint -f $@ -c $(INCLUDES) $^

clean-local:
	rm -f *Dict*

testexternals_SOURCES = testexternals.C
