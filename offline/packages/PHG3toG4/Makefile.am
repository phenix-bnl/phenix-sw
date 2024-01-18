AUTOMAKE_OPTIONS = foreign

AM_CPPFLAGS = \
  -I$(includedir) \
  -I$(OFFLINE_MAIN)/include \
  -I$(ROOTSYS)/include \
  -I$(G4_MAIN)/include \
  -I$(G4_MAIN)/include/Geant4

AM_LDFLAGS = \
  -L$(libdir) \
  -L$(OFFLINE_MAIN)/lib \
  -L$(G4_MAIN)/lib

lib_LTLIBRARIES = libPHG3toG4.la 


libPHG3toG4_la_LIBADD = \
  -lSubsysReco \
  -lpythia8 \
  -lHepMC \
  -lgeant4vmc \
  -lg4root \
  -lPHHepMCNode


libPHG3toG4_la_SOURCES = \
  PHPy8GenTrigger.cc \
  PHPy8ParticleTrigger.cc \
  PHG3toG4PrimaryGeneratorAction.cc \
  PHG3toG4ParticleGun.cc \
  PHG3toG4EventAction.cc \
  PHG3toG4StackingAction.cc \
  PHG3toG4SteppingAction.cc \
  PHG3toG4TrackingAction.cc \
  PHG3toG4MuonArmHit.cc \
  PHG3toG4MuonArmSD.cc \
  PHG3toG4MuonArmPara.cc \
  PHG3toG4MuonIDHit.cc \
  PHG3toG4MuonIDSD.cc \
  PHG3toG4MuonIDPara.cc \
  PHG3toG4MuonRPCHit.cc \
  PHG3toG4MuonRPCSD.cc \
  PHG3toG4SvxHit.cc \
  PHG3toG4SvxSD.cc \
  PHG3toG4SvxPara.cc \
  PHG3toG4BbcHit.cc \
  PHG3toG4BbcSD.cc \
  PHG3toG4BbcPara.cc \
  PHG3toG4KinHit.cc \
  PHG3toG4PriHit.cc \
  PHG3toG4PostDetConstruction.cc \
  PHG3toG4MagneticField.cc \
  PHG3toG4RootManager.cc \
  PHG3toG4UserTrackInfo.cc \
  PHG3toG4UserPrimaryParticleInfo.cc \
  PHG3toG4GeneralTrigger.cc \
  PHG3toG4ActivityTrigger.cc \
  PHG3toG4.cc \
  PHG3toG4_Dict.C

BUILT_SOURCES = \
  testexternals.cc

testexternals_SOURCES = \
  testexternals.cc

noinst_PROGRAMS = \
  testexternals

testexternals.cc:
	echo "//*** this is a generated file. Do not commit, do not edit" > $@
	echo "int main()" >> $@
	echo "{" >> $@
	echo "  return 0;" >> $@
	echo "}" >> $@

PHG3toG4_Dict.C: \
  PHPy8GenTrigger.h \
  PHPy8ParticleTrigger.h \
  PHG3toG4PrimaryGeneratorAction.h \
  PHG3toG4ParticleGun.h \
  PHG3toG4EventAction.h \
  PHG3toG4StackingAction.h \
  PHG3toG4SteppingAction.h \
  PHG3toG4TrackingAction.h \
  PHG3toG4MuonArmHit.h \
  PHG3toG4MuonArmSD.h \
  PHG3toG4MuonArmPara.h \
  PHG3toG4MuonIDHit.h \
  PHG3toG4MuonIDSD.h \
  PHG3toG4MuonIDPara.h \
  PHG3toG4MuonRPCHit.h \
  PHG3toG4MuonRPCSD.h \
  PHG3toG4SvxHit.h \
  PHG3toG4SvxSD.h \
  PHG3toG4SvxPara.h \
  PHG3toG4BbcHit.h \
  PHG3toG4BbcSD.h \
  PHG3toG4BbcPara.h \
  PHG3toG4KinHit.h \
  PHG3toG4PriHit.h \
  PHG3toG4PostDetConstruction.h	\
  PHG3toG4MagneticField.h \
  PHG3toG4RootManager.h \
  PHG3toG4UserTrackInfo.h \
  PHG3toG4UserPrimaryParticleInfo.h \
  PHG3toG4GeneralTrigger.h \
  PHG3toG4ActivityTrigger.h \
  PHG3toG4.h \
  PHG3toG4LinkDef.h
	rootcint -f $@ -c -p $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $^

clean-local:
	rm -f *Dict*
	rm -f testexternals*
