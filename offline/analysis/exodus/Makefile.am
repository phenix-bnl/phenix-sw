AUTOMAKE_OPTIONS = foreign

AM_CXXFLAGS = -Wall -Wextra -Werror -pedantic \
	      -Wold-style-cast \
	      -Woverloaded-virtual \
	      -Wsign-promo \
	      -Wconversion \
	      -Wuninitialized \
	      -Wredundant-decls \
	      -Wcast-align

INCLUDES := -isystem `root-config --incdir` -isystem $(OFFLINE_MAIN)/include

lib_LTLIBRARIES = libexodus.la

libexodus_la_SOURCES = \
AdjustDecaySum.cpp \
ApplyCERESResolution.cpp \
ApplyPHENIXResolution.cpp \
ApplyPHENIXResolutionRun10.cpp \
ApplyResolution.cpp \
BookROOTObjects.cpp \
Bremsstrahlung.cpp \
CleanupRandom.cpp \
CloseInputFile.cpp \
CloseOutputFile.cpp \
CloseFullEventFile.cpp \
CloseROOTFile.cpp \
CreateDalitzLeptonPairMass.cpp \
DalitzDecay.cpp \
DecayMachine.cpp \
DoubleDalitzDecay.cpp \
DecayList.cpp \
DefineDecayProperties.cpp \
DefineParticleProperties.cpp \
DoAllDecays.cpp \
ElectronInPHENIXAcceptance.cpp \
FillROOTObjects.cpp \
Filler.cpp \
FormFactor.cpp \
GenerateParticles.cpp \
GenerateSingleParticles.cpp \
GenerateSingleParticlesEMCal.cpp \
GenerateFullEvent.cpp \
GenerateFullEventFlow.cpp \
GetEventFromInputStream.cpp \
GetPYTHIAEventFromInputStream.cpp \
InCERESAcceptance.cpp \
InitializeExpPt.cpp \
InitializeM.cpp \
InitializePtCERES.cpp \
InitializePtISR.cpp \
InitializePtPHENIX.cpp \
InitializeRandom.cpp \
InitializeRandomHist.cpp \
InitializeSetup.cpp \
InitializeWhitePt.cpp \
InitializeWhiteY.cpp \
InitializeYCERES.cpp \
InitializeYISR.cpp \
InitializeYPHENIX.cpp \
InPHENIXAcceptance.cpp \
Momentum.cpp \
OpenInputFile.cpp \
OpenOutputFile.cpp \
OpenFullEventFile.cpp \
OpenROOTFile.cpp \
PairInAcceptance.cpp \
Particle.cpp \
ParticleGenerator.cpp \
ParticleGeneratorList.cpp \
ParticleIsLepton.cpp \
ParticleList.cpp \
ParticleProperty.cpp \
ParticlePropertyList.cpp \
PHENIXFiducial.cpp \
PHENIXFilter.cpp \
PHENIXTrigger.cpp \
ReadParticleListFromFile.cpp \
ThreeBodyDecay.cpp \
Tools.cpp \
TwoBodyDecay.cpp \
WriteEventToOutputStream.cpp \
WriteParticleListToFile.cpp \
WriteFullEventFile.cpp \
WriteParticleListToOscarFile.cpp \
WriteParticleListToOscarFile_decay.cpp

pkginclude_HEADERS = \
AdjustDecaySum.h \
ApplyCERESResolution.h \
ApplyPHENIXResolution.h \
ApplyPHENIXResolutionRun10.h \
ApplyResolution.h \
BookROOTObjects.h \
Bremsstrahlung.h \
CleanupRandom.h \
CloseFullEventFile.h \
CloseInputFile.h \
CloseOutputFile.h \
CloseROOTFile.h \
CreateDalitzLeptonPairMass.h \
DalitzDecay.h \
DecayList.h \
DecayMachine.h \
DeclareROOTObjects.h \
DefineDecayProperties.h \
DefineParticleProperties.h \
DoAllDecays.h \
DoubleDalitzDecay.h \
ElectronInPHENIXAcceptance.h \
FillROOTObjects.h \
Filler.h \
FormFactor.h \
GenerateFullEvent.h \
GenerateFullEventFlow.h \
GenerateParticles.h \
GenerateSingleParticles.h \
GenerateSingleParticlesEMCal.h \
GetEventFromInputStream.h \
GetPYTHIAEventFromInputStream.h \
InCERESAcceptance.h \
InPHENIXAcceptance.h \
InitializeExpPt.h \
InitializeM.h \
InitializePtCERES.h \
InitializePtISR.h \
InitializePtPHENIX.h \
InitializeRandom.h \
InitializeRandomHist.h \
InitializeSetup.h \
InitializeWhitePt.h \
InitializeWhiteY.h \
InitializeYCERES.h \
InitializeYISR.h \
InitializeYPHENIX.h \
Momentum.h \
OpenFullEventFile.h \
OpenInputFile.h \
OpenOutputFile.h \
OpenROOTFile.h \
PHENIXFiducial.h \
PHENIXFilter.h \
PHENIXTrigger.h \
PairInAcceptance.h \
Particle.h \
ParticleGenerator.h \
ParticleGeneratorList.h \
ParticleIsLepton.h \
ParticleList.h \
ParticleProperty.h \
ParticlePropertyList.h \
ReadParticleListFromFile.h \
ThreeBodyDecay.h \
Tools.h \
TwoBodyDecay.h \
WriteEventToOutputStream.h \
WriteFullEventFile.h \
WriteParticleListToFile.h \
WriteParticleListToOscarFile.h \
WriteParticleListToOscarFile_decay.h

bin_PROGRAMS = \
  exodus \
  exodus_generate \
  exodus_decay \
  OSCARtoROOT \
  OSCARtoROOT_fullevent \
  JAMtoROOT

dist_pkgdata_DATA = \
defined_decays.txt \
defined_particles.txt

exodus_SOURCES = exodus.cpp
exodus_generate_SOURCES = exodus_generate.cpp
exodus_decay_SOURCES = exodus_decay.cpp
OSCARtoROOT_SOURCES = OSCARtoROOT.cpp
OSCARtoROOT_fullevent_SOURCES = OSCARtoROOT_fullevent.cpp
JAMtoROOT_SOURCES = JAMtoROOT.cpp

LDADD = libexodus.la
libexodus_la_LDFLAGS = `root-config --libs`

check_PROGRAMS = tests
TESTS = $(check_PROGRAMS)
tests_SOURCES = tests.cpp
TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$$PWD/.libs:$$LD_LIBRARY_PATH
tests_LDFLAGS = libexodus.la
