#include "emcEfficiency.h"

// STL
#include <iostream>
#include <cassert>
#include <cmath>
#include <string>
#include <map>
#include <vector>
#include <iomanip>
#include <fstream>

// Plain old C
#include <string.h>

// ROOT stuff
#include "TDirectory.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TFile.h"
#include "TSystem.h"
#include "TROOT.h"
#include "TStyle.h"
#include "TNamed.h"
#include "TCanvas.h"
#include "TPaveText.h"
#include "TLatex.h"
#include "TStyle.h"
#include "TString.h"
#include "TNtuple.h"

// Our emc/emc-embed/pi0 stuff
#include "utils.h"
#include "PHNodeHelper.h"
#include "EventSorting.h"
#include "HistogramCollection.h"
#include "effParticle.h"
#include "effPair.h"
#include "mEmcGeometryModule.h"
#include "mEmcToolsModule.h"
#include "dEmcGeaTrackWrapper.h"
#include "dEmcGeaClusterTrackWrapper.h"
#include "EmcIndexer.h"

// New emc-embed versioned objects

#include "EmcGeaParticlev2.h"
#include "EmcGeaClusterv2.h"

// EmcGeaParticle * emcEfficiency::getNewGeaParticle()
// { return (new EmcGeaParticlev2()); }
// EmcGeaCluster * emcEfficiency::getNewGeaCluster()
// { return (new EmcGeaClusterv2()); }

// PHOOL business
#include "PHString.h"
#include "PHNodeIOManager.h"


// Constants used for cut & histo names
// and to work-around the map<string> memory leak

// non-PID cuts

static const std::string kNoCut = "NoCut";
int kNoCutMask = (1<<0);
static const std::string kFiduCut = "FiduCut";
int kFiduCutMask = (1<<1);
static const std::string kNoW3Cut = "NoW3Cut";
int kNoW3CutMask = (1<<2);
static const std::string kDeadWarn5x5Cut = "DeadWarn5x5Cut";
int kDeadWarn5x5CutMask = (1<<3);
static const std::string kDeadWarn3x3Cut = "DeadWarn3x3Cut";
int kDeadWarn3x3CutMask = (1<<4);
static const std::string kEnergyCut = "EnergyCut";
int kEnergyCutMask = (1<<5);
static const std::string kCosCut = "CosCut";
int kCosCutMask = (1<<6);

static const std::string kPairSameSectCut = "PairSameSectCut";
int kPairSameSectCutMask = (1<<20);
static const std::string kPairSameArmCut = "PairSameArmCut";
int kPairSameArmCutMask = (1<<21);

// conversion studies

static const std::string kConversionOffCut = "ConversionOffCut";
int kConversionOffCutMask = (1<<7);
static const std::string kElectronEvtsCut = "ElectronEvtsCut";
int kElectronEvtsCutMask = (1<<8);

// PID cuts

static const std::string kAsym1Cut = "Asym1Cut";
int kAsym1CutMask = (1<<10);
static const std::string kAsym2Cut = "Asym2Cut";
int kAsym2CutMask = (1<<11);

static const std::string kToF1Cut = "ToF1Cut";
int kToF1CutMask = (1<<12);
static const std::string kToF2Cut = "ToF2Cut";
int kToF2CutMask = (1<<13);
static const std::string kRToFCut = "RToFCut";
int kRToFCutMask = (1<<14);

static const std::string kChiSq1Cut = "ChiSq1Cut";
int kChiSq1CutMask = (1<<15);
static const std::string kChiSq2Cut = "ChiSq2Cut";
int kChiSq2CutMask = (1<<16);

// Composite cuts (all possible combinations defined here)

static const std::string kFiduDeadWarn5x5Cut = "FiduDeadWarn5x5Cut";
static const std::string kFiduDeadWarn3x3Cut = "FiduDeadWarn3x3Cut";

static const std::string kFiduNoW3DeadWarn5x5Cut = "FiduNoW3DeadWarn5x5Cut";
static const std::string kFiduNoW3DeadWarn3x3Cut = "FiduNoW3DeadWarn3x3Cut";

//static const std::string kBasicCuts5x5 = "BasicCuts_FiduNoW3DeadWarn5x5EnergyCosCut";
//static const std::string kBasicCuts3x3 = "BasicCuts_FiduNoW3DeadWarn3x3EnergyCosCut";

static const std::string kBasicCuts5x5 = "BasicCuts_FiduNoW3DeadWarn5x5PairSameArmEnergyCut";
static const std::string kBasicCuts3x3 = "BasicCuts_FiduNoW3DeadWarn3x3PairSameArmEnergyCut";

//static const std::string kBasicCuts5x5 = "BasicCuts_FiduNoW3DeadWarn5x5PairSameSectEnergyCut";
//static const std::string kBasicCuts3x3 = "BasicCuts_FiduNoW3DeadWarn3x3PairSameSectEnergyCut";

static const std::string kBasicCuts5x5Asym1Cut = "BasicCuts5x5Asym1Cut";
static const std::string kBasicCuts5x5Asym2Cut = "BasicCuts5x5Asym2Cut";
static const std::string kBasicCuts3x3Asym1Cut = "BasicCuts3x3Asym1Cut";
static const std::string kBasicCuts3x3Asym2Cut = "BasicCuts3x3Asym2Cut";

static const std::string kBasicCuts5x5Asym1ToF1Cut = "BasicCuts5x5Asym1ToF1Cut";
static const std::string kBasicCuts5x5Asym1ToF2Cut = "BasicCuts5x5Asym1ToF2Cut";
static const std::string kBasicCuts3x3Asym1ToF1Cut = "BasicCuts3x3Asym1ToF1Cut";
static const std::string kBasicCuts3x3Asym1ToF2Cut = "BasicCuts3x3Asym1ToF2Cut";

static const std::string kBasicCuts5x5Asym1ChiSq1Cut = "BasicCuts5x5Asym1ChiSq1Cut";
static const std::string kBasicCuts5x5Asym1ChiSq2Cut = "BasicCuts5x5Asym1ChiSq2Cut";
static const std::string kBasicCuts3x3Asym1ChiSq1Cut = "BasicCuts3x3Asym1ChiSq1Cut";
static const std::string kBasicCuts3x3Asym1ChiSq2Cut = "BasicCuts3x3Asym1ChiSq2Cut";

static const std::string kBasicCuts5x5Asym1ChiSq1ToF1Cut = "BasicCuts5x5Asym1ChiSq1ToF1Cut";
static const std::string kBasicCuts5x5Asym1ChiSq1ToF2Cut = "BasicCuts5x5Asym1ChiSq1ToF2Cut";
static const std::string kBasicCuts3x3Asym1ChiSq1ToF1Cut = "BasicCuts3x3Asym1ChiSq1ToF1Cut";
static const std::string kBasicCuts3x3Asym1ChiSq1ToF2Cut = "BasicCuts3x3Asym1ChiSq1ToF2Cut";

static const std::string kBasicCuts5x5Asym2ChiSq1ToF1Cut = "BasicCuts5x5Asym2ChiSq1ToF1Cut";
static const std::string kBasicCuts5x5Asym2ChiSq1ToF2Cut = "BasicCuts5x5Asym2ChiSq1ToF2Cut";
static const std::string kBasicCuts3x3Asym2ChiSq1ToF1Cut = "BasicCuts3x3Asym2ChiSq1ToF1Cut";
static const std::string kBasicCuts3x3Asym2ChiSq1ToF2Cut = "BasicCuts3x3Asym2ChiSq1ToF2Cut";

static const std::string kBasicCuts5x5Asym1ChiSq2ToF1Cut = "BasicCuts5x5Asym1ChiSq2ToF1Cut";
static const std::string kBasicCuts5x5Asym1ChiSq2ToF2Cut = "BasicCuts5x5Asym1ChiSq2ToF2Cut";
static const std::string kBasicCuts3x3Asym1ChiSq2ToF1Cut = "BasicCuts3x3Asym1ChiSq2ToF1Cut";
static const std::string kBasicCuts3x3Asym1ChiSq2ToF2Cut = "BasicCuts3x3Asym1ChiSq2ToF2Cut";

// composite (no fidu or dead-warn cut)

static const std::string kNoW3EnergyCosAsym1Cut = "NoW3EnergyCosAsym1Cut";
static const std::string kNoW3EnergyCosAsym1ChiSq2ToF2Cut = "NoW3EnergyCosAsym1ChiSq2ToF2Cut";

// composite for different tests: conversion, RToF, ...

static const std::string kBasicCuts5x5ConvOffCut  = "BasicCuts5x5ConvOffCut";
static const std::string kBasicCuts5x5Asym2RToFCut = "BasicCuts5x5Asym2RToFCut";
static const std::string kBasicCuts5x5Asym1ChiSq1ToF1ConvOffCut = "BasicCuts5x5Asym1ChiSq1ToF1ConvOffCut";
static const std::string kBasicCuts5x5Asym1ChiSq1ToF1ElectronEvtsCut = "BasicCuts5x5Asym1ChiSq1ToF1ElectronEvtsCut";
static const std::string kBasicCuts5x5Asym2ChiSq1RToFCut = "BasicCuts5x5Asym2ChiSq1RToFCut";
static const std::string kBasicCuts5x5Asym2ChiSq2RToFCut = "BasicCuts5x5Asym2ChiSq2RToFCut";

// eta studies with Saskia's acceptance, fiduc. & dead implementation
static const std::string kChiSq2ToF2Cut = "ChiSq2ToF2Cut";
static const std::string kEnergyChiSq2ToF2Cut = "EnergyChiSq2ToF2Cut";
static const std::string kNoW3EnergyChiSq2ToF2Cut = "NoW3EnergyChiSq2ToF2Cut";
 
// histograms

static const std::string kgeneral = "general";
static const std::string kSimultracks = "Simultracks";

static const std::string khCentrality = "hCentrality";
static const std::string khCentClass = "hCentClass";

static const std::string khArmSector = "hArmSector";
static const std::string khMeasx = "hMeasx";
static const std::string khMeasy = "hMeasy";
static const std::string khMeasz = "hMeasz";
static const std::string khMeasyz_e = "hMeasyz_e";
static const std::string khMeasyz_w = "hMeasyz_w";
static const std::string khMeasyz_w_fiduc = "hMeasyz_w_fiduc";
static const std::string khMeasyz_e_fiduc = "hMeasyz_e_fiduc";
static const std::string khEcore = "hEcore";
static const std::string khEcorePtot = "hEcorePtot";
static const std::string khTof = "hTof";
static const std::string khChi2 = "hChi2";
static const std::string khMease = "hMease";
static const std::string kEta_vs_Phi = "Eta_vs_Phi";
static const std::string khRelatTof_vs_Asym = "hRelatTof_vs_Asym";
static const std::string khMinv_vs_Asym = "hMinv_vs_Asym";
static const std::string khfiducial_local_yz_geatrack = "hfiducial_local_yz_geatrack";
static const std::string khfiducial_local_yz_geaclustertrack = "hfiducial_local_yz_geaclustertrack";
static const std::string khTotalClusterMul = "hTotalClusterMul";
static const std::string khEmbedClusterMul = "hEmbedClusterMul";

static const std::string khXVtxPairs = "hXVtxPairs";

static const std::string khRecoParticleEnergyAsym = "hRecoParticleEnergyAsym";
static const std::string khRecoParticleMinvPt = "hRecoParticleMinvPt";
static const std::string khRecoParticleMinvPtVtx = "hRecoParticleMinvPtVtx";

static const std::string khInitialPt = "hInitialPt";
static const std::string khInitialPtConversionOff = "hInitialPtConversionOff";
static const std::string khInitialPtUnweighted = "hInitialPtUnweighted";
static const std::string khInitialPtUnweightedCorr = "hInitialPtUnweightedCorr";

static const std::string khRecoParticleMinvPeak = "hRecoParticleMinvPeak";
static const std::string khRecoParticleMinvSigma = "hRecoParticleMinvSigma";
static const std::string khEfficiency_fit = "hEfficiency_fit";
static const std::string khEfficiency_counts = "hEfficiency_counts";
static const std::string khSignal2Background = "hSignal2Background";

static const std::string khSimulPi0MinvPt = "hSimulPi0MinvPt";
static const std::string khSimulParticleMinvPt = "khSimulParticleMinvPt";
static const std::string khEffsimul_counts= "hEffsimul_counts";

// control ntuple
// TNtuple *NtupRecoParticleMinvPtImpAngle = new TNtuple( "NtupRecoParticleMinvPtImpAngle",
// 						       "NtupRecoParticleMinvPtImpAngle",
// 						       "Cut:CC:Minv:Pt:ImpAngle:y1:z1:y2:z2");


//_____________________________________________________________________________
emcEfficiency::emcEffValue& 
emcEfficiency::emcEffValue::operator*=(double v)
{
  fValue *= v;
  fValueMin *= v;
  fValueMax *= v;
  return *this;
}

void
emcEfficiency::emcEffValue::set(double value, double error)
{
  fValue = value;
  fValueMin = value-error/2.0;
  fValueMax = value+error/2.0;
}

void
emcEfficiency::emcEffValue::set(double value, double valmin, double valmax)
{
  fValue = value;
  fValueMin = valmin;
  fValueMax = valmax;
}

//_____________________________________________________________________________
emcEfficiency::emcEfficiency( const int run, const int codeOfparticle ) :  // default is Run-2 pi0
  fEfficiencyFile(0),fIOmanager(0),fNodeTOP(0),fNodeEVA(0),fNodeUDST(0),
  fEmcGeaTrack(0),fEmcGeaClusterTrack(0),
  fEmcGeaPart(0),fEmcGeaCluster(0),
  fEventSorting(0),
  fListOfMergedParticles(0),fListOfMergedPairs(0),
  fListOfSimulParticles(0),fListOfSimulPairs(0),
  fNMergedParticles(0),fNMergedPairs(0),
  fNSimulParticles(0),fNSimulPairs(0),
  fNEvents(0),fNSuitableEvents(0),fNConvEvents(0),fNConvEvents2(0),
  fRun2ExodusFluctCorrection(0),fVerbose(0)
{

  fRUN = run;
  fEffParticleCode = codeOfparticle; 
  assert( fEffParticleCode==7 || fEffParticleCode==17);

  fEmbedFileName = "";
  fEfficiencyFileName = "";

  fCorrectRun2ExodusFluct = false;
  
  fUseFastMCDeadWarnLoss = false ; // default

  cout << "<I> Efficiency calculation SETTINGS for ** " ;
  if (fEffParticleCode==7) cout << "pi0 ** : " << endl;
  else if (fEffParticleCode==17) cout << "eta ** : " << endl;

  if (run == 2)
    { 
      cout << "<I> Using RUN-2 cuts/centralities." << endl;
      fMinClusterEnergy = fMIN_CLUSTER_ENERGY_RUN2_pi0_2ndpass;

      if ( fEffParticleCode == 7 )
	{
	  fMaxAsym1 = fMAX_ASYMMETRY1;
	  fMaxAsym2 = fMAX_ASYMMETRY2_pi0_2ndpass;

	  fMaxChiSq1 = fMAX_CHISQ1;
	  fMaxChiSq2 = fMAX_CHISQ2;

	  fMaxTof1 = fMAX_TOF1_RUN2_pi0_2ndpass;
	  fMaxTof2 = fMAX_TOF2_RUN2_pi0_2ndpass;
	  fRTof = fMAX_TOF2_RUN2_pi0_2ndpass;

	  fMinvLowCut = fMINV_LOWCUT_PI0;

	  fFIT_MINV_LO = fFIT_LOWER_BOUND_PI0;
	  fFIT_MINV_UP = fFIT_UPPER_BOUND_PI0;
	  //fBCKGD_MINV_LO = fBCKGD_ABOVE_PI0;

	  fMINV_MIN = 0.0;
	  fMINV_MAX = 1.0;
	  fMINV_NBINS = (size_t)((fMINV_MAX-fMINV_MIN)/fMINV_BINWIDTH);

	  // PPG014 fidu cuts (10cm)
// 	  fFiduXmin = fFIDUCIAL_LOCAL_X_MIN;
// 	  fFiduXmax = fFIDUCIAL_LOCAL_X_MAX;
// 	  fFiduYmin = fFIDUCIAL_LOCAL_Y_MIN;
// 	  fFiduYmax = fFIDUCIAL_LOCAL_Y_MAX;

	  // Latest fidu cuts (8cm)
	  fFiduXmin = fFIDUCIAL_LOCAL_X_MIN2;
	  fFiduXmax = fFIDUCIAL_LOCAL_X_MAX2;
	  fFiduYmin = fFIDUCIAL_LOCAL_Y_MIN2;
	  fFiduYmax = fFIDUCIAL_LOCAL_Y_MAX2;

	  fCorrectRun2ExodusFluct = true;

	  fUseFastMCDeadWarnLoss = true ;
	  fExtraDeadWarn5x5EfficLoss = ADDITIONAL_RUN2_FIDU8cm_DEADWARN5x5_EFF_LOSS;
	  fExtraDeadWarn3x3EfficLoss = ADDITIONAL_RUN2_FIDU8cm_DEADWARN3x3_EFF_LOSS;
	}
      else if ( fEffParticleCode == 17 )
	{
	  fMaxAsym1 = fMAX_ASYMMETRY1_eta;
	  fMaxAsym2 = fMAX_ASYMMETRY2_eta; 

	  fMaxChiSq1 = fMAX_CHISQ1;
	  fMaxChiSq2 = fMAX_CHISQ2;

	  fMaxTof1 = 3.0;
	  fMaxTof2 = 1.2;
	  fRTof = -9999.; // unused

	  fMinvLowCut = fMINV_LOWCUT_ETA;

	  fFIT_MINV_LO = fFIT_LOWER_BOUND_ETA;
	  fFIT_MINV_UP = fFIT_UPPER_BOUND_ETA;
	  //fBCKGD_MINV_LO = fBCKGD_LOWER_ETA;

	  fMINV_MIN = 0.0;
	  fMINV_MAX = 1.0;
	  // binwidth = 10 MeV/c2 instead of default 5
	  fMINV_NBINS = (size_t)(0.5*(fMINV_MAX-fMINV_MIN)/fMINV_BINWIDTH); 

	  // Latest fidu cuts (8cm)
	  fFiduXmin = fFIDUCIAL_LOCAL_X_MIN2;
	  fFiduXmax = fFIDUCIAL_LOCAL_X_MAX2;
	  fFiduYmin = fFIDUCIAL_LOCAL_Y_MIN2;
	  fFiduYmax = fFIDUCIAL_LOCAL_Y_MAX2;

	  fUseFastMCDeadWarnLoss = true ; // default
	  fExtraDeadWarn5x5EfficLoss = ADDITIONAL_RUN2_FIDU8cm_DEADWARN5x5_EFF_LOSS;
	  fExtraDeadWarn3x3EfficLoss = ADDITIONAL_RUN2_FIDU8cm_DEADWARN3x3_EFF_LOSS;
	}

      fPT_MIN =  0.0;
      fPT_MAX = 20.0;
      fPT_NBINS = (size_t)((fPT_MAX-fPT_MIN)/fPT_BINWIDTH);

    }
  else if (run == 1) 
    { 
      cout << "<I> Using RUN-1 cuts/centralities." << endl;
      fMinClusterEnergy = fMIN_CLUSTER_ENERGY_RUN1;
      fMaxAsym1 = fMAX_ASYMMETRY1;
      fMaxAsym2 = fMAX_ASYMMETRY2;
      fMaxChiSq1 = fMAX_CHISQ1;
      fMaxChiSq2 = -999.;
      fMaxTof1 = fMAX_TOF1_RUN1;
      fMaxTof2 = -999.;
      fRTof = -999.;

      fPT_MIN =  0.0;
      fPT_MAX =  5.0;
      fPT_NBINS = (size_t)((fPT_MAX-fPT_MIN)/fPT_BINWIDTH);

      fMINV_MIN = 0.0;
      fMINV_MAX = 0.6;
      fMINV_NBINS = (size_t)((fMINV_MAX-fMINV_MIN)/fMINV_BINWIDTH);

      cout << "<W> FIXME: Using some of Run-2 cuts ... Probably incorrect ! " << endl;
      fFiduXmin = fFIDUCIAL_LOCAL_X_MIN;
      fFiduXmax = fFIDUCIAL_LOCAL_X_MAX;
      fFiduYmin = fFIDUCIAL_LOCAL_Y_MIN;
      fFiduYmax = fFIDUCIAL_LOCAL_Y_MAX;

      fUseFastMCDeadWarnLoss = true ; // default
      fExtraDeadWarn5x5EfficLoss = ADDITIONAL_RUN2_FIDU10cm_DEADWARN5x5_EFF_LOSS; 
    }
  else
    {
      cerr << "<E> I don't know this RUN (year): " << run << endl;
      cerr << "    Try: emcEfficiency eff(2); or emcEfficiency eff(1); " << endl;
      return;
    }

  fListOfMergedParticles = new TClonesArray("effParticle",500,1);
  fListOfMergedPairs = new TClonesArray("effPair",500,1);

  fListOfSimulParticles = new TClonesArray("effParticle",500,1);
  fListOfSimulPairs = new TClonesArray("effPair",500,1);

  fEmbedObjectsType = 1; // new type of embedding objects is default

  defaultCentralityClasses();
  defaultHagWeightPerCentrality();
  defineCutNames();

  fBookedHistos = false;

  setFixedExtractionWindows(true); // default
  computeEfficiencyFor("PbSc"); // default

  geometryModule(); // PISA geometry built by default

  fMesonpT = -1.;
  fCentralityClassIndex = 9999;
  fMesonZvertex = -9999.;
  fBbcT0 = -9999.;

}

//_____________________________________________________________________________
emcEfficiency::~emcEfficiency()
{
  delete fListOfMergedParticles;
  delete fListOfMergedPairs;
  delete fListOfSimulParticles;
  delete fListOfSimulPairs;
  close();
}

//_____________________________________________________________________________
void
emcEfficiency::book(void)
{

  gROOT->SetStyle("Plain") ;
  gStyle->SetPalette(1) ; 
  //gStyle->SetOptTitle(0);
  //gStyle->SetOptStat(0);
  //gStyle->SetMarkerStyle(20);
  gROOT->ForceStyle();

  if ( fVerbose > 0 )
    {
      cout << "<I> emcEfficiency::book --- BEGIN" << endl;
    }

  std::vector<int> centralities = fEventSorting->getCentralities();

  std::map<int,std::string>::const_iterator it;
  std::string path ;
  std::string titleCentClass ;

  //
  // ** Creatings dirs per /Cut/Centrality **
  //

  if ( fVerbose > 0 )
    {
      cout << "<I> Cuts description: " << endl;
    }

  for ( it = fCutMaskToString.begin(); it != fCutMaskToString.end(); ++it ) 
    {

      std::string cut_name = it->second;

      // A simple object describing the Cut
      const char* GenName = "Current CUT definition ";
      TNamed *CutDescriptor = new TNamed(GenName,cut_name.c_str());
      CutDefinition(CutDescriptor);
      if ( fVerbose > 0 )
	{
	  cout << CutDescriptor->GetTitle() << endl;
	}

      // create centralities dirs. for each cut
      for ( size_t ic = 0 ; ic < centralities.size(); ++ic)
	{

	  fEventSorting->getCentralityDescription(ic,titleCentClass);
	  // cout << titleCentClass << endl;
	  path = getPath(cut_name,ic);
	  mkpath(fEfficiencyFile,path,titleCentClass);
	  bool ok = fEfficiencyFile->cd(path.c_str());  
	  assert(ok==true);
	  if ( fVerbose > 1 ) 
	    {
	      cout << gDirectory->GetPath() << endl;
	    }

	  // Write the description of the cut in each directory
	  CutDescriptor->Write("",TObject::kOverwrite);

	  // Book relevant histograms per /Cut/Centrality // FIXME: Mem. leak here this guy is never deleted
	  HistogramCollection* hc_cut = new HistogramCollection(gDirectory);
	  fH[cut_name].push_back(hc_cut); // store dir names and associated HistoCollection pointers in a map

	  int full = 0;
	  if ( (it->first & (kNoCutMask | kElectronEvtsCutMask)) == (kNoCutMask | kElectronEvtsCutMask) ) // conversion evts
	    {
	      full = 1;
	      cout << "<I> Booking 3-D histo for collection " << cut_name << endl;
	    }
	  if ( ic == centralities.size()-1 ) // minbias
	    {
	      full += 2;
	    }
	  createCutCollection(hc_cut,full);
	}

      // create cuts directories in /Simultracks
      path = getPath(kSimultracks,cut_name);
      mkpath(fEfficiencyFile,path);
      fEfficiencyFile->cd(path.c_str());  
      if ( fVerbose > 1 ) 
	{
	  cout << gDirectory->GetPath() << endl;
	}

      // Book relevant histograms in /Simultracks/Cut // FIXME: Mem. leak here this guy is never deleted
      HistogramCollection* hc_simultracks = new HistogramCollection(gDirectory);
      fH[path].push_back(hc_simultracks);
      createSimCollection(hc_simultracks);
      
      delete CutDescriptor; //CutDescriptor->Delete();
    }

  // directory /general with global histos
  path = getPath(kgeneral,"");
  mkpath(fEfficiencyFile,path);
  fEfficiencyFile->cd(path.c_str());  
  if ( fVerbose > 1 ) 
    {
      cout << gDirectory->GetPath() << endl;
    }
  // Booking relevant histograms in /general  // FIXME: Mem. leak here this guy is never deleted
  HistogramCollection* hc_gen = new HistogramCollection(gDirectory);
  fH[kgeneral].push_back(hc_gen);
  createGeneralCollection(hc_gen);
   
  if ( fVerbose > 1 ) 
    {
      cout << "<I> emcEfficiency::book : END" << endl;
    }

  fBookedHistos = true;

  fEfficiencyFile->cd();

}

//_____________________________________________________________________________
void
emcEfficiency::close(void)
{
  closeEmbedFile();
  closeEfficiencyFile();
}

//_____________________________________________________________________________
void
emcEfficiency::closeEmbedFile(void)
{
  if (fIOmanager)
    {
      cout << "<I> Closing " << fEmbedFileName << endl;

      delete fIOmanager;
      fIOmanager=0;
      fEmbedFileName="";
    }
  else
    {
      if (fVerbose>0)
	{
	  cout << "<I> No embed file to close ... " << endl;
	}
    }

}

//_____________________________________________________________________________
void
emcEfficiency::closeEfficiencyFile(void)
{
  if ( fEfficiencyFile ) 
    {
      if (fVerbose>1)
	{
	  cout << "<I> Closing " << fEfficiencyFile->GetPath() << endl;
	}

      writeEfficiencyFile();

      fEfficiencyFile->Close();

      delete fEfficiencyFile;
      fEfficiencyFile=0;
      // we do not delete the name on purpose.

      /////delete fRun2ExodusFluctCorrection; // this segs. fault
      fRun2ExodusFluctCorrection=0;
    } 
  else
    {
      if (fVerbose>1)
	{
	  cout << "<I> No efficiency file to close ... " << endl;
	}
    }
}

//_____________________________________________________________________________
//
// STEP 2) in computing final efficiencies
//
void 
emcEfficiency::computeEfficiency(const TH1* hInitialPt,
				 TH1* hminv,
				 const size_t iCent,
				 const size_t ipt,
				 const double normalization,
				 std::vector<emcEffValue>& values)
{
  values.resize(kEValueMax);

  double meson_count = 0.0;
  double meson_count_error = 0.0;
  double meson_fit = 0.0;
  double meson_fit_error = 0.0;
  double meson_initial = 0.0;
  double meson_initial_error = 0.0;
  
  double effic_fit_error = 0.0;
  double effic_count_error = 0.0;
  double effic_fit = 0.0;
  double effic_count = 0.0;

  // Retrieve the number of initial (simulated) mesons

  integral(hInitialPt,ipt,ipt,meson_initial,meson_initial_error);

  double meson_initial_relative_error2 =
    (meson_initial_error*meson_initial_error)/(meson_initial*meson_initial);

  values[kInitial_pions].set(meson_initial,meson_initial_error);
  
  if ( meson_initial == 0 ) 
    {
      // Don't waste CPU cycles for fitting output, if
      // input is void !
      //	delete hSimulPi0Minv;
      return;
    }
       
  // STEP 2.1) Now count the number of pions/etas and compute efficiencies
  
  countNumberOfMesons(hminv,
		      iCent,
		      ipt,
		      normalization,
		      values);
  
  meson_count = values[kDetected_count].value();
  meson_count_error = values[kDetected_count].error();

  meson_fit = values[kDetected_fit].value();
  meson_fit_error = values[kDetected_fit].error();

  // Compute efficiencies "count"
  if ( meson_count > 0 ) 
    {
      effic_count = meson_count/meson_initial;
      effic_count_error = effic_count;
      
      effic_count_error *= TMath::Sqrt
	( meson_count_error*meson_count_error/(meson_count*meson_count) +
	  meson_initial_relative_error2 );	
    }
  
  // Compute efficiencies "fit"
  if ( meson_fit > 0 ) 
    {
      // The fit error is taken from the counting above, *not*
      // from any fit parameters.
      
      effic_fit = meson_fit/meson_initial;
      
      if ( effic_count > 0 ) 
	{
	  effic_fit_error = effic_fit * effic_count_error/effic_count;
	}
      else
	{
	  effic_fit_error = 0.0;
	}
    }    

  values[kEfficiency_count].set(effic_count, effic_count_error);
  values[kEfficiency_fit].set(effic_fit, effic_fit_error);

  double effic_comb = (effic_count+effic_fit)/2.0;

  double effic_min;
  double effic_max;

  if ( effic_count < effic_fit ) 
    {
      effic_min = effic_count;
      effic_max = effic_fit;
    }
  else
    {
      effic_min = effic_fit;
      effic_max = effic_count;
    }

  values[kEfficiency_comb].set( effic_comb, effic_min, effic_max );
}

//_____________________________________________________________________________
//
// STEP 2.1) in computing final efficiencies
//
void
emcEfficiency::countNumberOfMesons(TH1* hminv, 	
				   const size_t iCent,
				   const size_t ipT, 
				   const double normalization,
				   std::vector<emcEffValue>& values)
{

  TF1* fgauss = static_cast<TF1*>(hminv->GetListOfFunctions()->FindObject("gaussian"));
  assert(fgauss!=0);

  //_____________________________________________________________________________
  // +-2sigma integration window around mean peak

  double peakPos = fgauss->GetParameter(1);
  double peakWidth = fgauss->GetParameter(2);

  double minv_min = peakPos - 2.0*peakWidth;
  double minv_max = peakPos + 2.0*peakWidth;

  values[kIntegrationWindow].set(peakPos,minv_min,minv_max);

  //_____________________________________________________________________________
  // Fixed integration window and mean

  if ( fFixedExtractionWindows )
    {
      std::vector<pair<double,double> > peak_and_width; // index of peak_and_width runs over pT bins
      peak_and_width = fIntegrationParameters[iCent];   // index of fIntegrationParameters runs over centrality

      peakPos = peak_and_width[ipT].first ;
      peakWidth = peak_and_width[ipT].second ;

      minv_min = peakPos - 2.0*peakWidth;
      minv_max = peakPos + 2.0*peakWidth;

      values[kFixedIntegrationWindow].set(peakPos,minv_min,minv_max);

      // cross-check our fixed means and widths

      double check_mean = values[kIntegrationWindow].value() - peakPos;

      if ( TMath::Abs(check_mean) > 5. ) // should coincide within 5 MeV
	{
	  if (fEffParticleCode==7) cout << "<W> pi0" ;
	  else if (fEffParticleCode==17) cout << "<W> eta";

	  cout << " peak position used for yield extraction (" 
	       << values[kIntegrationWindow].value()*1000.
	       << " MeV) is off by " << check_mean*1000. 
	       << " MeV compared to the observed value (" << peakPos*1000.
	       << " MeV) for this cut/centrality (pT=" << ((ipT-1)+fPT_BINWIDTH)/2 
	       << " GeV/c)"<< endl;
	}

      double check_width = values[kIntegrationWindow].error() - values[kFixedIntegrationWindow].error();
      check_width /= 2;

      if ( TMath::Abs(check_width) > 2. ) // should coincide within 2 MeV
	{
	  if (fEffParticleCode==7) cout << "<W> pi0" ;
	  else if (fEffParticleCode==17) cout << "<W> eta";
	  cout << " width used for yield extraction ("
	       << values[kIntegrationWindow].error()*1000./2.
	       << " MeV) is off by " << check_width*1000. 
	       << " MeV compared to the observed value (" << peakWidth*1000.
	       << " MeV) for this cut/centrality (pT=" << ((ipT-1)+fPT_BINWIDTH)/2 
	       << " GeV/c)"<< endl;
	  //< " MeV) for this pT/cut/centrality." << endl;
	}
    }

  int bin_min = hminv->FindBin(minv_min);
  int bin_max = hminv->FindBin(minv_max);

  //_____________________________________________________________________________
  // ** Number of mesons below gaussian by simple counting **

  double meson_count=0, meson_count_error=0;

  integral( hminv, bin_min, bin_max, meson_count, meson_count_error);
  if (meson_count != hminv->Integral(bin_min,bin_max))
    {
      cout << "<W> " << meson_count << " != " << hminv->Integral(bin_min,bin_max) << endl;
    }

  values[kDetected_count].set(meson_count,meson_count_error);

  //_____________________________________________________________________________
  // ** Subtracted linear background **
  // FIXME: do we need that for eta's ?

  double meson_count_bckgd=0, meson_count_error_bckgd=0;

  TF1* fbckgd = static_cast<TF1*>(hminv->GetListOfFunctions()->FindObject("background"));
  assert(fbckgd!=0);

  fbckgd->Draw(); // need for GetHistogram()
  TH1F *hbckgd  = (TH1F*)(fbckgd->GetHistogram());

  integral( hbckgd, bin_min, bin_max, meson_count_bckgd, meson_count_error_bckgd);

  values[kDetected_bckgd].set(meson_count_bckgd,meson_count_error_bckgd);
  
  //_____________________________________________________________________________
  // ** Signal (Gaussian) to Background **

  double meson_signal2bckgd=0, meson_signal2bckgd_error=0;

  // let's restrict the values of Gaussian/Background to "reasonable" ones ...
  // condition for very small (or zero) bckgd's
  meson_signal2bckgd = ( meson_count_bckgd > (meson_count/50.) ) ? (meson_count/meson_count_bckgd) : 50. ;
  meson_signal2bckgd_error = TMath::Power(meson_count_error/meson_count,2);
  if ( meson_count_bckgd )
    {
      meson_signal2bckgd_error += TMath::Power(meson_count_error_bckgd/meson_count_bckgd,2) ;
    }
  meson_signal2bckgd_error = meson_signal2bckgd*TMath::Sqrt( meson_signal2bckgd_error );
  meson_signal2bckgd_error = ( meson_signal2bckgd_error > meson_count_bckgd ) ? meson_count_bckgd : meson_signal2bckgd_error;

  values[kSignal2Background].set(meson_signal2bckgd,meson_signal2bckgd_error);

  //_____________________________________________________________________________
  // ** Number of mesons by integrating the gaussian fit **

  double meson_fit=0, meson_fit_error=0;

  meson_fit = fgauss->Integral(minv_min,minv_max); 

  double binsize = hminv->GetBinWidth(1);
  assert(binsize!=0);  
  meson_fit /= binsize;

  // "error on fit" = "error on count"
  if ( meson_count > 0 ) 
    {
      meson_fit_error = (meson_count_error/meson_count)*meson_fit;
    }

  values[kDetected_fit].set(meson_fit,meson_fit_error);
 
  // Normalization should be exactly 1 for minbias efficiency,
  // and roughly ~10. for each centrality class if you've
  // chosen 10% centrality bins 

  values[kDetected_count] *= normalization;
  values[kDetected_fit]   *= normalization;
  values[kDetected_bckgd] *= normalization;

  //_____________________________________________________________________________
  // Store integration windows and mean peak as lines in histo

  TLine *lmin  = new TLine(minv_min,hminv->GetMinimum(),minv_min,hminv->GetMaximum());
  TLine *lpeak = new TLine(peakPos, hminv->GetMinimum(),peakPos, hminv->GetMaximum());
  TLine *lmax  = new TLine(minv_max,hminv->GetMinimum(),minv_max,hminv->GetMaximum());
  lmin->SetLineStyle(2); lmin->SetLineWidth(3);
  lmax->SetLineStyle(2); lmax->SetLineWidth(3);
  lpeak->SetLineWidth(3);

  hminv->GetListOfFunctions()->Add(lmin);
  hminv->GetListOfFunctions()->Add(lpeak);
  hminv->GetListOfFunctions()->Add(lmax);
  hminv->SetFillColor(5);
  hminv->SetFillStyle(3001);
  hminv->Draw("eh");
  hminv->Write("",TObject::kOverwrite);

  /*
    delete lmin;
    delete lpeak;
    delete lmax;
  */

}

//_____________________________________________________________________________
void
emcEfficiency::createCutCollection(HistogramCollection* hcol_cut, int full)
{ 

  hcol_cut->book(khRecoParticleMinvPt,khRecoParticleMinvPt,
		 fMINV_NBINS,fMINV_MIN,fMINV_MAX,
		 fPT_NBINS,fPT_MIN,fPT_MAX);
  
  hcol_cut->book(khRecoParticleMinvPeak,khRecoParticleMinvPeak,
		 fPT_NBINS,fPT_MIN,fPT_MAX);
  
  hcol_cut->book(khRecoParticleMinvSigma,khRecoParticleMinvSigma,
		 fPT_NBINS,fPT_MIN,fPT_MAX);
  
  hcol_cut->book(khEfficiency_fit,khEfficiency_fit,
		 fPT_NBINS,fPT_MIN,fPT_MAX);
  
  hcol_cut->book(khEfficiency_counts,khEfficiency_counts,
		 fPT_NBINS,fPT_MIN,fPT_MAX);
  
  hcol_cut->book(khSignal2Background,khSignal2Background,
		 fPT_NBINS,fPT_MIN,fPT_MAX);
  
  hcol_cut->book(khInitialPt,khInitialPt,
		 fPT_NBINS,fPT_MIN,fPT_MAX); 

  hcol_cut->book(khInitialPtConversionOff,khInitialPtConversionOff,
		 fPT_NBINS,fPT_MIN,fPT_MAX);   

  size_t n = fEventSorting->getCentralities().size();

  hcol_cut->book(khCentClass,"Centrality Class",n,-0.5,n-0.5);
  hcol_cut->book(khTof,"hTof",1000,-50,200);
  hcol_cut->book(khChi2,"hChi2",200,0,50);

  hcol_cut->book(khXVtxPairs,"x-vertex (cm) of decay  #gamma conversions",1200,-600.,600.); // (cm)

  if ( full == 1 || full == 3 ) // for "electron" evts. (conversion studies)
    {
      hcol_cut->book(khRecoParticleMinvPtVtx,khRecoParticleMinvPtVtx,
		     fMINV_NBINS,fMINV_MIN,fMINV_MAX,
		     fPT_NBINS,fPT_MIN,fPT_MAX,
		     600,-600.,600);
    }

  //hcol_cut->book(khRelatTof_vs_Asym,"hRelatTof_vs_Asym",2500,-50,200,100,-0.05,1.05);
  //hcol_cut->book(khMinv_vs_Asym,"hMinv_vs_Asym",fMINV_NBINS,fMINV_MIN,fMINV_MAX,100,-0.05,1.05);

  if ( full >= 2 ) 
    {  

      if ( fYZLocalHistoNames.empty() ) 
	{
	  fYZLocalHistoNames.resize(8);
	  char name[80];
	  for ( size_t is = 0; is < 8; ++is )
	    {
	      sprintf(name,"hyzlocal_%s",EmcIndexer::EmcSectorId(is));
	      int isg = geometryModule()->emcOnlineToEmc(is);
	      fYZLocalHistoNames[isg] = name;
	    }
	}

      int ny,nz;
      
      for ( size_t is = 0; is < 8; ++is )
	{
	  int isg = geometryModule()->emcOnlineToEmc(is);
	  geometryModule()->GetSectorDim(isg,nz,ny);
	  hcol_cut->book(fYZLocalHistoNames[isg],fYZLocalHistoNames[isg],
			 nz*2,-50,450,
			 ny*2,-50,300);
	}
    }
}

//_____________________________________________________________________________
void
emcEfficiency::createGeneralCollection(HistogramCollection* hcol_gen)
{

  hcol_gen->book(khArmSector,"Arm vs Sector",4,-0.5,3.5,2,-0.5,1.5);

  hcol_gen->book("hSector","Sector (mEmcGeometryModule convention)",
		 8,-0.5,7.5);

  hcol_gen->book("hSectorFidu","Sector in Fidu (mEmcGeometryModule convention)",
		 8,-0.5,7.5);
  
  hcol_gen->book(khfiducial_local_yz_geaclustertrack,
		 "hfiducial_local_yz0_geaclustertrack",
		 1000,-500.,500.,1000,-500.,500);
  
  int n = fEventSorting->getCentralities().size();
  
  hcol_gen->book(khRecoParticleEnergyAsym,"hRecoParticleEnergyAsym",
		 fPT_NBINS,fPT_MIN,fPT_MAX,100,-0.05,1.05);
  
  hcol_gen->book(khCentClass,"Centrality Class",n,-0.5,n-0.5);
  
  hcol_gen->book(khCentrality,"Centrality (percentage)",100,-0.5,100.5);

  // Special histos to compute fluctuation corrections on input pi0 pT spectrum 
  // (due to random bug in the Exodus version we used for QM'02 pi0 simulation + embedding).

  int nbins = static_cast<int>((fPT_MAX-fPT_MIN)/fPT_CORR_BINWIDTH); // 0.1 GeV bins

  hcol_gen->book(khInitialPtUnweighted,khInitialPtUnweighted,
		 nbins,fPT_MIN,fPT_MAX); 

  hcol_gen->book(khInitialPtUnweightedCorr,
		 "Control Histo: UnweightedCorr == Unweighted *fit*, if fluctuation correction applied",
		 nbins,fPT_MIN,fPT_MAX); 

  hcol_gen->book(kEta_vs_Phi,"Eta vs Phi",157,-TMath::Pi(),TMath::Pi(),
		 100,-0.5,0.5);
  
  hcol_gen->book(khMeasx,"hMeasx",1000,-1000,1000);
  hcol_gen->book(khMeasy,"hMeasy",1000,-500,500);
  hcol_gen->book(khMeasz,"hMeasz",1000,-500,500);
  hcol_gen->book(khMeasyz_w,"hMeasyz_west",1000,-500,500,1000,-500,500);
  hcol_gen->book(khMeasyz_e,"hMeasyz_east",1000,-500,500,1000,-500,500);
  hcol_gen->book(khMeasyz_w_fiduc,"hMeasyz_west_fiduc",1000,-500,500,1000,-500,500);
  hcol_gen->book(khMeasyz_e_fiduc,"hMeasyz_east_fiduc",1000,-500,500,1000,-500,500);
  hcol_gen->book(khMease,"hMease",500,0,fPT_MAX);
  hcol_gen->book(khEcore,"hEcore",500,0,fPT_MAX);
  hcol_gen->book(khEcorePtot,"ecore vs p_tot",500,0,fPT_MAX,500,0,fPT_MAX);
  hcol_gen->book(khTof,"hTof",1000,-100,100);
  hcol_gen->book(khChi2,"hChi2",200,0,50);

  hcol_gen->book(khTotalClusterMul,"hTotalClusterMul",700,-0.5,699.5);
  hcol_gen->book(khEmbedClusterMul,"hEmbedClusterMul",100,-0.5,99.5);
}

//_____________________________________________________________________________
void
emcEfficiency::createSimCollection(HistogramCollection* hcol_sim)
{
  hcol_sim->book(khSimulParticleMinvPt,khSimulParticleMinvPt,
		 fMINV_NBINS,fMINV_MIN,fMINV_MAX,
		 fPT_NBINS,fPT_MIN,fPT_MAX);
  
  hcol_sim->book("hSimulPairsMinvAllPairs","hSimulPairsMinvAllPairs",
		 fMINV_NBINS,fMINV_MIN,fMINV_MAX);
  
  hcol_sim->book(khSimulPi0MinvPt,khSimulPi0MinvPt,
		 fMINV_NBINS,fMINV_MIN,fMINV_MAX,
		 fPT_NBINS,fPT_MIN,fPT_MAX);
  
  hcol_sim->book(khEffsimul_counts,khEffsimul_counts,
		 fPT_NBINS,fPT_MIN,fPT_MAX);
}
//_____________________________________________________________________________
// List of Cut Conditions as defined in emcEfficiency::passCuts

void
emcEfficiency::CutDefinition(TNamed *definition_of_this_cut)
{

  TString current_title = definition_of_this_cut->GetTitle();
  current_title += " = ";

  char cutdef[200];

  if ( current_title.Contains("Fidu",TString::kExact) ) 
    {
      sprintf(cutdef," %3.0f<x_local<%3.0f,%3.0f<y_local<%3.0f,",
	      fFiduXmin,fFiduXmax,fFiduYmin,fFiduYmax);
      current_title += cutdef;
    }
  if ( current_title.Contains("Energy",TString::kExact) ) 
    {
      sprintf(cutdef," Ecore1,Ecore2>%4.1f,",fMinClusterEnergy);
      current_title += cutdef;
    }
  if ( current_title.Contains("NoW3",TString::kExact) ) 
    {
      sprintf(cutdef," W3 excluded,");
      current_title += cutdef;
    }
  if ( current_title.Contains("5x5",TString::kExact) ) 
    {
      sprintf(cutdef," DeadMap1,2==WarnMap1,2==0(5x5),");
      current_title += cutdef;
    }
  if ( current_title.Contains("3x3",TString::kExact) ) 
    {
      sprintf(cutdef," DeadMap1,2==WarnMap1,2==0(3x3),");
      current_title += cutdef;
    }

  if ( current_title.Contains("Cos",TString::kExact) ) 
    {
      sprintf(cutdef," pair.Cosine()<[1-(%4.4f^2)/(2.*Ecore1*Ecore2)],",fMinvLowCut);
      current_title += cutdef;
    }
  if ( current_title.Contains("PairSameSect",TString::kExact) ) 
    {
      sprintf(cutdef," g-g pair in same sect.,");
      current_title += cutdef;
    }
  if ( current_title.Contains("PairSameArm",TString::kExact) ) 
    {
      sprintf(cutdef," g-g pair in same Arm,");
      current_title += cutdef;
    }
  else if ( current_title.Contains("BasicCuts",TString::kExact) ) 
    {
      sprintf(cutdef," Basic Cuts,");
      current_title += cutdef;
    }

  if ( current_title.Contains("Asym1",TString::kExact) ) 
    {
      sprintf(cutdef," Asym<%4.1f,",fMaxAsym1);
      current_title += cutdef;
    }
  if ( current_title.Contains("Asym2",TString::kExact) ) 
    {
      sprintf(cutdef," Asym<%4.1f,",fMaxAsym2);
      current_title += cutdef;
    }
  if ( current_title.Contains("ChiSq1",TString::kExact) ) 
    {
      sprintf(cutdef," Chi2<%4.1f,",fMaxChiSq1);
      current_title += cutdef;
    }
  if ( current_title.Contains("ChiSq2",TString::kExact) ) 
    {
      sprintf(cutdef," Chi2<%4.1f,",fMaxChiSq2);
      current_title += cutdef;
    }
  if ( current_title.Contains("ToF1",TString::kExact) )
    {
      sprintf(cutdef," TOF<%4.1f,",fMaxTof1);
      current_title += cutdef;
    }
  if ( current_title.Contains("ToF2",TString::kExact) ) 
    {
      sprintf(cutdef," TOF<%4.1f,",fMaxTof2);
      current_title += cutdef;
    }
  if ( current_title.Contains("RToF",TString::kExact) ) 
    {
      sprintf(cutdef," |TOF1-TOF2|<%4.1f,",fRTof);
      current_title += cutdef;
    }
  if ( current_title.Contains("Conv",TString::kExact) ) 
    {
      sprintf(cutdef," No e+/- tracks ");
      current_title += cutdef;
    }
  if ( current_title.Contains("Electron",TString::kExact) ) 
    {
      sprintf(cutdef," Evts. with e+/- tracks ");
      current_title += cutdef;
    }

  current_title = current_title.Strip(TString::kTrailing,','); // remove trailing ","
  definition_of_this_cut->SetTitle(current_title);

}
//_____________________________________________________________________________
void
emcEfficiency::defaultRun2ExodusFluctCorrection()
{

  if ( fVerbose > 0)
    {
      cout << "<I> Applying the default Run-2 Exodus Fluct. correction (corr=1.)" << endl;
    }

  int nbins = static_cast<int>((fPT_MAX-fPT_MIN)/fPT_CORR_BINWIDTH); // 0.1 GeV bins

  TH1 *default_correction = new TH1F("default_correction","default_correction",
				     nbins,fPT_MIN,fPT_MAX);

  for ( int i = 1; i <= default_correction->GetXaxis()->GetNbins(); ++i )
    {
      default_correction->SetBinContent(i,1.0); // the default correction is simply one
    }

  setRun2ExodusFluctCorrection(default_correction,"Default one (correction = 1.)"); 

  delete default_correction;

}
//_____________________________________________________________________________
void
emcEfficiency::defaultCentralityClasses()
{
  fEventSorting = new EventSorting;

  std::vector<int> centralities;
  
  if (fEffParticleCode == 17) // eta's
    {
      ////////////////////////////// centrality index
      centralities.push_back(20); // 0 = [ 0-20[%
      centralities.push_back(60); // 1 = [20-60[%
      centralities.push_back(92); // 2 = [60-92[%
      // Last centrality bin for minbias
      centralities.push_back(10000);// 3 (min.bias)
    }
  else if (fRUN == 2)
    {
      ////////////////////////////// centrality index
      centralities.push_back(10); // 0 = [ 0-10[%
      centralities.push_back(20); // 1 = [10-20[%
      centralities.push_back(30); // 2 = [20-30[%
      centralities.push_back(40); // 3 = [30-40[%
      centralities.push_back(50); // 4 = [40-50[%
      centralities.push_back(60); // 5 = [50-60[%
      centralities.push_back(70); // 6 = [60-70[%
      centralities.push_back(80); // 7 = [70-80[%
      centralities.push_back(93); // 8 = [80-93[%
      // Last centrality bin for minbias
      centralities.push_back(10000);// 9 (min.bias)
    }
  else if (fRUN == 1)
    {
      ////////////////////////////// centrality index
      centralities.push_back(10); // 0 = [ 0-10[%
      centralities.push_back(20); // 1 = [10-20[%
      centralities.push_back(40); // 2 = [20-40[%
      centralities.push_back(60); // 3 = [40-60[%
      centralities.push_back(80); // 4 = [60-80[%
      // Last centrality bin for minbias
      centralities.push_back(10000);// 5 (min.bias)
    }

  // not really used in this class.
  std::vector<double> vertices;

  vertices.push_back(-30);
  vertices.push_back(+30);

  fEventSorting->set(centralities,vertices);

  fMinBiasIndex = centralities.size()-1;
}

//_____________________________________________________________________________
void
emcEfficiency::defaultHagWeightPerCentrality()
{
  // The real raw pi0/eta spectrum is fitted
  // to A*(1+(pT/p0))^(-n)
  //
  // In the array below, we store <p0,-n> for each centrality class
  //

  fHagedornWeightParameters.clear();

  // Per centrality, give (p0,n0) of Hagedorn fit.
  if (fEffParticleCode == 17) // eta's 
    {
      // 3rd iteration:
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-11.1));//0 =  0-20
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.6));//1 = 20-60
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.0));//2 = 60-92
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.8));//3 = 0-92

//       // 2nd iteration:
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.88));//0 =  0-20
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.57));//1 = 20-60
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.02));//2 = 60-92
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.86));//3 = 0-92

//       // 2nd iteration:
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.40));//0
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72, -8.85));//1
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72, -9.78));//2
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72, -9.85));//3

//       // 1st iteration:
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.40));//0
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72, -9.60));//1
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72, -8.90));//2
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.00));//3
    }
  else if ( fRUN == 2 )
    {
      // 3rd iteration:
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.0));//0
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.85));//1
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.75));//2
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.65));//3
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.55));//4
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.45));//5
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.35));//6
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.25));//7
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.15));//8
      // Last one is for minbias
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.60));//9

//       // 3rd iteration:
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.0));//0
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.95));//1
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.95));//2
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.78));//3
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.60));//4
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.68));//5
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.50));//6
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.50));//7
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.50));//8
//       // Last one is for minbias
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.64));//9

//       // 2nd iteration:
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.83));//0
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.34));//1
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.48));//2
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.843));//3
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.653));//4
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.10));//5
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.868));//6
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.512));//7
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.865));//8
//       // Last one is for minbias
//       fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.06));//9

      // 1st iteration:
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.66));//0
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.15));//1
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.26));//2
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.672));//3
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.438));//4
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.872));//5
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.630));//6
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.320));//7
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.683));//8
      //   // Last one is for minbias
      //   fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.808 ));//9
      
    }
  else if ( fRUN == 1 )
    {
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.83));//0
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.34));//1
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.48));//2
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.843));//3
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-9.653));//4
      fHagedornWeightParameters.push_back(make_pair<double,double>(1.72,-10.100));//5

      cout << "<W> FIXME: Using Run-2 Hagedorn weights for Run-1 ... Probably incorrect ! " << endl;
    }
  assert(fHagedornWeightParameters.size()==fEventSorting->getCentralities().size());

  std::cout << "<I> emcEfficiency::defaultHagWeightPerCentrality" << std::endl;

  int centMin = -1;
  int centMax = -1;

  for ( size_t i = 0; i < fHagedornWeightParameters.size(); ++i ) 
    {
      fEventSorting->getCentralityClassLimits(i,centMin,centMax);
      std::cout << "    Cent " << i << " = [" << centMin << "-" << centMax << "[%  --> "
		<< " p0 = " << fHagedornWeightParameters[i].first
		<< " n = " << fHagedornWeightParameters[i].second
		<< std::endl;
    }
}

//_____________________________________________________________________________
// Analysis cuts as defined in the real pi0 analysis.
//
// For pi0 (PPG014, ...) see e.g.:
// http://www.phenix.bnl.gov/software/cvsweb/cvsweb.cgi/offline/analysis/pi0_reconstruction/lib/Combination.cxx
// http://www.phenix.bnl.gov/software/cvsweb/cvsweb.cgi/offline/analysis/pi0_reconstruction/lib/KEvent.cxx
// http://www.phenix.bnl.gov/software/cvsweb/cvsweb.cgi/offline/analysis/pi0_reconstruction/mkggntp/mkgghist_uudst.C
//

// Comment in/out the definition below for different cut studies
// (Having all cuts implemented is more time-consuming)
// If you change/add one or more cuts, you need to rerun the loop on the embed files again

void
emcEfficiency::defineCutNames()
{

  // Fill fCuts (map<string,int>)

  if ( fCuts.empty() )
    {
      cout << "<I> emcEfficiency::defineCutNames " << endl;

      // Non-PID cuts alone

      fCuts[kNoCut] = 0;

      fCuts[kFiduCut] = kFiduCutMask;
      fCuts[kDeadWarn5x5Cut] = kDeadWarn5x5CutMask;
      fCuts[kDeadWarn3x3Cut] = kDeadWarn3x3CutMask;

      fCuts[kNoW3Cut] = kNoW3CutMask;
      fCuts[kPairSameArmCut] = kPairSameArmCutMask;
      fCuts[kPairSameSectCut] = kPairSameSectCutMask;
      //fCuts[kEnergyCut] = kEnergyCutMask;
      //fCuts[kCosCut] = kCosCutMask;

      fCuts[kAsym1Cut]  = kAsym1CutMask;
      //fCuts[kAsym2Cut]  = kAsym2CutMask;

      // conversion studies

      //fCuts[kConversionOffCut] = kConversionOffCutMask;
      //fCuts[kElectronEvtsCut] = kElectronEvtsCutMask;

      // PID cuts alone

      //fCuts[kToF1Cut]   = kToF1CutMask;
      //fCuts[kChiSq1Cut] = kChiSq1CutMask;

      //fCuts[kToF2Cut]   = kToF2CutMask;
      //fCuts[kRToFCut]   = kRToFCutMask;	
      //fCuts[kChiSq2Cut] = kChiSq2CutMask;

      // Composite cuts

      fCuts[kFiduDeadWarn5x5Cut] = kFiduCutMask | kDeadWarn5x5CutMask ;
      fCuts[kFiduDeadWarn3x3Cut] = kFiduCutMask | kDeadWarn3x3CutMask ;

      fCuts[kFiduNoW3DeadWarn5x5Cut] = kFiduCutMask | kNoW3CutMask | kDeadWarn5x5CutMask ;
      fCuts[kFiduNoW3DeadWarn3x3Cut] = kFiduCutMask | kNoW3CutMask | kDeadWarn3x3CutMask ;

      fCuts[kBasicCuts5x5] = kFiduCutMask | kNoW3CutMask | kDeadWarn5x5CutMask | kEnergyCutMask ;
      fCuts[kBasicCuts3x3] = kFiduCutMask | kNoW3CutMask | kDeadWarn3x3CutMask | kEnergyCutMask ;

      //fCuts[kBasicCuts5x5] = fCuts[kBasicCuts5x5] | kCosCutMask ;
      //fCuts[kBasicCuts3x3] = fCuts[kBasicCuts3x3] | kCosCutMask ;

      fCuts[kBasicCuts5x5] = fCuts[kBasicCuts5x5] | kPairSameArmCutMask ;
      fCuts[kBasicCuts3x3] = fCuts[kBasicCuts3x3] | kPairSameArmCutMask ;

      //fCuts[kBasicCuts5x5] = fCuts[kBasicCuts5x5] | kPairSameSectCutMask ;
      //fCuts[kBasicCuts3x3] = fCuts[kBasicCuts3x3] | kPairSameSectCutMask ;

      fCuts[kBasicCuts5x5Asym1Cut] = fCuts[kBasicCuts5x5] | kAsym1CutMask;
      fCuts[kBasicCuts3x3Asym1Cut] = fCuts[kBasicCuts3x3] | kAsym1CutMask;

      //fCuts[kBasicCuts5x5Asym2Cut] = fCuts[kBasicCuts5x5] | kAsym2CutMask;
      //fCuts[kBasicCuts3x3Asym2Cut] = fCuts[kBasicCuts3x3] | kAsym2CutMask;

      // Special cuts without Fidu and DeadWarn cuts

      //fCuts[kNoW3EnergyCosAsym1Cut] = kNoW3CutMask | kEnergyCutMask | kCosCutMask | kAsym1CutMask ;
      //fCuts[kNoW3EnergyCosAsym1ChiSq2ToF2Cut] = fCuts[kNoW3EnergyCosAsym1Cut] | kChiSq2CutMask | kToF2CutMask;

      // TOF or ChiSq alone

      //fCuts[kBasicCuts5x5Asym1ToF1Cut] = fCuts[kBasicCuts5x5Asym1Cut] | kToF1CutMask;
      //fCuts[kBasicCuts5x5Asym1ToF2Cut] = fCuts[kBasicCuts5x5Asym1Cut] | kToF2CutMask;

      //fCuts[kBasicCuts5x5Asym1ChiSq1Cut] = fCuts[kBasicCuts5x5Asym1Cut] | kChiSq1CutMask;
      //fCuts[kBasicCuts5x5Asym1ChiSq2Cut] = fCuts[kBasicCuts5x5Asym1Cut] | kChiSq2CutMask;

      // TOF & ChiSq :

      fCuts[kBasicCuts5x5Asym1ChiSq1ToF1Cut] = fCuts[kBasicCuts5x5Asym1Cut] | kChiSq1CutMask | kToF1CutMask;
      //fCuts[kBasicCuts3x3Asym1ChiSq1ToF1Cut] = fCuts[kBasicCuts3x3Asym1Cut] | kChiSq1CutMask | kToF1CutMask;

      //fCuts[kBasicCuts5x5Asym1ChiSq1ToF2Cut] = fCuts[kBasicCuts5x5Asym1Cut] | kChiSq1CutMask | kToF2CutMask;
      //fCuts[kBasicCuts3x3Asym1ChiSq1ToF2Cut] = fCuts[kBasicCuts3x3Asym1Cut] | kChiSq1CutMask | kToF2CutMask;
      
      //fCuts[kBasicCuts5x5Asym1ChiSq1ToF2Cut] = fCuts[kBasicCuts5x5Asym1Cut] | kChiSq1CutMask | kToF2CutMask;
      fCuts[kBasicCuts3x3Asym1ChiSq2ToF2Cut] = fCuts[kBasicCuts3x3Asym1Cut] | kChiSq2CutMask | kToF2CutMask;

      //

      //fCuts[kBasicCuts5x5Asym2ChiSq1ToF1Cut] = fCuts[kBasicCuts5x5] | kAsym2CutMask | kChiSq1CutMask | kToF1CutMask;
      //fCuts[kBasicCuts3x3Asym2ChiSq1ToF1Cut] = fCuts[kBasicCuts3x3] | kAsym2CutMask | kChiSq1CutMask | kToF1CutMask;

      //fCuts[kBasicCuts5x5Asym2ChiSq1ToF2Cut] = fCuts[kBasicCuts5x5Asym2Cut] | kChiSq1CutMask | kToF2CutMask;
      //fCuts[kBasicCuts3x3Asym2ChiSq1ToF2Cut] = fCuts[kBasicCuts3x3Asym2Cut] | kChiSq1CutMask | kToF2CutMask;


      //fCuts[kBasicCuts5x5Asym1ChiSq2ToF1Cut] = fCuts[kBasicCuts5x5Asym1ChiSq2Cut] | kToF1CutMask;
      //fCuts[kBasicCuts5x5Asym1ChiSq2ToF2Cut] = fCuts[kBasicCuts5x5Asym1ChiSq2Cut] | kToF2CutMask;
      
      // eta studies with Saskia's acceptance, fiduc. & dead implementation
      //fCuts[kChiSq2ToF2Cut] = kChiSq2CutMask | kToF2CutMask ;
      //fCuts[kEnergyChiSq2ToF2Cut] = fCuts[kChiSq2ToF2Cut] | kEnergyCutMask ;
      //fCuts[kNoW3EnergyChiSq2ToF2Cut] = fCuts[kChiSq2ToF2Cut] | kNoW3CutMask | kEnergyCutMask ;
      
      // conversion studies
      //fCuts[kBasicCuts5x5ConvOffCut]  = fCuts[kBasicCuts5x5] | kConversionOffCutMask;
      //fCuts[kBasicCuts5x5Asym1ChiSq1ToF1ConvOffCut] = fCuts[kBasicCuts5x5Asym1ChiSq1ToF1Cut] | kConversionOffCutMask;
      //fCuts[kBasicCuts5x5Asym1ChiSq1ToF1ElectronEvtsCut] = fCuts[kBasicCuts5x5Asym1ChiSq1ToF1Cut] | kElectronEvtsCutMask;
      
      //fCuts[kBasicCuts5x5Asym2RToFCut] = fCuts[kBasicCuts5x5Asym2Cut] | kRToFCutMask;
      //fCuts[kBasicCuts5x5Asym2ChiSq1RToFCut] = fCuts[kBasicCuts5x5Asym2Cut] | kChiSq1CutMask | kRToFCutMask;
      //fCuts[kBasicCuts5x5Asym2ChiSq2RToFCut] =	fCuts[kBasicCuts5x5Asym2Cut] | kChiSq2CutMask | kRToFCutMask;	

      // Fill fCuts (yet!) and fCutMaskToString (map<int,string>)

      fCutMaskToString.clear();

      std::map<std::string,int>::const_iterator it;

      for ( it = fCuts.begin(); it != fCuts.end(); ++it ) 
	{
	  fCuts[it->first] |= kNoCutMask; // Mind that we set the first bit (NoCut) for all cuts !
	  fCutMaskToString[fCuts[it->first]] = it->first;
	  // cout << "<I> Filling fCuts with " << it->first << endl;
	}

      printCuts();
    }
}

//_____________________________________________________________________________
void
emcEfficiency::printCuts()
{

  std::map<int,std::string>::const_iterator it;

  std::cout << "<I> " << fCuts.size() 
	    << " Cuts defined for your RUN-" << fRUN 
	    << " efficiency object " << std::endl;

  // names & masks

  for ( it = fCutMaskToString.begin(); it != fCutMaskToString.end(); ++it ) 
    {	  
      std::string cut_name = it->second;
      std::cout << "    " << cut_name << "  - mask=" << it->first 
		<< "("; binary(it->first); cout << ")" << endl;
    }

  std::cout << "    Description:" << endl;

  // detailed description
  for ( it = fCutMaskToString.begin(); it != fCutMaskToString.end(); ++it ) 
    {	  
      std::string cut_name = it->second;
      TNamed *CutDescriptor = new TNamed("cut",cut_name.c_str());
      CutDefinition(CutDescriptor);
      std::cout  << "    " << CutDescriptor->GetTitle() << endl;
      delete CutDescriptor;
    }

}

//_____________________________________________________________________________
//
// 1) reads the Efficiency file just produced from the embed+eval files
// 2) recovers the bi-dim histos, fits + integrates the m_inv histos,

void
emcEfficiency::efficiency(const char* cutname, 
			  const size_t iCent,
			  const double normalization )
{
  if ( fVerbose > 0)
    {
      cout << "<I> emcEfficiency::efficiency for " << cutname 
	   << " and centrality class " << iCent
	   << " with (centrality) normalization " << normalization << endl;
    }

  if ( fEfficiencyFile && fEfficiencyFile->IsOpen() )
    {
      if ( fVerbose > 2 )
	{
	  cout << "<I> emcEfficiency::efficiency : Closing " 
	       << fEfficiencyFile->GetPath() << " first." << endl;
	}
      closeEfficiencyFile();
    }

  bool fileok = openEfficiencyFile(fEfficiencyFileName.c_str(),"UPDATE");

  if ( !fileok ) 
    {
      std::cerr << "<E> emcEfficiency::efficiency : Cannot open efficiency file"
		<< fEfficiencyFileName << std::endl;
      return;
    }

  //---- Input histograms to compute the efficiency

  // ** Simulated input ** [EFFICIENCY DENOMINATOR]

  TH1* hInitialPt = getHistogram<TH1>(cutname,khInitialPt,iCent);

  // For conversion-off cuts we use the ** conversion-off ** simulated input

  TString cutname_str = cutname;

  if (cutname_str.Contains("Off")) // for "Conv(ersion)Off" events
    {
      hInitialPt = getHistogram<TH1>(cutname,khInitialPtConversionOff,iCent);
    }

  // *** Merged output *** [EFFICIENCY NUMERATOR]
  // *** This is the most important bi-dim histo to compute efficiencies ***

  TH2* hPt_vs_Minv = getHistogram<TH2>(cutname,khRecoParticleMinvPt,iCent);

  if ( !hPt_vs_Minv || !hInitialPt )
    {
      std::cerr << "<E> emcEfficiency::efficiency : Something wrong in this directory." 
		<< " Not enough info to compute efficiencies." << endl
		<< "   Skipping Cut: \"" << cutname
		<< "\" and Centrality Class: " << iCent << std::endl;
      closeEfficiencyFile();
      return;
    }

  //---- Output histograms of the efficiency computation

  TH1* hPeakPos = getHistogram<TH1>(cutname,khRecoParticleMinvPeak,iCent);
  TH1* hPeakWidth = getHistogram<TH1>(cutname,khRecoParticleMinvSigma,iCent);
  TH1* hEfficiency_fit = getHistogram<TH1>(cutname,khEfficiency_fit,iCent);
  TH1* hEfficiency_counts = getHistogram<TH1>(cutname,khEfficiency_counts,iCent);
  TH1* hSignal2Background = getHistogram<TH1>(cutname,khSignal2Background,iCent);

  // we could rebook this guys actually if not found ...
  if ( !hPeakPos || !hPeakWidth  || !hEfficiency_fit || !hEfficiency_counts )
    {
      std::cerr << "<E> emcEfficiency::efficiency : Something wrong in this directory." 
		<< " Not enough info to compute efficiencies." << endl
		<< "   Skipping Cut: \"" << cutname
		<< "\" and Centrality Class: " << iCent << std::endl;
      closeEfficiencyFile();
      return;
    }

  // rebook this histo if not found (just a fast hack for now)
  if ( !hSignal2Background )
    {
      hSignal2Background = new TH1F(khSignal2Background.c_str(),khSignal2Background.c_str(),
				    fPT_NBINS,fPT_MIN,fPT_MAX);
     // FIXME: memory leak here !!
    }
  
  //---- Conversion studies ....

  //  TH3* hMinvPtVtx = getHistogram<TH3>(cutname,khRecoParticleMinvPtVtx,iCent);

  // Reset output histograms

  hPeakPos->Reset();
  hPeakWidth->Reset();
  hEfficiency_counts->Reset();
  hEfficiency_fit->Reset();
  hSignal2Background->Reset();

  char name[80];
  char title[80];

  //---- Compute the efficiency for each pT bin

  int ipt2 = 0;
  
  for ( size_t ipt = 1 ; ipt <= fPT_NBINS; ++ipt ) 
    {
      double pT_inf = hPt_vs_Minv->GetYaxis()->GetBinLowEdge(ipt);
      double pT_sup = pT_inf + hPt_vs_Minv->GetYaxis()->GetBinWidth(ipt);

      sprintf(title,"p_{T}=[%5.2f-%5.2f] GeV/c", pT_inf, pT_sup);
      
      if ( fVerbose > 1 ) 
	{
	  std::cout << title << std::endl;
	}
      
      // This is the invariant mass spectrum from merged clusters,
      // for the current pT bin.
      
      sprintf(name,"hMinv_%d",ipt);    
      TH1* hminv = hPt_vs_Minv->ProjectionX(name,ipt,ipt,"e");    
      assert(hminv!=0);
      hminv->SetTitle(title);

      // Let's scale hMinv with the FastMC DeadWarn loss
      // for all composite cuts containing the "DeadWarn" cut
      TString thiscut = cutname;
      if ( ( thiscut.Contains("Dead",TString::kExact) || 
	     thiscut.Contains("BasicCuts",TString::kExact) ) &&
	   fUseFastMCDeadWarnLoss )  
	{

	  double ExtraLoss = 1.;

	  if ( thiscut.Contains("5x5",TString::kExact) ) { ExtraLoss = fExtraDeadWarn5x5EfficLoss; }
	  if ( thiscut.Contains("3x3",TString::kExact) ) { ExtraLoss = fExtraDeadWarn3x3EfficLoss; }

	  hminv->Scale(ExtraLoss);
	  char titlehminv[200];
	  sprintf(titlehminv,"%s * extraDeadWarnLoss = %f",hminv->GetTitle(),ExtraLoss);
	  hminv->SetTitle(titlehminv);
	  if ( ipt == 1 ) // just one message
	    {
	      cout << "<W> REMEMBER: Scaling hMinv histos of cut \"" << cutname
		   << "\" by a factor " << ExtraLoss   
		   << " (extra dead-warn effic. loss)" << endl;
	    }
	}

//       // For the 8cm fidu and 3x3 dead-warn cuts, Saskia finds 0.80
//       if ( thiscut.Contains("NoW3EnergyCosAsym1",TString::kExact) )
// 	{
// 	  double ExtraLoss = 0.80;
// 	  hminv->Scale(ExtraLoss);
// 	  if ( ipt == 1 ) // just one message
// 	    {
// 	      cout << "<W> REMEMBER: Scaling hMinv histos of cut \"" << cutname
// 		   << "\" by a factor " << ExtraLoss   
// 		   << " (extra dead-warn effic. loss)" << endl;
// 	    }
// 	}

      // STEP 1) Find the peak position and widths, together with
      // associated errors (done by fitting the minv spectrum).
      
      fitMesonPeak(hminv,pT_inf);
      
      // Save the histogram with its associated (fit) functions
      hminv->Write("",TObject::kOverwrite);

      // STEP 2) Integrate the fitted m_inv. Do the ratio measured/initial

      std::vector<emcEffValue> values;
      //values.clear();

      computeEfficiency(hInitialPt, hminv, iCent, ipt, normalization, values);

      // STEP 3) Fill the peak and width histos

      fillPeakAndWidth(ipt,hminv,hPeakPos,hPeakWidth);

      // Fill the efficiency histos

      hEfficiency_counts->SetBinContent(ipt,values[kEfficiency_count].value());
      hEfficiency_counts->SetBinError(ipt,values[kEfficiency_count].error());

      hEfficiency_fit->SetBinContent(ipt,values[kEfficiency_fit].value());
      hEfficiency_fit->SetBinError(ipt,values[kEfficiency_fit].error());

      hSignal2Background->SetBinContent(ipt,values[kSignal2Background].value());
      hSignal2Background->SetBinError(ipt,values[kSignal2Background].error());
     
      if ( fVerbose > 0 )
	{
	  // Report on pi0/eta counting and efficiency	
	}
      
//       // conversion studies (coarser p_T binning)

//       if ( hMinvPtVtx && ipt2 < fPT_NBINS )
// 	{
// 	  int minv_min = hminv->FindBin(values[kIntegrationWindow].valueMin());
// 	  int minv_max = hminv->FindBin(values[kIntegrationWindow].valueMax());

// 	  sprintf(name,"hVtx_saved_%d",ipt2);
// 	  TH1* hVtx = hMinvPtVtx->ProjectionZ(name,
// 					      minv_min,
// 					      minv_max,
// 					      ipt2,ipt2+4,"e");

// 	  sprintf(name,"hVtx_lost_%d",ipt2); // lost below 
// 	  TH1* hVtx_lost = hMinvPtVtx->ProjectionZ(name,
// 						   0,
// 						   minv_min-1,
// 						   ipt2,ipt2+4,"e");

// 	  sprintf(name,"hVtx_lostabove"); // lost above
// 	  TH1* hVtx_lost2 = hMinvPtVtx->ProjectionZ(name,
// 						    minv_max+1,
// 						    10000,
// 						    ipt2,ipt2+4,"e");

// 	  cout << "<I> Projection of hMinvPtVtx for pT = " << ipt2 
// 	       << " and m_inv = " << values[kIntegrationWindow].valueMin() 
// 	       << " - " << values[kIntegrationWindow].valueMax() 
// 	       << " gives " << hVtx->GetEntries() << " counts" << endl;

// 	  pT_inf = hPt_vs_Minv->GetYaxis()->GetBinLowEdge(ipt2)+hPt_vs_Minv->GetYaxis()->GetBinWidth(ipt2);
// 	  pT_sup = pT_inf + 4*hPt_vs_Minv->GetYaxis()->GetBinWidth(ipt2);
// 	  sprintf(title,"decay #gamma (p_{T}^{#pi^{0}}=[%5.2f-%5.2f] GeV/c) converted but saved: x-vertices (cm)", 
// 		  pT_inf, pT_sup);
// 	  hVtx->SetTitle(title);

// 	  sprintf(title,"decay #gamma (p_{T}^{#pi^{0}}=[%5.2f-%5.2f] GeV/c) converted and lost: x-vertices (cm)", 
// 		  pT_inf, pT_sup);
// 	  hVtx_lost->SetTitle(title);
// 	  hVtx_lost->Sumw2();
// 	  hVtx_lost->Add(hVtx_lost2,1.);

// 	  hVtx_lost->Write("",TObject::kOverwrite);
// 	  hVtx->Write("",TObject::kOverwrite);
// 	}

      ipt2 += 4;

      delete hminv;

    } // end of loop on pT bins

  
  //_____________________________________________________________________________

  std::string path = getPath(cutname,iCent);
  
  bool cdisok = fEfficiencyFile->cd(path.c_str());
  assert(cdisok==true);
  
  if ( fVerbose > 1 ) 
    {
      cout << "<I> emcEfficiency::efficiency : Writing histos"
	   << " in " << gDirectory->GetPath() << endl;
    }

  hPeakPos->Write("",TObject::kOverwrite);
  hPeakWidth->Write("",TObject::kOverwrite);
  hEfficiency_counts->Write("",TObject::kOverwrite);
  hEfficiency_fit->Write("",TObject::kOverwrite);
  hSignal2Background->Write("",TObject::kOverwrite);

  closeEfficiencyFile();
}

//_____________________________________________________________________________
void
emcEfficiency::efficiency_all_cuts(void)
{

  // Some sanity checks before we start...

  bool ok = readEfficiencyFile();

  if ( !ok ) 
    {
      return;
    }

  // Just a first check at the beginning about the status of the fluctuation correction ...
  // only relevant for PPG014 pi0's

  if ( fEffParticleCode == 7 && fCorrectRun2ExodusFluct )
    {

      TString fluct_corr;
      statusExodusFluctCorrectionHisto(fluct_corr);
      
      if ( !fluct_corr.Contains("applied",TString::kIgnoreCase) )
	{
	  cout << "<W> Efficiencies will be (seemingly) computed without fluctuation correction." << endl
	       << " Check the control histo general/hInitialPtUnweightedCorr in case of doubts ..." << endl;
	}
      else 
	{
	  cout << "<I> Efficiencies will be computed with the fluctuation correction." << endl;
	}
    }

  // Recall extraction yield mode (fixed/variable integration windows)
  setFixedExtractionWindows(fFixedExtractionWindows);  

  if ( fFixedExtractionWindows ) // pi0
    {
      bool ok = readFixedExtractionWindows();
      if (!ok) return;
    }

  // Go for it ...

  cout << "<I> OK. I'm computing the efficiencies now. Give me a few minutes ..." << endl;

  TH1* hCentClass = getHistogram<TH1>(kgeneral,khCentClass);
  assert(hCentClass!=0);

  int CentBinMax = hCentClass->GetXaxis()->FindBin(fEventSorting->getNCentralities()-1);
  CentBinMax -= 1;  
  // -1 because last bin is minbias (i.e. not a centrality bin really).

  double total = hCentClass->Integral(1,CentBinMax);

  if ( total < 1) 
    {
      std::cerr << "<E> emcEfficiency::efficiency_all_cuts : too few entries "
		<< "in hCentClass histogram : something is seriously wrong."
		<< "Cannot compute efficiencies"
		<< std::endl;
      return;
    }

  closeEfficiencyFile(); // will be reopened by each call to efficiency() method

  // OK, now we can start.
  std::vector<int> centralities = fEventSorting->getCentralities();

  double normalization=0;
  std::map<int,string>::const_iterator it;

  size_t centIndex ;

  // Compute the efficiencies per centrality (loop on each directory)
  for ( centIndex = 0 ; centIndex < centralities.size(); centIndex++)
    {
	
      if ( centIndex != fMinBiasIndex )
	{
	  normalization = getNormalization(hCentClass,centIndex);
	}
      else
	{
	  normalization = 1.0;
	}

      // let's compute iteratively the efficiency for each cut ...

      for ( it = fCutMaskToString.begin(); it != fCutMaskToString.end() ; ++it ) 
  	{
  	  efficiency((it->second).c_str(),centIndex,normalization);
	}

      cout << "<I> We are done with centrality class " << centIndex 
	   << ". Let's go on with the next ..." << endl;
    }

  std::cout << "<I> Output written in file " << fEfficiencyFileName << std::endl;

}

//_____________________________________________________________________________
void
emcEfficiency::fill(const std::string cutname,
		    const effPair& pair,
		    const size_t centIndex,
		    const double weight)
{

  HistogramCollection* hcol_cut = histoCollection(cutname,centIndex);

  // *** The key histogram to compute the efficiency ***

  hcol_cut->fillW(khRecoParticleMinvPt, pair.Minv(), pair.Pt(), weight);
  
  // Impact angle studies

  //float impangle = (180./TMath::Pi())*pair.AverageImpactAngle();

//   if ((pair.Particle1()).Sector()==(pair.Particle2()).Sector() &&
//       (pair.Cosine()<cos(TMath::DegToRad()*5.)) )
//     {
//       float y1 = (float)pair.Particle1().ImpY();
//       float z1 = (float)pair.Particle1().ImpX(); // local coordinates: X == Z and Z == 0
//       float y2 = (float)pair.Particle2().ImpY();
//       float z2 = (float)pair.Particle2().ImpX(); // local coordinates: X == Z and Z == 0

//       NtupRecoParticleMinvPtImpAngle->Fill( (float)fCuts[cutname], (float)centIndex,
// 					    (float)pair.Minv(), (float)pair.Pt(),
// 					    (float)TMath::RadToDeg()*pair.AverageImpactAngle(),
// 					    y1,z1,y2,z2);
//     }

  // x vertex position of the pair (for conversion studies)

  hcol_cut->fill(khXVtxPairs,pair.xVertex());

  // fill special histo for conversion evts.

  TString cutname_str = cutname.c_str();

  if (cutname_str.Contains("elect",TString::kIgnoreCase)) // for "ElectronEvt" cuts
    {
      hcol_cut->fillW(khRecoParticleMinvPtVtx, pair.Minv(), pair.Pt(), pair.xVertex(), 1.0);
    }

  // Sanity check histograms

  hcol_cut->fill(khCentClass,centIndex);

  double tof1 = 0.;
  double tofrelat = 0.;
  
  for ( size_t i = 0; i < 2; ++i )
    {
      const effParticle& part = pair.Particle(i);

      if (i==0) tof1 = part.TOF(); // tof of first particle
      if (i==1) tofrelat = TMath::Abs( tof1 - part.TOF() );
     
      hcol_cut->fill(khTof,part.TOF());
      hcol_cut->fill(khChi2,part.Chi2());            

      int is = part.Sector();

      if ( centIndex == fMinBiasIndex ) 
	{
	  hcol_cut->fill(fYZLocalHistoNames[is],part.ImpX(),part.ImpY());
	}
    }

  //  hcol_cut->fillW( khRelatTof_vs_Asym, tofrelat, pair.Asymmetry(), 1.);//weight );
  //  hcol_cut->fillW( khMinv_vs_Asym, pair.Minv(), pair.Asymmetry(), 1.);//weight );

}

//_____________________________________________________________________________
void
emcEfficiency::fill(const std::string cutname,
		    const effPair& pair,
		    const double weight)
{

  std::string path = getPath(kSimultracks,cutname);
  HistogramCollection* hcol_sim = histoCollection(path);
  if (!hcol_sim)
    {
      cerr << "<E> hcol_sim not found. Won't fill any histo ..." << endl;
      return ;
    }

  // *** The key histogram to compute the "efficiency" for simulated tracks analysis ***

  hcol_sim->fillW(khSimulParticleMinvPt, pair.Minv(), pair.Pt(), weight);
  
}

//_____________________________________________________________________________
void
emcEfficiency::fill(const effPair& pair)
{
  std::map<int,string>::const_iterator it;

  for ( it = fCutMaskToString.begin(); it != fCutMaskToString.end(); ++it ) 
    {
      int mask = it->first;
      int ok = (pair.CutMask() & mask);

      if ( ok == mask )
	{
	  std::string cut_name = it->second;

	  if ( !pair.isSimulPair() ) // pair of merged particles
	    {
	      // Fill minbias histograms first
	      fill(cut_name,pair,fMinBiasIndex,fMesonMinbiasHagedornWeight);
	      
	      // Fill centrality then
	      fill(cut_name,pair,fCentralityClassIndex,fMesonHagedornWeight);
	    }
	  else // fill mode for pair of simulated particles
	    {
	      fill(cut_name,pair,fMesonMinbiasHagedornWeight);
	    }
	}
    }
}

//_____________________________________________________________________________
//
// STEP 3) in computing final efficiencies
//
void
emcEfficiency::fillPeakAndWidth(const size_t ipt,
				const TH1* hminv, 
				TH1* hPeakPos, TH1* hPeakWidth)
{
  TF1* ffit = static_cast<TF1*>
    (hminv->GetListOfFunctions()->FindObject("gauss_plus_bckg"));
  
  assert(ffit!=0);

  // Fill the peak position and width histograms
  hPeakPos->SetBinContent(ipt,ffit->GetParameter(1));
  hPeakPos->SetBinError(ipt,ffit->GetParError(1));
  hPeakWidth->SetBinContent(ipt,ffit->GetParameter(2));
  hPeakWidth->SetBinError(ipt,ffit->GetParError(2));
} 

//_____________________________________________________________________________
//
// STEP 1.1) when computing final efficiencies
// 
void 
emcEfficiency::findPeak(TH1* htmp, double& xmax, double& ymax)
{
  int binmin = htmp->GetXaxis()->FindBin(fFIT_MINV_LO+0.02);
  int binmax = htmp->GetXaxis()->FindBin(fFIT_MINV_UP-0.02);

  xmax = 0.0;
  ymax = 0.0;
  
  for ( int i = binmax ; i > binmin ; --i ) {
    if ( htmp->GetBinContent(i) > ymax ) {
      ymax = htmp->GetBinContent(i);
      xmax = htmp->GetBinCenter(i);
    }
  }

  //cout << "findPeak(): (xmax,ymax)= " << xmax << "," << ymax << endl;
}

//_____________________________________________________________________________
// you have to call this function yourself once your efficiency histo has
// enough statistics

bool
emcEfficiency::fitFluctuation(void)
{
  closeEfficiencyFile();

  bool fileok = openEfficiencyFile(fEfficiencyFileName.c_str(),"UPDATE");

  if ( !fileok ) 
    {
      std::cerr << "<E> emcEfficiency::fitFluctuation : Cannot "
		<< "open file " << fEfficiencyFileName
		<< std::endl;
      return false;
    }

  TString fluct_corr;
  statusExodusFluctCorrectionHisto(fluct_corr);

  if ( fluct_corr.Contains("fitted",TString::kIgnoreCase) )
    {
      cout << "<W> Fluctuation correction histo already fitted." 
	   << " Leaving it as it is. " << endl;
      return false;
    }

  // we retrieve the Initial pT spectrum
  // and fit it to the expected randomly uniform value 
  // (with (1-exponential)-like shape at low pT)

  TH1* hUnweightpT = getHistogram<TH1>(kgeneral,khInitialPtUnweighted);
  if ( !hUnweightpT ) 
    {
      std::cerr << "<W> emcEfficiency::fitFluctuation: " 
		<< " Null pointer for input pT histogram. Cannot fit it ... " << std::endl;
      return false;
    }

  TH1* hFluctCorr = static_cast<TH1*>(hUnweightpT->Clone());
  hFluctCorr->Reset();

  TF1* f1 = new TF1("fCorrectionFuntion",
		    "[0]*(1.0-exp(-([1]+x*[2])))",fPT_MIN,fPT_MAX);

  f1->SetParameter(0,1.0);
  f1->SetParameter(1,0.46);
  f1->SetParameter(2,0.49);

  hUnweightpT->Fit(f1,"Q0+","",0.5,fPT_MAX); // fit added to histo

  int nbins = hUnweightpT->GetXaxis()->GetNbins(); 

  for ( int i = 1 ; i <= nbins; i++ ) 
    {
      double content = hUnweightpT->GetBinContent(i);
      double pt = hUnweightpT->GetBinCenter(i);

      if ( content > 0 ) 
	{
	  hFluctCorr->SetBinContent(i, f1->Eval(pt)/content);
	}
      else
	{
	  hFluctCorr->SetBinContent(i,1.0);
	}
    }

  cout << "<I> pT fluctuation fitted (Chi2/Ndf = " << f1->GetChisquare()/f1->GetNDF() 
       << " . Entries in initial pT distrib. = " << hUnweightpT->GetEntries() << "]. " << endl;

  setRun2ExodusFluctCorrection(hFluctCorr,"Fitted fluctuation correction");

  delete hFluctCorr;

  hUnweightpT->GetListOfFunctions()->Add(f1);
  hUnweightpT->Write("",TObject::kOverwrite);

  cout << "<I> New fluctuation correction histo written in file " 
       << fEfficiencyFileName << endl
       << " It can be exported to another file before re-running over the"
       << " embed+eval files." << endl;

  delete f1;

  return true;
}

//_____________________________________________________________________________
//
// STEP 1) in computing final efficiencies
//
void
emcEfficiency::fitMesonPeak( TH1* hminv, double pT )
{

  // pi0:  85 MeV/c2 - 230 MeV/c2 
  // eta: 450 MeV/c2 - 700 MeV/c2

  TF1* ftotal = new TF1("gauss_plus_bckg","gaus(0)+pol0(3)",fFIT_MINV_LO,fFIT_MINV_UP);

  TF1* fgauss = new TF1("gaussian","gaus(0)",0.,fMINV_MAX);

  TF1* fbackground = new TF1("background","pol0",0.,fMINV_MAX);

  double peakGuess ;
  double heightGuess ;
  double widthGuess = 0.;
  double nominalPeak = 0.;

  // STEP 1.1) Get a guess of peak position and height

  findPeak(hminv,peakGuess,heightGuess);

  if (fEffParticleCode==7)
    {
      nominalPeak = 0.135;
      widthGuess  = 0.020;
    }
  else if (fEffParticleCode==17)
    {
      nominalPeak = 0.550;
      widthGuess  = 0.080;
    }

  // Reset unreasonable peak guess (if any) to nominalPeak position (GeV/c^2)
  if ( fabs(peakGuess - nominalPeak) > 3*widthGuess ) 
    {
      cout << "<W> Euh ? PeakGuess = " << peakGuess 
	   << " for " << hminv->GetName() 
	   << " in " 
	   << gDirectory->GetName() //Path()
	   << ". Resetting to " 
	   << nominalPeak << endl;
      peakGuess = nominalPeak;
    }

  // Get a guess of background level:
  // Minv values at 80-90 MeV/c2 and at 210-230 MeV/c2 (pi0)
  // Minv values at ~300 MeV/c2 and at ~700 MeV/c2 (eta)

  double bckgd_below1 = 0.;
  double bckgd_below2 = 0.;
  double bckgd_above1 = 0.;
  double bckgd_above2 = 0.;

  if (fEffParticleCode==7)
    {
      bckgd_below1 = fFIT_MINV_LO;
      bckgd_below2 = fFIT_MINV_LO+0.010;
      bckgd_above1 = fBCKGD_ABOVE_PI0;
      bckgd_above2 = fFIT_UPPER_BOUND_PI0; // changes with increasing pt
      //if (pT < 4) bckgd_above1 = 0.23;
      if (pT >= 4 && pT < 6.) bckgd_above1 = 0.25;
      if (pT >= 6.) bckgd_above1 = 0.5;
   }
  else if (fEffParticleCode==17)
    {
      bckgd_below1 = 0.300;
      bckgd_below2 = 0.400;
      bckgd_above1 = fBCKGD_ABOVE_ETA;
      bckgd_above2 = fFIT_UPPER_BOUND_ETA;
    }

  int bin_bckgd_below1 = hminv->GetXaxis()->FindBin(bckgd_below1);
  int bin_bckgd_below2 = hminv->GetXaxis()->FindBin(bckgd_below2);
  int bin_bckgd_above1 = hminv->GetXaxis()->FindBin(bckgd_above1);
  int bin_bckgd_above2 = hminv->GetXaxis()->FindBin(bckgd_above2);

  TF1 *bckgd_low  = new TF1("bckgd_low","pol0(0)",bin_bckgd_below1,bin_bckgd_below2);
  TF1 *bckgd_high = new TF1("bckgd_high","pol0(0)",bin_bckgd_above1,bin_bckgd_above2);

  hminv->Fit(bckgd_low,"QR");
  double background_low = bckgd_low->GetParameter(0);
  hminv->Fit(bckgd_high,"QR");
  double background_high = bckgd_high->GetParameter(0);

  // Use the guess as starting points for the fit...
  ftotal->SetParameters(heightGuess, peakGuess, widthGuess, background_low);

  // ... and limit parameter variations
  ftotal->SetParLimits(0,0.,2.*heightGuess);
  ftotal->SetParLimits(1,fFIT_MINV_LO,fFIT_MINV_UP);
 // Peak width variation restricted to 4-30 MeV/c^2 (pi0)
  if (fEffParticleCode== 7) ftotal->SetParLimits(2,0.004,0.03);
 // Peak width variation restricted to 20-120 MeV/c^2 (eta)
  if (fEffParticleCode==17) ftotal->SetParLimits(2,0.020,0.120);
  // Background limited within the mean Minv values at: pi0 =  80- 90 MeV/c2 and at 220-230 MeV/c2
  //                                                    eta = 300-310 MeV/c2 and at 700-710 MeV/c2
  ftotal->SetParLimits(3,TMath::Max(0.0,TMath::Min(background_high,background_low)),
   		         TMath::Max(background_high,background_low));

  if ( fVerbose > 2 )
    {
      std::cout << "<D> emcEfficiency::fitMesonPeak : I'm going to fit "
		<< " in " << gDirectory->GetPath() << std::endl;
      std::cout << " Initial guesses : height=" << heightGuess
		<< " peak=" << peakGuess
		<< " width=" << widthGuess
		<< " bckd=" << background_low << endl;
    }

  // This is just for convenience (e.g. for plotting the results)
  int npx = hminv->GetNbinsX();
  ftotal->SetNpx(npx); 

  hminv->Rebin(fMINV_REBIN_FACTOR); // Rebinning
  hminv->Fit(ftotal,"Q0IR");

//   double actual_peak = ftotal->GetParameter(1);

//   double low = fFIT_MINV_LO;
//   double high = fFIT_MINV_UP;

//   while ( fabs( (actual_peak-peakGuess)/peakGuess ) > 0.2 && low < high ) 
//     {
//       low += 0.01;
//       high -= 0.01;
//       // Refit with smaller integration window
//       if ( fVerbose ) 
// 	{
// 	  std::cout << "WARNING : Refitting " << hminv->GetName()
// 		    << std::endl;
// 	  hminv->Fit(ftotal,"Q0IR","",low,high);
// 	}
//     }
 
  if ( fVerbose > 2 )
    {     
      std::cout << " Fit results : "
		<< " height=" << ftotal->GetParameter(0)
		<< " peak=" << ftotal->GetParameter(1)
		<< " width=" << ftotal->GetParameter(2)
		<< " bckd=" << ftotal->GetParameter(3)
		<< " chi2/ndf=" << ftotal->GetChisquare() 
		<< "/" << ftotal->GetNDF()
		<< endl;
    }

  // Use the fit results to feed individual (gaus and pol0) 
  // functions (needed later to compute efficiency)
  // Do not forget to transmit parameters errors also !

  fgauss->SetParameter(0, ftotal->GetParameter(0));
  fgauss->SetParameter(1, ftotal->GetParameter(1));
  fgauss->SetParameter(2, ftotal->GetParameter(2));

  fgauss->SetParError(0, ftotal->GetParError(0));
  fgauss->SetParError(1, ftotal->GetParError(1));
  fgauss->SetParError(2, ftotal->GetParError(2));

  fgauss->SetNpx( npx );
  fgauss->SetLineColor(2);

  fbackground->SetParameter(0, ftotal->GetParameter(3));
  fbackground->SetParError(0, ftotal->GetParError(3));

  fbackground->SetNpx( npx );
  fbackground->SetLineColor(4);

  // Attach the 3 functions to the histograms
  // so they will be saved with it.
  hminv->GetListOfFunctions()->Add(ftotal);
  hminv->GetListOfFunctions()->Add(fgauss);
  hminv->GetListOfFunctions()->Add(fbackground);   

  // fixme: mem. leak here ?
  //delete ftotal;
  //delete fgauss;
  //delete fbackground;

  delete bckgd_low;
  delete bckgd_high;

}

//_____________________________________________________________________________
// 3 possibilities: 1) "Default" (corr=1)
//                  2) "Fitted"
//                  3) "Fitted in file 1, exported to file 2 and applied there"
void 
emcEfficiency::statusExodusFluctCorrectionHisto( TString& status )
{

  fEfficiencyFile->cd();

  TString fluct_status = "unknown";

  TH1* hFluctCorr = 0;
  hFluctCorr = static_cast<TH1*>(fEfficiencyFile->Get("hRun2ExodusFluctCorrection"));
  if ( !hFluctCorr )
    {
      std::cerr << "<W> Couldn't find any fluctuation correction histo in this file ?!?! " << std::endl ;
      return;
    }

  fluct_status = hFluctCorr->GetTitle();
  
  if (fluct_status.Contains("Default",TString::kIgnoreCase) ) // i.e. corr = 1.0
    {
      status = fluct_status;
      cout << "<I> emcEfficiency::statusExodusFluctCorrection : " << status << endl;
      return; // "Default"
    }

  TH1* h_uncorr = getHistogram<TH1>(kgeneral,khInitialPtUnweighted);
  TH1* h_corr = getHistogram<TH1>(kgeneral,khInitialPtUnweightedCorr);

  if ( !h_uncorr || !h_corr )
    {
      cout << "<W> emcEfficiency::statusExodusFluctCorrection : " << status
	   << " (I could not find out if the found correction has been applied ...)" << endl;
      return ;
    }

  int test_pT_bin = 10; // arbitrary
  double corr = hFluctCorr->GetBinContent(test_pT_bin);
  double corr_test = h_uncorr->GetBinContent(test_pT_bin)/h_corr->GetBinContent(test_pT_bin);

  if ( TMath::Abs(corr-corr_test) < 0.05 ) // we assume are the same
    {
      status = "Imported and (seemingly) *applied* fitted fluctuation correction";
    }

  status = fluct_status; // just "Fitted"
  
  cout << "<I> emcEfficiency::statusExodusFluctCorrection : " << status << endl;

}

//_____________________________________________________________________________
mEmcGeometryModule*
emcEfficiency::geometryModule(void) const
{
  static mEmcGeometryModule* geom = 
    new mEmcGeometryModule(mEmcGeometryModule::kReal); // kPISA);

  return geom;
}

//_____________________________________________________________________________
template<class T>
T*
emcEfficiency::getHistogram(const std::string cutname, 
			    const std::string histo,
			    const size_t iCent) const
{
  T* rv = 0;

  std::string path = getPath(cutname,iCent);
  rv = getHistogram<T>(path,histo);

  return rv;
}

//_____________________________________________________________________________
template<class T>
T*
emcEfficiency::getHistogram(const std::string path, 
			    const std::string histo) const
{
  T* rv = 0;

  if ( fEfficiencyFile ) 
    {
      bool ok = fEfficiencyFile->cd(path.c_str());
      if (ok)
	{
	  rv = static_cast<T*>(gDirectory->Get(histo.c_str()));
	  if (rv)
	    {
	      rv->SetDirectory(gROOT);
	    }
	  else
	    {
	      cerr << "<E> emcEfficiency::getHistogram : Cannot get histo "
		   << histo << " from " << gDirectory->GetPath() << endl;
	    }
	}
      else
	{
	  cerr << "<E> emcEfficiency::getHistogram : Could not cd into "
	       << path << endl;
	}
    }

  return rv;
}

//_____________________________________________________________________________
// OK, here we want to retrieve some information:
// 1) pT of primary (simulated) meson
// 2) Vertex and Centrality of the real event in which this pion
//    was embedded.
//
// IMPORTANT: We only keep events where *both* decay photons of 
//            the primary meson go towards PbSc detectors.

bool
emcEfficiency::getInitialParticleInfo(void)
{

//   TDatabasePDG* pdgDB = TDatabasePDG::Instance();
//   TParticlePDG* pi0 = pdgDB->GetParticle(PDGCODE_PIZERO);
//   assert(pi0!=0);

  int iphoton[2];
  iphoton[0]=iphoton[1]=-1;

  int nphotons=0;
  size_t ntr = (fEmbedObjectsType==0) ? (size_t)fEmcGeaTrack->RowCount() 
    : (size_t)fEmcGeaPart->get_EmcNGeaParticle();

  // Find the index of the (0,1,2) decay photons that go towards EMCAL
  for ( size_t i = 0 ; i < ntr && nphotons<2 ; ++i )
    {
      int idparent = (fEmbedObjectsType==0) ? (int)fEmcGeaTrack->get_idparent(i) 
	: (int)fEmcGeaPart->get_pidparent(i);
      int pid = (fEmbedObjectsType==0) ? (int)fEmcGeaTrack->get_pid(i) 
	: (int)fEmcGeaPart->get_pid(i);

      if ( idparent == fEffParticleCode &&
	   pid == GEANTCODE_GAMMA ) 	 
	{
	  iphoton[nphotons]=i;
	  nphotons++;
	}
    }

  assert(nphotons<=2);

  if ( nphotons < 2 ) 
    {
      // This shouldn't happen if the "forced acceptance" requirement was on when
      // producing the single particle simulated DST ... it seems to happen
      // from time to time due to small kReal and kPISA geometry module differences
      if ( fVerbose > 1 ) 
	{
	  std::cout << "<W> Weird simulated event !! Only " << nphotons 
		    << " gamma found heading towards EMCal. Skipping it. " << std::endl;
	}
      return false; 
    }

  bool bothInPbSc = fComputePbScEfficiency && isInPbSc(iphoton[0]) && isInPbSc(iphoton[1]);
  bool bothInPbGl = fComputePbGlEfficiency && isInPbGl(iphoton[0]) && isInPbGl(iphoton[1]);

  // Both photons heading towards PbSc. 

  if ( ( bothInPbSc ) || 
       ( bothInPbGl ) )
    {

      fMesonpT = -1.;
      fCentralityClassIndex = 9999;
      fMesonZvertex = -9999.;
      fBbcT0 = -9999.;
      int centrality = -1;

      // Retrieve information about primary meson.

      for ( size_t i = 0 ; i < ntr ; ++i )
	{

	  int idparent = (fEmbedObjectsType == 0) ? (int)fEmcGeaTrack->get_idparent(i) 
	    : (int)fEmcGeaPart->get_pidparent(i);
	  int pid = (fEmbedObjectsType == 0) ? (int)fEmcGeaTrack->get_pid(i) 
	    : (int)fEmcGeaPart->get_pid(i);

	  if ( idparent == 0 && pid == fEffParticleCode ) // primary && pi0 or eta
	    {
	      
	      float px = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_pxyz(0,i) : fEmcGeaPart->get_px(i);
	      float py = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_pxyz(1,i) : fEmcGeaPart->get_py(i);
	      fMesonpT = sqrt(px*px+py*py);

	      // Real evt. centrality
	      centrality = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_input(0) // "overloaded" field in "old" geaclusters
		: fEmcGeaCluster->get_real_event_centrality();

	      // Simulated vertex
	      double mesonZvertexSimul = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_xyz(2,i) 
		: fEmcGeaPart->get_vtx_xyz(i,2);

	      // Real event vertex
	      fMesonZvertex = (fEmbedObjectsType == 0) ? fEmcGeaClusterTrack->get_pc3proj(2,0) // "overloaded" field in "old" geaclusters
		: fEmcGeaCluster->get_real_event_zvertex();

	      // Real event BBC T0
	      fBbcT0 = (fEmbedObjectsType == 0) ? fEmcGeaClusterTrack->get_chglist(0,0) // "overloaded" field in "old" geaclusters
		: fEmcGeaCluster->get_real_event_bbcT0();

	      if ( fVerbose > 2 ) 
		{
		  std::cout << "<I> Primary INFO: ptot=" << fMesonpT 
			    << " vtx_z sim=" << mesonZvertexSimul
			    << " vtx_z real=" << fMesonZvertex 
			    << " BBC-T0=" << fBbcT0 
			    << " Centrality(%)=" << centrality
			    << std::endl;
		}

	      break;
	    }
	}
      
      // some checks

      if (fMesonpT<0)
	{
	  std::cerr << "<E> No primary pi0/eta or with negative pT !?!?!? : " << fMesonpT
		    << " . Skipping event !" << std::endl;
	  return false;
	}
      if (centrality<0)
	{
	  std::cerr << "<E> Negative centrality !?!?! : " << centrality
		    << " . Skipping event !" << std::endl;
	  return false;
	}

      // Fine. We got all the simulated pi0/eta info wanted.
      // Fill relevant histograms then.

      fNSuitableEvents++;

      // Get the centrality class index of the current event
      fCentralityClassIndex = fEventSorting->getCentralityIndex(centrality);

      histoCollection(kgeneral)->fill(khCentrality,centrality);            
      histoCollection(kgeneral)->fill(khCentClass,fCentralityClassIndex);
      histoCollection(kgeneral)->fill(khCentClass,fMinBiasIndex);

      // Get the corresponding Hagedorn weight (corrected by the fluctuation factor value ...)

      double corr = 1.0;

      fMesonHagedornWeight = mesonHagedornWeight(fMesonpT,fCentralityClassIndex,corr);
      fMesonMinbiasHagedornWeight = mesonHagedornWeight(fMesonpT,fMinBiasIndex,corr);

      // check if there is a conversion too in the event

      bool ConversionEvt = isConversionEvt();
      if (ConversionEvt) fNConvEvents2++;

      // Fill the reference (input) histogram for each cut and centrality

      std::vector<int> centralities = fEventSorting->getCentralities();
      std::vector<double> weights;

      weights.resize(centralities.size());
      std::map<int,string>::const_iterator it;

      HistogramCollection* hcol = 0;

      // loop over all centralities
      for ( size_t icent = 0 ; icent < centralities.size(); ++icent )
	{	 

	  // retrieve the associated Hagedorn weight (and fluctuation corr.) for the pT and centrality class
	  weights[icent] = mesonHagedornWeight(fMesonpT,icent,corr);

	  // loop over all cuts (directories)
	  for ( it = fCutMaskToString.begin(); it != fCutMaskToString.end(); ++it )
	    {
	      std::string cut_name = it->second;
	      hcol = histoCollection(cut_name,icent);

	      // *** This is the key input ("denominator") histogram for efficiencies ***

  	      hcol->fillW(khInitialPt,fMesonpT,weights[icent]);

	      // we fill this one also if the event has no gamma conversion
	      if (!ConversionEvt)
		{
		  hcol->fillW(khInitialPtConversionOff,fMesonpT,weights[icent]);
		}	
	
	      // control histos for the fluctuation correction application
	      if ( cut_name == kNoCut ) 
		{
		  histoCollection(kgeneral)->fill(khInitialPtUnweighted,fMesonpT);
		  histoCollection(kgeneral)->fillW(khInitialPtUnweightedCorr,fMesonpT,corr);
		}
	    }
	}
      
      if ( fVerbose > 2 ) 
	{
	  if (fEffParticleCode==7) cout << "<I> pi0: " ;
	  else if (fEffParticleCode==17) cout << "<I> eta: ";
	  printf("pT=%e GeV/c Vertex=%5.2f BBC_T0=%5.2f Centrality=%3d CentClass=%2d HagWeight=%e (minbias HagWeight=%e)\n",
		 fMesonpT,fMesonZvertex,fBbcT0,centrality,fCentralityClassIndex,
		 fMesonHagedornWeight,fMesonMinbiasHagedornWeight);
	}

      return true;
    }
  else // 1 (or both) of the 2 gammas not in PbSc
    {
      if ( fVerbose > 2 ) 
	{
	  if (fEffParticleCode==7) cerr << "<E> pi0 not in" ;
	  else if (fEffParticleCode==17) cerr << "<W> eta not in";
	  if (fComputePbScEfficiency) cerr << " PbSc. " ;
	  else if (fComputePbScEfficiency) cerr << " PbGl. ";
	  cerr << "Skipping event." << endl;
	}
      return false;
    }
}

//_____________________________________________________________________________
// loop on all merged clusters in the event
// Fill ListOfMergedParticles

void
emcEfficiency::getListOfMergedParticles(void)
{

  dEmcGeaClusterTrackWrapper& clus = *fEmcGeaClusterTrack;
  EmcGeaCluster& clust = *fEmcGeaCluster;
    
  fNMergedParticles=0;
  effParticle* particle = 0;

  // control general histos
  HistogramCollection* hcol_gen = histoCollection(kgeneral);
  if (!hcol_gen)
    {
      cerr << "<E> hcol_gen not found. Cannot fill general control histos ! " << endl;
      return ;
    }

  size_t nclus = (fEmbedObjectsType==0) ? (size_t)clus.RowCount() : (size_t)clust.get_EmcNGeaCluster();

  for ( size_t i = 0 ; i < nclus; ++i )
    {
      int dominantTrack = (fEmbedObjectsType==0) ? clus.get_trkno(0,i) : clust.get_geapart(i,0);
      if ( !dominantTrack ) 
	{
	  cout << " <W> No dominant contributor found in this cluster !?!? "
	       << ": fEmcGeaCluster(Track)->get_trkno(0," << i << ")=" << dominantTrack
	       << endl;
	  continue;
	}
     
      // "0" index = pid of the dominant contributor of the cluster
      int geantPID = (fEmbedObjectsType==0) ? static_cast<int>(clus.get_pid(0,i)) 
	: static_cast<int>(clust.get_pid(i,0));
      int pdgPID = effParticle::geantToPDG(geantPID);

      bool in_fiducial = isInFiducial(i);

      // FIXME: original "old" code not exactly as this ...
 
      int dead = (fEmbedObjectsType==0) ? static_cast<int>(clus.get_pc3proj(0,i))  // "0" index contains deadmap info
	: clust.get_deadmap(i);

      int warn = (fEmbedObjectsType==0) ? static_cast<int>(clus.get_pc3proj(1,i))  // "1" index contains warnmap info
	: clust.get_warnmap(i);

      float impx = (fEmbedObjectsType==0) ? clus.get_measxyz(0,i) : clust.get_xyz(i,0);
      float impy = (fEmbedObjectsType==0) ? clus.get_measxyz(1,i) : clust.get_xyz(i,1);
      float impz = (fEmbedObjectsType==0) ? clus.get_measxyz(2,i) : clust.get_xyz(i,2);

      // Important point here: 
      // Correct z hit position in EMCal by event vtx position ! 
      double tmpz = impz-fMesonZvertex; // (for meson momentum determination *ONLY* !)

      double d = sqrt(impx*impx + impy*impy + tmpz*tmpz);

      double impactangle = acos(impx/d); // from particle vertex

      // x-vertex of the *dominant* contributor to the cluster (2nd field of getter function is *0*)
      // could be =! (0.,0.,fMesonZvertex) if e+/- track !
      float vtx[3];
      vtx[0]=vtx[1]=vtx[2]=0.;

      vtx[0] = (fEmbedObjectsType==0) ? clus.get_vertex(0,0,i) : clust.get_vtx_xyz(i,0);
      vtx[1] = (fEmbedObjectsType==0) ? clus.get_vertex(1,0,i) : clust.get_vtx_xyz(i,1);
      vtx[2] = (fEmbedObjectsType==0) ? clus.get_vertex(2,0,i) : clust.get_vtx_xyz(i,2);

      // should be ecore for PbSc and ecorr for PbGl (as propagated in mEmcGeaClusterEval)
      double ecore = (fEmbedObjectsType==0) ? clus.get_ecore(i) : clust.get_ecore(i);

      double ptot  = (fEmbedObjectsType==0) ? clus.get_ptot(0,i) : clust.get_ptot(i,0);

      float p[3];
      p[0] = ecore*impx/d;
      p[1] = ecore*impy/d;
      p[2] = ecore*tmpz/d; 

      int arm = (fEmbedObjectsType==0) ? clus.get_arm(i) : clust.get_arm(i);
      int sector = (fEmbedObjectsType==0) ? clus.get_sector(i) : clust.get_sector(i);

      int is = geometryModule()->emcOfflineToEmc(arm,sector);

      float lx,ly,lz;

      geometryModule()->GlobalToLocal(impx,impy,impz,is,lx,ly,lz);

      // Append this cluster to the list of merged clusters.

      double chi2 = (fEmbedObjectsType==0) ? clus.get_chi2_sh(i) : clust.get_chi2_sh(i) ;
      double tof = (fEmbedObjectsType==0) ? clus.get_tof(i) : clust.get_tofcorr(i) ;
      tof -= fBbcT0; // we subtract the BBC T0 since the TOF cut assumes the time is centered at 0.

      particle = new ( (*fListOfMergedParticles)[fNMergedParticles] )
	effParticle(pdgPID,
		    p[0],p[1],p[2],ecore,
		    vtx[0],vtx[1],vtx[2], // 0.0,0.0,fMesonZvertex,
		    lx,ly,lz, // local coordinates to sector
		    chi2,
		    tof,
		    in_fiducial,
		    dead,warn,
		    is,arm,
		    impactangle);

      ++fNMergedParticles;

      // Fill some general histograms

      hcol_gen->fill(khArmSector,sector,arm);

      hcol_gen->fill(khMeasx,impx);
      hcol_gen->fill(khMeasy,impy);
      hcol_gen->fill(khMeasz,impz);

      if ( impx < 0 )
	{
	  // EAST
	  hcol_gen->fill(khMeasyz_e,impz,impy);
	  if ( in_fiducial ) 
	    {
	      hcol_gen->fill(khMeasyz_e_fiduc,impz,impy);
	    }
	}
      else
	{
	  // WEST
	  hcol_gen->fill(khMeasyz_w,impz,impy);
	  if ( in_fiducial ) 
	    {
	      hcol_gen->fill(khMeasyz_w_fiduc,impz,impy);
	    }
	}

      hcol_gen->fill(khEcore,ecore);
      hcol_gen->fill(khEcorePtot,ecore,ptot);
      hcol_gen->fill(khTof,tof);
      hcol_gen->fill(khChi2,chi2);
      //hcol_gen->fill(khMease,clus.get_mease(i));

      TLorentzVector P(p[0],p[1],p[2],ecore);

      hcol_gen->fill(kEta_vs_Phi,P.Phi(),P.Eta());

      hcol_gen->fill("hSector",is);
      
      if ( isInFiducial(i) )
	{
	  hcol_gen->fill("hSectorFidu",is);
	}					
    }

  int real_evt_mult = (fEmbedObjectsType==0) ? clus.get_charged(0) //  "overloaded" field in "old" geaclusters: 
    : clust.get_real_event_emc_multiplicity();

  hcol_gen->fill(khTotalClusterMul,real_evt_mult); 

  hcol_gen->fill(khEmbedClusterMul,nclus);
}

//_____________________________________________________________________________
// Loop on all simulated tracks in the event
// Fill ListOfSimulParticles

void
emcEfficiency::getListOfSimulParticles(void)
{

  dEmcGeaTrackWrapper& geatrack = *fEmcGeaTrack;
  EmcGeaParticlev2& geapart = *fEmcGeaPart;

  fNSimulParticles = 0;
  effParticle* particle = 0;

  TDatabasePDG* pdgDB = TDatabasePDG::Instance();
  TParticlePDG* pdgPart;
  
  size_t ntr = (fEmbedObjectsType==0) ? (size_t)geatrack.RowCount() : (size_t)geapart.get_EmcNGeaParticle();

  for ( size_t i = 0 ; i < ntr; ++i )
    {
      int anclvl = (fEmbedObjectsType==0) ? geatrack.get_anclvl(i) : 
	geapart.get_ancestry_lvl(i,0); // FIXME: 2nd arg. ?!?!

      if ( anclvl == 1 ) // all simulated particles (pi0/eta decay products)
	                 // actually entering in EMCAL (ancestry == 1)
	{
	  int geantPID = (fEmbedObjectsType==0) ? static_cast<int>(geatrack.get_pid(i)) 
	    : static_cast<int>(geapart.get_pid(i)) ;
	  int pdgPID = effParticle::geantToPDG(geantPID);	 
	  pdgPart = pdgDB->GetParticle(pdgPID);
	  
	  bool in_fiducial = isInFiducial(i); // true; // forced in_fiducial

	  float vtx[3];
	  float p[3];
	  vtx[0]=vtx[1]=vtx[2]=0.;
	  p[0]=p[1]=p[2]=0.;

	  p[0] = (fEmbedObjectsType == 0) ? geatrack.get_pxyz(0,i) : geapart.get_px(i);
	  p[1] = (fEmbedObjectsType == 0) ? geatrack.get_pxyz(1,i) : geapart.get_py(i);
	  p[2] = (fEmbedObjectsType == 0) ? geatrack.get_pxyz(2,i) : geapart.get_pz(i);

	  double impx = (fEmbedObjectsType == 0) ? geatrack.get_impxyz(0,i) : geapart.get_impxyz(i,0);
	  double impy = (fEmbedObjectsType == 0) ? geatrack.get_impxyz(1,i) : geapart.get_impxyz(i,1);
	  double impz = (fEmbedObjectsType == 0) ? geatrack.get_impxyz(2,i) : geapart.get_impxyz(i,2);

	  // we don't compute the 3-momentum from ecore*(impx,impy,impz-z-vtx) here at variance to "MergedParticles"

	  double d = sqrt(impx*impx + impy*impy + (impz-fMesonZvertex)*(impz-fMesonZvertex));
	  double impactangle = acos(impx/d); // from particle vertex

	  vtx[0] = (fEmbedObjectsType == 0) ? geatrack.get_xyz(0,i) : geapart.get_vtx_xyz(i,0);
	  vtx[1] = (fEmbedObjectsType == 0) ? geatrack.get_xyz(1,i) : geapart.get_vtx_xyz(i,1);
	  vtx[2] = (fEmbedObjectsType == 0) ? geatrack.get_xyz(2,i) : geapart.get_vtx_xyz(i,2);

	  //int is = -1;
	  int is = geometryModule()->HitInEMCalAcceptance(vtx,p);
	  assert(is!=-1); // just a cross-check: all should be in ...

	  int arm = (is<=3) ? 0 : 1 ;

	  float lx,ly,lz;
	  geometryModule()->GlobalToLocal(impx,impy,impz,is,lx,ly,lz);

	  // Append this track to the list of simulated tracks

	  double etot = (fEmbedObjectsType == 0) ? (geatrack.get_ekin(i) + pdgPart->Mass())
	    : geapart.get_e(i);

	  particle = new((*fListOfSimulParticles)[fNSimulParticles])
	    effParticle(pdgPID,
			p[0],p[1],p[2],
			etot,
			vtx[0],vtx[1],vtx[2],
			//impx,impy,impz, // global coordinates
			lx,ly,lz,  // local coordinates
			0.0,0.0, // chi2 and tof
			in_fiducial, // (before was forced true)
			0,0, // dead and warn
			is, // (before was forced -1)
			arm,
			impactangle);
	  
	  // be sure to avoid double counting particles in the different
	  // ancestry levels
	  
	  TLorentzVector p1,p2;
	  
	  particle->Momentum(p1);
	  for ( size_t j = 0 ; j < fNSimulParticles ; ++j ) 
	    {
	      effParticle* particle2 = static_cast<effParticle*>
		( (*fListOfSimulParticles)[j] );
	      particle2->Momentum(p2);
	      if ( p1 == p2 )
		{
		  fListOfSimulParticles->RemoveAt(j);
		  particle=0;
		}
	    }
	  
	  if (particle)
	    {
	      ++fNSimulParticles;
	    }
	}
    }

}

//_____________________________________________________________________________
double
emcEfficiency::getNormalization(TH1* hCentClass, const size_t centIndex)
{
  assert(hCentClass!=0);
  assert(fEventSorting!=0);

  int CentBinMax = hCentClass->GetXaxis()->FindBin(fEventSorting->getNCentralities()-1);
  CentBinMax -= 1 ;
  // -1 because last bin is minbias (i.e. not a centrality bin actually).

  double total = hCentClass->Integral(1,CentBinMax);
  
  return total/hCentClass->GetBinContent(centIndex+1);
}

//_____________________________________________________________________________
std::string
emcEfficiency::getPath(const std::string cutname, const size_t iCent) const
{
  static char name[200];

  sprintf(name,"/%s/C%d",cutname.c_str(),iCent);
  return std::string(name);
}

//_____________________________________________________________________________
std::string
emcEfficiency::getPath(const std::string dir, const std::string subdir) const
{
  static char name[200];

  if (!strcmp(subdir.c_str(),""))
    {
      sprintf(name,"/%s",dir.c_str());
    }
  else
    {
      sprintf(name,"/%s/%s",dir.c_str(),subdir.c_str());
    }
  return std::string(name);
}

//_____________________________________________________________________________
HistogramCollection*
emcEfficiency::histoCollection(const std::string dir)
{
  static std::map<string,vector<HistogramCollection*> >::iterator it;

  it = fH.find(dir);

  if ( it != fH.end() )
    {
      vector<HistogramCollection*>& vec = it->second;
      return vec[0];
    }
  else
    {
      cerr << "<E> emcEfficiency::histoCollection : Cannot find collection " 
	   << dir << endl;
      return 0;
    }
}

//_____________________________________________________________________________
HistogramCollection*
emcEfficiency::histoCollection(const std::string cutname, 
			       const size_t centClass)
{
  static std::map<string,vector<HistogramCollection*> >::iterator it;

  it = fH.find(cutname);

  if ( it != fH.end() )
    {
      vector<HistogramCollection*>& vec = it->second;
      if ( centClass < vec.size() ) 
	{
	  return vec[centClass];
	}
      else
	{
	  cerr << "<E> emcEfficiency::histoCollection : centClass="
	       << centClass << " is out of bounds for "
	       << cutname << endl;
	  return 0;
	}
    }
  else
    {
      cerr << "<E> emcEfficiency::histoCollection : Cannot find collection " 
	   << cutname 
	   << " centClass=" << centClass << endl;
      return 0;
    }
}

//_____________________________________________________________________________
void
emcEfficiency::integral(const TH1* h, const int MinBin, const int MaxBin,
			double& integralValue, 
			double& integralError) const
{
  integralValue=0.0;
  integralError=0.0;

  if ( !h ) 
    {
      cerr << "<E> emcEfficiency::integral : got a null pointer for "
	   << "histogram " << h->GetName() << endl;     
    }
  else
    {
      for ( int bin = MinBin; bin <= MaxBin; ++bin)
	{
	  integralValue += h->GetBinContent(bin);
	  integralError += h->GetBinError(bin)*h->GetBinError(bin);
	}
      
      integralError = TMath::Sqrt(integralError);
    }
}

//_____________________________________________________________________________
bool 
emcEfficiency::isInFiducial(const size_t iTrack)
{
  float xyz[3];
  xyz[0]=xyz[1]=xyz[2]=0.;

  if (fEmbedObjectsType==0)
    assert(fEmcGeaClusterTrack!=0);
  else
    assert(fEmcGeaCluster!=0);

  // FIXME: we use "meas" xyz instead of "true" xyz here ...
  xyz[0] = (fEmbedObjectsType == 0) ? fEmcGeaClusterTrack->get_measxyz(0,iTrack) 
    : fEmcGeaCluster->get_xyz(iTrack,0);
  xyz[1] = (fEmbedObjectsType == 0) ? fEmcGeaClusterTrack->get_measxyz(1,iTrack)
    : fEmcGeaCluster->get_xyz(iTrack,0);
  xyz[2] = (fEmbedObjectsType == 0) ? fEmcGeaClusterTrack->get_measxyz(2,iTrack)
    : fEmcGeaCluster->get_xyz(iTrack,0);

  // iS is the sector number (in mEmcGeometryModule convention)
  int arm = (fEmbedObjectsType == 0) ? fEmcGeaClusterTrack->get_arm(iTrack) : fEmcGeaCluster->get_arm(iTrack);
  int sec = (fEmbedObjectsType == 0) ? fEmcGeaClusterTrack->get_sector(iTrack) : fEmcGeaCluster->get_sector(iTrack);
  int iS = geometryModule()->emcOfflineToEmc(arm,sec);

  // No fiducial cut for PbGl events !!!
  if ( fComputePbGlEfficiency && (iS == 6 || iS == 7) )
    {
      return true;
    }

  float lx,ly,lz;

  geometryModule()->GlobalToLocal(xyz[0],xyz[1],xyz[2],iS,lx,ly,lz);

  // Each sector in its *local* coordinates occupies an area between: 
  // y_local ~ [0, 200] and z_global = x_local ~ [0, 400]

  if ( lx>=fFiduXmin && lx<=fFiduXmax &&
       ly>=fFiduYmin && ly<=fFiduYmax )
    {
      histoCollection(kgeneral)->fill(khfiducial_local_yz_geaclustertrack,
				      lx,ly);
      return true ;
    }

  return false;
}

//_____________________________________________________________________________
bool
emcEfficiency::isInPbSc(const size_t i) const
{

  if ( i<0 ) return false ;

  int sector;
  float v[3];
  float p[3];
  v[0]=v[1]=v[2]=0.;
  p[0]=p[1]=p[2]=0.;

  // vertex of track
  v[0] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_xyz(0,i) : fEmcGeaPart->get_vtx_xyz(i,0);
  v[1] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_xyz(1,i) : fEmcGeaPart->get_vtx_xyz(i,1);
  v[2] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_xyz(2,i) : fEmcGeaPart->get_vtx_xyz(i,2);

  if (fVerbose>2) cout << " vtx: (" << v[0] << "," << v[1] << "," << v[2] << ")" << endl;  

  // momentum of track
  p[0] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_pxyz(0,i) : fEmcGeaPart->get_px(i);
  p[1] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_pxyz(1,i) : fEmcGeaPart->get_py(i);
  p[2] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_pxyz(2,i) : fEmcGeaPart->get_pz(i);

  if (fVerbose>2) cout << " mom: (" << p[0] << "," << p[1] << "," << p[2] << ")" << endl;  

  bool inPbSc = geometryModule()->HitInPbSc(v,p,sector);

  if ( inPbSc )
    {
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
emcEfficiency::isInPbGl(const size_t i) const
{

  if ( i<0 ) return false ;

  int sector;
  float v[3];
  float p[3];
  v[0]=v[1]=v[2]=0.;
  p[0]=p[1]=p[2]=0.;
  
  // vertex of track
  v[0] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_xyz(0,i) : fEmcGeaPart->get_vtx_xyz(i,0);
  v[1] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_xyz(1,i) : fEmcGeaPart->get_vtx_xyz(i,1);
  v[2] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_xyz(2,i) : fEmcGeaPart->get_vtx_xyz(i,2);

  if (fVerbose>2) cout << " vtx: (" << v[0] << "," << v[1] << "," << v[2] << ")" << endl;  

  // momentum of track
  p[0] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_pxyz(0,i) : fEmcGeaPart->get_px(i);
  p[1] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_pxyz(1,i) : fEmcGeaPart->get_py(i);
  p[2] = (fEmbedObjectsType == 0) ? fEmcGeaTrack->get_pxyz(2,i) : fEmcGeaPart->get_pz(i);

  if (fVerbose>2) cout << " mom: (" << p[0] << "," << p[1] << "," << p[2] << ")" << endl;  

  bool inPbGl = geometryModule()->HitInPbGl(v,p,sector);

  if ( inPbGl )
    {
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
emcEfficiency::isConversionEvt(void) const
{

  bool ConversionEvt = false;

  dEmcGeaTrackWrapper& geatrack = *fEmcGeaTrack;
  EmcGeaParticlev2& geapart = *fEmcGeaPart;

  size_t ntr = (fEmbedObjectsType==0) ? (size_t)geatrack.RowCount() : (size_t)geapart.get_EmcNGeaParticle();

  for ( size_t i = 0 ; i < ntr ; ++i )
    {
      int pid = (fEmbedObjectsType==0) ? (int)geatrack.get_pid(i) : (int)geapart.get_pid(i);
      if ( TMath::Abs(pid) == GEANTCODE_ELECTRON )
	{
	  ConversionEvt = true;
	  break;
	}
    }

  return ConversionEvt;
}

//_____________________________________________________________________________
// Loop on ListOfMergedParticles (all reconstructed clusters in EMCal)
// Calculate inv. mass and fill cuts/centralities bi-dim efficiency histos

void
emcEfficiency::minvMergedParticles(void)
{
  fNMergedPairs = 0;
  effPair* pair = 0;

  for ( size_t i1 = 0 ; i1 < fNMergedParticles; ++i1 )
    {
      effParticle* particle1 = static_cast<effParticle*>
	( (*fListOfMergedParticles)[i1] );      

      for ( size_t i2 = i1+1 ; i2 < fNMergedParticles; ++i2 )
	{      	  
	  effParticle* particle2 = static_cast<effParticle*>
	    ( (*fListOfMergedParticles)[i2] );      
	  
	  pair = new ( (*fListOfMergedPairs)[fNMergedPairs] )
	    effPair( *particle1, *particle2 );
	  
	  ++fNMergedPairs;

	  // ** Check what analysis cuts the pair passes **

	  int mask = passCuts(*pair);
	  
	  pair->setCutMask(mask);

	  // ** Filling of all needed histos is done here **

	  fill(*pair);

	} // i2
    } // i1
}

//_____________________________________________________________________________
// Loop on ListOfSimulParticles (all pi0/eta decay products actually hitting EMCal)
// Calculate inv. mass and fill cuts/centralities bi-dim efficiency histos

void
emcEfficiency::minvSimulParticles(void)
{
  fNSimulPairs=0;
  effPair* pair=0;

  int conversion = 0;

  for ( size_t i1 = 0 ; i1 < fNSimulParticles; ++i1 )
    {
      effParticle* particle1 = static_cast<effParticle*>
	( (*fListOfSimulParticles)[i1] );      

      for ( size_t i2 = i1+1 ; i2 < fNSimulParticles; ++i2 )
	{      	  
	  effParticle* particle2 = static_cast<effParticle*>
	    ( (*fListOfSimulParticles)[i2] );      
	  
	  pair = new ( (*fListOfSimulPairs)[fNSimulPairs] )
	    effPair( *particle1, *particle2 );
	  
	  pair->setSimulPair(true);

	  if ( TMath::Abs(particle1->GetPdgCode()) == PDGCODE_ELECTRON || 
	       TMath::Abs(particle2->GetPdgCode()) == PDGCODE_ELECTRON )
	    {
	      if (!conversion) fNConvEvents++; // count only once per event
	      conversion++ ;
	    }
	  
	  ++fNSimulPairs;

	  // ** Check what analysis cuts the pair passes **

	  int mask = passCuts(*pair); // returns the cuts mask

	  pair->setCutMask(mask);

	  // ** Filling of all needed histos is done here **

	  fill(*pair);

	} // i2
    } // i1

  if (conversion) cout << conversion << " " ;
}

//_____________________________________________________________________________
//
// The efficiency file can be opened in 3 modes (see README)
// 1) "RECREATE" (default): 1st step --> produce efficiency bi-dim histos
// 2) "UPDATE"            : 2nd step --> compute efficiencies
// 3) "READ"              : 3rd step --> output efficiency factors, plots, tables

bool 
emcEfficiency::openEfficiencyFile(const char* outputfile, const char* opt)
{
  if ( fEfficiencyFile ) 
    {
      std::cerr << "<E> emcEfficiency::openEfficiencyFile : output file "
		<< "already opened. Close it first" << std::endl;
      return false;
    }

  fEfficiencyFileName = outputfile;

  //_____________________________________________________________________________
  // let's check first that the user really wants to open it in RECREATE mode

  bool fileNotThere = gSystem->AccessPathName(outputfile, kFileExists);

  std::string answer;
  std::string yes("y");

  if ( strcasecmp("RECREATE",opt) == 0 && !(fileNotThere) )
    {
      cout << " <W> Ummh you seem to want to recreate an existing efficiency file ..." << endl;

      // let's try to read it first in update mode
      fEfficiencyFile = new TFile(fEfficiencyFileName.c_str());
      std::string path = getPath(kgeneral,"");
      bool filewithdata = fEfficiencyFile->cd(path.c_str());

      if (filewithdata)
	{
	  TH1* hEmbedClusterMul = getHistogram<TH1>(kgeneral,khEmbedClusterMul);
	  if ( hEmbedClusterMul && hEmbedClusterMul->GetEntries() )
	    {
	      cout << "<I> " << outputfile << " contains already " 
		   << hEmbedClusterMul->GetEntries() << " entries" << endl;
	      cout << "<W> Opening in RECREATE mode (default) will delete the current contents of " << outputfile
		   << " Are you sure ? (y/n) " << endl;
	      std::cin >> answer;
	      if ( answer != yes )
		{
		  cout << "<I> OK. You can then try to open " << outputfile << " as: " << endl
		       << "    openEfficiencyFile(\"" << outputfile << "\",\"UPDATE\"); // or as " << endl
		       << "    readEFficiencyFile(\"" << outputfile << "); " << endl;
		  fEfficiencyFile->Close();
		  delete fEfficiencyFile;
		  fEfficiencyFile=0;
		  fEfficiencyFileName="";
		  return false;
		}
	    }
	  else
	    {
	      cout << " <I> OK. The existing efficiency file seems to be empty ... Let's recreate it" << endl;
	    }
	}
      else
	{
	  cout << " <I> OK. The existing efficiency file seems to be empty ... Let's recreate it" << endl;
	}
      fEfficiencyFile->Close();
    }

  //_____________________________________________________________________________
  // Now let's open it as the user requested ...
  fEfficiencyFile = new TFile(fEfficiencyFileName.c_str(),opt);

  if ( !fEfficiencyFile || !fEfficiencyFile->IsOpen() ) 
    {
      std::cerr << "<E> emcEfficiency::openEfficiencyFile : Problems opening " << outputfile << std::endl;
      delete fEfficiencyFile;
      fEfficiencyFile=0;
      fEfficiencyFileName="";
      return false;
    }

  //_____________________________________________________________________________
  // a part from opening the file let's do a few more things ...
  
  if ( strcasecmp("RECREATE",opt) == 0 )
    {
      if ( fRUN == 2 && fCorrectRun2ExodusFluct && !fRun2ExodusFluctCorrection )
	{
	  defaultRun2ExodusFluctCorrection(); // apply by default the "fluctuation correction" = 1 function

	  if ( fEffParticleCode == 7 ) // only for pi0 Run-2
	    {
	      cout << "<Q> Do you want to export a fluctuation correction function from another file (y/n) ?"
		   << endl;
	      std::cin >> answer;
	      if ( answer == yes )
		{
		  cout << "<Q> Enter file name from where to export the correction: "
		       << endl;
		  std::cin >> answer;
		  bool ok = setRun2ExodusFluctCorrectionFromFile(answer.c_str());
		  if ( !ok ) 
		    cout << "<W> No fluctuation correction exported. Using the default one." << endl;
		}
	    }
	}

      book(); // create all directory structure and book all histos

      fNEvents=0;
      fNSuitableEvents=0;
      fNConvEvents=0;
      fNConvEvents2=0;
    }
  else if ( strcasecmp("UPDATE",opt) == 0 )
    {
      // implement me
    }
  else // opened in "READ" mode ...
    {
      // implement if necessary
    }

  return true;
}

//_____________________________________________________________________________
bool
emcEfficiency::openEmbedFile(const char* filename)
{
  if ( fIOmanager ) 
    {
      std::cerr << "<E> emcEfficiency::openEmbedFile : There's a file already "
		<< " opened. Close it first" << std::endl;
      return false;
    }

  fEmbedFileName = filename;
    
  PHString inputfile = fEmbedFileName.c_str();

  fIOmanager = new PHNodeIOManager(inputfile,PHReadOnly);

  if ( ! fIOmanager || ! fIOmanager->isFunctional() ) 
    {
      std::cerr << "<E> emcEfficiency::openEmbedFile : Problems opening " << filename << std::endl;
      delete fIOmanager;
      fIOmanager=0;
      fEmbedFileName="";
      return false;
    }

  fNodeTOP = new PHCompositeNode("TOP");

  // "old" embed objects type
  if (fEmbedObjectsType==0)
    {
      fIOmanager->selectObjectToRead("*",false);
      fIOmanager->selectObjectToRead("EVA/dEmcGeaTrack",true);
      fIOmanager->selectObjectToRead("EVA/dEmcGeaClusterTrack",true);

      fIOmanager->print();

      fNodeEVA = new PHCompositeNode("EVA");
      fNodeTOP->addNode(fNodeEVA);
    }
  else
    {
      //fIOmanager->selectObjectToRead("*",false);
      fIOmanager->selectObjectToRead("UDST/EmcGeaParticle",true);
      fIOmanager->selectObjectToRead("UDST/EmcGeaClusterMix",true);

      fIOmanager->print();

      fNodeUDST = new PHCompositeNode("UDST");
      fNodeTOP->addNode(fNodeUDST);
    }

  // Total number of events in Embed file
  TFile f(filename,"");
  int embedEvts = (int)((TTree*)f.Get("T"))->GetEntries();
  f.Close();

  std::cout << "<I> Embed file " << fEmbedFileName << " opened (it contains " << embedEvts
	    << " entries)" << std::endl;

  if (embedEvts==0) 
    {
      std::cerr << "<E> emcEfficiency::openEmbedFile :  " << filename << " has no entries !" << std::endl;
      closeEmbedFile();
      return false;
    }

  return true;
}

//_____________________________________________________________________________
int
emcEfficiency::passCuts(const effPair& pair) const
{
  // Check if particles pass the various cuts
  // NOTE: Make sure all data members cuts are defined in the constructor !

  int mask = kNoCutMask;

  const effParticle& particle1 = pair.Particle1();
  const effParticle& particle2 = pair.Particle2();

  // Fiducial cuts
  if ( particle1.InFiducial() && particle2.InFiducial() )
    {
      mask |= kFiduCutMask;
    }

  // W3 sector cut
  const int iW3 = 3; //  W3 sector number (mEmcGeometryModule convention)
  if ( particle1.Sector() != iW3 && particle2.Sector() != iW3 ) 
    {
      mask |= kNoW3CutMask;
    }

  // Dead && Warn cut 5x5
  if ( particle1.Dead()==0 && particle1.Warn()==0 &&
       particle2.Dead()==0 && particle2.Warn()==0 ) 
    {
      mask |= kDeadWarn5x5CutMask;
    }

  // Dead && Warn cut 3x3
  const static unsigned int sccut3x3Map = 0xffe1ce70;     
  unsigned int cut3x3Map = sccut3x3Map;    

  if ( (particle1.Dead() & cut3x3Map)==0 && (particle1.Warn() & cut3x3Map) == 0 &&
       (particle2.Dead() & cut3x3Map)==0 && (particle2.Warn() & cut3x3Map) == 0 ) 
    {
      mask |= kDeadWarn3x3CutMask;
    }

  // Min. energy cuts
  if ( particle1.Energy() > fMinClusterEnergy &&
       particle2.Energy() > fMinClusterEnergy )
    {
      mask |= kEnergyCutMask;
    }

  // Cosine cut
  double cosine_factor = TMath::Power(fMinvLowCut,2)/(2.*particle1.Energy()*particle2.Energy());
  cosine_factor = 1-cosine_factor;
  if ( pair.Cosine() < cosine_factor )
    {
      mask |= kCosCutMask;
    }

  // Pair in same sector cut
  if ( particle1.Sector() == particle2.Sector() ) 
    {
      mask |= kPairSameSectCutMask;
    }

  // Pair is same arm cut
  if ( particle1.Arm() == particle2.Arm() ) 
    {
      mask |= kPairSameArmCutMask;
    }

  // Conversion cut: Require pairs where both are gamma 
  // i.e. exclude evts where one/both of the clusters is an electron/positron
  if ( particle1.GetPdgCode() == PDGCODE_GAMMA && //!= PDGCODE_ELECTRON &&
       particle2.GetPdgCode() == PDGCODE_GAMMA ) //!= PDGCODE_ELECTRON )
    {
      mask |= kConversionOffCutMask;
    }

  // Electron evt. cut: Require evts where one/both of the clusters is an electron/positron
  if ( TMath::Abs(particle1.GetPdgCode()) == PDGCODE_ELECTRON ||
       TMath::Abs(particle2.GetPdgCode()) == PDGCODE_ELECTRON )
    {
      mask |= kElectronEvtsCutMask;
    }

  // Asym. cuts    
  if ( pair.Asymmetry() < fMaxAsym1 )
    {
      mask |= kAsym1CutMask;
    }
  if ( pair.Asymmetry() < fMaxAsym2 )
    {
      mask |= kAsym2CutMask;
    }

  // Chi2 cuts  
  if ( TMath::Abs(particle1.Chi2()) < fMaxChiSq1 &&
       TMath::Abs(particle2.Chi2()) < fMaxChiSq1 )
    {
      mask |= kChiSq1CutMask;
    }
  if ( TMath::Abs(particle1.Chi2()) < fMaxChiSq2 &&
       TMath::Abs(particle2.Chi2()) < fMaxChiSq2 )
    {
      mask |= kChiSq2CutMask;
    }

  // TOF cuts
  if ( TMath::Abs(particle1.TOF()) < fMaxTof1 &&
       TMath::Abs(particle2.TOF()) < fMaxTof1 ) 
    {
      mask |= kToF1CutMask;
    }
  if ( TMath::Abs(particle1.TOF()) < fMaxTof2 &&
       TMath::Abs(particle2.TOF()) < fMaxTof2 ) 
    {
      mask |= kToF2CutMask;
    }
  
  // Relative TOF cut
  if ( TMath::Abs(particle1.TOF() - particle2.TOF()) < fRTof )
    {
      mask |= kRToFCutMask;
    }

  return mask;
}

//_____________________________________________________________________________
double
emcEfficiency::mesonHagedornWeight(const double MesonpT, const size_t iCent, 
				   double& corr)
{
  if ( iCent >= 0 && 
       iCent <= fMinBiasIndex )
    {

      pair<double,double> hag = fHagedornWeightParameters[iCent];

      double p0 = hag.first;
      double n = hag.second;
      
      // Both parametrizations are equivalent within a constant 1/p0^n normalization
      // for weight purposes one can use one or the other ...
      double weight = TMath::Power( (MesonpT + p0), n);
      //double weight = TMath::Power( ( 1 + MesonpT/p0), n);

      // in the first pass over the data there is no fluctuation correction yet and thus corr=1

      corr = 1.0;

      if (fCorrectRun2ExodusFluct)
	{
	  int binMesonpT = fRun2ExodusFluctCorrection->FindBin(MesonpT);
	  corr = fRun2ExodusFluctCorrection->GetBinContent(binMesonpT);

	  //cout << "fluctuation corr is : " << corr << endl;
	  weight *= corr;
	}

      return weight;
    }
  else
    {

      corr = 0.;

      std::cerr << "<E> emcEfficiency::mesonHagedornWeight : iCent "
		<< iCent << " out of bounds" << std::endl;
      return 0;
    }
}
 
//_____________________________________________________________________________
void
emcEfficiency::meson_reconstruction(void)
{
  // Find back the simulated pi0/eta transverse momentum,
  // the centrality class and z-vertex position of the merged event.
  bool ok = getInitialParticleInfo();

  if ( ok ) 
    {
      // ** for conversion studies **

      // Get the list of simulated secondary particles 
      //getListOfSimulParticles();
      
      // Fill histograms for those secondaries
      //minvSimulParticles();
      
      // Get the list of merged clusters that do have a simulated contribution
      getListOfMergedParticles();
      
      // Fill Minv-pT histograms for those merged clusters
      minvMergedParticles();
    }
}

//_____________________________________________________________________________
void 
emcEfficiency::printDecodedMask(const int mask)
{
  std::string name;

  for ( int i = 0; i < 32 ; ++i ) 
    {
      int lm = ( 1 << i ) ;

      if ( mask & lm ) 
	{
	  name += fCutMaskToString[lm];
	  name += " ";
	}
    }
  std::cout << name << std::endl;
}

//_____________________________________________________________________________
bool
emcEfficiency::readFixedExtractionWindows()
{

  fIntegrationParameters.clear(); // index of fIntegrationParameters runs over centrality

  std::vector<pair<double,double> > peak_and_width; // index of peak_and_width runs over pT bins

  int centMin = -1;
  int centMax = -1;

  const size_t n = fEventSorting->getCentralities().size(); // last one is min.bias

  size_t centIndex = 0 ;
  char PeakWidthFiles[300];
  double pT = 0., meanfix = 0., sigfix =0.;
  double truePT = 0.;

  if ( fEffParticleCode == 17 ) // eta
    {
      for ( centIndex = 0 ; centIndex < n; centIndex++)
	{
	  fEventSorting->getCentralityClassLimits(centIndex,centMin,centMax);
	  if ( centIndex == fMinBiasIndex )  { centMin = 0 ; centMax = 92 ; }

	  // this file contains the peaks and widths with coarse binning (1 GeV/c)
	  // some gymnastics has to be done below to fill our finer binning (0.5 GeV/c) 
	  // with the values on file ...

	  //char location[200] = "/phenix/workarea/saskia/eta_peaks";
	  char location[400] = "/afs/rhic.bnl.gov/phenix/users/enterria/run2_eta/eta_finalscan";
	  sprintf(PeakWidthFiles,"%s/peaksfix_%d_%d.txt",location,centMin,centMax);
	  
	  ifstream ifiledata(PeakWidthFiles);
	  if (!ifiledata)
	    {
	      cerr << "<E> emcEfficiency::readFixedExtractionWindows : Can't open file: " 
		   << PeakWidthFiles << endl;
	      return false;
	    }
	  cout << "<I> Reading " << PeakWidthFiles << endl;

	  size_t icount = 0;

	  peak_and_width.clear();

// 	  // constant eta peak and width values ...
// 	  pT=0.25; 
// 	  meanfix = 0.578; // 578 MeV
// 	  sigfix = 0.049;  //  49 MeV

// 	  while ( icount < fPT_NBINS )
// 	    {
// 	      peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
// 	      pT += fPT_BINWIDTH;
// 	      cout << "pT = " << pT << " (i = " << icount << ") --> mean = " 
// 		   << meanfix << ", sigma = " << sigfix << endl; 
// 	      icount++;
// 	    }
// 	  fIntegrationParameters.push_back(peak_and_width);

	  // reading peak and width from files
	  // pT bin width in file is 0.5 or 1.0 GeV/c (bin centers: 2.50, 3.50 GeV/c ...)

	  while ( ifiledata >> 
		  pT >>
		  meanfix >> 
		  sigfix
		  )
	    {
	      if (fPT_MIN==0)
		{  
		  // fill the first 4 pT bins (0.0-0.5 ... 1.5-2.0 GeV not in the files) with the pT=2.5 values
		  while (icount<4) 
		    {

		      peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
		      truePT = (icount+fPT_BINWIDTH)/2 ;
		      if ( fVerbose > 1 )
			{
			  cout << "pT(read) = " << pT << " (pt = " << truePT << ", i = " << icount 
			       << ") --> mean = " << meanfix << ", sigma = " << sigfix << endl; 
			}
		      icount++;

		    }

		  // fill the rest of pT bins with the values read from file

		  peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
		  truePT = (icount+fPT_BINWIDTH)/2 ;
		  if ( fVerbose > 1 )
		    {
		      cout << "pT(read) = " << pT << " (pt = " << truePT << ", icount = " << icount 
			   << ") --> mean = " << meanfix << ", sigma = " << sigfix << endl;
		    }
		  icount++;

		  // pT binning in ascii files is 2 times pT binning in effic. calculations ...
		  while (pT>=truePT+fPT_BINWIDTH/2) //(truePT<pT+fPT_BINWIDTH/2) 
		    {
		      peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
		      truePT = (icount+fPT_BINWIDTH)/2 ;
		      if ( fVerbose > 1 )
			{
			  cout << "pT(read) = " << pT << " (pt = " << truePT << ", icount = " << icount 
			       << ") --> mean = " << meanfix << ", sigma = " << sigfix << endl;
			}
		      icount++;
		    }
		}
	      else // no filling of pairs below the selected pT_min
		{

		  if (pT>=fPT_MIN) 
		    {
		      peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
		      truePT = (icount+fPT_BINWIDTH)/2 ;
		      if ( fVerbose > 1 )
			{
			  cout << "pT(read) = " << pT << " (pt = " << truePT << ", i = " << icount 
			       << ") --> mean = " << meanfix << ", sigma = " << sigfix << endl; 
			}
		      icount++;
		    }
		}
	    }

	  // after having read the file
	  double pt = pT;
	  while ( icount < fPT_NBINS ) // fill the highest pT bins (up to fPT_NBINS) with the latest read values
	    {
	      peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
	      truePT = (icount+fPT_BINWIDTH)/2 ;
	      //pt += fPT_BINWIDTH;
	      if ( fVerbose > 1 )
		{
		  cout << "pT(read) = " << pt << " (pt = " << truePT << ", i = " << icount 
		       << ") --> mean = " << meanfix << ", sigma = " << sigfix << endl; 
		}
	      icount++;
	    }
	  fIntegrationParameters.push_back(peak_and_width);
	  ifiledata.close();
	}
    }
  else // pi0
    {
      for ( centIndex = 0 ; centIndex < n; centIndex++)
	{
	  fEventSorting->getCentralityClassLimits(centIndex,centMin,centMax);
	  centMin = ( centMin >= 60 ) ? 60 : centMin;
	  centMax = ( centMin >= 60 ) ? centMin+20 : centMax; // last file is 60_80.txt (we use it for all most periph. classes)
	  if ( centIndex == fMinBiasIndex )  { centMin = 0 ; centMax = 100 ; }
	  
	  char location[200] = "/phenix/u/saskia/pi0_rescan";	  // PPG014 peaks & widths
	  //char location[200] = "/phenix/u/saskia/year2/dec03_scan"; // Dec.03 peaks & widths
	  sprintf(PeakWidthFiles,"%s/peaksfix_%d_%d.txt",location,centMin,centMax);
	  
	  ifstream ifiledata(PeakWidthFiles);
	  if (!ifiledata)
	    {
	      cerr << "<E> emcEfficiency::readFixedExtractionWindows : Can't open file: " 
		   << PeakWidthFiles << endl;
	      return false;
	    }
	  cout << "<I> Reading " << PeakWidthFiles << endl;
	  
	  size_t icount = 0;

	  peak_and_width.clear();
	  
	  while ( ifiledata >> 
		  pT >>        // pT is at bin center (e.g. 0.25, 0.75 GeV/c ...)
		  meanfix >> 
		  sigfix
		  )
	    {
	      if (fPT_MIN==0)
		{  
		  // fill the first 2 pT bins (0.0-0.5 and 0.5-1.0 GeV not in the files) with the pT=1.-1.5 values
		  while (icount<2) 
		    {
		      peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
		      truePT = (icount+fPT_BINWIDTH)/2 ;
		      if ( fVerbose > 1 )
			{
			  cout << "pT(read) = " << pT << " (pt = " << truePT << ", i = " << icount 
			       << ") --> mean = " << meanfix << ", sigma = " << sigfix << endl; 
			}
		      icount++;
		    }

		  // fill the rest of pT bins with the values read from file
		  peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
		  truePT = (icount+fPT_BINWIDTH)/2 ;
		  if ( fVerbose > 1 )
		    {
		      cout << "pT(read) = " << pT << " (pt = " << truePT << ", i = " << icount 
			   << ") --> mean = " << meanfix << ", sigma = " << sigfix << endl;
		    }
		  assert( (pT+0.25) == icount*fPT_BINWIDTH );
		  icount++;
		}
	      else // no filling of pairs below the selected pT_min
		{
		  if (pT>=fPT_MIN) 
		    {
		      peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
		      truePT = (icount+fPT_BINWIDTH)/2 ;
		      if ( fVerbose > 1 )
			{
			  cout << "pT(read) = " << pT << " (pt = " << truePT << ", i = " << icount 
			       << ") --> mean = " << meanfix << ", sigma = " << sigfix << endl; 
			}
		      icount++;
		    }
		}
	    }

	  // after having read the file
	  double pt = pT;
	  while ( icount < fPT_NBINS ) // fill the highest pT bins (up to fPT_NBINS) with the latest read values
	    {
	      peak_and_width.push_back(make_pair<double,double>(meanfix,sigfix));
	      //pt += fPT_BINWIDTH;
	      truePT = (icount+fPT_BINWIDTH)/2 ;
	      if ( fVerbose > 1 )
		{
		  cout << "pT(read) = " << pt << " (pt = " << truePT << ", i = " << icount 
		       << ") --> mean = " << meanfix << ", sigma = " << sigfix << endl; 
		}
	      icount++;
	    }
	  fIntegrationParameters.push_back(peak_and_width);
	  ifiledata.close();
	}
    }

  assert(fIntegrationParameters.size() == n );
  assert(peak_and_width.size() == fPT_NBINS );
  
  return true;
}

//_____________________________________________________________________________
bool
emcEfficiency::readEfficiencyFile(void)
{
  if ( fEfficiencyFileName.empty() )
    {
      std::cerr << "<E> No Efficiency file opened."
		<< " Try first openEfficiencyFile(\"myEfficFile.root\");"
		<< " or readEfficiencyFile(\"myEfficFile.root\"); " 
		<< std::endl;
      return false;
    }
  
  closeEfficiencyFile();
  
  bool fileok = openEfficiencyFile(fEfficiencyFileName.c_str(),"READ");
  
  if ( !fileok ) 
    {
      std::cerr << "<E> emcEfficiency::readEfficiencyFile : Cannot "
		<< "open file " << fEfficiencyFileName
		<< std::endl;
      return false;
    }
  return true;
}

//_____________________________________________________________________________
bool
emcEfficiency::readEfficiencyFile( const char* effic_file )
{

  closeEfficiencyFile();
  
  bool fileok = openEfficiencyFile(effic_file,"READ");
  
  if ( !fileok ) 
    {
      std::cerr << "<E> emcEfficiency::readEfficiencyFile : Cannot "
		<< "open file " << fEfficiencyFileName
		<< std::endl;
      return false;
    }
  return true;
}

//_____________________________________________________________________________
void
emcEfficiency::reset(void)
{
  if (fEmcGeaTrack)
    fEmcGeaTrack->SetRowCount(0);
  if (fEmcGeaClusterTrack)
    fEmcGeaClusterTrack->SetRowCount(0);

  if (fEmcGeaPart)
    fEmcGeaPart->Reset();
  if (fEmcGeaCluster)
    fEmcGeaCluster->Reset();

  // not needed actually (reseting fNMerged(Simul)Particles counters for every event)
  fListOfMergedParticles->Clear();
  fListOfSimulParticles->Clear();

  fListOfMergedPairs->Clear();
  fListOfSimulPairs->Clear();
}

//_____________________________________________________________________________
// Reads and loops on the input embed+eval file
//
void
emcEfficiency::run(const int maxevents)
{

  if ( !fEfficiencyFile )
    {
      std::cerr << "<E> emcEfficiency::run : No output file opened."
		<< " Call openEfficiencyFile() method first." 
		<< std::endl;
      return;
    }

  if ( !fIOmanager )
    {
      std::cerr << "<E> emcEfficiency::run : No input file opened."
		<< " Call openEmbedFile() method first." 
		<< std::endl;
      return;
    }

  if ( !fBookedHistos )
    {
      std::cerr << "<E> emcEfficiency::run : Output histos not booked yet."
		<< " Call openEfficiencyFile() in *RECREATE* mode." 
		<< std::endl;
      return;
    }


  int eventNumber=0;
  PHCompositeNode* rv=0;

  // Loop on events (New Objects Type)

  if (fEmbedObjectsType == 1)
    {

      cout << "<I> Will read the new (Justin's) evaluation objects from " << fEmbedFileName << endl;

      while ( (rv=fIOmanager->read(fNodeUDST,eventNumber))!=0 &&
	      (eventNumber<maxevents || maxevents<0 ) )
	{
	  if (eventNumber==0 && fVerbose>1 ) 
	    {
	      fIOmanager->print();
	    }
	  
	  // 1) We read for each event the track and cluster tables

	  fEmcGeaPart = PHNodeHelper<EmcGeaParticlev2>::getObject("EmcGeaParticle",fNodeTOP);//UDST);
	  assert(fEmcGeaPart!=0);
	  cout << "zvtx1: " << fEmcGeaPart->get_vtx_xyz(0,2) << " zvtx2:" << fEmcGeaPart->get_ZVertex(0) << endl;

	  fEmcGeaCluster = PHNodeHelper<EmcGeaClusterv2>::getObject("EmcGeaClusterMix",fNodeTOP); //UDST);
	  assert(fEmcGeaCluster!=0);
	  
	  // 2) We reconstruct the pions from every cluster pair
	  
	  meson_reconstruction();
	  
	  fNEvents++;
	  
	  eventNumber++;
	  
	  reset();
	  
	  if ( eventNumber%100==0 ) 
	    {
	      //cout << "Event #" << eventNumber << endl;
	      cout << "." << flush;
	    }
	}
    }

  // Loop on events ("OLD" Objects Type)

  else
    {
      cout << "<I> Will read the old (STAF-table) evaluation objects from " << fEmbedFileName << endl;

      while ( (rv=fIOmanager->read(fNodeEVA,eventNumber))!=0 &&
	      (eventNumber<maxevents || maxevents<0 ) )
	{
	  if (eventNumber==0 && fVerbose>1 ) 
	    {
	      fIOmanager->print();
	    }
	  
	  // 1) We read for each event the track and cluster tables
	  
	  fEmcGeaTrack = PHNodeHelper<dEmcGeaTrackWrapper>::getTable("dEmcGeaTrack",fNodeEVA);
	  assert(fEmcGeaTrack!=0);
	  
	  fEmcGeaClusterTrack = PHNodeHelper<dEmcGeaClusterTrackWrapper>::getTable("dEmcGeaClusterTrack",fNodeEVA);
	  assert(fEmcGeaClusterTrack!=0);
	  
	  // 2) We reconstruct the pions/etas from every cluster pair
	  
	  meson_reconstruction();
	  
	  fNEvents++;
	  
	  eventNumber++;
	  
	  reset();
	  
	  if ( eventNumber%100==0 ) 
	    {
	      //cout << "Event #" << eventNumber << endl;
	      cout << "." << flush;
	    }
	}
    }

  cout << "\n NEvents = " << fNEvents << endl;
  cout << " NSuitableEvents = " << fNSuitableEvents << endl;
  cout << " NConvEvents = " << fNConvEvents << endl;
  cout << " NConvEvents2 = " << fNConvEvents2 << endl;

}

//_____________________________________________________________________________
void
emcEfficiency::setPTrange(double ptmin, double ptmax )
{
  cout << "<I> pT range of all histograms modified from (" 
       << fPT_MIN << "," << fPT_MAX << ") to: (" ;

  fPT_MIN = ptmin;
  fPT_MAX = ptmax;
  fPT_NBINS = (size_t)((fPT_MAX-fPT_MIN)/fPT_BINWIDTH);

  cout << fPT_MIN << "," << fPT_MAX << ")" << endl ;

}

//_____________________________________________________________________________
void
emcEfficiency::setFixedExtractionWindows(const bool mode )
{

  fFixedExtractionWindows = mode;

  if (fEffParticleCode==7) cout << endl << "<I> pi0 ";
  else if (fEffParticleCode==17) cout << endl << "<I> eta ";
  cout << "extraction yield mode: ";

  if ( fFixedExtractionWindows )
    {
      cout << " FIXED integration windows.";
    }
  else
    {
      cout << " Centrality & Cut VARIABLE integration windows.";
    }

  cout << " ---- To change it use emcEfficiency::setFixedExtractionWindows( bool )." << endl;

}

//_____________________________________________________________________________
void
emcEfficiency::computeEfficiencyFor( const char *calorimeter )
{

  TString calo_str = calorimeter;
  TString mess = "";

  if (calo_str.Contains("PbGl",TString::kIgnoreCase))
    {
      fComputePbGlEfficiency = true;
      fComputePbScEfficiency = false;
      mess = " ** PbGl ONLY **";
   }
  else if (calo_str.Contains("both",TString::kIgnoreCase))
    {
      fComputePbScEfficiency = true;
      fComputePbGlEfficiency = true;
      mess = " ** BOTH PbSc & PbGl **";
   }
  else // (calo_str.Contains("PbSc",TString::kIgnoreCase))
    {
      fComputePbScEfficiency = true;
      fComputePbGlEfficiency = false;
      mess = " ** PbSc ONLY (default) **"; 
    }

  cout << "<I> Will compute efficiency for : " << mess 
       << " ---- To change it use emcEfficiency::computeEfficiencyFor( char* )" << endl;

}

//_____________________________________________________________________________
void
emcEfficiency::setRun2ExodusFluctCorrection(const TH1* h, const char* title)
{
  if ( !title )
    {
      std::cerr << "<E> emcEfficiency::setRun2ExodusFluctCorrection: " 
		<< " Provide a title (e.g. \"Fitted fluctuation correction\")." << std::endl;
      return;
    }

  if ( !h ) 
    {
      std::cerr << "<W> emcEfficiency::setRun2ExodusFluctCorrection: " 
		<< " Null pointer for exported histogram. No Fluctuation Correction applied. " << std::endl;
      return;
    }
  
  if (fRun2ExodusFluctCorrection) delete fRun2ExodusFluctCorrection;
  fRun2ExodusFluctCorrection = static_cast<TH1*>(h->Clone());
  fRun2ExodusFluctCorrection->SetDirectory(fEfficiencyFile);
  fRun2ExodusFluctCorrection->SetName("hRun2ExodusFluctCorrection");
  fRun2ExodusFluctCorrection->SetTitle(title);
  
  std::cout << "<I> emcEfficiency::setRun2ExodusFluctCorrection: \"" 
	    << title << "\" is set to file " << fEfficiencyFileName << std::endl;

  fCorrectRun2ExodusFluct = true;

}

//_____________________________________________________________________________
bool
emcEfficiency::setRun2ExodusFluctCorrectionFromFile( const char* efficFile1 )
{
  // Let's recover the correction function
  TFile file1(efficFile1);
  
  TH1* h = 0;
  h = static_cast<TH1*>(file1.Get("hRun2ExodusFluctCorrection"));
  if ( h )
    {
      TString type = h->GetTitle();
      if ( !type.Contains("Fitted",TString::kIgnoreCase) )
	{
	  cout << "<W> This file " <<  efficFile1
	       <<" contains only a default fluctuation correction (correction = 1.) histo."
	       << "\n If you think the file may have enough statistics try running fitFluctuation() on it first"
	       << " prior to exporting its correction." << endl ;
	  return false;
	}
    }
  else 
    {
      file1.Close();
      std::cerr << "<E> Couldn't find any fluctuation correction histo in this file ?!?! " << std::endl ;
      return false;
    }

  h->SetDirectory(0);

  cout << "<I> Fitted fluctuation correction function found in file " <<  efficFile1
       << " . The corresponding correction *WILL* be exported to your efficiency object." << endl;

  setRun2ExodusFluctCorrection(h,"Imported fitted fluctuation correction"); // we set it

  file1.Close();

  fCorrectRun2ExodusFluct = true;

  return true;

}

//_____________________________________________________________________________
void
emcEfficiency::setHagWeightPerCentrality(std::vector< pair<double,double> >&
					 weights)
{
  std::cerr << "<E> emcEfficiency::setHagWeightPerCentrality : IMPLEMENT ME !"
	    << std::endl;
}

//_____________________________________________________________________________
void
emcEfficiency::subtractLikePlot(const size_t ptbin,
				const char* cutName, 
				const size_t iCent)
{
  gStyle->SetPaperSize(TStyle::kA4);

  bool ok = check_cut(cutName);

  if ( !ok ) 
    {
      return;
    }

  TH1* hInitialPt = getHistogram<TH1>(cutName,khInitialPt,
				      iCent);

  TString cutname_str = cutName;
  if (cutname_str.Contains("Off"))
    {
      hInitialPt = getHistogram<TH1>(cutName,khInitialPtConversionOff,iCent);
    }

  if ( !hInitialPt ) 
    {
      std::cerr << "<E> emcEfficiency::subtractLikePlot : Could not find "
		<< "hInitialPt histogram." << std::endl;
      return;
    }

  char name[80];

  sprintf(name,"hMinv_%d",ptbin);

  TH1* hRecoMinv = getHistogram<TH1>(cutName,name,iCent);

  if ( !hRecoMinv )
    {
      std::cerr << "<E> emcEfficiency::subtractLikePlot : Could not find "
		<< name << " histogram." << std::endl;
      return;
    }

  sprintf(name,"Counting_%s_P%d_C%d",cutName,ptbin,iCent);

  TH1* hCentClass = getHistogram<TH1>(kgeneral,khCentClass);
  assert(hCentClass!=0);

  double normalization = getNormalization(hCentClass,iCent);
  
  std::vector<emcEffValue> values;

  computeEfficiency(hInitialPt,hRecoMinv,iCent,ptbin,normalization,values);

  std::vector<std::string> report;

  std::string title;
  fEventSorting->getCentralityDescription(iCent,title);
  report.push_back(title);

  char s[200];
  double ptmin = hInitialPt->GetBinLowEdge(ptbin);
  double ptmax = ptmin + hInitialPt->GetBinWidth(ptbin);
  sprintf(s,"pT=[%7.2f-%7.2f[ GeV/c",ptmin,ptmax);
  report.push_back(s);

  TF1* fgauss = static_cast<TF1*>(hRecoMinv->GetListOfFunctions()
				->FindObject("gauss_plus_bckg"));

  sprintf(s,"Fit region = [%7.4f-%7.4f] GeV/c2",
	  fFIT_MINV_LO,fFIT_MINV_UP);
  report.push_back(s);

  sprintf(s,"Peak position = %7.4f +- %7.4f GeV/c2",fgauss->GetParameter(1),
	  fgauss->GetParError(1));
  report.push_back(s);

  sprintf(s,"Peak sigma = %7.4f +- %7.4f GeV/c2",fgauss->GetParameter(2),
	  fgauss->GetParError(2));
  report.push_back(s);

  sprintf(s,"Integration window = [%7.4f-%7.4f]",
	  values[kIntegrationWindow].valueMin(),
	  values[kIntegrationWindow].valueMax());
  report.push_back(s);

  sprintf(s,"Number of pi0/eta (count method) = %7.4f +- %7.4f",
	  values[kDetected_count].value(),values[kDetected_count].error());
  report.push_back(s);

  sprintf(s,"Number of pi0/eta (fit method) = %7.4f +- %7.4f",
	  values[kDetected_fit].value(),values[kDetected_fit].error());
  report.push_back(s);

  sprintf(s,"Number of pi0/eta (subtracted background) = %7.4f +- %7.4f",
	  values[kDetected_bckgd].value(),values[kDetected_bckgd].error());
  report.push_back(s);

  sprintf(s,"Efficiency (count method) = %7.4f +- %7.4f",
	  values[kEfficiency_count].value(), values[kEfficiency_count].error());
  report.push_back(s);

  sprintf(s,"Signal to Background = %7.4f +- %7.4f",
	  values[kSignal2Background].value(), values[kSignal2Background].error());
  report.push_back(s);

  sprintf(s,"Efficiency (fit method) = %7.4f +- %7.4f",
	  values[kEfficiency_fit].value(), values[kEfficiency_fit].error());
  report.push_back(s);

  sprintf(s,"Efficiency (combined) = %7.4f +- %7.4f",
	  values[kEfficiency_comb].value(), values[kEfficiency_comb].error());
  report.push_back(s);

  //------------------------

  sprintf(name,"Counting_%s_P%d_C%d",cutName,ptbin,iCent);
  TCanvas* c1 = new TCanvas(name,name);
  
  c1->UseCurrentStyle();

  c1->Divide(1,2);
  c1->cd(1);

  TIter next(hRecoMinv->GetListOfFunctions());
  TObject* obj;
  
  int color=0;
  const int colors[] = {1,2,4,1};

  while ( (obj=next()) ) 
  {
    TF1* f1 = dynamic_cast<TF1*>(obj);
    if ( f1 ) 
      {
	f1->SetFillStyle(0);
	f1->SetFillColor(0);
	f1->SetLineColor(colors[color]);
	color++;
      }
  };

  int b2 = hRecoMinv->GetXaxis()->FindBin(0.4);
  hRecoMinv->GetXaxis()->SetRange(1,b2);
  hRecoMinv->Draw();

  //--- 

  c1->cd(2);

  TPaveText* pave = new TPaveText(0.01,0.01,0.99,0.99);
  pave->SetTextFont(82);
  pave->SetTextSize(0.025);
  pave->SetTextAlign(10);
  pave->SetBorderSize(1);
  pave->SetFillColor(0);
  pave->SetTextSize(0.04);

  for ( size_t i = 0; i < report.size(); ++i ) 
    {
      std::cout << report[i] << std::endl;
      pave->AddText(report[i].c_str());
    }

  pave->Draw();

  c1->Update();

  char ceps[200];

  sprintf(ceps,"%s.eps",c1->GetName());
  c1->Print(ceps);
}

//_____________________________________________________________________________
std::ostream&
emcEfficiency::table(std::ostream& out,
		     const char* cutName,
		     const bool latex)
{
  bool ok = check_cut(cutName);

  if ( !ok ) 
    {
      return out;
    }

  std::vector<int> centralities = fEventSorting->getCentralities();

  int centMin, centMax;

  if ( latex ) 
    {
      out << "\\begin{table}[hbt]" << std::endl;
      out << "\\begin{tabular}{|c|c|}" << std::endl;
    }

  for ( size_t i = 0; i < centralities.size(); ++i ) 
    {
      fEventSorting->getCentralityClassLimits(i,centMin,
					      centMax);
      if ( centMax == 100 ) continue;

      table(out,cutName,i,latex);
    }

  if ( latex ) 
    {
      out << "\\end{tabular}" << std::endl;
      out << "\\end{table}" << std::endl;
    }

  return out;
}

//_____________________________________________________________________________
std::ostream&
emcEfficiency::table(std::ostream& out,
		     const char* cutName, 
		     const size_t iCent,
		     const bool latex)
{

  bool ok = check_cut(cutName);

  if ( !ok ) 
    {
      return out;
    }

  TH1* hInitialPt = getHistogram<TH1>(cutName,khInitialPt,
				      iCent);

  TString cutname_str = cutName;
  if (cutname_str.Contains("Off"))
    {
      hInitialPt = getHistogram<TH1>(cutName,khInitialPtConversionOff,iCent);
    }

  if ( !hInitialPt ) 
    {
      std::cerr << "<E> emcEfficiency::table : Could not find "
		<< "hInitialPt histogram." << std::endl;
      return out;
    }

  int centMin, centMax;

  fEventSorting->getCentralityClassLimits(iCent,centMin,
					  centMax);

  std::ostream::fmtflags oldflags = out.flags();

  std::string line_separator = string(50,'-');
  std::string col_separator = " ";
  std::string end_line = "";
  std::string pm = "+-";
  std::string percent = "%";

  if ( latex ) 
    {
      line_separator = "\\hline";
      col_separator = " & ";
      end_line = "\\\\";
      pm = "$\\pm$";
      percent = "\\%";
    }

  out << line_separator << std::endl;

  if ( iCent == fMinBiasIndex ) 
    {
      out << "Minbias" << col_separator << " " << end_line << std::endl;
    }
  else
    {
      out << "Centrality [" << dec << setw(2) << centMin
	  << "-" << dec << setw(2) << centMax 
	  << "[ " << percent  
	  << col_separator << " " << end_line << std::endl;
   }

  out << line_separator << std::endl;

  TH1* hCentClass = getHistogram<TH1>(kgeneral,khCentClass);
  assert(hCentClass!=0);

  double normalization = getNormalization(hCentClass,iCent);

  std::vector<emcEffValue> values;
 
  out << "pT (GeV/c)" << col_separator << "Efficiency " << end_line << std::endl;

  out << line_separator << std::endl;

  for ( int ptbin = 2; ptbin < 15; ptbin++ )
    {
      char name[80];

      sprintf(name,"hMinv_%d",ptbin);

      TH1* hRecoMinv = getHistogram<TH1>(cutName,name,iCent);
      
      if ( !hRecoMinv )
	{
	  std::cerr << "<E> emcEfficiency::table : Could not find "
		    << name << " histogram." << std::endl;
	  return out;
	}
 
      computeEfficiency(hInitialPt,hRecoMinv,iCent,ptbin,normalization,values);
      
      double ptmin = hInitialPt->GetBinLowEdge(ptbin);
      double ptwidth = hInitialPt->GetBinWidth(ptbin);
      
      double pt = ptmin + ptwidth/2.0;

      out.setf(ostream::scientific);

      out.precision(2);

      out << pt << pm << ptwidth/2.0 << col_separator;

      out.precision(4);

      out << values[kEfficiency_comb].value() << pm
	  << values[kEfficiency_comb].error() << end_line;
      if (!latex)
	{
	  out << " " << values[kEfficiency_comb].error()*100/values[kEfficiency_comb].value();
	}
      out << std::endl;
    }

  out << line_separator << std::endl;

  out.setf(oldflags);
  return out;
}


//_____________________________________________________________________________
void
emcEfficiency::writeEfficiencyFile(void)
{
  // Checkpoint writing of ouput file. 

  if ( fEfficiencyFile ) 
    {

      if ( fEfficiencyFile->IsWritable() ) // opened in update/recreate mode
	{
	  if (fVerbose>1)
	    {
	      cout << "<I> Writing " << fEfficiencyFile->GetPath() << " ... " << endl;
	    }
	  
	  fEfficiencyFile->cd();
	  if (fVerbose>1) cout << "<I> Writing EventSorting ... " << endl;
	  if (fEventSorting) fEventSorting->Write("EventSorting",TObject::kOverwrite);
	  if (fCorrectRun2ExodusFluct)
	    {
	      if (fVerbose>1) cout << "<I> Writing Run2ExodusFluctCorrection histo ... " << endl;
	      if (fRun2ExodusFluctCorrection) fRun2ExodusFluctCorrection->Write("hRun2ExodusFluctCorrection",
									    TObject::kOverwrite);
	    }
	  //NtupRecoParticleMinvPtImpAngle->Write("",TObject::kOverwrite);
	  
	  if (fVerbose>1) cout << "<I> Writing everything else ... " << endl;
	  fEfficiencyFile->Write("",TObject::kOverwrite);
	}
      else if (fVerbose>0)
	{
	  cout << "<W> " << fEfficiencyFileName << " not opened in writable mode ... " << endl;
	}
    }
}

//_____________________________________________________________________________
// Output the efficiency correction factors either to cout (default)
// or into a given file:
//
// > std::ofstream fileout("my_correction_factors.h");
// > emcEFficiency::outputFinalEfficiencyFactors("mycut","counts",fileout);
// 
// to be used in the code which corrects and produces the final spectra 
// (e.g. analyzer.C, QM02/pi0_spectra_run2.C or comparator.C ...)
// 
// type = "counts" (default), "fit", or "combined"
// format = "c++" (default: C++ table), or "latex" (latex table)

void
emcEfficiency::outputFinalEfficiencyFactors(const char* cutname, 
					    const char* type,
					    const double ptminfit,
					    const double ptmaxfit,
					    const bool ploteffic,
					    ostream& out,
					    const char* format)
{

  if (!(check_cut(cutname)))
    {
      return;
    }

  // First index (external index) of vector runs over number of fit parameters
  // Second index (internal index) of vectors runs over cut type

  std::vector<vector<double> > par;
  std::vector<vector<double> > parerr;

  size_t n = fEventSorting->getCentralities().size();

  for ( size_t icent = 0; icent < n ; ++icent ) 
    {

      // ***** The important business is done here !!! *****
      TF1* f1 = fitEfficiency( cutname, type, icent, ptminfit, ptmaxfit, ploteffic); 
      if (!f1) continue;     

      size_t nfitpar = f1->GetNpar();

      par.resize(nfitpar);
      parerr.resize(nfitpar);

      size_t ip = 0;
      while(ip<nfitpar)
	{
	  par[ip].push_back( f1->GetParameter(ip) );
	  parerr[ip].push_back( f1->GetParError(ip) );
	  ip++;
	}

   }

  // Now, let's dump the obtained fit factors ...

  //if ( strcasecmp("BasicCuts = FiduNoW3DeadWarn5x5EnergyCosCut",cutname) == 0 ) sprintf(cutname,"BasicCuts");

  char partic[20];

  if (fEffParticleCode==7)
    {
      sprintf(partic,"_pi0[CentClassesPi0]");
    }
  else if (fEffParticleCode==17)
    {
      sprintf(partic,"_eta[CentClassesEta]");
    }

  if ( strcasecmp("c++",format) == 0 )
    {

      size_t nfitpar = par.size();
      size_t ip = 0;
      while(ip<nfitpar)
	{
	  out << "double P" << ip << "_" << cutname << "_" << type << partic << " = { " << endl;
	  makeCorrectionFactors(out,par[ip]);
	  out << "double P" << ip << "ERR_" << cutname << "_" << type << partic << " = { " << endl;
	  makeCorrectionFactors(out,parerr[ip]);
	  ip++;
	}

    }
  else if ( strcasecmp("latex",format) == 0 )
    {
      size_t nfitpar = par.size();

      out << "\\begin{table}[hbt]" << std::endl;
      out << "\\begin{center}" << std::endl;
      out << "\\caption{ .}" << std::endl;
      //out << "\\begin{tabular}{|c|c|c|c|c|c|}\\hline\\hline" << std::endl;
      out << "\\begin{tabular}{|";
      for (size_t ip=0;ip<nfitpar;ip++){ out << "c|"; }
      out << "}\\hline\\hline" << std::endl;
      out << "Centrality &";
      //out << "$p_0 \\pm p_0^{err}$ & $p_1 \\pm p_1^{err}$  & $p_2 \\pm p_2^{err}$ & $p_3 \\pm p_3^{err}$ & $p_4 \\pm p_4^{err}$";
      for (size_t ip=0;ip<nfitpar;ip++){ out << " $p_" << ip << " \\pm p_" << ip << "^{err}$ &" ; }
      out <<"\\\\ \\hline" << std::endl;

      int cent1 = 0;
      int cent2 = 0;
     
      cout << " <W> This procedure has not yet been tested --- FIXME: outputFinalEfficiencyFactors()" << endl;
 
      for ( size_t i = 0 ; i < par[0].size(); ++i ) // fixme: change p0 --> par[0]. is this correct?
	{
	  fEventSorting->getCentralityClassLimits( i, cent1, cent2 );
	  
	  out.setf(ostream::scientific);
	  out.precision(3);
	  
	  if (i<9) out << cent1 << " - " << cent2 << "\\% & " ;
	  else out << "min.bias" << " & " ;

	  for (size_t ip=0;ip<nfitpar;ip++) { out << par[ip][i] << " $\\pm$ " << parerr[ip][i] << " & " ; }
	  out << "\\\\ \n";
	}

      out << "\\hline\\hline" << std::endl;
      out << "\\end{tabular}" << std::endl;
      out << "\\label{tab:eff_fit_parameters}" << std::endl;
      out << "\\end{center}" << std::endl;
      out << "\\end{table}" << std::endl;
    }

  par.clear();
  parerr.clear();

  return;
}
//_____________________________________________________________________________
void
emcEfficiency::outputFinalEfficiencyFactors_all_cuts( const char* type, 
						      const double ptminfit,
						      const double ptmaxfit,
						      ostream& out )
{

  // Some sanity checks before we start...

  bool ok = readEfficiencyFile();

  if ( !ok ) 
    {
      return;
    }

  // Just a first check at the beginning about the status of the fluctuation correction ...
  // only relevant for PPG014 pi0's

  if ( fEffParticleCode == 7 && fCorrectRun2ExodusFluct )
    {
      TString fluct_corr;
      statusExodusFluctCorrectionHisto(fluct_corr);
      
      if ( !fluct_corr.Contains("applied",TString::kIgnoreCase) )
	{
	  cout << "<W> Efficiencies will be computed (seemingly) without fluctuation correction." 
	       << " Check the control histo general/hInitialPtUnweightedCorr in case of doubts ..." << endl;
	}
      else 
	{
	  cout << "<I> Efficiencies will be computed with the fluctuation correction applied." << endl;
	}
    }

  // Output file format

  out << "#ifndef __emc" << gSystem->BaseName(fEfficiencyFileName.c_str()) << "_counts_h__" << endl 
      << "#define __emc" << gSystem->BaseName(fEfficiencyFileName.c_str()) << "_counts_h__" << endl << endl;

  out << "// IMPORTANT MESSAGE: " << endl << "//" << endl;
  out << "// This set of efficiency correction factors were produced running: " << endl << "//" << endl
      << "// emcEfficiency::outputFinalEfficiencyFactors_all_cuts(\"" << type << "\"" 
      << "," << ptminfit << "," << ptmaxfit << "," << "\"thisfile.h\")" << endl
      << "// on file: " << fEfficiencyFileName << endl << endl
      << "// The parameters correspond to the fitting function: " 
      << (InitFitEfficiency(ptminfit,ptmaxfit))->GetTitle() << endl << "//" << endl
      << "// below or above the fit limits there is **NO** warranty that the fit provides any trustable result" 
      << endl << "//" << endl;

    if (fEffParticleCode==7)
      {
	out << "const int CentClassesPi0 = 20;" << endl << endl;
      }
    else if (fEffParticleCode==17)
      {
	out << "const int CentClassesEta = 22;" << endl << endl;
      }

  // let's compute iteratively the efficiency factors for each cut ...

  std::map<int,string>::const_iterator it;

  for ( it = fCutMaskToString.begin(); it != fCutMaskToString.end() ; ++it ) 
    {
      std::string cut_name = it->second;
      bool ploteffic = false;
      // plot by default our "favourite"-cuts efficiency
      //if ( !strcmp(cut_name.c_str(),"BasicCuts3x3Asym1ChiSq1ToF1Cut") ) ploteffic = true; // favourite cut for PPG014
      if ( !strcmp(cut_name.c_str(),"BasicCuts3x3Asym1ChiSq2ToF2Cut") ) ploteffic = true; // latest favourite cut
      //if ( !strcmp(cut_name.c_str(),"NoW3EnergyCosAsym1ChiSq2ToF2Cut") ) ploteffic = true; // latest favourite cut
      outputFinalEfficiencyFactors( cut_name.c_str(), type, ptminfit, ptmaxfit, ploteffic, out, "c++" );
    }

  out << endl << "#endif" << endl;

//   cout << "<I> Efficiency fit factors output to: " << out.getloc() << endl ;
//   cout << "<I> Recall that the range of the Efficiency fit was set between: pT = " << ptminfit 
//        << " - " << ptmaxfit << "  GeV/c" << endl;

}

//_____________________________________________________________________________
void
emcEfficiency::makeCorrectionFactors(ostream& out, std::vector<double>& p0)
{
  std::ostream::fmtflags oldflags = out.flags();
  std::string centClass;

  size_t cc = p0.size();

  assert( cc == fEventSorting->getCentralities().size() );

  if (fEffParticleCode==17) cc = 22; // eta classes: 0-20%(=20), 20-60%(=21), plus 60-92%(=17), minbias(=19)

  for ( size_t i = 0 ; i < cc; ++i ) 
    {
      fEventSorting->getCentralityDescription( i, centClass );

      out.setf(ostream::scientific);
      out.precision(6);

      if (fEffParticleCode==7)
	{
	  if ( i < p0.size()-2 ) // 0-10%, 10-20% ... ,70%-80%
	    {
	      out << string(10,' ') << p0[i] << ", " << p0[i] ; // twice the same value written (0-5%,5-10%)=(0-10%) ...
	      out << "," << " // " << centClass << endl; // add a comma and a comment with the description of the class
	    }
	  else if ( i < p0.size()-1 ) // 80-92%
	    {
	      out << string(10,' ') << p0[i] << ", " << p0[7] ; // before-last-value=80-92%, last-value=60-92% (==70-80%)
	      out << "," << " // " << centClass // add a comma and a comment with the description of the class
		  << ", and Centrality 60-92% (=70-80%)" << endl;
	    }
	  else // last line (min.bias)
	    {
	      out << string(10,' ') << p0[6] << ", " << p0[i] ; // before-last-value=60-80% (==60-70%), last-value=min.bias
	      //out << string(10,' ') << (p0[6]+p0[7])/2 << ", " << p0[i] ; // before-last-value=60-80%, last-value=min.bias 
	      out << "  // Centrality 60-80%(=60-70%), and " << centClass << endl; // no comma in the last class
	    }
	}
      else if (fEffParticleCode==17)  // eta classes: 0-20%(=20), 20-60%(=21), plus 60-92%(=17), minbias(=19)
	{
	  if ( i < 17 || i == 18) // 0-10%, 10-20% ... ,70%-80%
	    {
	      out << string(10,' ') << "0.," ;
	      out << " // " << i << endl; // add a comma and a comment with the description of the class
	    }
	  else if ( i == 17 ) // 60-92%
	    {
	      out << string(10,' ') << p0[2] ;
	      out << "," << " // " << i // add a comma and a comment with the description of the class
		  << " 60-92% " << endl;
	    }
	  else if ( i == 19 ) // minbias
	    {
	      out << string(10,' ') << p0[3] ;
	      out << "," << " // " << i // add a comma and a comment with the description of the class
		  << " minbias " << endl;
	    }
	  else if ( i == 20 ) // 0-20%
	    {
	      out << string(10,' ') << p0[0] ;
	      out << ", // 0-20%" << endl; // no comma in the last class
	    }
	  else if ( i == 21 ) // 20-40%
	    {
	      out << string(10,' ') << p0[1] ;
	      out << "  // 20-60%" << endl; // no comma in the last class
	    }
	}
    }

  out << "};" << endl;

  out.setf(oldflags);
}

//_____________________________________________________________________________
TF1*
emcEfficiency::InitFitEfficiency(const double ptminfit,
				 const double ptmaxfit)
{

  TF1* EffFit = 0;

  gStyle->SetOptFit(111);
  gStyle->SetOptStat(0);

  // Polynomial-4 fit for eta

//    if ( fEffParticleCode==17 )
//     {
//       EffFit = new TF1("EffFit","pol4",ptminfit,ptmaxfit);
//     }
//   else // pi0
//     {
      // More complex fit
      //double ptmin = fPT_MIN;
      //if (!ptmin) ptmin = 1.; // fit starts at 1 GeV/c
      //double ptmax = fPT_MAX-1.;
      
      EffFit = new TF1("EffFit","[0]+[1]*x+[2]*x*x+[3]*log(x)+[4]*log(x)*log(x)",ptminfit,ptmaxfit);
      //EffFit = new TF1("EffFit","([0]*x*x+[1]*x+[2])*(1.0-exp([3]*x+[4]))",ptminfit,ptmaxfit);
      EffFit->SetParameters(0.25,0.1,0.1,0.2,1.);

      //EffFit = new TF1("EffFit","pol6",ptminfit,ptmaxfit);
      //EffFit->SetParameters(0.2,0.1);
//    }

  return EffFit;
}

//_____________________________________________________________________________
TF1*
emcEfficiency::fitEfficiency(const char* cutname, const char* type,
			     const size_t icent, 
			     const double ptminfit,
			     const double ptmaxfit,
			     const bool ploteffic)
{

  if ( fVerbose >0 )
    {
      cout << "<I> Fitting efficiency for cut: " << cutname 
	   << " and centrality: C" << icent 
	   << " between pT = " << ptminfit 
	   << " - " << ptmaxfit << "  GeV/c" << endl;
    }

  TString type_str = type;
  TString cutname_str = cutname;

  TH1* h = 0;
  char histoeff[200];

  if ( type_str.Contains("comb",TString::kIgnoreCase) )
    {
      // counts+fit combined efficiency
      h = getCombinedEfficHisto(cutname,icent);
    }
  else
    {
      sprintf(histoeff,"hEfficiency_%s",type);
      h = getHistogram<TH1>(cutname,histoeff,icent);
    }

  if  (!h) return 0;

  h->GetListOfFunctions()->Delete();

  // cheating here for high pT PPG014 pi0 !!!
  //   if ( fEffParticleCode==7 )
  //     {
  //       modifyEfficHisto( h );
  //     }

  h->SetMaximum(1.);
  h->SetMinimum(0.);

  gStyle->SetOptTitle(0);
  //gStyle->SetStatX(0.75); // stats box position
  //gStyle->SetStatY(0.65);

  TH1* h1 = (TH1*)h->Clone();

  TF1* EffFit = (TF1*)InitFitEfficiency(ptminfit,ptmaxfit);
  h->Fit(EffFit,"QR0","",ptminfit,ptmaxfit);
  EffFit->SetRange(fPT_MIN,fPT_MAX); // back to its widest range

  // Control histos messages

  double chi2 = EffFit->GetChisquare()/EffFit->GetNDF();
  cout << "Chi2 of fit is: " << chi2;
  if (chi2 > 2.)
    {
      cout << " --------------> " << "<W> Large Chi2 !! Check this guy: " 
	   << cutname_str << "_C" << icent << " <--------------" << endl;
    }
  cout << endl;

  // Control histos written on disk
  //if ( cutname_str.Contains("BasicCuts5x5Asym1ChiSq1ToF1") ) // chisq1tof1 and chisq1tof2
  //if ( cutname_str.Contains("BasicCuts3x3Asym1ChiSq1ToF1") ) // chisq1tof1 and chisq1tof2
  if ( ploteffic || chi2 >2. )
    {
      int cent1 = 0;
      int cent2 = 0;
      fEventSorting->getCentralityClassLimits(icent,cent1,cent2);

      if (icent!=fMinBiasIndex) sprintf(histoeff,"hEff_%s_%s_C%d_%d",type,cutname,cent1,cent2);
      else sprintf(histoeff,"hEff_%s_%s_minbias",type,cutname);
      TCanvas *cc = new TCanvas(histoeff,histoeff,600,600);
      cc->SetFillColor(0);
      cc->SetBorderMode(0);
      cc->SetBorderSize(0);

      char ytitle[200];
      if (icent!=fMinBiasIndex) sprintf(ytitle,"Efficiency (%d-%d%%)",cent1,cent2);
      else sprintf(ytitle,"Efficiency (min.bias)");
      cc->cd();
      h->SetMaximum(1.5); //0.75);
      h->GetXaxis()->SetTitle("p_{T} (GeV/c)");
      h->GetXaxis()->SetTitleOffset(1.1);
      h->GetYaxis()->SetTitle(ytitle);
      h->GetYaxis()->SetTitleOffset(1.25);
      h->SetLineColor(0); // white
      //h->SetLineColor(18); // light grey
      h->SetMarkerStyle(20);
      h->Draw();
      cc->Update();

      cc->cd();
      h1->DrawCopy("same");
      EffFit->SetLineColor(2);
      EffFit->Draw("same");
      cc->Update();

      //sprintf(histoeff,"%s.gif",histoeff);
      sprintf(histoeff,"%s.eps",histoeff);
      
      char label[50];
      int hiptbin = 20;
      sprintf(label,"#epsilon (p_{T}=%2.1f GeV/c) = %f",h->GetBinCenter(hiptbin),h->GetBinContent(hiptbin));
      TLatex *tex = new TLatex(6.75,0.5,label);
      tex->SetTextSize(0.0362319);
      tex->SetLineWidth(2);
      cc->cd();
      tex->Draw("same");

      //TPaveStats *stat = (TPaveStats*)h->GetListOfFunctions()->Find("stats");
      //stat->SetOptFit(1011);

      cc->Update();
      cc->SaveAs(histoeff);
      //cc->Delete();
    }

  // if it's really bad throw the fit away ...
  if (chi2 > 4.)
    {
      cout << " <W> Really bad fit: Setting pars. to zero." << endl;
      for (int i=0 ; i<=EffFit->GetNpar(); i++) EffFit->SetParameter(i,0.);
    }

  return EffFit;
}

//_____________________________________________________________________________
TH1*
emcEfficiency::getCombinedEfficHisto(const char* cutname, const size_t icent)
{

  bool ok = check_cut(cutname);

  if ( !ok ) 
    {
      return 0;
    }

  std::string cut = cutname;
  TH1* h1 = getHistogram<TH1>(cut,khEfficiency_counts,icent);
  TH1* h2 = getHistogram<TH1>(cut,khEfficiency_fit,icent);

  if ( !h1 || !h2 )
    {
      return 0;
    }

  TH1* h = (TH1*)h1->Clone();
  char title[200];
  sprintf(title,"hEff_%s_comb_C%d.ps",cutname,icent);
  h->SetName(title);
  h->Reset();

  for ( size_t i = 1; i <= (size_t)h1->GetXaxis()->GetNbins(); ++i ) 
    {

      double c1 = h1->GetBinContent(i);
      double c2 = h2->GetBinContent(i);

      h->SetBinContent(i, 0.5*(c1+c2) );

      double max;
      double min;

      if ( c1 < c2 )
	{
	  min = c1;
	  max = c2;
	}
      else
	{
	  min = c2;
	  max = c1;
	}

      h->SetBinError(i, (max-min)/2.0);
   }
  return h;
}

//_____________________________________________________________________________
// Let's cheat in the fitting by setting the upper values
// of the Efficiency histo to the asymptotic "true" value
// ONLY "valid" for PPG014 pi0's between 8 and 12 GeV/c
// 
void
emcEfficiency::modifyEfficHisto( TH1 *h1 )
{

  if ( !h1 )
    {
      return ;
    }

  if ( fEffParticleCode == 17 ) return;

  cout << "<W> BE CAREFUL: Modifiying efficiency histo at high pT !!!!!" << endl;

  size_t bins = (size_t)h1->GetXaxis()->GetNbins();

  size_t p7GeVbin = h1->GetXaxis()->FindBin(7.0);
  double p7GeVEffval = h1->GetBinContent(p7GeVbin);
  double p7GeVEfferr = h1->GetBinError(p7GeVbin);

  size_t p6GeVbin = h1->GetXaxis()->FindBin(6.0);
  double p6GeVEffval = h1->GetBinContent(p6GeVbin);
  double p6GeVEfferr = h1->GetBinError(p6GeVbin);

  for ( size_t i = bins-4 ; i <= bins ; ++i ) 
    {
      if (i%2)
	{
	  h1->SetBinContent(i,p7GeVEffval);
	  h1->SetBinError(i,p7GeVEfferr);
	}
      else
	{
	  h1->SetBinContent(i,p6GeVEffval);
	  h1->SetBinError(i,p6GeVEfferr);
	}
   }

}

//_____________________________________________________________________________
bool 
emcEfficiency::check_cut(const char* cutname)
{

  // 1st check
  if ( fEfficiencyFileName.empty() )
    {
      std::cerr << "<E> No Efficiency file opened."
		<< " Try first readEfficiencyFile(\"myEfficFile.root\"); " 
		<< std::endl;
      return false ;
    }

  // 2nd check
  std::map<std::string,int>::const_iterator itr;
  std::string cut(cutname);

  itr = fCuts.find(cut);
  if ( itr == fCuts.end() )
    {
      std::cerr << "<E> I don't know cut \"" << cut << "\"" << std::endl;
      printCuts();
      return false;
    }

  return true;

}

//_____________________________________________________________________________
// print in binary format with leading zeros

void 
emcEfficiency::binary(int n) 
{
  const int maxNbits = 16; // 32.;
  for (int i=maxNbits; i>=0; i--) 
    {
      int bit = ((n >> i) & 1);
      cout << bit;
    }
}

