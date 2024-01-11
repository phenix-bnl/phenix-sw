//****************************************************************
// Efficiency Calculations in EMCAL
//        
// Copyright (C) PHENIX collaboration, 2001
//
// Authors: David D'ENTERRIA & G. MARTINEZ - SUBATECH, 2002
//
// Purpose: Reads embed+eval pi0 file and outputs a ROOT file with
//          efficiency histograms for all analysis cuts and for 
//          each centrality class
//
//****************************************************************

/*  To compile this macro simply do:

 > root

 .includepath $AFSHOME/install/include
 .includepath $OFFLINE_MAIN/include
 gSystem->Load("libpreco.so");
 gSystem->Load("libEG.so");
 .L EmcalEfficiencyAnalysisRun2.C++
 reading_from_embed(2500000,"/phenix/data06/enterria/year1_pi0_embed_files_May/all2.root") ;
 reading_from_embed(2500000,"~/year1dsts/last_year1_pi0_embed_files/merged_0_314_runs.root");

 reading_from_embed(2500000,"/phenix/data06/enterria/year1_pi0_embed_files_May_fake/merged_976_runs12XXX_fake.root");
// reading_from_embed(250000,"/phenix/data06/enterria/year1_pi0_embed_files_fake/pi0_embed_512_runs_fake.root");

 efficiency_all_cuts() ;

// To run this macro do:

 > root

 gSystem->Load("libpreco.so");
 gSystem->Load("libEG.so");
 gSystem->Load("~/afs/effic2/EmcalEfficiencyAnalysisRun2_C.so");
 reading_from_embed(250000,"/phenix/data06/enterria/year1_pi0_embed_files_fake/pi0_embed_512_runs.root") ;
 efficiency_all_cuts() ;

*/

// Root Classes
#include "TClonesArray.h"
#include "TF1.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TStyle.h"
#include "TLorentzVector.h"
#include "TNtuple.h"
#include "TParticle.h"
#include "TTree.h"
#include "TVector3.h"

#include "TRandom.h"

// Phenix classes
#include "dEmcGeaTrackWrapper.h"  
#include "dEmcGeaTrackClusterWrapper.h"
#include "dEmcGeaTrackCluster.h"
#include "dEmcGeaClusterTrack.h"
#include "dEmcGeaClusterTrackWrapper.h"
#include "mEmcGeometryModule.h"
#include "mEmcToolsModule.h"

// Phool classes
#include "PHCompositeNode.h"
#include "PHNodeIterator.h" 
#include "PHNodeReset.h"
#include "PHString.h"
#include "PHNodeIOManager.h"
#include "PHIODataNode.h"
#include "PHPointerListIterator.h"

#include "PHTable.hh"

template<class T>
class PHNodeHelper
{
public:

  static
  T* getTable(const char* tableName, PHCompositeNode* from);

  static
  T* getObject(const char* objectName, PHCompositeNode* from);

};


template<class T>
T*
PHNodeHelper<T>::getTable(const char* tableName, PHCompositeNode* from)
{
  assert(from!=0);
  PHNodeIterator iter(from);

  PHIODataNode<PHTable>* node = 
    static_cast<PHIODataNode<PHTable>*>(iter.findFirst("PHIODataNode",
						       tableName));

  T* rv=0;

  if ( node ) 
    {
      rv = static_cast<T*>(node->getData());
    }
  return rv;
}

template<class T>
T*
PHNodeHelper<T>::getObject(const char* objectName, PHCompositeNode* from)
{
  assert(from!=0);
  PHNodeIterator iter(from);

  PHIODataNode<PHObject>* node = 
    static_cast<PHIODataNode<PHObject>*>(iter.findFirst("PHIODataNode",
							objectName));

  T* rv=0;

  if ( node ) 
    {
      rv = static_cast<T*>(node->getData());
    }
  return rv;
}


// A few global variables:

static PHString OutputEfficFileName ;

static Int_t PDG_gam   =  22;
static Int_t PDG_elec  =  11;
static Int_t PDG_eplus = -11;
static Int_t PDG_pi0   = 111;
static Float_t pi0mass = 0.1349766 ;

static Int_t init_geo_done = 0 ;
static mEmcGeometryModule *EmcGeometryModule = 0;
static mEmcToolsModule* EmcTools = mEmcToolsModule::instance();

static TH2F *hfiducial_local_yz_geatrack = 0;
static TH2F *hfiducial_local_yz_geaclustertrack = 0;

static const Int_t CENTCLASSES = 11 ; // 0 [0-5], 1 [5-10], ..., 9 [70-80], 10 [80,92]

// those below are "tunable" global parameters ...

static Float_t pTmin = 0.;
static Float_t pTmax = 10.;
static Float_t pTbinsize = 0.5 ; // 0.5 GeV/c bins
static Int_t NumOfpTbins = (Int_t)((pTmax-pTmin)/pTbinsize);

static Float_t Minvmax = 0.6;
static Float_t Minvbinsize = 0.002 ; // 2 MeV/c bins
static Int_t nMinvbin = (Int_t)(Minvmax/Minvbinsize); 

static Float_t Min_Cluster_Energy = 0.05 ; // clusters below 50 MeV excluded

static Int_t NMAXSIMULCLUS = 200 ; // well above any expected number

//   evCent.setCentBins(0, 0, 5);
//   evCent.setCentBins(1, 5, 10);
//   evCent.setCentBins(2, 10, 15);
//   evCent.setCentBins(3, 15, 20);
//   evCent.setCentBins(4, 20, 30);
//   evCent.setCentBins(5, 30, 40);
//   evCent.setCentBins(6, 40, 50);
//   evCent.setCentBins(7, 50, 60);
//   evCent.setCentBins(8, 60, 70);
//   evCent.setCentBins(9, 70, 80);
//   evCent.setCentBins(10, 80, 100);

//=================================================================================
// Fiducial cuts 

Bool_t in_fiducial_area( const Int_t iTrack, const dEmcGeaTrackWrapper* dEmcGeaTrack,
			 const dEmcGeaClusterTrackWrapper* dEmcGeaClusterTrack )
{

  Float_t xyz[3];
  Float_t lx,ly,lz;
  Int_t arm;
  Int_t emc_geom_sector;

  if ( !init_geo_done ) 
    {  
      EmcGeometryModule = new mEmcGeometryModule ; 
      EmcGeometryModule->BuildGeometryPISA() ; // fiducial cuts in PISA geometry (as wrongly done in real data pi0 production)
      init_geo_done = 1;
    }

  if ( dEmcGeaTrack && !(dEmcGeaClusterTrack) )
    {
      xyz[0] = dEmcGeaTrack->get_impxyz(0,iTrack) ;      
      xyz[1] = dEmcGeaTrack->get_impxyz(1,iTrack) ;      
      xyz[2] = dEmcGeaTrack->get_impxyz(2,iTrack) ;
      emc_geom_sector = 0;
    }
  else if ( !(dEmcGeaTrack) && dEmcGeaClusterTrack )
    {
      xyz[0] = dEmcGeaClusterTrack->get_measxyz(0,iTrack) ;      
      xyz[1] = dEmcGeaClusterTrack->get_measxyz(1,iTrack) ;      
      xyz[2] = dEmcGeaClusterTrack->get_measxyz(2,iTrack) ;
      arm = dEmcGeaClusterTrack->get_arm(iTrack);
      emc_geom_sector = dEmcGeaClusterTrack->get_sector(iTrack);
      if (arm == 1) emc_geom_sector = 7 - emc_geom_sector;
    }
  else
    {
      cout << "<E> I don't know how to calculate impxyz for this (these 2 ?) object(s) !!! " << endl
	   << " in_fiducial_area == false !!!" << endl ;
      return false;
    }

  // Fiducial cuts Run-2 as in  KCluster::passFiducialCuts(Fiducial *fid)
  //  if ( lx < 20. || lx > 377. || ly < 10. || ly > 189. ) 
  // Mind that global_z direction corresponds to local_x direction and viceversa !!

  EmcGeometryModule->GlobalToLocal(xyz[0],xyz[1],xyz[2],emc_geom_sector,lx,ly,lz);

  // Each sector in its *local* coordinates ocupies an area between: 
  // y_local ~ [0, 200] and z_global = x_local ~ [0, 400]
  if ( lx>=20 && lx<=377 && ly>=10 && ly<=189 )
    {
      if (dEmcGeaTrack) hfiducial_local_yz_geatrack->Fill(lx,ly);
      else hfiducial_local_yz_geaclustertrack->Fill(lx,ly);

      return true ;
    }

  return false;

}

//=============================================================================
//

void Fill_Minv_pT_bidim_histo( TH2F *hRecoParticleMinvPtWhatever, 
			       const Float_t inv_mass_pair_reco, 
			       const Float_t pt_pair_reco, 
			       const Float_t hagWeight = 1. ,
			       const Float_t error = 0. )
{

  Float_t binMinv, binpT, errorWeight;

  hRecoParticleMinvPtWhatever->Fill( inv_mass_pair_reco, pt_pair_reco, hagWeight );
  binMinv = hRecoParticleMinvPtWhatever->GetXaxis()->FindBin( inv_mass_pair_reco );
  binpT = hRecoParticleMinvPtWhatever->GetYaxis()->FindBin( pt_pair_reco);
  if ( error )
    {
      errorWeight = error; //TMath::Sqrt(pow(hRecoParticleMinvPtWhatever->GetBinError(binMinv,binpT),2)); 
    }
  else
    {
      errorWeight = TMath::Sqrt(pow(hRecoParticleMinvPtWhatever->GetBinError(binMinv,binpT),2) + hagWeight*hagWeight); 
    }
  hRecoParticleMinvPtWhatever->SetBinError( binMinv, binpT, errorWeight );

}

//=================================================================================
// pi0 reconstruction:
// From dEmcGeaTrack (Input pi0) and dEmcGeaClusterTrack (merged reconstructed cluster)
//

Int_t pi0_reconstruction( dEmcGeaTrackWrapper* dEmcGeaTrack,
			  dEmcGeaClusterTrackWrapper* dEmcGeaClusterTrack, 
			  Int_t eventNumber, Int_t maxEvents)
{ 

  gROOT->SetStyle("Plain");
  gStyle->SetHistFillStyle(1001); // solid
  gStyle->SetHistFillColor(2);   // red
  gROOT->ForceStyle();

  static TFile * ReconstructionEfficiencyFile = 0; // Output file

  // ----------------------------------------------------------
  // 1) Histograms booking
  //
  static TH1F *hSimulPairsMinvAllPairs = 0;
  static TH2F *hSimulPi0MinvPt = 0;
  static TH2F *hSimulPairsMinvPt = 0;

  static TH1F *hRecoParticleMinvAllPairs = 0;
  static TH2F *hRecoParticleMinvPt = 0;
  static TH2F *hRecoParticleMinvPtFiduCut = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymCut = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymChi2Cut = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymTOFCut = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymChi2TOFCut = 0;
  static TH2F *hRecoParticleMinvPtFiduAsym2Chi2TOFCut = 0;
  static TH2F *hRecoParticleMinvPtTruePhotonCut = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymTruePhotonCut = 0;

  static TH2F *hRecoParticleMinvPtCent_[CENTCLASSES];
  static TH2F *hRecoParticleMinvPtFiduCutCent_[CENTCLASSES];
  static TH2F *hRecoParticleMinvPtFiduAsymCutCent_[CENTCLASSES];
  static TH2F *hRecoParticleMinvPtFiduAsymChi2CutCent_[CENTCLASSES];
  static TH2F *hRecoParticleMinvPtFiduAsymTOFCutCent_[CENTCLASSES];
  static TH2F *hRecoParticleMinvPtFiduAsymChi2TOFCutCent_[CENTCLASSES];
  static TH2F *hRecoParticleMinvPtFiduAsym2Chi2TOFCutCent_[CENTCLASSES];
  static TH2F *hRecoParticleMinvPtTruePhotonCutCent_[CENTCLASSES];
  static TH2F *hRecoParticleMinvPtFiduAsymTruePhotonCutCent_[CENTCLASSES];

  static TH1F *hArm    = 0;
  static TH1F *hSector = 0;
  static TH1F *hMeasx  = 0;
  static TH1F *hMeasy  = 0;
  static TH1F *hMeasz  = 0;
  static TH2F *hMeasyz_w = 0;
  static TH2F *hMeasyz_e = 0;
  static TH2F *hMeasyz_w_out = 0;
  static TH2F *hMeasyz_e_out = 0;
  static TH1F *hMease = 0;
  static TH1F *hEcore = 0;
  static TH1F *hTof   = 0;
  static TH1F *hProbPhot = 0;
  static TH1F *hChi2  = 0;  

  static TH1F *hCentClass = 0 ;
  static TH1F *hTotalClusterMul = 0 ;
  static TH1F *hEmbedClusterMul = 0 ;

  static TH1F *hEmbedClusterMulCent_[CENTCLASSES] ;
  
  static TH1F *hSimulSecondaryParticlePt = 0;
  static TH1F *hRecoSecondaryParticlePt = 0;

  static TH2F *hRecoParticleEnergyAsym = 0;
  static TH2F *hRecoParticleEnergyAsymPi0 = 0;

  if (eventNumber==0) {

    delete hSimulPairsMinvAllPairs;
    delete hSimulPi0MinvPt;
    delete hSimulPairsMinvPt;
    delete hRecoParticleMinvAllPairs;
    delete hRecoParticleMinvPt;
    delete hRecoParticleMinvPtFiduCut;
    delete hRecoParticleMinvPtFiduAsymCut;
    delete hRecoParticleMinvPtFiduAsymChi2Cut;
    delete hRecoParticleMinvPtFiduAsymTOFCut;
    delete hRecoParticleMinvPtFiduAsymChi2TOFCut;
    delete hRecoParticleMinvPtFiduAsym2Chi2TOFCut;
    delete hRecoParticleMinvPtTruePhotonCut;
    delete hRecoParticleMinvPtFiduAsymTruePhotonCut;

    for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
      {
	delete hRecoParticleMinvPtCent_[iCent];
	delete hRecoParticleMinvPtFiduCutCent_[iCent];
	delete hRecoParticleMinvPtFiduAsymCutCent_[iCent];
	delete hRecoParticleMinvPtFiduAsymChi2CutCent_[iCent];
	delete hRecoParticleMinvPtFiduAsymTOFCutCent_[iCent];
	delete hRecoParticleMinvPtFiduAsymChi2TOFCutCent_[iCent];
	delete hRecoParticleMinvPtFiduAsym2Chi2TOFCutCent_[iCent];
	delete hRecoParticleMinvPtTruePhotonCutCent_[iCent];
	delete hRecoParticleMinvPtFiduAsymTruePhotonCutCent_[iCent];
	delete hEmbedClusterMulCent_[iCent];
      }

    delete hArm;
    delete hSector;
    delete hMeasx; 
    delete hMeasy; 
    delete hMeasz;
    delete hMeasyz_w; 
    delete hMeasyz_e; 
    delete hMeasyz_w_out; 
    delete hMeasyz_e_out; 
    delete hMease;
    delete hEcore;
    delete hTof;
    delete hProbPhot;
    delete hChi2;

    delete hfiducial_local_yz_geatrack ;
    delete hfiducial_local_yz_geaclustertrack ;

    delete hCentClass ;
    delete hTotalClusterMul ;
    delete hEmbedClusterMul ;
    for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
      {
	delete hEmbedClusterMulCent_[iCent] ;
      }

    delete hSimulSecondaryParticlePt;
    delete hRecoSecondaryParticlePt;

    delete hRecoParticleEnergyAsym;
    delete hRecoParticleEnergyAsymPi0;

    ReconstructionEfficiencyFile = new TFile(OutputEfficFileName.getString(),"recreate"); 

    cout << gDirectory->GetPath() << endl;

    hSimulPairsMinvAllPairs = new TH1F("hSimulPairsMinvAllPairs","hSimulPairsMinvAllPairs",2*nMinvbin,0.,Minvmax);
    hSimulPi0MinvPt = new TH2F("hSimulPi0MinvPt","hSimulPi0MinvPt",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hSimulPairsMinvPt = new TH2F("hSimulPairsMinvPt","hSimulPairsMinvPt",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);

    hRecoParticleMinvAllPairs = new TH1F("hRecoParticleMinvAllPairs","hRecoParticleMinvAllPairs",2*nMinvbin,0.,Minvmax );
    hRecoParticleMinvPt = new TH2F("hRecoParticleMinvPt","hRecoParticleMinvPt",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduCut  = 
      new TH2F("hRecoParticleMinvPtFiduCut","hRecoParticleMinvPtFiduCut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymCut  = 
      new TH2F("hRecoParticleMinvPtFiduAsymCut","hRecoParticleMinvPtFiduAsymCut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymChi2Cut  = 
      new TH2F("hRecoParticleMinvPtFiduAsymChi2Cut","hRecoParticleMinvPtFiduAsymChi2Cut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymTOFCut  = 
      new TH2F("hRecoParticleMinvPtFiduAsymTOFCut","hRecoParticleMinvPtFiduAsymTOFCut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymChi2TOFCut  = 
      new TH2F("hRecoParticleMinvPtFiduAsymChi2TOFCut","hRecoParticleMinvPtFiduAsymChi2TOFCut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsym2Chi2TOFCut  = 
      new TH2F("hRecoParticleMinvPtFiduAsym2Chi2TOFCut","hRecoParticleMinvPtFiduAsym2Chi2TOFCut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtTruePhotonCut   = 
      new TH2F("hRecoParticleMinvPtTruePhotonCut","hRecoParticleMinvPtTruePhotonCut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymTruePhotonCut = 
      new TH2F("hRecoParticleMinvPtFiduAsymTruePhotonCut","hRecoParticleMinvPtFiduAsymTruePhotonCut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);

    char title[50];
    for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
      {
	sprintf(title,"hRecoParticleMinvPtCent_%i",iCent);
	hRecoParticleMinvPtCent_[iCent] = new TH2F(title,title,nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
	sprintf(title,"hRecoParticleMinvPtFiduCutCent_%i",iCent);
	hRecoParticleMinvPtFiduCutCent_[iCent] = new TH2F(title,title,nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
	sprintf(title,"hRecoParticleMinvPtFiduAsymCutCent_%i",iCent);
	hRecoParticleMinvPtFiduAsymCutCent_[iCent] = new TH2F(title,title,nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
	sprintf(title,"hRecoParticleMinvPtFiduAsymChi2CutCent_%i",iCent);
	hRecoParticleMinvPtFiduAsymChi2CutCent_[iCent] = new TH2F(title,title,nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
	sprintf(title,"hRecoParticleMinvPtFiduAsymTOFCutCent_%i",iCent);
	hRecoParticleMinvPtFiduAsymTOFCutCent_[iCent]  = new TH2F(title,title,nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
	sprintf(title,"hRecoParticleMinvPtFiduAsymChi2TOFCutCent_%i",iCent);
	hRecoParticleMinvPtFiduAsymChi2TOFCutCent_[iCent] = new TH2F(title,title,nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
	sprintf(title,"hRecoParticleMinvPtFiduAsym2Chi2TOFCutCent_%i",iCent);
	hRecoParticleMinvPtFiduAsym2Chi2TOFCutCent_[iCent] = new TH2F(title,title,nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
	sprintf(title,"hRecoParticleMinvPtTruePhotonCutCent_%i",iCent);
	hRecoParticleMinvPtTruePhotonCutCent_[iCent] = new TH2F(title,title,nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
	sprintf(title,"hRecoParticleMinvPtFiduAsymTruePhotonCutCent_%i",iCent);
	hRecoParticleMinvPtFiduAsymTruePhotonCutCent_[iCent] = new TH2F(title,title,nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
      }

    hArm    = new TH1F("hArm","hArm",20,0,3);
    hSector = new TH1F("hSector","hSector",20,0,10);
    hMeasx  = new TH1F("hMeasx","hMeasx",1000,-1000,1000);
    hMeasy  = new TH1F("hMeasy","hMeasy",1000,-500,500);
    hMeasz  = new TH1F("hMeasz","hMeasz",1000,-500,500);
    hMeasyz_w = new TH2F("hMeasyz_w","hMeasyz_west",1000,-500,500,1000,-500,500);
    hMeasyz_e = new TH2F("hMeasyz_e","hMeasyz_east",1000,-500,500,1000,-500,500);
    hMeasyz_w_out = new TH2F("hMeasyz_w_out","hMeasyz_west_out",1000,-500,500,1000,-500,500);
    hMeasyz_e_out = new TH2F("hMeasyz_e_out","hMeasyz_east_out",1000,-500,500,1000,-500,500);
    hMease  = new TH1F("hMease","hMease",500,0,pTmax);
    hEcore  = new TH1F("hEcore","hEcore",500,0,pTmax);
    hTof    = new TH1F("hTof","hTof",1000,-100,100);
    hProbPhot = new TH1F("hProbPhot","hProbPhot",100,0,1);
    hChi2   = new TH1F("hChi2","hChi2",200,0,50);

    hfiducial_local_yz_geatrack = new TH2F("hfiducial_local_yz_geatrack","hfiducial_local_yz0_geatrack",1000,-500.,500.,1000,-500.,500);
    hfiducial_local_yz_geaclustertrack = new TH2F("hfiducial_local_yz_geaclustertrack","hfiducial_local_yz0_geaclustertrack",1000,-500.,500.,1000,-500.,500);

    hCentClass = new TH1F("hCentClass","hCentClass",101,-0.5,100.5);
    hTotalClusterMul = new TH1F("hTotalClusterMul","hTotalClusterMul",400,-0.5,399.5);
    hEmbedClusterMul = new TH1F("hEmbedClusterMul","hEmbedClusterMul",50,-0.5,49.5);

    for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
      {
	sprintf(title,"hEmbedClusterMulCent_%i",iCent);
	hEmbedClusterMulCent_[iCent] = new TH1F(title,title,50,-0.5,49.5);
      }

    hSimulSecondaryParticlePt = new TH1F("hSimulSecondaryParticlePt","hSimulSecondaryParticlePt",40,0,pTmax);
    hRecoSecondaryParticlePt = new TH1F("hRecoSecondaryParticlePt","hRecoSecondaryParticlePt",40,0,pTmax);

    hRecoParticleEnergyAsym = new TH2F("hRecoParticleEnergyAsym","hRecoParticleEnergyAsym",NumOfpTbins,pTmin,pTmax,200,0.,1.0);
    hRecoParticleEnergyAsymPi0 = new TH2F("hRecoParticleEnergyAsymPi0","hRecoParticleEnergyAsymPi0",NumOfpTbins,pTmin,pTmax,200,0.,1.0);

    hSimulPairsMinvAllPairs->Sumw2();
    hSimulPi0MinvPt->Sumw2();
    hSimulPairsMinvPt->Sumw2();
    hRecoParticleMinvAllPairs->Sumw2();
    hRecoParticleMinvPt->Sumw2();
    hRecoParticleMinvPtFiduCut->Sumw2();
    hRecoParticleMinvPtFiduAsymCut->Sumw2();
    hRecoParticleMinvPtFiduAsymChi2Cut->Sumw2();
    hRecoParticleMinvPtFiduAsymTOFCut->Sumw2();
    hRecoParticleMinvPtFiduAsymChi2TOFCut->Sumw2();
    hRecoParticleMinvPtFiduAsym2Chi2TOFCut->Sumw2();
    hRecoParticleMinvPtTruePhotonCut->Sumw2();
    hRecoParticleMinvPtFiduAsymTruePhotonCut->Sumw2();

    for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
      {
	hRecoParticleMinvPtCent_[iCent]->Sumw2();
	hRecoParticleMinvPtFiduCutCent_[iCent]->Sumw2();
	hRecoParticleMinvPtFiduAsymCutCent_[iCent]->Sumw2();
	hRecoParticleMinvPtFiduAsymChi2CutCent_[iCent]->Sumw2();
	hRecoParticleMinvPtFiduAsymTOFCutCent_[iCent]->Sumw2();
	hRecoParticleMinvPtFiduAsymChi2TOFCutCent_[iCent]->Sumw2();
	hRecoParticleMinvPtFiduAsym2Chi2TOFCutCent_[iCent]->Sumw2();
	hRecoParticleMinvPtTruePhotonCutCent_[iCent]->Sumw2();
	hRecoParticleMinvPtFiduAsymTruePhotonCutCent_[iCent]->Sumw2();
      }

    hMeasx->Sumw2(); 
    hMeasy->Sumw2(); 
    hMeasz->Sumw2();
    hMeasyz_w->Sumw2();
    hMeasyz_e->Sumw2();
    hMeasyz_w_out->Sumw2();
    hMeasyz_e_out->Sumw2();
    hMease->Sumw2();
    hEcore->Sumw2();
    hTof->Sumw2();
    hProbPhot->Sumw2();
    hChi2->Sumw2();

    hSimulSecondaryParticlePt->Sumw2();
    hRecoSecondaryParticlePt->Sumw2();

  }

  // TParticles

  static TClonesArray *ListOfSimulParticles   = new TClonesArray("TParticle",NMAXSIMULCLUS,1);
  static TClonesArray *ListOfClusterParticles = new TClonesArray("TParticle",NMAXSIMULCLUS,1);
  
  TParticle *SimulPi0  = 0 ;
  TParticle *Particle  = 0 ;
  TParticle *Particle2 = 0;

  TLorentzVector momentum1(0.,0.,0.,0.); 
  TLorentzVector momentum2(0.,0.,0.,0.); 

  // Vars.

  Int_t centClass = 0;
  Int_t pid = 0 ;
  Float_t partmass = 0. ;
  Float_t Pi0pT = 0. ; 
  Float_t Pi0Zvtx = 0. ; 
  Float_t hagWeight = 1.;
  Float_t hagWeight2 = 1.;

  // Cent-dependent Hagedorn fit parameters for pi0 Au+Au @ 200A GeV
  // p0hag_min_bias = 1.66 ; nhag_min_bias = -10.2
  Float_t p0hag_[CENTCLASSES] = {3.40,3.40,3.02,2.53,2.52,2.33,2.48,2.77,1.92,2.62};   // Param. Hagedorn fit Au+Au->pi0
  Float_t nhag_[CENTCLASSES]  = {-14.6,-14.6,-13.3,-11.9,-11.8,-11.1,-11.4,-12.0,-9.9,-11.6};  // Param. Hagedorn fit Au+Au->pi0 
  Int_t binMinv = 0;
  Float_t errorWeight = 0. ;
 
  // ----------------------------------------------------------
  // 2) Loop on simulated Tracks
  // For all Simulated tracks entering EMCal in a given event 
  // we fill ListOfSimulParticles

  Int_t NumOfTracks = (Int_t) dEmcGeaTrack->RowCount();
  Int_t iTracks =0;
  Int_t j=0;
  Int_t CounterOfSimulParticles=0;  
  Bool_t MONO_PT_BIN = false;

  for( iTracks=0; iTracks<NumOfTracks; iTracks++ )  // loop over all tracks in the history
   {     

     // Get vtx and momentum of the track. We need it to check if it goes towards one of the PbSc sectors
     Float_t vtx_vec[] = {dEmcGeaTrack->get_xyz(0,iTracks),dEmcGeaTrack->get_xyz(1,iTracks),dEmcGeaTrack->get_xyz(2,iTracks)};
     Float_t p_vec[] = {dEmcGeaTrack->get_pxyz(0,iTracks),dEmcGeaTrack->get_pxyz(1,iTracks),dEmcGeaTrack->get_pxyz(2,iTracks)};

     // 2.1) pT weight:
     // Look for the primary pi0 just to
     // determine the power-law weight of its associated pT
     if ( ( dEmcGeaTrack->get_idparent(iTracks) == 0 )  &&   // it's a primary (if negative: PID*ancestry level)
	  ( dEmcGeaTrack->get_pid(iTracks) == 7 )       &&   // it's a pion 
	  //( mEmcToolsModule::HitInPbSc(vtx_vec,p_vec) ) &&   // it heads one of PbSc sectors
	  ( !Pi0pT ) ) // Once we got it (from 1st ancestry level) do not get it again (otherwise too many entries in the ref. Minv-pT bidim !)
       {
	 SimulPi0 = new TParticle(PDG_pi0,0,0,0,0,0,dEmcGeaTrack->get_pxyz(0,iTracks),dEmcGeaTrack->get_pxyz(1,iTracks), 
				  dEmcGeaTrack->get_pxyz(2,iTracks),
				  //TMath::Sqrt(pow(dEmcGeaTrack->get_ptot(iTracks),2)+pow(pi0mass,2)),0.,0.,0.,0.);
				  //dEmcGeaTrack->get_ptot(iTracks),0.,0.,0.,0.);
	                          dEmcGeaTrack->get_ekin(iTracks)+pi0mass,0.,0.,0.,0.); 
	 Pi0pT = SimulPi0->Pt(); 
	 Pi0Zvtx = dEmcGeaTrack->get_xyz(2,iTracks); 
	 // REAL-EVENT CENTRALITY CLASS: "input" field of dEmcGeaTrack contains now the real event centrality class !
	 centClass = dEmcGeaTrack->get_input(0); // the same centClass info is stored for all tracks 0,1,...

	 // one weight per cent-class

         //hagWeight = TMath::Power(Pi0pT,-7.02);  // Weight from measured run-01 pi0 spectrum Au+Au @ 200A GeV
	 // Cent-dependent Weight from measured run-01 pi0 Au+Au @ 200A GeV
	 hagWeight = TMath::Power((Pi0pT+p0hag_[centClass])/p0hag_[centClass],nhag_[centClass]);
	 //hagWeight = 1.;
	 hagWeight2 = hagWeight*hagWeight;

	 // These will be our 1st-REFERENCE minv-pT bi-dim histo for efficiencies
	 Fill_Minv_pT_bidim_histo( hSimulPi0MinvPt, pi0mass, Pi0pT, hagWeight );

	 delete SimulPi0; 
      } 

     // "MONOENERGETIC" ANALYSIS
     if ( ( Pi0pT >= 1.5) && ( Pi0pT < 2.0) ) MONO_PT_BIN = true;
     else MONO_PT_BIN = false;

     // 2.2) Fill ListOfSimulParticles:
     // Look for all simulated particles (pi0 decay products)
     // actually entering in EMCAL (ancestry == 1) and heading one of PbSc sectors
     if  ( ( dEmcGeaTrack->get_anclvl(iTracks) == 1 )  )//&&
	   //  ( mEmcToolsModule::HitInPbSc(vtx_vec,p_vec) ) )   // it heads one of PbSc sectors
         //( in_fiducial_area(iTracks,dEmcGeaTrack,0) ) ) // Fiducial cuts Run-2
       {
	 if      ( dEmcGeaTrack->get_pid(iTracks)==1 ) { pid = PDG_gam;   partmass = 0. ;}
	 else if ( dEmcGeaTrack->get_pid(iTracks)==2 ) { pid = PDG_eplus; partmass = 0.000511 ;}
	 else if ( dEmcGeaTrack->get_pid(iTracks)==3 ) { pid = PDG_elec;  partmass = 0.000511 ;}
	 else  { pid = -999 ; partmass = 0. ;}
	 
	 Particle = new( (*ListOfSimulParticles)[CounterOfSimulParticles] )  TParticle(pid,0,0,0,0,0,
			    dEmcGeaTrack->get_pxyz(0,iTracks),dEmcGeaTrack->get_pxyz(1,iTracks), 
			    dEmcGeaTrack->get_pxyz(2,iTracks),
			  //dEmcGeaTrack->get_ptot(iTracks),0.,0.,0.,0.);
		            dEmcGeaTrack->get_ekin(iTracks)+partmass,0.,0.,0.,0.);
	 	
	 // To avoid double counting of particles in the different ancestry levels
	 Particle->Momentum(momentum1);
	 for(j=0; j<CounterOfSimulParticles; j++)
	   {
	     Particle2 = ((TParticle *) (*ListOfSimulParticles)[j]);
	     Particle2->Momentum(momentum2);
	     if ( momentum1 == momentum2 )
	       {
		 ListOfSimulParticles->RemoveAt(j);
		 Particle = 0;
	       }
	   }
	 if (Particle) CounterOfSimulParticles++;
       } // End fill ListOfSimulParticles

   }  // End of loop over all Simul Tracks
 
  Int_t iSimulParticles=0;
  Int_t iSimulParticles2=0;

  // ----------------------------------------------------------
  // 3) Loop on ListOfSimulParticles (all Simul particles entering EMCal)
  //    Weight (power-law) pT, Calculate inv. mass, and Fill histos

  for( iSimulParticles=0; iSimulParticles<CounterOfSimulParticles; iSimulParticles++ ) 
    {
      Particle = ((TParticle *) (*ListOfSimulParticles)[iSimulParticles]);

      Particle->Momentum(momentum1);
      hSimulSecondaryParticlePt->Fill( momentum1.Pt(), hagWeight );

      // let's look for its companion(s) and fill inv. mass histos
      for( iSimulParticles2=iSimulParticles+1; iSimulParticles2<CounterOfSimulParticles; iSimulParticles2++) 
	{
	  Particle2 = ((TParticle *) (*ListOfSimulParticles)[iSimulParticles2]);

	  Particle2->Momentum(momentum2);
	  momentum2+=momentum1;
	  

	  hSimulPairsMinvAllPairs->Fill( momentum2.M() , hagWeight );
	  binMinv = hSimulPairsMinvAllPairs->FindBin( momentum2.M() );
	  errorWeight = TMath::Sqrt(pow(hSimulPairsMinvAllPairs->GetBinError(binMinv),2)+hagWeight2); 
	  hSimulPairsMinvAllPairs->SetBinError( binMinv, errorWeight );
	  
	  // These will be our 2nd REFERENCE minv-pT bi-dim histo for efficiencies !
	  // The momentum of the track is the *original* one from geatrack
	  // (and Minv is calculated from there ...)
	  Fill_Minv_pT_bidim_histo( hSimulPairsMinvPt, momentum2.M(), momentum2.Pt(), hagWeight );
	}
    }

  //if (MONO_PT_BIN)
  // {

  //___________________________________________________________
  // ----------------------------------------------------------
  // Now let's go with the embedded "REAL+SIMUL" clusters
  //___________________________________________________________
  // ----------------------------------------------------------

  // ----------------------------------------------------------
  // 4) Loop on EMCal clusters
  // For all clusters measured in EMCal
  // Fill ListOfClusterParticles

  Int_t NumOfClusters = (Int_t) dEmcGeaClusterTrack->RowCount();
  Int_t iClusters = 0;
  Int_t CounterOfClusters = 0;
  Int_t CounterOfClustersCent_[CENTCLASSES];
  Float_t Distance = 0.;
  Float_t x, y, z ;
  Float_t px, py, pz ;
  Float_t reco_vtx = 0. ;
  Int_t in_fiducial = 0;
  Int_t dead = 0;
  Int_t warn = 0;

  // NOTE: "charged" field contains now the real evt. multiplicity !
  Int_t real_evt_mult = dEmcGeaClusterTrack->get_charged(0); // the same mult. info is stored for all tracks 0,1,...

  Float_t flight_time = 0.0 ; // TOF already centered at 0.0 in Run-2

  for( iClusters=0; iClusters<NumOfClusters; iClusters++ ) 
    {

      // PbSc CLUSTERS WITH A CONTRIBUTED GEANT TRACK (should be all of them now in the embed+eval file ...)
      if ( ( dEmcGeaClusterTrack->get_trkno(0,iClusters) )     )//   &&
	   //( dEmcGeaClusterTrack->get_type(iClusters) == 1 ) )//&& // PbSc only
	 //( in_fiducial_area(iClusters,0,dEmcGeaClusterTrack) ) )   // Fiducial cuts Run-2
	{
	  
	  // Fill general EMCal cluster histograms
	  hArm->Fill(dEmcGeaClusterTrack->get_arm(iClusters));
	  hSector->Fill(dEmcGeaClusterTrack->get_sector(iClusters));
	  hMeasx->Fill(dEmcGeaClusterTrack->get_measxyz(0,iClusters));
	  hMeasy->Fill(dEmcGeaClusterTrack->get_measxyz(1,iClusters));
	  hMeasz->Fill(dEmcGeaClusterTrack->get_measxyz(2,iClusters));
	  if (dEmcGeaClusterTrack->get_measxyz(0,iClusters)<0.) // East 
	    hMeasyz_e->Fill(dEmcGeaClusterTrack->get_measxyz(2,iClusters),dEmcGeaClusterTrack->get_measxyz(1,iClusters));
	  if (dEmcGeaClusterTrack->get_measxyz(0,iClusters)>0.) // West
	    hMeasyz_w->Fill(dEmcGeaClusterTrack->get_measxyz(2,iClusters),dEmcGeaClusterTrack->get_measxyz(1,iClusters));
	  hMease->Fill(dEmcGeaClusterTrack->get_mease(iClusters));
	  hEcore->Fill(dEmcGeaClusterTrack->get_ecore(iClusters));
	  hTof->Fill( dEmcGeaClusterTrack->get_tof(iClusters) - flight_time );
	  hProbPhot->Fill(dEmcGeaClusterTrack->get_prob_photon_sh(iClusters));
	  hChi2->Fill(dEmcGeaClusterTrack->get_chi2_sh(iClusters));
	  
	  x = dEmcGeaClusterTrack->get_measxyz(0,iClusters);
	  y = dEmcGeaClusterTrack->get_measxyz(1,iClusters);
	  //reco_vtx = dEmcGeaClusterTrack->get_vertex(2,iClusters);
	  if (iClusters==0) reco_vtx =  Pi0Zvtx+((0.5-gRandom->Rndm())*10.);
	  z = dEmcGeaClusterTrack->get_measxyz(2,iClusters) - reco_vtx; // correction for z_vtx position	  
	  //cout << " vtx_z simul: " << Pi0Zvtx << " vtx_z real (or simul randomized): " <<  reco_vtx << endl ;
	  Distance = TMath::Sqrt( x*x + y*y + z*z );
	  px = dEmcGeaClusterTrack->get_ecore(iClusters)*x/Distance ;
	  py = dEmcGeaClusterTrack->get_ecore(iClusters)*y/Distance ;
	  pz = dEmcGeaClusterTrack->get_ecore(iClusters)*z/Distance ;

	  // PID of the dominant GEANT contributor to the cluster
	  if      ( dEmcGeaClusterTrack->get_pid(0,iClusters)==1 ) pid = PDG_gam;
	  else if ( dEmcGeaClusterTrack->get_pid(0,iClusters)==2 ) pid = PDG_eplus;
	  else if ( dEmcGeaClusterTrack->get_pid(0,iClusters)==3 ) pid = PDG_elec;
	  else  pid = -999 ; // unknown
	  
	  // Fiducial cuts Run-2
	  if  ( in_fiducial_area(iClusters,0,dEmcGeaClusterTrack) )
	    {
	      in_fiducial = 1;
	    }
	  else
	    {
	      in_fiducial = 0;
	      if (dEmcGeaClusterTrack->get_measxyz(0,iClusters)<0.) // East
		hMeasyz_e_out->Fill(dEmcGeaClusterTrack->get_measxyz(2,iClusters),dEmcGeaClusterTrack->get_measxyz(1,iClusters));
	      if (dEmcGeaClusterTrack->get_measxyz(0,iClusters)>0.) // West
		hMeasyz_w_out->Fill(dEmcGeaClusterTrack->get_measxyz(2,iClusters),dEmcGeaClusterTrack->get_measxyz(1,iClusters));
	    }

	  dead = (Int_t)dEmcGeaClusterTrack->get_pc3proj(0,iClusters); // Float_t stored dead-map (21+9 bits int)
	  warn = (Int_t)dEmcGeaClusterTrack->get_pc3proj(1,iClusters); // Float_t stored warn-map (21+9 bits int)
	    //(Int_t)*(&dEmcGeaClusterTrack->get_pc3proj(0,iClusters));
	    //(Int_t)*(&dEmcGeaClusterTrack->get_pc3proj(1,iClusters));
	  	  
	  // Fill List of Clusters
	  Particle = new( (*ListOfClusterParticles)[CounterOfClusters] )  
	    TParticle(pid,
		      in_fiducial,// Status field will store in_fiducial
		      dead, // "Mother1" stores dead-map
		      warn, // "Mother2" stores warn-map
		      0,0,px, py, pz,           
		      dEmcGeaClusterTrack->get_ecore(iClusters),0.,0.,
		      dEmcGeaClusterTrack->get_chi2_sh(iClusters),   // Vz (vtx-z) field stores chi2
		      dEmcGeaClusterTrack->get_tof(iClusters));

	  //if (dead*warn) cout << "dead: " << dead  << " warn: " << warn << endl;
	  
	  CounterOfClusters++;

	  for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
	    { 
	      if ( centClass == iCent ) CounterOfClustersCent_[iCent]++; 
	    }
	  
	} // if have some GEANT track contributor and head west
      else if ( !(dEmcGeaClusterTrack->get_trkno(0,iClusters)) )
	{
	  cout << "<W> No simulated track found in this cluster !!" << endl;
	}
    }  // loop on all EMCal clusters
  
  hCentClass->Fill((Float_t)centClass);

  hTotalClusterMul->Fill(real_evt_mult);
  hEmbedClusterMul->Fill(CounterOfClusters);
  //hEmbedClusterMulCent_[iCent]->Fill(CounterOfClustersCent_[iCent]);

  //hTotalClusterMul->Fill(NumOfClusters);

  Int_t iClusters2 = 0 ;
  Float_t ReCal = 1. ;
  Float_t asymmetry = 0.;
  Float_t inv_mass_pair_reco ;
  Float_t pt_pair_reco ;
  Bool_t CENT_[CENTCLASSES] ;
  for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
    { 
      CENT_[iCent] = false;
    }
  Bool_t FIDUCUT = false;
  Bool_t ASYMCUT = false;
  Bool_t ASYMCUT2

 = false;
  Bool_t DEADWARNCUT = false;
  Bool_t CHI2CUT = false;
  Bool_t TOFCUT  = false;
  Bool_t TRUEPHOTON = false;

  // ----------------------------------------------------------
  // 5) Loop on ListOfClusterParticles (all reconstructed clusters in EMCal)
  //    Calculate inv. mass and fill histos: hRecoParticleMinvPtWhateverCut

  for( iClusters=0; iClusters<CounterOfClusters; iClusters++) 
    {
      momentum1.Delete();
      Particle = ((TParticle *) (*ListOfClusterParticles)[iClusters]);

      Particle->Momentum(momentum1);

      // let's look for its companion(s) and fill inv. mass histos
      for( iClusters2=iClusters+1; iClusters2<CounterOfClusters; iClusters2++ ) 
	{
	  momentum1.Delete();
	  Particle2 = ((TParticle *) (*ListOfClusterParticles)[iClusters2]);

	  Particle2->Momentum(momentum2);	  
	  asymmetry = TMath::Abs( momentum1.Energy()-momentum2.Energy() ) / ( momentum1.Energy()+momentum2.Energy() );
	  momentum2+=momentum1;
	  
	  // Let's recalibrate the pT to recenter the reconstructed peak to its nominal position
	  // The recalibration factors are obtained from a Minv analysis (below) of "fake" embedded events
	  if ( (momentum2.Pt()>=0.5) && (momentum2.Pt()<=5.0) )
	    {    
	      ReCal = 1.0;
// 	      ReCal = 9.4734e-01 //9.54364e-01                
// 		    + 1.08902e-02*momentum2.Pt() 
// 		    - 8.31507e-04*momentum2.Pt()*momentum2.Pt() 
// 		    - 1.01014e-04*momentum2.Pt()*momentum2.Pt()*momentum2.Pt() ;
	    }
	  else
	    {
	      ReCal = 1.0;
	    }
	  hRecoParticleEnergyAsym->Fill(momentum2.Energy(), asymmetry);
	  if ( TMath::Abs(ReCal*momentum2.M() - pi0mass)< 0.035 )
	    {
	      hRecoParticleEnergyAsymPi0->Fill(momentum2.Energy(), asymmetry);
	    }
	  // End recalibration stuff
	  
	  inv_mass_pair_reco = ReCal*momentum2.M();
	  pt_pair_reco =  ReCal*momentum2.Pt();
	  
	  //_____________________________________________________________________________
	  // DEFINITION OF ANALYSIS CUTS:
	  
	  // Fiducial cuts
	  if ( Particle->GetStatusCode() ) FIDUCUT = true;
	  else FIDUCUT = false ;
	  
	  // Asym + Low-energy clusters (<50 MeV) cut
	  if ( asymmetry<0.8 && (Particle->Energy()>Min_Cluster_Energy) && (Particle2->Energy()>Min_Cluster_Energy) )
	    ASYMCUT = true;
	  else ASYMCUT = false ;

	  // Asym2 + Low-energy clusters (<50 MeV) cut
	  if (asymmetry<0.6 && (Particle->Energy()>Min_Cluster_Energy) && (Particle2->Energy()>Min_Cluster_Energy)) 
	    ASYMCUT2 = true;
	  else ASYMCUT2 = false ;

	  // DEAD/WARN
	  Int_t dead_warn = (Particle->GetMother(0)  | Particle->GetMother(1) | 
			     Particle2->GetMother(0) | Particle2->GetMother(1) );
	  if ( dead_warn == 0 ) DEADWARNCUT = true;
	  else DEADWARNCUT = false ;

	  // Chi2
	  if ( ( TMath::Abs(Particle->Vz())<3.0 ) && 
	       ( TMath::Abs(Particle2->Vz())<3.0 ) ) CHI2CUT = true;
	  else  CHI2CUT = false ;

	  // TOF
	  if ( ( TMath::Abs(Particle->T() - flight_time)<1.2 ) && 
	       ( TMath::Abs(Particle2->T()- flight_time)<1.2 ) ) TOFCUT = true;
	  else TOFCUT = false ;

	  // Dominant contributor to the cluster:  GEANT ("true") Photon PID cut
	  // Reference cut to assess how far "true PID photon" == "photon-like Chi2 * TOF cuts"
	  if ( ( Particle->GetPdgCode()  == PDG_gam ) && 
	       ( Particle2->GetPdgCode() == PDG_gam ) ) TRUEPHOTON = true ;
	  else TRUEPHOTON = false ;

	  // Centrality classes
	  for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
	    { 
	      if ( centClass == iCent ) CENT_[iCent] = true;
	      else CENT_[iCent] = false;
	    }
// 	  if ( centClass >=  && centClass <=  ) CENT_ = true ; // centClass = --> 0%-0% 
// 	  else CENT_ = false ;

	  //_____________________________________________________________________________
	  
	  // No CUTS: All clusters participate to the Invariant Mass analysis: 
	  hRecoParticleMinvAllPairs->Fill( inv_mass_pair_reco, hagWeight );
	  binMinv = hRecoParticleMinvAllPairs->FindBin( inv_mass_pair_reco );
	  errorWeight = TMath::Sqrt(pow( hRecoParticleMinvAllPairs->GetBinError(binMinv),2) + hagWeight2); 
	  hRecoParticleMinvAllPairs->SetBinError( binMinv, errorWeight );
	  
	  Fill_Minv_pT_bidim_histo( hRecoParticleMinvPt, inv_mass_pair_reco, pt_pair_reco, hagWeight );

	  if ( FIDUCUT && DEADWARNCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( FIDUCUT  && DEADWARNCUT && ASYMCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( FIDUCUT && DEADWARNCUT && ASYMCUT && CHI2CUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2Cut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( FIDUCUT && DEADWARNCUT && ASYMCUT && CHI2CUT && TOFCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2TOFCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( FIDUCUT && DEADWARNCUT && ASYMCUT2 && CHI2CUT && TOFCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsym2Chi2TOFCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( FIDUCUT && DEADWARNCUT && ASYMCUT && TOFCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymTOFCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  } 
	  if ( FIDUCUT && DEADWARNCUT && TRUEPHOTON ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtTruePhotonCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if (  FIDUCUT && DEADWARNCUT && ASYMCUT && TRUEPHOTON ) { 
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymTruePhotonCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  
	  // ----------------------------------------------------------
	  // CENTRALITY DEPENDENT CUTS

	  for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
	    { 
	      if ( CENT_[iCent] ) {
		Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtCent_[iCent], inv_mass_pair_reco, pt_pair_reco, hagWeight );
	      }
	      if ( CENT_[iCent] && FIDUCUT && DEADWARNCUT ) {
		Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduCutCent_[iCent], inv_mass_pair_reco, pt_pair_reco, hagWeight );
	      }
	      if ( CENT_[iCent] && FIDUCUT && DEADWARNCUT && ASYMCUT ) { 
		Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymCutCent_[iCent], inv_mass_pair_reco, pt_pair_reco, hagWeight );
	      } 
	      if ( CENT_[iCent] && FIDUCUT && DEADWARNCUT && ASYMCUT && CHI2CUT ) {
		Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2CutCent_[iCent], inv_mass_pair_reco, pt_pair_reco, hagWeight );
	      }    
	      if ( CENT_[iCent] && FIDUCUT && DEADWARNCUT && ASYMCUT && CHI2CUT && TOFCUT ) {
		Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2TOFCutCent_[iCent], inv_mass_pair_reco, pt_pair_reco, hagWeight );
	      } 
	      if ( CENT_[iCent] && FIDUCUT && DEADWARNCUT && ASYMCUT2 && CHI2CUT && TOFCUT ) {
		Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsym2Chi2TOFCutCent_[iCent], inv_mass_pair_reco, pt_pair_reco, hagWeight );
	      } 
	      if ( CENT_[iCent] && FIDUCUT && DEADWARNCUT && ASYMCUT && TOFCUT ) {
		Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymTOFCutCent_[iCent], inv_mass_pair_reco, pt_pair_reco, hagWeight );
	      } 
	      if ( CENT_[iCent] && FIDUCUT && DEADWARNCUT && TRUEPHOTON ) {
		Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtTruePhotonCutCent_[iCent], inv_mass_pair_reco, pt_pair_reco, hagWeight );
	      }
	      if ( CENT_[iCent] && FIDUCUT && DEADWARNCUT && ASYMCUT && TRUEPHOTON ) { 
		Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymTruePhotonCutCent_[iCent], inv_mass_pair_reco, pt_pair_reco, hagWeight );
	      } 	  
	    }

	} // End inv. mass loop
      
      hRecoSecondaryParticlePt->Fill( ReCal*momentum1.Pt(), hagWeight );

    } // End loop on ListOfClusterParticles

  ListOfSimulParticles->Delete(); 
  ListOfClusterParticles->Delete(); 

  //} // end MONO_PT_BIN

  // ----------------------------------------------------------
  // 6) Write histos
  // Write all histos when reaching the last event
  if (eventNumber == maxEvents-1)
    {
      ReconstructionEfficiencyFile->cd();

      hSimulPairsMinvAllPairs->Write();
      hSimulPi0MinvPt->Write();
      hSimulPairsMinvPt->Write();
      hRecoParticleMinvAllPairs->Write();
      hRecoParticleMinvPt->Write();
      hRecoParticleMinvPtFiduCut->Write();
      hRecoParticleMinvPtFiduAsymCut->Write();
      hRecoParticleMinvPtFiduAsymChi2Cut->Write();
      hRecoParticleMinvPtFiduAsymTOFCut->Write();
      hRecoParticleMinvPtFiduAsymChi2TOFCut->Write();
      hRecoParticleMinvPtFiduAsym2Chi2TOFCut->Write();
      hRecoParticleMinvPtTruePhotonCut->Write();
      hRecoParticleMinvPtFiduAsymTruePhotonCut->Write();

      for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
	{ 
	  hRecoParticleMinvPtCent_[iCent]->Write();
	  hRecoParticleMinvPtFiduCutCent_[iCent]->Write();
	  hRecoParticleMinvPtFiduAsymCutCent_[iCent]->Write();
	  hRecoParticleMinvPtFiduAsymChi2CutCent_[iCent]->Write();
	  hRecoParticleMinvPtFiduAsymTOFCutCent_[iCent]->Write();
	  hRecoParticleMinvPtFiduAsymChi2TOFCutCent_[iCent]->Write();
	  hRecoParticleMinvPtFiduAsym2Chi2TOFCutCent_[iCent]->Write();
	  hRecoParticleMinvPtTruePhotonCutCent_[iCent]->Write();
	  hRecoParticleMinvPtFiduAsymTruePhotonCutCent_[iCent]->Write();
	}

      // Recalibration factor due to Pt smearing
      Float_t ReCal, eReCal;
      TH1F* hRecalibrationFactors  = new TH1F("hRecalibrationFactors","hRecalibrationFactors",NumOfpTbins,pTmin,pTmax);
      TH1F* hRecoParticleMinvPeak  = new TH1F("hRecoParticleMinvPeak","hRecoParticleMinvPeak",NumOfpTbins,pTmin,pTmax);
      TH1F* hRecoParticleMinvSigma = new TH1F("hRecoParticleMinvSigma","hRecoParticleMinvSigma",NumOfpTbins,pTmin,pTmax);

      char hMinv_name[20];
      char hMinv_title[40];
      TH1D *hMinv_[NumOfpTbins]; 
      TH1D *hMinv_tempo;

      for(Int_t pTbin=1; pTbin<=NumOfpTbins; pTbin++)
	{
	  sprintf(hMinv_name,"hMinv_%d",pTbin);
          hMinv_tempo =  hRecoParticleMinvPt->ProjectionX(hMinv_name,pTbin,pTbin,"ed");
          hMinv_tempo->Fit("gaus","QER","",0.105,0.165);
          TF1 *gauss = hMinv_tempo->GetFunction("gaus");
          gauss->SetLineColor(2); 
          gauss->SetLineWidth(5);
          //gauss->Draw("same");
          Float_t ptvalue = pTbin*0.5; // 0.5 GeV/c is the pT bin width
          sprintf(hMinv_title,"Minv for pT= %f2.1 GeV/c bin",ptvalue);
          hMinv_[pTbin] = new TH1D(*hMinv_tempo);
          hMinv_[pTbin]->SetNameTitle(hMinv_name,hMinv_title);
          hMinv_[pTbin]->Write();

	  ReCal  = pi0mass/gauss->GetParameter(1);
	  eReCal = pi0mass*gauss->GetParError(1)/gauss->GetParameter(1)/gauss->GetParameter(1);
	  hRecoParticleMinvPeak->SetBinContent(pTbin,gauss->GetParameter(1));
	  hRecoParticleMinvPeak->SetBinError(pTbin,gauss->GetParError(1));
	  hRecoParticleMinvSigma->SetBinContent(pTbin,gauss->GetParameter(2));
	  hRecoParticleMinvSigma->SetBinError(pTbin,gauss->GetParError(2));

	  cout << " >> ReCal :" << ReCal << "+-" << eReCal << " for pTbin =" << pTbin << endl;      
	  hRecalibrationFactors->SetBinContent(pTbin,ReCal);
	  hRecalibrationFactors->SetBinError(pTbin,eReCal);
	  delete hMinv_[pTbin];
	}
      
      hRecoParticleMinvPeak->Write();
      hRecoParticleMinvSigma->Write();
      hRecalibrationFactors->Write();
      
      hArm->Write();
      hSector->Write();
      hMeasx->Write();
      hMeasy->Write();
      hMeasz->Write();
      hMeasyz_e->Write();
      hMeasyz_w->Write();
      hMeasyz_e_out->Write();
      hMeasyz_w_out->Write();
      hMease->Write();
      hEcore->Write();
      hTof->Write();
      hProbPhot->Write();
      hChi2->Write();
      
      hCentClass->Write();
      hTotalClusterMul->Write();
      hEmbedClusterMul->Write() ;
      for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
	{ 
	  hEmbedClusterMulCent_[iCent]->Write() ;
	}
      hSimulSecondaryParticlePt->Write();
      hRecoSecondaryParticlePt->Write();
      hRecoParticleEnergyAsym->Write();
      hRecoParticleEnergyAsymPi0->Write();
      
      hfiducial_local_yz_geatrack->Write();
      hfiducial_local_yz_geatrack->Write();

      ReconstructionEfficiencyFile->Close();
      cout << endl << "<I> >>> Output Efficiency file: " << ReconstructionEfficiencyFile->GetName() 
	   << endl << endl;
    } 
  
  return CounterOfSimulParticles;  
}

//=============================================================================
//

void efficiency( const TH2F *hRecoParticleMinvPtWhatever, 
		 const char *InputEfficiencyFile = OutputEfficFileName.getString(),
		 const Float_t normalization = 1. )
{

 TFile in(InputEfficiencyFile,"UPDATE");
 in.cd();

 // Measured
 TH2F *hRecoParticle = (TH2F*) (hRecoParticleMinvPtWhatever);

 // Expected (from simulated reference) <--- Efficiency Denominator
 TH2F *hSimulPairs = (TH2F*) in.Get("hSimulPairsMinvPt");
 TH2F *hSimulPi0 = (TH2F*) in.Get("hSimulPi0MinvPt");

 //_____________________________________________________________________________
 // get the name of the efficiency analysis (cuts)

 PHString HistoName = hRecoParticleMinvPtWhatever->GetName(); 
 PHString CutsString;
 PHPointerList<PHString> split_name;
 PHString separator ="hRecoParticleMinvPt";
 if ( HistoName == separator)
   {
     CutsString = "NoCuts";
   }
 else
   {
     HistoName.split(split_name,(const char*)separator.getString());
     CutsString = *split_name[1] ;
   }
 const char *CutsName = CutsString.getString();
 cout << endl << " ================ Calculating efficiency for Cuts: " << CutsName 
              << " ================ " << endl;

 //_____________________________________________________________________________
 // Make directory for output

 if (!in.Get(CutsName)) in.mkdir(CutsName);
 in.cd(CutsName);

 //_____________________________________________________________________________ 
 // Measured Inv. mass peak and sigma

 char htitle[60];
 sprintf(htitle,"hRecoParticleMinvPeak%s",CutsName);
 TH1F *hRecoParticleMinvPeak  = new TH1F("hRecoParticleMinvPeak",htitle,NumOfpTbins,pTmin,pTmax);
 sprintf(htitle,"hRecoParticleMinvSigma%s",CutsName);
 TH1F *hRecoParticleMinvSigma = new TH1F("hRecoParticleMinvSigma",htitle,NumOfpTbins,pTmin,pTmax);
 
 char hMinv_name[20];
 char hMinv_title[40];
 char func_name[30];

 TH1D *hMinv_[NumOfpTbins]; 
 TH1D *hMinv_tempo; 
 TF1 *gaussian_[NumOfpTbins];
 TF1 *background_[NumOfpTbins];
 TF1 *gauss_plus_bckgd_[NumOfpTbins];
 TH2F *hRecoMinvPtBackground = new TH2F("hRecoMinvPtBackground","hRecoMinvPtBackground",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
 hRecoMinvPtBackground->Sumw2();

 Float_t fit_lower = 0.09;  // Nominal 0.04, 2nd 0.06
 Float_t fit_upper = 0.18;  // Nominal 0.25, 2nd 0.23 
 Float_t Agauss = 0. ;
 Float_t peak_pi0_real = 0.; 
 Float_t width_pi0_real = 0.; 
 Float_t bckgd = 0. ;
 Float_t err_bckgd = 0. ;
 Float_t ptvalue = 0. ;

 for(Int_t pTbin=1; pTbin<=NumOfpTbins; pTbin++)
   {
     sprintf(hMinv_name,"hMinv_%d",pTbin);
     hMinv_tempo = hRecoParticle->ProjectionX(hMinv_name,pTbin,pTbin,"ed");
     
     // Fit Minv to gauss+pol0
     width_pi0_real = 0.02 ; // guess (20 MeV/c2 "worst-case" width)
     Agauss = hMinv_tempo->GetBinContent(pi0mass/Minvbinsize);  // guess
     bckgd = hMinv_tempo->GetBinContent(fit_lower/Minvbinsize); // guess
     sprintf(func_name,"gauss_plus_bckgd_%d",pTbin);
     gauss_plus_bckgd_[pTbin] = new TF1(func_name,"gaus(0)+pol0(3)", fit_lower, fit_upper);
     gauss_plus_bckgd_[pTbin]->SetParameters( Agauss, pi0mass, width_pi0_real, bckgd ); 
     gauss_plus_bckgd_[pTbin]->SetParLimits( 1, fit_lower, fit_upper ); // peak boundaries: 90 - 180 MeV/c2 
     gauss_plus_bckgd_[pTbin]->SetParLimits( 2, 0.004, 0.03 ); // peak width boundaries: 4 - 30 MeV/c2 
     gauss_plus_bckgd_[pTbin]->SetParLimits( 3, 0., Agauss ); // background boundaries:  0 - height at pi0 peak
     hMinv_tempo->Fit(func_name,"QIEMR");
     //hMinv_tempo->Fit("gauss_plus_bckgd","QLMR");
     gauss_plus_bckgd_[pTbin]->SetLineColor(2); 
     gauss_plus_bckgd_[pTbin]->SetLineWidth(5);
     gauss_plus_bckgd_[pTbin]->SetNpx( hMinv_tempo->GetNbinsX() ); // needed for GetHistogram() later
     gauss_plus_bckgd_[pTbin]->Write(func_name,TObject::kOverwrite);

     Agauss = gauss_plus_bckgd_[pTbin]->GetParameter(0);
     peak_pi0_real = gauss_plus_bckgd_[pTbin]->GetParameter(1);
     width_pi0_real = gauss_plus_bckgd_[pTbin]->GetParameter(2);
     bckgd = gauss_plus_bckgd_[pTbin]->GetParameter(3);
     err_bckgd = gauss_plus_bckgd_[pTbin]->GetParError(3);

     // Gaussian alone
     sprintf(func_name,"gaussian_%d",pTbin);
     gaussian_[pTbin] = new TF1(func_name, "gaus(0)", 0, Minvmax);//fit_lower, fit_upper);
     gaussian_[pTbin]->SetParameter(0, Agauss );
     gaussian_[pTbin]->SetParameter(1, peak_pi0_real );
     gaussian_[pTbin]->SetParameter(2, width_pi0_real);
     gaussian_[pTbin]->SetLineColor(2);
     gaussian_[pTbin]->SetLineWidth(5);
     gaussian_[pTbin]->SetNpx( hMinv_tempo->GetNbinsX() ); // needed for GetHistogram() later
     gaussian_[pTbin]->Write(func_name,TObject::kOverwrite);

     // Background of the Gaussian
     sprintf(func_name,"background_%d",pTbin);
     background_[pTbin] = new TF1(func_name, "pol0", 0, Minvmax);//fit_lower, fit_upper);
     background_[pTbin]->SetParameter(0, bckgd );
     background_[pTbin]->SetLineColor(2);
     background_[pTbin]->SetLineWidth(5);
     background_[pTbin]->SetNpx( hMinv_tempo->GetNbinsX() ); // needed for GetHistogram() later
     background_[pTbin]->Write(func_name,TObject::kOverwrite);

     ptvalue = (pTbin*pTbinsize)-(pTbinsize/2.); // 0.5 GeV/c is the pT bin width - 0.25 GeV/c for the center of the bin
     for (Int_t minvbin = 1 ; minvbin<=nMinvbin ; minvbin++)
       {
	 Fill_Minv_pT_bidim_histo( hRecoMinvPtBackground, minvbin*Minvbinsize, ptvalue, bckgd , err_bckgd );
       }
     sprintf(hMinv_title,"Minv for pT= %f2.1 GeV/c bin",ptvalue);
     hMinv_[pTbin] = (TH1D*)hMinv_tempo->Clone();
     hMinv_[pTbin]->SetNameTitle(hMinv_name,hMinv_title);
     hMinv_[pTbin]->Write(hMinv_name,TObject::kOverwrite);

     cout << "Fit yields: Mpi0 = " << peak_pi0_real*1000. << " +/- " << width_pi0_real*1000. << endl;
     hRecoParticleMinvPeak->SetBinContent( pTbin , peak_pi0_real );
     hRecoParticleMinvPeak->SetBinError( pTbin , gauss_plus_bckgd_[pTbin]->GetParError(1) );
     hRecoParticleMinvSigma->SetBinContent( pTbin , width_pi0_real );
     hRecoParticleMinvSigma->SetBinError( pTbin , gauss_plus_bckgd_[pTbin]->GetParError(2));
     
     delete hMinv_[pTbin] ;

     delete hMinv_tempo;
   }

 // ----------------------------------------------------------
 // Efficiency_fixedwidth = Straightforward division of [Reco-Background]/Simul histos
 // with *fixed* inv. mass peak and window

 Int_t pi0bin = (Int_t) (pi0mass/Minvbinsize)+1 ;

 TH1D *hRecoParticlePt   = hRecoParticle->ProjectionY("hRecoParticlePt",53,83,"ed"); // 53 - 83 = 106 - 166 MeV
 TH1D *hRecoBackgroundPt = hRecoMinvPtBackground->ProjectionY("hRecoBackgroundPt",53,83,"ed"); // 53 - 83 = 106 - 166 MeV
 TH1D *hSimulPairsPt     = hSimulPairs->ProjectionY("hSimulPairsPt",pi0bin-1,pi0bin+1,"ed"); // delta function at m_pi0
 TH1D *hSimulPi0Pt       = hSimulPi0->ProjectionY("hSimulPi0Pt",pi0bin-1,pi0bin+1,"ed"); // delta function at m_pi0

 TH1D *hEfficiency_fixedwidth = new TH1D(*hRecoParticlePt);
 sprintf(htitle,"hEfficiency_fixedwidth%s",CutsName);
 hEfficiency_fixedwidth->SetNameTitle("hEfficiency_fixedwidth",htitle);
 hEfficiency_fixedwidth->Sumw2();
 hEfficiency_fixedwidth->Add(hRecoBackgroundPt, -1.);
 //hEfficiency_fixedwidth->Divide(hSimulPairsPt);
 hEfficiency_fixedwidth->Divide(hSimulPi0Pt);
 hEfficiency_fixedwidth->Scale(normalization);

 hEfficiency_fixedwidth->SetMinimum(0.1);
 hEfficiency_fixedwidth->SetMaximum(1.5);
 hEfficiency_fixedwidth->SetMarkerStyle(5);
 hEfficiency_fixedwidth->SetMarkerColor(1);

 // ----------------------------------------------------------
 // Efficiency_fit,_counts = Ratio [Measured-Background]/Expected
 // integrated within a variable pT+centrality dependent peak and width
 
 sprintf(htitle,"hEfficiency_fit%s",CutsName);
 TH1D *hEfficiency_fit = new TH1D("hEfficiency_fit",htitle,NumOfpTbins,pTmin,pTmax);
 sprintf(htitle,"hEfficiency_counts%s",CutsName);
 TH1D *hEfficiency_counts = new TH1D("hEfficiency_counts",htitle,NumOfpTbins,pTmin,pTmax);

 Int_t pi0bin_real = 0; 
 Int_t pi0window_minbin_real = 0; 
 Int_t pi0window_maxbin_real = 0;
 Float_t pi0_detected_counts = 0.; 
 Float_t pi0_detected_fit = 0.; 
 Float_t pi0_background_counts = 0.;
 Float_t pi0_background_fit = 0. ;
 Float_t pi0_expected = 0.;
 Float_t pi0_pseudoexpected = 0.;

 for(Int_t pTbin=1; pTbin<=NumOfpTbins; pTbin++)
   {
     // Reconstructed extraction window
     peak_pi0_real  = hRecoParticleMinvPeak->GetBinContent(pTbin); // pi0 peak in the relevant pT bin
     pi0bin_real = (Int_t)(peak_pi0_real/Minvbinsize)+1; // pi0 peak bin
     width_pi0_real = hRecoParticleMinvSigma->GetBinContent(pTbin); // pi0 sigma in the relevant pT bin
     Float_t pTvalue = (pTbin*pTbinsize)-(pTbinsize/2.); // 0.5 GeV/c is the pT bin width - 0.25 GeV/c for the center of the bin
     cout << " pT = " << pTvalue << " GeV/c : Reconstructed Peak = " 
	  << peak_pi0_real << " +/- " << width_pi0_real << " GeV/c2" << endl;
     pi0window_minbin_real = (Int_t)(pi0bin_real-2.*width_pi0_real/Minvbinsize); // -2*sigma
     pi0window_maxbin_real = (Int_t)(pi0bin_real+2.*width_pi0_real/Minvbinsize); // +2*sigma
     cout << "                   Two-sigma Minv extraction window = [ " 
	  << pi0window_minbin_real*Minvbinsize << " - " << pi0window_maxbin_real*Minvbinsize << " ] (GeV/c2)" << endl;

     // Reconstructed Yield
     // integral of "true" yield (counts)
     TH1D *hMinv_counts = hRecoParticle->ProjectionX("hMinv_counts",pTbin,pTbin,"ed");
     pi0_detected_counts = hMinv_counts->Integral(pi0window_minbin_real,pi0window_maxbin_real);

     // integral of fitted "true" yield
     gaussian_[pTbin]->Draw(); // needed for GetHistogram() 
     TH1D *hMinv_fit  = (TH1D*)(gaussian_[pTbin]->GetHistogram());                                                 
     //gauss_plus_bckgd_[pTbin]->Draw(); // needed for GetHistogram() 
     //TH1D *hMinv_Fit  = (TH1D*)(gauss_plus_bckgd_[pTbin]->GetHistogram());                                                 
     pi0_detected_fit = hMinv_fit->Integral(pi0window_minbin_real,pi0window_maxbin_real);

     cout << "   pi0_det_counts = " << pi0_detected_counts << "  pi0_det_fit = " << pi0_detected_fit << endl;

     // Reconstructed background
     pi0_background_counts  = (4.*width_pi0_real/Minvbinsize)*(background_[pTbin]->Eval(peak_pi0_real));

     TH1D *hRecoBackgroundMinv = hRecoMinvPtBackground->ProjectionX("hRecoBackgroundMinv",pTbin,pTbin,"ed");
     pi0_background_fit = hRecoBackgroundMinv->Integral(pi0window_minbin_real,pi0window_maxbin_real);


     cout << "   pi0_bckgd_counts = " << pi0_background_counts << "  pi0_bckgd_fit = " << pi0_background_fit << endl;

     // Simulated Reference Yield
     // Simulated extraction window: *pure* input simulation should have delta function at pi0 mass
     TH1D *hSimulPairsMinv = hSimulPairs->ProjectionX("hSimulPairsMinv",pTbin,pTbin,"ed");
     pi0_pseudoexpected = hSimulPairsMinv->Integral(pi0bin-1,pi0bin+1);

     TH1D *hSimulPi0Minv = hSimulPi0->ProjectionX("hSimulPi0Minv",pTbin,pTbin,"ed");
     pi0_expected = hSimulPi0Minv->Integral(pi0bin-1,pi0bin+1);
 
     // Efficiency
     //Float_t effic = (pi0_expected) ? normalization*(pi0_detected_counts_fit+pi0_background)/pi0_expected : 0. ;
     //Float_t pseudoeffic = (pi0_pseudoexpected) ? normalization*(pi0_detected_counts)/pi0_pseudoexpected : 0. ;
     Float_t effic_counts = (pi0_expected) ? normalization*(pi0_detected_counts)/pi0_expected : 0. ;
     Float_t effic_fit = (pi0_expected) ? normalization*(pi0_detected_fit)/pi0_expected : 0. ;
     // we propagate the error calculated "automatically" by Root
     Float_t err_effic = hEfficiency_fixedwidth->GetBinError(pTbin);

     cout << "                   EFICIENCY [(pi0_det_counts)/pi0_expect*Norm) = [" 
	  << pi0_detected_counts << "]/" << pi0_expected << "*" << normalization 
	  << " = " << effic_counts << " +/- " << err_effic << endl;
     cout << "                   EFICIENCY [(pi0_det_fit)/pi0_expect*Norm) = [" 
	  << pi0_detected_fit << "]/" << pi0_expected << "*" << normalization 
	  << " = " << effic_fit << " +/- " << err_effic << endl;

     hEfficiency_fit->SetBinContent(pTbin,effic_fit);
     hEfficiency_fit->SetBinError(pTbin,err_effic);

     hEfficiency_counts->SetBinContent(pTbin,effic_counts);
     hEfficiency_counts->SetBinError(pTbin,err_effic);

     delete hMinv_counts;
     delete hRecoBackgroundMinv;
     delete hSimulPairsMinv ;
     delete hSimulPi0Minv;
     delete gaussian_[pTbin];
     delete background_[pTbin];
     delete gauss_plus_bckgd_[pTbin] ;
   }

 hEfficiency_fit->SetMinimum(0.1);
 hEfficiency_fit->SetMaximum(1.5);
 hEfficiency_fit->SetMarkerStyle(5);
 hEfficiency_fit->SetMarkerColor(1);

 hEfficiency_counts->SetMinimum(0.1);
 hEfficiency_counts->SetMaximum(1.5);
 hEfficiency_counts->SetMarkerStyle(5);
 hEfficiency_counts->SetMarkerColor(1);

 //_____________________________________________________________________________
 // Write output

 hEfficiency_fixedwidth->Write("hEfficiency_fixedwidth",TObject::kOverwrite);
 hEfficiency_fit->Write("hEfficiency_fit",TObject::kOverwrite);
 hEfficiency_counts->Write("hEfficiency_counts",TObject::kOverwrite);
 hRecoParticleMinvPeak->Write("hRecoParticleMinvPeak",TObject::kOverwrite);
 hRecoParticleMinvSigma->Write("hRecoParticleMinvSigma",TObject::kOverwrite);
 hRecoMinvPtBackground->Write("hRecoMinvPtBackground",TObject::kOverwrite);

 in.Close();

}


//=============================================================================
//

void efficiency_all_cuts(const char *InputEfficiencyFile = OutputEfficFileName.getString() )
{

 TFile in(InputEfficiencyFile,"read");
 in.cd();

 //_____________________________________________________________________________
 // Minimum-bias Efficiencies

 // 0. No Cut
 TH2F *hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPt");
 efficiency( hRecoParticle, InputEfficiencyFile);
 // 1. Fiducial Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduCut");
 efficiency( hRecoParticle, InputEfficiencyFile);
 // 2. FiduAsym Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymCut");
 efficiency( hRecoParticle, InputEfficiencyFile);
 // 3. GEANT ("true") Photon PID * FiduAsym Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymTruePhotonCut");
 efficiency( hRecoParticle, InputEfficiencyFile);
 // 4. FiduAsym * Chi2 Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymChi2Cut");
 efficiency( hRecoParticle, InputEfficiencyFile);
 // 5. FiduAsym * TOF Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymTOFCut");
 efficiency( hRecoParticle, InputEfficiencyFile);
 // 6. FiduAsym * Chi2*TOF Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymChi2TOFCut");
 efficiency( hRecoParticle, InputEfficiencyFile);
 // 7. FiduAsym2 * Chi2*TOF Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsym2Chi2TOFCut");
 efficiency( hRecoParticle, InputEfficiencyFile);

 //_____________________________________________________________________________
 // Centrality-Class Efficiencies

 TH1F *hCentClass = (TH1F*) in.Get("hCentClass");

 char histo_name[50];
 for (Int_t iCent=0; iCent<CENTCLASSES; iCent++)
   { 
     // Normalization
     Float_t normalizationCent = hCentClass->GetBinContent(iCent+1);
     //Float_t normalizationCent = hCentClass->Integral()/hCentClass->Integral(1,2);
    
     // 0. No Cut
     sprintf(histo_name,"hRecoParticleMinvPtCent_%i",iCent);
     hRecoParticle = (TH2F*) in.Get(histo_name);
     efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
     // 1. Fidu Cut
     sprintf(histo_name,"hRecoParticleMinvPtFiduCutCent_%i",iCent);
     hRecoParticle = (TH2F*) in.Get(histo_name);
     efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
     // 2. FiduAsym Cut
     sprintf(histo_name,"hRecoParticleMinvPtFiduAsymCutCent_%i",iCent);
     hRecoParticle = (TH2F*) in.Get(histo_name);
     efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
     // 3. GEANT ("true") Photon PID * FiduAsym Cut
     sprintf(histo_name,"hRecoParticleMinvPtFiduAsymTruePhotonCutCent_%i",iCent);
     hRecoParticle = (TH2F*) in.Get(histo_name);
     efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
     // 4. FiduAsym * Chi2 Cuts
     sprintf(histo_name,"hRecoParticleMinvPtFiduAsymChi2CutCent_%i",iCent);
     hRecoParticle = (TH2F*) in.Get(histo_name);
     efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
     sprintf(histo_name,"_%i",iCent);
     // 5. FiduAsym * TOF Cuts
     sprintf(histo_name,"hRecoParticleMinvPtFiduAsymTOFCutCent_%i",iCent);
     hRecoParticle = (TH2F*) in.Get(histo_name);
     efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
     // 6. FiduAsym * Chi2 * TOF Cuts
     sprintf(histo_name,"hRecoParticleMinvPtFiduAsymChi2TOFCutCent_%i",iCent);
     hRecoParticle = (TH2F*) in.Get(histo_name);
     efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
     // 7. FiduAsym2 * Chi2 * TOF Cuts
     sprintf(histo_name,"hRecoParticleMinvPtFiduAsym2Chi2TOFCutCent_%i",iCent);
     hRecoParticle = (TH2F*) in.Get(histo_name);
     efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
   }
 
 in.Close();
 cout << endl << "<I> >>> Output Efficiency file: " << in.GetName() 
      << endl << endl;

}

//=============================================================================
// Function for reading the embedding file
//

void reading_from_embed(Int_t maxEvents=5000,
			const char *dstIFile="/phenix/data06/enterria/year1_pi0_embed_files_fake/pi0_embed_302_runs_fake.root")
                     // const char *dstIFile="/phenix/data06/enterria/year1_pi0_embed_files/pi0_embed_302_runs.root")
{

 // Total number of events in embed+eval file
  TFile f(dstIFile,"");
  Int_t Events = (Int_t)((TTree*)f.Get("T"))->GetEntries();
  cout << "<I> Input file " << dstIFile << " has " << Events << " entries." << endl ;
  maxEvents = (Events < maxEvents) ? Events : maxEvents ;
  f.Close();

  // Output Efficiency file
  PHString dstInFile = dstIFile;
  PHString separator ="/";
  PHString effic_prefix = "effic_";
  PHString *element ;

  PHPointerList<PHString> split_name;
  Int_t num_of_dirs = dstInFile.split(split_name,(const char*)separator.getString());
  PHString temp_name = effic_prefix + *split_name[num_of_dirs-1] ;
  (*split_name[num_of_dirs-1]) = temp_name ;
  PHPointerListIterator<PHString> iter(split_name);   
  OutputEfficFileName = *(iter());
  while ((element = iter())) OutputEfficFileName = OutputEfficFileName + separator + *element;
  cout << "<I> Output file will be: " << OutputEfficFileName.getString() << endl;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);

  // Set up input file
  PHNodeIOManager* dstIn = new PHNodeIOManager(dstInFile,PHReadOnly);

  dEmcGeaTrackWrapper*         dEmcGeaTrack        = 0;
  dEmcGeaClusterTrackWrapper*  dEmcGeaClusterTrack = 0;

  // STARTING THE EVENT LOOP
  Int_t eventNumber = 0;

  while (eventNumber < maxEvents) 
    {
      
      PHCompositeNode* rv = dstIn->read(evaNode,eventNumber);
     
      if ( !rv ) break;

      if (eventNumber == 0)  dstIn->print();

      if (!(eventNumber%1000)) cout << " event " << eventNumber << flush;
      if (!(eventNumber%100))  cout << "." << flush;

      dEmcGeaTrack = PHNodeHelper<dEmcGeaTrackWrapper>::getTable
	("dEmcGeaTrack",evaNode);

      assert(dEmcGeaTrack!=0);

      dEmcGeaClusterTrack = PHNodeHelper<dEmcGeaClusterTrackWrapper>::getTable
	("dEmcGeaClusterTrack",evaNode);

      assert(dEmcGeaClusterTrack!=0);

      // pi0 reconstruction
      pi0_reconstruction( dEmcGeaTrack , dEmcGeaClusterTrack, 
			  eventNumber, maxEvents );
      
      eventNumber++;
    }
}
