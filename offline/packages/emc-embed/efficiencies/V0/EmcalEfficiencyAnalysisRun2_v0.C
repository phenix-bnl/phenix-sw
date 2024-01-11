//****************************************************************
// Efficiency Calculations in EMCAL
//        
// Copyright (C) PHENIX collaboration, 2001
//
// Authors: David D'ENTERRIA & G. MARTINEZ - SUBATECH, 2002
//
// Purpose: Reads embed+eval pi0 file and outputs a ROOT file with
//          efficiency histograms for min-bias, periph, central 
//          collisions
//
//****************************************************************

/*  To compile this macro simply do:

 > root

 .includepath $AFSHOME/install/include
 .includepath $OFFLINE_MAIN/include
 gSystem->Load("libpreco.so");
 gSystem->Load("libEG.so");
 .L EmcalEfficiencyAnalysisRun2_v0.C++
 reading_from_embed(2500000,"/phenix/data06/enterria/year1_pi0_embed_files_May/all2.root") ;
 reading_from_embed(2500000,"~/year1dsts/last_year1_pi0_embed_files/merged_0_314_runs.root");

 reading_from_embed(2500000,"/phenix/data06/enterria/year1_pi0_embed_files_May_fake/merged_976_runs12XXX_fake.root");
// reading_from_embed(250000,"/phenix/data06/enterria/year1_pi0_embed_files_fake/pi0_embed_512_runs_fake.root");

 efficiency_all_cuts() ;

// To run this macro do:

 > root

 gSystem->Load("libpreco.so");
 gSystem->Load("libEG.so");
 gSystem->Load("~/afs/effic2/EmcalEfficiencyAnalysisRun2_v0_C.so");
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

// Phenix classes
#include "dEmcGeaTrackWrapper.h"  
#include "dEmcGeaTrackClusterWrapper.h"
#include "dEmcGeaTrackCluster.h"
#include "dEmcGeaClusterTrack.h"
#include "dEmcGeaClusterTrackWrapper.h"
#include "mEmcGeometryModule.h"

// Phool classes
#include "PHCompositeNode.h"
#include "PHNodeIterator.h" 
#include "PHNodeReset.h"
#include "PHString.h"
#include "PHNodeIOManager.h"
#include "PHIODataNode.h"
#include "PHPointerListIterator.h"

// A few global variables:

static PHString OutputEfficFileName ;

static Int_t PDG_gam   =  22;
static Int_t PDG_elec  =  11;
static Int_t PDG_eplus = -11;
static Int_t PDG_pi0   = 111;
static Float_t pi0mass = 0.1349766 ;

static Int_t init_geo_done = 0 ;
static mEmcGeometryModule *EmcGeometryModule = 0 ; 

static TH2F *hfiducial_local_yz0_geatrack = 0;
static TH2F *hfiducial_local_yz0_geaclustertrack = 0;
static TH2F *hfiducial_local_yz1_geatrack = 0;
static TH2F *hfiducial_local_yz1_geaclustertrack = 0;

// those below are "tunable" global parameters ...

static Float_t pTmin = 0.;
static Float_t pTmax = 5.;
static Float_t pTbinsize = 0.5 ; // 0.5 GeV/c bins
static Int_t NumOfpTbins = (Int_t)((pTmax-pTmin)/pTbinsize);

static Float_t Minvmax = 0.6;
static Float_t Minvbinsize = 0.002 ; // 2 MeV/c bins
static Int_t nMinvbin = (Int_t)(Minvmax/Minvbinsize); 

static Float_t Min_Cluster_Energy = 0.1 ; // clusters below 100 MeV excluded

static Int_t NMAXSIMULCLUS = 200 ; // well above any expected number

//=================================================================================
// Fiducial cuts 

Bool_t in_fiducial_area( const Int_t iTrack, const dEmcGeaTrackWrapper* dEmcGeaTrack,
			 const dEmcGeaClusterTrackWrapper* dEmcGeaClusterTrack )
{

  Float_t xyz[3];
  Float_t local_x0,local_y0,local_z0;
  Float_t local_x1,local_y1,local_z1;

  if ( !init_geo_done ) 
    {  
      EmcGeometryModule = new mEmcGeometryModule ; 
      init_geo_done = 1;
    }

  if ( dEmcGeaTrack && !(dEmcGeaClusterTrack) )
    {
      xyz[0] = dEmcGeaTrack->get_impxyz(0,iTrack) ;      
      xyz[1] = dEmcGeaTrack->get_impxyz(1,iTrack) ;      
      xyz[2] = dEmcGeaTrack->get_impxyz(2,iTrack) ;
    }
  else if ( !(dEmcGeaTrack) && dEmcGeaClusterTrack )
    {
      xyz[0] = dEmcGeaClusterTrack->get_measxyz(0,iTrack) ;      
      xyz[1] = dEmcGeaClusterTrack->get_measxyz(1,iTrack) ;      
      xyz[2] = dEmcGeaClusterTrack->get_measxyz(2,iTrack) ;
    }
  else
    {
      cout << "<E> I don't know how to calculate impxyz for this (these 2 ?) object(s) !!! " << endl
	   << " in_fiducial_area == false !!!" << endl ;
      return false;
    }

  // Fiducial cuts Run-1 according to Saskia: 
  // (local_z<20 || local_z>380 || local_y<10 || local_y>190) return(0)

  // Mind that global_z direction corresponds to local_x direction and viceversa !!
  EmcGeometryModule->GlobalToLocal(xyz[0],xyz[1],xyz[2],0,local_x0,local_y0,local_z0); // sector 0
  EmcGeometryModule->GlobalToLocal(xyz[0],xyz[1],xyz[2],1,local_x1,local_y1,local_z1); // sector 1

  // Each sector in its *local* coordinates ocupies an area between: 
  // y_local ~ [0, 200] and z_global = x_local ~ [0, 400]
  //if ( local_x0>=20 && local_x0<=380 && local_y0>=10 && local_y0<=190 ) // in sector 0 fiducial area
  if ( local_x0>=5 && local_x0<=395 && local_y0>=5 && local_y0<=190 ) // in sector 0 fiducial area
    {
      if (dEmcGeaTrack) hfiducial_local_yz0_geatrack->Fill(local_x0,local_y0);
      else hfiducial_local_yz0_geaclustertrack->Fill(local_x0,local_y0);

      return true ;
    }
  //else if ( local_x1>=20 && local_x1<=380 && local_y1>=10 && local_y1<=190 ) // in sector 1 fiducial area
  else if ( local_x1>=5 && local_x1<=395 && local_y1>=5 && local_y1<=190 ) // in sector 1 fiducial area
    {
      if (dEmcGeaTrack) hfiducial_local_yz1_geatrack->Fill(local_x1,local_y1);
      else hfiducial_local_yz1_geaclustertrack->Fill(local_x1,local_y1);
      
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
  static TH2F *hRecoParticleMinvPtTruePhotonCut = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymTruePhotonCut = 0;

  static TH2F *hRecoParticleMinvPtCent = 0;
  static TH2F *hRecoParticleMinvPtFiduCutCent = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymCutCent = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymChi2CutCent = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymTOFCutCent = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymChi2TOFCutCent = 0;
  static TH2F *hRecoParticleMinvPtTruePhotonCutCent = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymTruePhotonCutCent = 0;

  static TH2F *hRecoParticleMinvPtPeriph = 0;
  static TH2F *hRecoParticleMinvPtFiduCutPeriph = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymCutPeriph = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymChi2CutPeriph = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymTOFCutPeriph = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymChi2TOFCutPeriph = 0;
  static TH2F *hRecoParticleMinvPtTruePhotonCutPeriph = 0;
  static TH2F *hRecoParticleMinvPtFiduAsymTruePhotonCutPeriph = 0;

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
  static TH1F *hEmbedClusterMulCent = 0 ;
  static TH1F *hEmbedClusterMulPeriph = 0 ;

  static TH1F *hSimulSecondaryParticlePt = 0;
  static TH1F *hRecoSecondaryParticlePt = 0;

  static TH2F *hRecoParticleEnergyAsym = 0;
  static TH2F *hRecoParticleEnergyAsymPi0 = 0;

  if (eventNumber==1) {

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
    delete hRecoParticleMinvPtTruePhotonCut;
    delete hRecoParticleMinvPtFiduAsymTruePhotonCut;

    delete hRecoParticleMinvPtCent;
    delete hRecoParticleMinvPtFiduCutCent;
    delete hRecoParticleMinvPtFiduAsymCutCent;
    delete hRecoParticleMinvPtFiduAsymChi2CutCent;
    delete hRecoParticleMinvPtFiduAsymTOFCutCent;
    delete hRecoParticleMinvPtFiduAsymChi2TOFCutCent;
    delete hRecoParticleMinvPtTruePhotonCutCent;
    delete hRecoParticleMinvPtFiduAsymTruePhotonCutCent;

    delete hRecoParticleMinvPtPeriph;
    delete hRecoParticleMinvPtFiduCutPeriph;
    delete hRecoParticleMinvPtFiduAsymCutPeriph;
    delete hRecoParticleMinvPtFiduAsymChi2CutPeriph;
    delete hRecoParticleMinvPtFiduAsymTOFCutPeriph;
    delete hRecoParticleMinvPtFiduAsymChi2TOFCutPeriph;
    delete hRecoParticleMinvPtTruePhotonCutPeriph;
    delete hRecoParticleMinvPtFiduAsymTruePhotonCutPeriph;

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

    delete hfiducial_local_yz0_geatrack ;
    delete hfiducial_local_yz0_geaclustertrack ;
    delete hfiducial_local_yz1_geatrack ;
    delete hfiducial_local_yz1_geaclustertrack ;

    delete hCentClass ;
    delete hTotalClusterMul ;
    delete hEmbedClusterMul ;
    delete hEmbedClusterMulCent ;
    delete hEmbedClusterMulPeriph ;

    delete hSimulSecondaryParticlePt;
    delete hRecoSecondaryParticlePt;

    delete hRecoParticleEnergyAsym;
    delete hRecoParticleEnergyAsymPi0;

    ReconstructionEfficiencyFile = new TFile(OutputEfficFileName.getString(),"recreate"); 

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
    hRecoParticleMinvPtTruePhotonCut   = 
      new TH2F("hRecoParticleMinvPtTruePhotonCut","hRecoParticleMinvPtTruePhotonCut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymTruePhotonCut = 
      new TH2F("hRecoParticleMinvPtFiduAsymTruePhotonCut","hRecoParticleMinvPtFiduAsymTruePhotonCut",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);

    hRecoParticleMinvPtCent = 
      new TH2F("hRecoParticleMinvPtCent","hRecoParticleMinvPtCent",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduCutCent = 
      new TH2F("hRecoParticleMinvPtFiduCutCent","hRecoParticleMinvPtFiduCutCent",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymCutCent = 
      new TH2F("hRecoParticleMinvPtFiduAsymCutCent","hRecoParticleMinvPtFiduAsymCutCent",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymChi2CutCent  = 
      new TH2F("hRecoParticleMinvPtFiduAsymChi2CutCent","hRecoParticleMinvPtFiduAsymChi2CutCent",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymTOFCutCent  = 
      new TH2F("hRecoParticleMinvPtFiduAsymTOFCutCent","hRecoParticleMinvPtFiduAsymTOFCutCent",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymChi2TOFCutCent  = 
      new TH2F("hRecoParticleMinvPtFiduAsymChi2TOFCutCent","hRecoParticleMinvPtFiduAsymChi2TOFCutCent",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtTruePhotonCutCent = 
      new TH2F("hRecoParticleMinvPtTruePhotonCutCent","hRecoParticleMinvPtTruePhotonCutCent",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymTruePhotonCutCent= 
      new TH2F("hRecoParticleMinvPtFiduAsymTruePhotonCutCent","hRecoParticleMinvPtFiduAsymTruePhotonCutCent",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);

    hRecoParticleMinvPtPeriph = 
      new TH2F("hRecoParticleMinvPtPeriph","hRecoParticleMinvPtPeriph",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduCutPeriph =
      new TH2F("hRecoParticleMinvPtFiduCutPeriph","hRecoParticleMinvPtFiduCutPeriph",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymCutPeriph =
      new TH2F("hRecoParticleMinvPtFiduAsymCutPeriph","hRecoParticleMinvPtFiduAsymCutPeriph",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymChi2CutPeriph  = 
      new TH2F("hRecoParticleMinvPtFiduAsymChi2CutPeriph","hRecoParticleMinvPtFiduAsymChi2CutPeriph",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymTOFCutPeriph  = 
      new TH2F("hRecoParticleMinvPtFiduAsymTOFCutPeriph","hRecoParticleMinvPtFiduAsymTOFCutPeriph",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymChi2TOFCutPeriph  = 
      new TH2F("hRecoParticleMinvPtFiduAsymChi2TOFCutPeriph","hRecoParticleMinvPtFiduAsymChi2TOFCutPeriph",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtTruePhotonCutPeriph =
      new TH2F("hRecoParticleMinvPtTruePhotonCutPeriph","hRecoParticleMinvPtTruePhotonCutPeriph",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);
    hRecoParticleMinvPtFiduAsymTruePhotonCutPeriph = 
      new TH2F("hRecoParticleMinvPtFiduAsymTruePhotonCutPeriph","hRecoParticleMinvPtFiduAsymTruePhotonCutPeriph",nMinvbin,0.,Minvmax,NumOfpTbins,pTmin,pTmax);

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

    hfiducial_local_yz0_geatrack = new TH2F("hfiducial_local_yz0_geatrack","hfiducial_local_yz0_geatrack",1000,-500.,500.,1000,-500.,500);
    hfiducial_local_yz1_geatrack = new TH2F("hfiducial_local_yz1_geatrack","hfiducial_local_yz1_geatrack",1000,-500.,500.,1000,-500.,500);
    hfiducial_local_yz0_geaclustertrack 
      = new TH2F("hfiducial_local_yz0_geaclustertrack","hfiducial_local_yz0_geaclustertrack",1000,-500.,500.,1000,-500.,500);
    hfiducial_local_yz1_geaclustertrack 
      = new TH2F("hfiducial_local_yz1_geaclustertrack","hfiducial_local_yz1_geaclustertrack",1000,-500.,500.,1000,-500.,500);

    hCentClass = new TH1F("hCentClass","hCentClass",100,-0.5,100.5);
    hTotalClusterMul = new TH1F("hTotalClusterMul","hTotalClusterMul",400,-0.5,399.5);
    hEmbedClusterMul = new TH1F("hEmbedClusterMul","hEmbedClusterMul",50,-0.5,49.5);
    hEmbedClusterMulCent = new TH1F("hEmbedClusterMulCent","hEmbedClusterMulCent",50,-0.5,49.5);
    hEmbedClusterMulPeriph = new TH1F("hEmbedClusterMulPeriph","hEmbedClusterMulPeriph",50,-0.5,49.5);

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
    hRecoParticleMinvPtTruePhotonCut->Sumw2();
    hRecoParticleMinvPtFiduAsymTruePhotonCut->Sumw2();

    hRecoParticleMinvPtCent->Sumw2();
    hRecoParticleMinvPtFiduCutCent->Sumw2();
    hRecoParticleMinvPtFiduAsymCutCent->Sumw2();
    hRecoParticleMinvPtFiduAsymChi2CutCent->Sumw2();
    hRecoParticleMinvPtFiduAsymTOFCutCent->Sumw2();
    hRecoParticleMinvPtFiduAsymChi2TOFCutCent->Sumw2();
    hRecoParticleMinvPtTruePhotonCutCent->Sumw2();
    hRecoParticleMinvPtFiduAsymTruePhotonCutCent->Sumw2();

    hRecoParticleMinvPtPeriph->Sumw2();
    hRecoParticleMinvPtFiduCutPeriph->Sumw2();
    hRecoParticleMinvPtFiduAsymCutPeriph->Sumw2();
    hRecoParticleMinvPtFiduAsymChi2CutPeriph->Sumw2();
    hRecoParticleMinvPtFiduAsymTOFCutPeriph->Sumw2();
    hRecoParticleMinvPtFiduAsymChi2TOFCutPeriph->Sumw2();
    hRecoParticleMinvPtTruePhotonCutPeriph->Sumw2();
    hRecoParticleMinvPtFiduAsymTruePhotonCutPeriph->Sumw2();

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

  Int_t pid = 0 ;
  Float_t partmass = 0. ;
  Float_t Pi0pT = 0. ; 
  Float_t Pi0Zvtx_z = 0. ; 
  Float_t hagWeight = 1.;
  Float_t hagWeight2 = 1.;
  Float_t p0hag = 1.72;   // Param. Hagedorn fit Au+Au->pi0
  Float_t nhag  = -12.0;  // Param. Hagedorn fit Au+Au->pi0 
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

 for( iTracks=0; iTracks<NumOfTracks; iTracks++ ) 
   {     
     // 2.1) pT weight:
     // Look for the primary pi0 just to
     // determine the power-law weight of its associated pT
     if ( ( dEmcGeaTrack->get_idparent(iTracks) == 0 )  &&   // this is a primary (if negative: PID*ancestry level)
	  ( dEmcGeaTrack->get_pid(iTracks) == 7 )      &&    // this is a pion 
	  ( dEmcGeaTrack->get_pxyz(0,iTracks) > 0.0 )  &&    // heading West
	  ( !Pi0pT ) )                                       // Once we got it (from 1st ancestry level) needless to get it again
       {
	 SimulPi0 = new TParticle(PDG_pi0,0,0,0,0,0,dEmcGeaTrack->get_pxyz(0,iTracks),dEmcGeaTrack->get_pxyz(1,iTracks), 
				  dEmcGeaTrack->get_pxyz(2,iTracks),
				  //TMath::Sqrt(pow(dEmcGeaTrack->get_ptot(iTracks),2)+pow(pi0mass,2)),0.,0.,0.,0.);
				  //dEmcGeaTrack->get_ptot(iTracks),0.,0.,0.,0.);
	                          dEmcGeaTrack->get_ekin(iTracks)+pi0mass,0.,0.,0.,0.); 
	 Pi0pT = SimulPi0->Pt(); 
	 Pi0Zvtx_z = dEmcGeaTrack->get_xyz(2,iTracks); 
         //hagWeight = TMath::Power(Pi0pT,-7.02);  // Weight from measured run-01 pi0 spectrum Au+Au @ 130A GeV
	 hagWeight = TMath::Power((Pi0pT+p0hag)/p0hag,nhag); // Weight from measured run-01 pi0 Au+Au @ 130A GeV
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
     // actually entering in EMCAL (ancestry == 1)  and heading West:
     if  ( ( dEmcGeaTrack->get_anclvl(iTracks) == 1 )  &&
	   ( dEmcGeaTrack->get_pxyz(0,iTracks) > 0.0 ) )//&& // heading West
         //( in_fiducial_area(iTracks,dEmcGeaTrack,0) ) ) // Fiducial cuts Run-1   
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
  Int_t CounterOfClustersCent = 0;
  Int_t CounterOfClustersPeriph = 0;
  Float_t Distance = 0;
  Float_t x, y, z ;
  Float_t px, py, pz ;
  Int_t in_fiducial = 0;
  Int_t dead = 0;
  Int_t warn = 0;

  // REAL-EVENT CENTRALITY CLASS
  // NOTE: "input" field of dEmcGeaTrack contains now the real event centrality class !
  Int_t centClass = dEmcGeaTrack->get_input(0); // the same centClass info is stored for all tracks 0,1,...
  // NOTE: "charged" field contains now the real evt. multiplicity !
  Int_t real_evt_mult = dEmcGeaClusterTrack->get_charged(0); // the same mult. info is stored for all tracks 0,1,...

  Float_t flight_time = 19.7 ; // 17+-1 ns (path) + 2.7 ns (pulse) 

  for( iClusters=0; iClusters<NumOfClusters; iClusters++ ) 
    {

      // CLUSTERS WITH A CONTRIBUTED GEANT TRACK (should be all of them now in the embed+eval file ...)
      if ( ( dEmcGeaClusterTrack->get_trkno(0,iClusters) )        && // trkno !=0 --> geant track contributing
	   ( dEmcGeaClusterTrack->get_measxyz(0,iClusters) > 0. ) )//&& // and hitting the West arm
	 //( in_fiducial_area(iClusters,0,dEmcGeaClusterTrack) ) )   // Fiducial cuts Run-1   
	{
	  
	  // Fill general EMCal cluster histograms
	  hArm->Fill(dEmcGeaClusterTrack->get_arm(iClusters));
	  hSector->Fill(dEmcGeaClusterTrack->get_sector(iClusters));
	  hMeasx->Fill(dEmcGeaClusterTrack->get_measxyz(0,iClusters));
	  hMeasy->Fill(dEmcGeaClusterTrack->get_measxyz(1,iClusters));
	  hMeasz->Fill(dEmcGeaClusterTrack->get_measxyz(2,iClusters));
	  if (dEmcGeaClusterTrack->get_measxyz(0,iClusters)<0.) // East (should be empty now)
	    hMeasyz_e->Fill(dEmcGeaClusterTrack->get_measxyz(2,iClusters),dEmcGeaClusterTrack->get_measxyz(1,iClusters));
	  if (dEmcGeaClusterTrack->get_measxyz(0,iClusters)>0.) // West
	    hMeasyz_w->Fill(dEmcGeaClusterTrack->get_measxyz(2,iClusters),dEmcGeaClusterTrack->get_measxyz(1,iClusters));
	  hMease->Fill(dEmcGeaClusterTrack->get_mease(iClusters));
	  hEcore->Fill(dEmcGeaClusterTrack->get_ecore(iClusters));
	  hTof->Fill( dEmcGeaClusterTrack->get_tof(iClusters) - flight_time );
	  hProbPhot->Fill(dEmcGeaClusterTrack->get_prob_photon_sh(iClusters));
	  hChi2->Fill(dEmcGeaClusterTrack->get_chi2_sh(iClusters));
	  
	  x = dEmcGeaClusterTrack->get_measxyz(0,iClusters)*dEmcGeaClusterTrack->get_measxyz(0,iClusters);
	  y = dEmcGeaClusterTrack->get_measxyz(1,iClusters)*dEmcGeaClusterTrack->get_measxyz(1,iClusters);
	  z = dEmcGeaClusterTrack->get_measxyz(2,iClusters)-Pi0Zvtx_z; // correction for z_vtx position
	  Distance = TMath::Sqrt( x*x + y*y + z*z );
	  px = dEmcGeaClusterTrack->get_ecore(iClusters)*x/Distance ;
	  py = dEmcGeaClusterTrack->get_ecore(iClusters)*y/Distance ;
	  pz = dEmcGeaClusterTrack->get_ecore(iClusters)*z/Distance ;
	  
	  // PID of the dominant GEANT contributor to the cluster
	  if      ( dEmcGeaClusterTrack->get_pid(0,iClusters)==1 ) pid = PDG_gam;
	  else if ( dEmcGeaClusterTrack->get_pid(0,iClusters)==2 ) pid = PDG_eplus;
	  else if ( dEmcGeaClusterTrack->get_pid(0,iClusters)==3 ) pid = PDG_elec;
	  else  pid = -999 ; // unknown
	  
	  // Fiducial cuts Run-1 
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

	  if (dead*warn) cout << "dead: " << dead  << " warn: " << warn << endl;

	  CounterOfClusters++;
	  if ( centClass <= 10 ) CounterOfClustersCent++; // centClass --> 0%-10% 
	  if ( centClass >= 60 && centClass <= 80 ) CounterOfClustersPeriph++; // centClass --> 60%-80% 
	  
	} // if have some GEANT track contributor and head west
      else if ( !(dEmcGeaClusterTrack->get_trkno(0,iClusters)) )
	{
	  cout << "<W> No simulated track found in this cluster !!" << endl;
	}
    }  // loop on all EMCal clusters
  
  hCentClass->Fill((Float_t)centClass);
  hTotalClusterMul->Fill(real_evt_mult);
  hEmbedClusterMul->Fill(CounterOfClusters);
  hEmbedClusterMulCent->Fill(CounterOfClustersCent);
  hEmbedClusterMulPeriph->Fill(CounterOfClustersPeriph);
  //hTotalClusterMul->Fill(NumOfClusters);

  Int_t iClusters2 = 0 ;
  Float_t ReCal = 1. ;
  Float_t asymmetry = 0.;
  Float_t inv_mass_pair_reco ;
  Float_t pt_pair_reco ;
  Bool_t CENT10  = false;
  Bool_t CENT60_80 = false;
  Bool_t FIDUCUT = false;
  Bool_t ASYMCUT = false;
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
	      //ReCal = 0.955;
	      ReCal = 9.4734e-01 //9.54364e-01                
		    + 1.08902e-02*momentum2.Pt() 
		    - 8.31507e-04*momentum2.Pt()*momentum2.Pt() 
		    - 1.01014e-04*momentum2.Pt()*momentum2.Pt()*momentum2.Pt() ;
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
	  
	  // Asym + Low-energy clusters (<100 MeV) cut
	  if ( asymmetry<0.8 && (Particle->Energy()<Min_Cluster_Energy) && (Particle2->Energy()<Min_Cluster_Energy) )
	    ASYMCUT = true;
	  else ASYMCUT = false ;

	  // DEAD/WARN
	  Int_t dead_warn = (Particle->GetMother(0)  | Particle->GetMother(1) | 
			     Particle2->GetMother(0) | Particle2->GetMother(1) );
	  if ( dead_warn == 0 ) DEADWARNCUT = true;
	  else DEADWARNCUT = false ;

	  // Chi2
	  if ( ( TMath::Abs(Particle->Vz())<3.0 ) && 
	       ( TMath::Abs(Particle2->Vz())<3.0 ) ) CHI2CUT = true;
	  else  CHI2CUT = false ;

	  // TOF (official cut is 2.8 ns but +0.2 to take into account cluster position)
	  if ( ( TMath::Abs(Particle->T() -flight_time)<3.0 ) && 
	       ( TMath::Abs(Particle2->T()-flight_time)<3.0 ) ) TOFCUT = true;
	  else TOFCUT = false ;

	  // Dominant contributor to the cluster:  GEANT ("true") Photon PID cut
	  // Reference cut to assess how far "true PID photon" == "photon-like Chi2 * TOF cuts"
	  if ( ( Particle->GetPdgCode()  == PDG_gam ) && 
	       ( Particle2->GetPdgCode() == PDG_gam ) ) TRUEPHOTON = true ;
	  else TRUEPHOTON = false ;

	  // 10% Central collisions
	  if ( centClass <= 10 ) CENT10 = true ; // centClass --> 0%-10% 
	  else CENT10 = false ;

	  // 60%-80% Peripheral collisions
	  if ( centClass >= 60 && centClass <= 80 ) CENT60_80 = true ; // centClass --> 60%-80% 
	  else CENT60_80 = false ;

	  //_____________________________________________________________________________
	  
	  // No CUTS: All clusters participate to the Invariant Mass analysis: 
	  hRecoParticleMinvAllPairs->Fill( inv_mass_pair_reco, hagWeight );
	  binMinv = hRecoParticleMinvAllPairs->FindBin( inv_mass_pair_reco );
	  errorWeight = TMath::Sqrt(pow( hRecoParticleMinvAllPairs->GetBinError(binMinv),2) + hagWeight2); 
	  hRecoParticleMinvAllPairs->SetBinError( binMinv, errorWeight );
	  
	  Fill_Minv_pT_bidim_histo( hRecoParticleMinvPt, inv_mass_pair_reco, pt_pair_reco, hagWeight );

	  if ( FIDUCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( FIDUCUT && ASYMCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( FIDUCUT && ASYMCUT && CHI2CUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2Cut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( FIDUCUT && ASYMCUT && CHI2CUT && TOFCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2TOFCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( FIDUCUT && ASYMCUT && TOFCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymTOFCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  } 
	  if ( FIDUCUT && TRUEPHOTON ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtTruePhotonCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if (  FIDUCUT && ASYMCUT && TRUEPHOTON ) { 
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymTruePhotonCut, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  
	  // ----------------------------------------------------------
	  // 10% CENTRAL COLLISIONS 
	  
	  if ( CENT10 ) {	    
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtCent, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( CENT10 && FIDUCUT ) {	    
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduCutCent, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( CENT10 && FIDUCUT && ASYMCUT ) { 
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymCutCent, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  } 
	  if ( CENT10 && FIDUCUT && ASYMCUT && CHI2CUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2CutCent, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }    
	  if ( CENT10 && FIDUCUT && ASYMCUT && CHI2CUT && TOFCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2TOFCutCent, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  } 
	  if ( CENT10 && FIDUCUT && ASYMCUT && TOFCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymTOFCutCent, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  } 
	  if ( CENT10 && FIDUCUT && TRUEPHOTON ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtTruePhotonCutCent, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( CENT10 && FIDUCUT && ASYMCUT && TRUEPHOTON ) { 
	    Fill_Minv_pT_bidim_histo(  hRecoParticleMinvPtFiduAsymTruePhotonCutCent, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  } 	  
	  
	  // ----------------------------------------------------------
	  // 60%-80% PERIPHERAL COLLISIONS 
	  
	  if ( CENT60_80 ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtPeriph, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }  
	  if ( CENT60_80 && FIDUCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduCutPeriph, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }  
	  if ( CENT60_80 && FIDUCUT && ASYMCUT ) { 
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymCutPeriph, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }  
	  if ( CENT60_80 && FIDUCUT && ASYMCUT && CHI2CUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2CutPeriph, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }  
	  if ( CENT60_80 && FIDUCUT && ASYMCUT && CHI2CUT && TOFCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymChi2TOFCutPeriph, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( CENT60_80 && FIDUCUT && ASYMCUT && TOFCUT ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymTOFCutPeriph, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( CENT60_80 && FIDUCUT && TRUEPHOTON ) {
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtTruePhotonCutPeriph, inv_mass_pair_reco, pt_pair_reco, hagWeight );
	  }
	  if ( CENT60_80 && FIDUCUT && ASYMCUT && TRUEPHOTON ) { 
	    Fill_Minv_pT_bidim_histo( hRecoParticleMinvPtFiduAsymTruePhotonCutPeriph, inv_mass_pair_reco, pt_pair_reco, hagWeight );
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
  if (eventNumber == maxEvents)
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
      hRecoParticleMinvPtTruePhotonCut->Write();
      hRecoParticleMinvPtFiduAsymTruePhotonCut->Write();

      hRecoParticleMinvPtCent->Write();
      hRecoParticleMinvPtFiduCutCent->Write();
      hRecoParticleMinvPtFiduAsymCutCent->Write();
      hRecoParticleMinvPtFiduAsymChi2CutCent->Write();
      hRecoParticleMinvPtFiduAsymTOFCutCent->Write();
      hRecoParticleMinvPtFiduAsymChi2TOFCutCent->Write();
      hRecoParticleMinvPtTruePhotonCutCent->Write();
      hRecoParticleMinvPtFiduAsymTruePhotonCutCent->Write();

      hRecoParticleMinvPtPeriph->Write();
      hRecoParticleMinvPtFiduCutPeriph->Write();
      hRecoParticleMinvPtFiduAsymCutPeriph->Write();
      hRecoParticleMinvPtFiduAsymChi2CutPeriph->Write();
      hRecoParticleMinvPtFiduAsymTOFCutPeriph->Write();
      hRecoParticleMinvPtFiduAsymChi2TOFCutPeriph->Write();
      hRecoParticleMinvPtTruePhotonCutPeriph->Write();
      hRecoParticleMinvPtFiduAsymTruePhotonCutPeriph->Write();

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
      hEmbedClusterMulCent->Write() ;
      hEmbedClusterMulPeriph->Write() ;
      
      hSimulSecondaryParticlePt->Write();
      hRecoSecondaryParticlePt->Write();
      hRecoParticleEnergyAsym->Write();
      hRecoParticleEnergyAsymPi0->Write();
      
      hfiducial_local_yz0_geatrack->Write();
      hfiducial_local_yz1_geatrack->Write();
      hfiducial_local_yz0_geaclustertrack->Write();
      hfiducial_local_yz1_geaclustertrack->Write();

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

 // Expected (from simulated reference)
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
     delete hMinv_fit;
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

 //_____________________________________________________________________________
 // Central Efficiencies

 // Normalization
 TH1F *hCentClass = (TH1F*) in.Get("hCentClass");
 Float_t normalizationCent = hCentClass->Integral()/hCentClass->Integral(1,2);

 // 0. No Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtCent");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
 // 1. Fidu Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduCutCent");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
 // 2. FiduAsym Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymCutCent");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
 // 3. GEANT ("true") Photon PID * FiduAsym Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymTruePhotonCutCent");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
 // 4. FiduAsym * Chi2 Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymChi2CutCent");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
 // 5. FiduAsym * TOF Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymTOFCutCent");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );
 // 6. FiduAsym * Chi2 * TOF Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymChi2TOFCutCent");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationCent );

 //_____________________________________________________________________________
 // Peripheral Efficiencies

 // Normalization
 Float_t normalizationPeriph = hCentClass->Integral()/hCentClass->Integral(13,16);

 // 0. No Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtPeriph");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationPeriph);
 // 1. Fidu Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduCutPeriph");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationPeriph);
 // 2. FiduAsym Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymCutPeriph");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationPeriph);
 // 3. GEANT ("true") Photon PID * FiduAsym Cut
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymTruePhotonCutPeriph");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationPeriph);
 // 4. FiduAsym*Chi2 Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymChi2CutPeriph");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationPeriph);
 // 5. FiduAsym*Chi2*TOF Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymTOFCutPeriph");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationPeriph);
 // 6. FiduAsym*Chi2*TOF Cuts
 hRecoParticle = (TH2F*) in.Get("hRecoParticleMinvPtFiduAsymChi2TOFCutPeriph");
 efficiency( hRecoParticle, InputEfficiencyFile, normalizationPeriph);

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
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up input file
  PHNodeIOManager* dstIn = new PHNodeIOManager(dstInFile,PHReadOnly);
  dstIn->selectObjectToRead("*",false);
  dstIn->selectObjectToRead("EVA/dEmcGeaTrack",true);
  dstIn->selectObjectToRead("EVA/dEmcGeaClusterTrack",true);
  //dstIn->selectObjectToRead("EVA/dEmcGeaTrackCluster",true);
  dstIn->read(evaNode);

  // Initialize the tables
  PHIODataNode<PHTable>* dEmcGeaTrackNode = 
    (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcGeaTrack");
  PHIODataNode<PHTable>* dEmcGeaClusterTrackNode = 
    (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcGeaClusterTrack");
//   PHIODataNode<PHTable>* dEmcGeaTrackClusterNode = 
//     (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcGeaTrackCluster");

  dEmcGeaTrackWrapper*         dEmcGeaTrack        = 0;
  dEmcGeaClusterTrackWrapper*  dEmcGeaClusterTrack = 0;
  //dEmcGeaTrackClusterWrapper*  dEmcGeaTrackCluster = 0; 

// STARTING THE EVENT LOOP
  Int_t eventNumber = 1;

  while (eventNumber <= maxEvents) 
    {
      if (eventNumber == 1)  dstIn->print();

      if (eventNumber >= 1) 
	{
	  dstIn->read(evaNode);
	}
      if (!(eventNumber%1000)) cout << " event " << eventNumber << flush;
      if (!(eventNumber%100))  cout << "." << flush;

      if (!dEmcGeaTrackNode) {
	cout << "Dst Read: Could not find data node dEmcGeaTrack" << endl;
      } 
      else {
	dEmcGeaTrack = 
	  (dEmcGeaTrackWrapper*)dEmcGeaTrackNode->getData();
      }
      
      if (!dEmcGeaClusterTrackNode) {
	cout << "Dst Read: Could not find data node dEmcGeaClusterTrack" << endl;
      } 
      else {
	dEmcGeaClusterTrack = 
	  (dEmcGeaClusterTrackWrapper*)dEmcGeaClusterTrackNode->getData();
      }
      
//       if (!dEmcGeaTrackClusterNode) {
// 	cout << "Dst Read: Could not find data node dEmcGeaTrackCluster" << endl;
//       } 
//       else {
// 	dEmcGeaTrackCluster = 
// 	  (dEmcGeaTrackClusterWrapper*)dEmcGeaTrackClusterNode->getData();
//       }

      // pi0 reconstruction
      pi0_reconstruction( dEmcGeaTrack , dEmcGeaClusterTrack, eventNumber, maxEvents );
      
      // Reset all data nodes for this event
      mainIter.cd();
      if (mainIter.cd("TOP")) {
	mainIter.forEach(reset);
	mainIter.cd();
      }
      mainIter.cd();
      if (mainIter.cd("EVA")) {
	mainIter.forEach(reset);
	mainIter.cd();
      }
      
      eventNumber++;
    }
}
