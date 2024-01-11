//************************************************************
// Acceptance Calculations in EMCAL
//        
// Copyright (C) PHENIX collaboration, 2001-2004
//
// Authors: Gines MARTINEZ & David D'ENTERRIA - SUBATECH, 2002
// Authors: David D'ENTERRIA & Jiamin Jin - NEVIS LABS, 2003
//
//************************************************************
/*
_____________________________________________________________________________

Description: Macro to calculate EMCal (Full, PbSc, PbGl) 
             geometrical acceptances for pi0, eta, K0short 
	     for Run-1 and Run-2 via (ROOT Random) 'fast MC'
_____________________________________________________________________________

To compile the macro:

gSystem->Load("libpreco.so");
gSystem->Load("libemc.so"); // for EMCal geometry
gSystem->Load("libEG.so");  // for TParticle (ROOT)
.includepath $OFFLINE_MAIN/include
.L EmcalAcceptanceAnalysis.C++

OR to run directly the macro:

gSystem->Load("libpreco.so");
gSystem->Load("libemc.so");
gSystem->Load("libEG.so");
gSystem->Load("EmcalAcceptanceAnalysis_C.so");

get_acceptance_from_fast_MC(1000000,1,2,"pi0",20.,"real")

and then, e.g.:

fit_acceptance("Acceptance_pi0_PbSc_Run2.root");

to plot your results.

*/

#include "TFile.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TMath.h"
#include "TRandom.h"
#include "TString.h"
#include "TParticle.h"
#include "TVector3.h"
#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TStyle.h"
#include "TCanvas.h"

#include "mEmcGeometryModule.h"

// A few global variables

static double ymax = 0.5;
static double phimax = 180.;
static double ptbinsize = 0.05;

static int PDG_gam   =  22;
static int PDG_pi0   = 111;
static int PDG_eta   = 221;
static int PDG_k0short = 310;

static int verbose  = 1;

static mEmcGeometryModule* EmcGeom = 0 ;

static TH1F *hMinv_photons = 0 ;
static TH2F *hAcc_theta_phi_photons = 0 ;

static TH1F *hAcc_pT_meson = 0 ;
static TH1F *hAcc_pT_meson_noweighted = 0 ;
static TH1F *hAcc_pT_decayphoton = 0 ;

static TH2F *hAcc_pT_y_meson = 0 ;
static TH2F *hAcc_pT_y_photons = 0 ;

static TH2F *hAcc_pT_vs_asym_meson = 0 ;

static TH1F *h_input_pT_meson = 0 ;
static TH2F *hAcc_pT_y_meson2 = 0 ;

static TF1 *powlaw = 0;

//_____________________________________________________________________________
//
int
sectorToArm( int sector)
{

  if (sector>=0 && sector<4)
    {
      return 0; // WEST arm (online convention)
    }
  else if(sector>=4 && sector<8)
    { 
      return 1; // EAST arm (online convention)
    }

  return -1;

}

//_____________________________________________________________________________
//

void init_geom(const char* geo = "PISA")
{
  if (strcmp(geo,"PISA") == 0)
    {
      EmcGeom = new mEmcGeometryModule(mEmcGeometryModule::kPISA); 
    }
  else
    {
      EmcGeom = new mEmcGeometryModule(mEmcGeometryModule::kReal);
    }
}

//_____________________________________________________________________________
//
// If particle heads towards any of the active sectors of Subsystem PbSc/PbGl/EMCal
// as of Year 1/2, then return the arm value, else return -1

int
Impact( TParticle *particle, int Subsystem, int Year )
{


  Bool_t bSector[8];

  float v[3];
  float p[3];

  p[0] = particle->Px();
  p[1] = particle->Py();
  p[2] = particle->Pz();

  v[0] = particle->Vx();
  v[1] = particle->Vy();
  v[2] = particle->Vz();

  for (int i=0; i<8; i++) 
    {
      bSector[i] = kFALSE;
    }

  int sector = -1; 
  sector = EmcGeom->HitInEMCalAcceptance(v, p); // Returns "Sector" (iS=0-7) if hit. Returns -1 if no sector hit

  if (sector>-1) bSector[sector] = kTRUE;
 
  
  if ( (Subsystem==0) && (Year==1) && 
       (bSector[0]+bSector[1]+bSector[7]) )
    {
      return sectorToArm(sector);
    }

  if ( (Subsystem==0) && (Year==2) &&
       (bSector[0]+bSector[1]+bSector[2]+bSector[3]+bSector[4]+bSector[5]+bSector[6]+bSector[7]) )
    {
      return sectorToArm(sector) ;
    }
  
  if ( (Subsystem==1) && (Year==1) && (bSector[0]+bSector[1]) )
    {
      return sectorToArm(sector) ;
    }

  if ( (Subsystem==1) && (Year==2) && (bSector[0]+bSector[1]+bSector[2]+bSector[3]+bSector[4]+bSector[5]) )
    {
      return sectorToArm(sector) ;
    }

  if ( (Subsystem==2) && (Year==1) && (bSector[7]))
    {
      return sectorToArm(sector) ;
    }

  if ( (Subsystem==2) && (Year==2) && (bSector[6]+bSector[7]) )
    {
      return sectorToArm(sector) ;
    }

  if ( (Subsystem==3) && (bSector[0]) )
    {
      return sectorToArm(sector) ;
    }

  return -1;

}

//_____________________________________________________________________________
// Histograms booking

void 
book_histos( double pTmax = 20.)
{

  int npTbins = (int)(pTmax/ptbinsize); // 0.05 GeV/c bins

  if (hMinv_photons) delete hMinv_photons;
  if (hAcc_theta_phi_photons) delete hAcc_theta_phi_photons;
  
  if (hAcc_pT_meson) delete hAcc_pT_meson;
  if (hAcc_pT_meson_noweighted) delete hAcc_pT_meson_noweighted;
  if (hAcc_pT_decayphoton) delete hAcc_pT_decayphoton;
  
  if (hAcc_pT_y_meson) delete hAcc_pT_y_meson;
  if (hAcc_pT_y_photons) delete hAcc_pT_y_photons;
  
  if (hAcc_pT_vs_asym_meson) delete hAcc_pT_vs_asym_meson;
  
  if (h_input_pT_meson) delete h_input_pT_meson;
  if (hAcc_pT_y_meson) delete hAcc_pT_y_meson;
    
  hAcc_pT_meson = new TH1F("hAcc_pT_meson","hAcc_pT_meson",npTbins,0.,pTmax);
  hAcc_pT_meson->Sumw2(); 

  h_input_pT_meson = new TH1F("h_input_pT_meson","h_input_pT_meson",npTbins,0.,pTmax);
  h_input_pT_meson->Sumw2(); 


  int npTbins2 = (int)(pTmax/0.5); // 0.5 GeV/c bins
  hAcc_pT_meson_noweighted = new TH1F("hAcc_pT_meson_noweighted","hAcc_pT_meson_noweighted",npTbins2,0.,pTmax);
  hAcc_pT_meson_noweighted->Sumw2(); 

  hMinv_photons = new TH1F("hMinv_photons","hMinv_photons",100,0.,1.);

  hAcc_theta_phi_photons = new TH2F("hAcc_theta_phi_photons","hAcc_theta_phi_photons",360,-phimax,phimax,180,0.,180.);

  hAcc_pT_y_photons = new TH2F("hAcc_pT_y_photons","hAcc_pT_y_photons",npTbins,0.,pTmax,20,-ymax,ymax);

  hAcc_pT_decayphoton = new TH1F("hAcc_pT_decayphoton","hAcc_pT_decayphoton",npTbins,0.,pTmax);
  
  hAcc_pT_y_meson = new TH2F("hAcc_pT_y_meson","hAcc_pT_y_meson",npTbins,0.,pTmax,20,-ymax,ymax);

  hAcc_pT_y_meson2 = new TH2F("hAcc_pT_y_meson2","hAcc_pT_y_meson2",npTbins,0.,pTmax,20,-ymax,ymax);
  
  hAcc_pT_vs_asym_meson = new TH2F("hAcc_pT_vs_asym_meson","hAcc_pT_vs_asym_meson",npTbins,0.,pTmax,20,0.,1.);

  return;
}
//_____________________________________________________________________________
// Fills/Writes the acceptance histos

void 
fill_histos( TClonesArray *ListOfPhotonParticles, 
	     TParticle *inputMesonParticle, 
	     int eventNumber, int maxEvents, 
	     int Subsystem = 1 , int Year = 2, 
	     const char *Particle_type = "pi0", 
	     double pTmax = 20. )
{

  int iphoton1, iphoton2;
  int iphoton3, iphoton4;

  TParticle *Photon1 = 0;
  TParticle *Photon2 = 0;
  TParticle *Photon3 = 0;
  TParticle *Photon4 = 0;

  TLorentzVector MesonMomentum;
  TLorentzVector Photon1Momentum, Photon2Momentum;
  TLorentzVector Photon3Momentum, Photon4Momentum;

  int numberOfPhotons =  ListOfPhotonParticles->GetEntries();
  if (verbose) cout << ">>> numberOfPhotons is " << numberOfPhotons << endl;

  inputMesonParticle->Momentum(MesonMomentum);
  double pTweight = powlaw->Eval(MesonMomentum.Pt());

  // Reference input pT histogram ("denominator" histo in Acceptance)

  h_input_pT_meson->Fill(MesonMomentum.Pt(), pTweight);

  // Both photons hitting EMCal

  if ((strcmp(Particle_type,"eta") ==0 || strcmp(Particle_type,"eta")==0) && numberOfPhotons==2)
    {
      hAcc_pT_meson->Fill(MesonMomentum.Pt(),pTweight);
      hAcc_pT_meson_noweighted->Fill(MesonMomentum.Pt());
      hAcc_pT_y_meson->Fill( MesonMomentum.Pt(), MesonMomentum.PseudoRapidity());
    }
  if((strcmp(Particle_type,"k0short") == 0) && numberOfPhotons == 4)
    {
      hAcc_pT_meson->Fill(MesonMomentum.Pt(),pTweight);
      hAcc_pT_meson_noweighted->Fill(MesonMomentum.Pt());
      hAcc_pT_y_meson->Fill( MesonMomentum.Pt(), MesonMomentum.PseudoRapidity());
    }

  // Photon pairs invariant mass analysis

  for( iphoton1=0; iphoton1<numberOfPhotons; iphoton1++) 
    {

      double Asymmetry=0.;
      Photon1 = ((TParticle *) (*ListOfPhotonParticles)[iphoton1]);
      Photon1->Momentum(Photon1Momentum);

      hAcc_theta_phi_photons->Fill( Photon1Momentum.Phi()*TMath::RadToDeg(), 
				    Photon1Momentum.Theta()*TMath::RadToDeg() );

      hAcc_pT_y_photons->Fill( Photon1Momentum.Pt(), Photon1Momentum.PseudoRapidity());

      hAcc_pT_decayphoton->Fill( MesonMomentum.Pt() );

      for( iphoton2=(iphoton1+1); iphoton2<numberOfPhotons; iphoton2++) 
	{
	  Photon2 = ((TParticle *) (*ListOfPhotonParticles)[iphoton2]);
	  Photon2->Momentum(Photon2Momentum);
	  Photon2Momentum+=Photon1Momentum;

	  Asymmetry = TMath::Abs(Photon1Momentum.Energy()-Photon2Momentum.Energy());
	  Asymmetry /= (Photon1Momentum.Energy()+Photon2Momentum.Energy());     
	  hAcc_pT_vs_asym_meson->Fill ( MesonMomentum.Pt(), Asymmetry ); 

	  if ( (strcmp(Particle_type,"k0short") != 0) ) // pi0, eta
	    {
	      hMinv_photons->Fill(Photon2Momentum.M());
	      //h_input_pT_meson->Fill(Photon2Momentum.Pt(),pTweight);
	      hAcc_pT_y_meson2->Fill( Photon2Momentum.Pt(), Photon2Momentum.PseudoRapidity());
	    }
	  else // k0short
	    {
	      for( iphoton3=(iphoton2+1); iphoton3<numberOfPhotons; iphoton3++) 
		{
		  Photon3 = ((TParticle *) (*ListOfPhotonParticles)[iphoton3]);
		  Photon3->Momentum(Photon3Momentum);
		  Photon3Momentum+=Photon2Momentum;
		  
		  for( iphoton4=(iphoton3+1); iphoton4<numberOfPhotons; iphoton4++) 
		    {
		      Photon4 = ((TParticle *) (*ListOfPhotonParticles)[iphoton4]);
		      Photon4->Momentum(Photon4Momentum);
		      Photon4Momentum+=Photon3Momentum;
		      
		      hMinv_photons->Fill(Photon4Momentum.M());
		      //h_input_pT_meson->Fill(Photon4Momentum.Pt(),pTweight);
		      hAcc_pT_y_meson2->Fill( Photon4Momentum.Pt(), Photon4Momentum.PseudoRapidity());
		    }
		}
	    }
	}
    }

  if (eventNumber == maxEvents)
    {

      // Normalize histos
      
      double normalization = 0;

      hAcc_pT_meson->Divide(h_input_pT_meson);

      normalization = hAcc_pT_meson_noweighted->GetNbinsX()/((double) maxEvents) ;
      hAcc_pT_meson_noweighted->Sumw2(); 
      hAcc_pT_meson_noweighted->Scale(normalization);

//       normalization = hAcc_pT_decayphoton->GetNbinsX()/((double) maxEvents) ;
//       hAcc_pT_decayphoton->Sumw2(); 
//       hAcc_pT_decayphoton->Scale(normalization);

//       normalization = hAcc_pT_y_meson->GetNbinsX()*hAcc_pT_y_meson->GetNbinsY()/((double)  maxEvents);
//       hAcc_pT_y_meson->Sumw2(); 
//       hAcc_pT_y_meson->Scale(normalization);

//       normalization = hAcc_pT_y_photons->GetNbinsX()*hAcc_pT_y_photons->GetNbinsY()/((double)  maxEvents);
//       hAcc_pT_y_photons->Sumw2(); 
//       hAcc_pT_y_photons->Scale(normalization);

//       normalization = h_input_pT_meson->GetNbinsX()/((double) maxEvents) ;
//       h_input_pT_meson->Sumw2(); 
//       h_input_pT_meson->Scale(normalization);
//       normalization = hAcc_pT_y_meson2->GetNbinsX()*hAcc_pT_y_meson2->GetNbinsY()/((double) maxEvents);
//       hAcc_pT_y_meson2->Sumw2(); 
//       hAcc_pT_y_meson2->Scale(normalization);

      // Write histos

      char outputfilename1[200];
      char outputfilename2[200];
      
      if(Subsystem==0)  sprintf(outputfilename1,"Acceptance_%s_Full_EMCal",Particle_type);
      if(Subsystem==1)  sprintf(outputfilename1,"Acceptance_%s_PbSc",Particle_type);
      if(Subsystem==2)  sprintf(outputfilename1,"Acceptance_%s_PbGl",Particle_type);
      if(Subsystem==3)  sprintf(outputfilename1,"Acceptance_%s_PbScW0Sector",Particle_type);

      if(Year==1) sprintf(outputfilename2,"%s_Run1.root",outputfilename1);
      if(Year==2) sprintf(outputfilename2,"%s_Run2.root",outputfilename1);

      TFile *outputfile = new TFile(outputfilename2,"recreate");

      hMinv_photons->Write("hMinv_photons",TObject::kOverwrite);
      hAcc_theta_phi_photons->Write("hAcc_theta_phi_photons",TObject::kOverwrite);

      hAcc_pT_meson->Write("hAcc_pT_meson",TObject::kOverwrite);
      hAcc_pT_meson_noweighted->Write("hAcc_pT_meson_noweighted",TObject::kOverwrite);
      hAcc_pT_decayphoton->Write("hAcc_pT_decayphoton",TObject::kOverwrite);

      hAcc_pT_y_meson->Write("hAcc_pT_y_meson",TObject::kOverwrite);
      hAcc_pT_y_photons->Write("hAcc_pT_y_photons",TObject::kOverwrite);

      hAcc_pT_vs_asym_meson->Write("hAcc_pT_vs_asym_meson",TObject::kOverwrite);

      h_input_pT_meson->Write("h_input_pT_meson",TObject::kOverwrite);
      hAcc_pT_y_meson2->Write("hAcc_pT_y_meson2",TObject::kOverwrite);

      outputfile->Close();
      cout << endl << "Output acceptance file: " << outputfilename2 << endl;
    }

}

//_____________________________________________________________________________
//

void fill_ListOfPhotonParticles( TClonesArray *ListOfPhotonParticles, 
				 TParticle *inputMesonParticle, bool continued,
				 int Subsystem=1, int Year=2 )
{

  TRandom   PhotonGenerator(456489581);
  TParticle *PhotonParticle1 = 0;
  TParticle *PhotonParticle2 = 0;

  TLorentzVector inputmesonVect(0.,0.,0.,0.);
  TLorentzVector photon1Vect(0.,0.,0.,0.); 
  TLorentzVector photon2Vect(0.,0.,0.,0.); 

  inputMesonParticle->Momentum(inputmesonVect);

  // 1. Generate randomly the decay photon kinematics

  double ThetaPhotonCM = TMath::ACos( 1. - 2. * PhotonGenerator.Rndm());
  double PhiPhotonCM = 2. * TMath::Pi() *  PhotonGenerator.Rndm();
  double MomentumPhotonCM = inputMesonParticle->GetMass()/2.;
  int photoncounter;

  if(continued) photoncounter = 2;
  else photoncounter = 0;

  // 1st photon
  photon1Vect.SetXYZM( MomentumPhotonCM * TMath::Sin(ThetaPhotonCM) * TMath::Cos(PhiPhotonCM),   
		       MomentumPhotonCM * TMath::Sin(ThetaPhotonCM) * TMath::Sin(PhiPhotonCM),  
		       MomentumPhotonCM * TMath::Cos(ThetaPhotonCM), 0.  );

  photon1Vect.Boost(inputmesonVect.BoostVector());

  // Fill ListOfPhotonParticles with the 1st photon 

  PhotonParticle1 = new( (*ListOfPhotonParticles)[photoncounter] )  
    TParticle(PDG_gam,0,0,0,0,0,
	      photon1Vect.Px(), photon1Vect.Py(), photon1Vect.Pz(), photon1Vect.E(),
	      0., 0., inputMesonParticle->Vz(), 0.);  
  
  // Check if the 1st track heads towards EMCal
  
  int Impact1 = Impact(PhotonParticle1,Subsystem,Year);
  if ( Impact1 > -1 )
    {
      if (PhotonParticle1) photoncounter++;
      if (verbose) cout << " <I> photon1 hit in Arm = " << Impact1 << " " << endl ;
    }
  else 
    {
      ListOfPhotonParticles->RemoveAt(photoncounter);
    }
  
  //2nd photon
  photon2Vect.SetXYZM( (-1.)*MomentumPhotonCM * TMath::Sin(ThetaPhotonCM) * TMath::Cos(PhiPhotonCM),   
		       (-1.)*MomentumPhotonCM * TMath::Sin(ThetaPhotonCM) * TMath::Sin(PhiPhotonCM),  
		       (-1.)*MomentumPhotonCM * TMath::Cos(ThetaPhotonCM), 0. );

  photon2Vect.Boost(inputmesonVect.BoostVector());

  // fill ListOfPhotonParticles with the 2nd photon 

  PhotonParticle2 = new( (*ListOfPhotonParticles)[photoncounter] )  
    TParticle(PDG_gam,0,0,0,0,0,
	      photon2Vect.Px(),  photon2Vect.Py(), photon2Vect.Pz(), photon2Vect.E(),
	      0., 0., inputMesonParticle->Vz() ,0.);
  
  // Check whether the 2nd track heads towards EMCal
  
  int Impact2 = Impact(PhotonParticle2,Subsystem,Year);
  
  // we require both photons heading towards the *same* arm
  if ( (Impact2 > -1) && (sectorToArm(Impact1) == sectorToArm(Impact2)) )
    {
      if (PhotonParticle2)  photoncounter++;
      if (verbose) cout << " <I> photon2 hit in Arm = " << Impact2 << endl;
    }
  else
    {
      ListOfPhotonParticles->RemoveAt(photoncounter);
      if (Impact2 > -1 && Impact1 > -1)
	{ 
	  if (verbose) cout << " <I> photon1 hit in Arm = " << Impact1 ;
	  if (verbose) cout << " and photon2 hit in Arm = " << Impact2
			    << " pi0 pT = " << inputMesonParticle->Pt() << " ";
	}
    }

//       if (verbose) cout << "  Photon Entries = " <<  ListOfPhotonParticles->GetEntries() 
// 			<< "  photoncounter is " << photoncounter << endl;

}

//**********************************************************************************************

void fill_ListOfPi0Particles( TClonesArray *ListOfPi0Particles, 
			      TParticle *inputMesonParticle,
			      int Subsystem=1, int Year=2 )
{

  TRandom   Pi0Generator(456489581);
  TParticle *Pi0Particle1 = 0;
  TParticle *Pi0Particle2 = 0;

  TLorentzVector inputmesonVect(0.,0.,0.,0.);
  TLorentzVector pi01Vect(0.,0.,0.,0.); 
  TLorentzVector pi02Vect(0.,0.,0.,0.); 

  inputMesonParticle->Momentum(inputmesonVect);

  // 1. Generate randomly the decay pi0 kinematics

  double ThetaPi0CM = TMath::ACos( 1. - 2. * Pi0Generator.Rndm());
  double PhiPi0CM = 2. * TMath::Pi() *  Pi0Generator.Rndm();
  double MomentumPi0CM = inputMesonParticle->GetMass()/2.;
  int pi0counter = 0;

  // 1st pi0
  pi01Vect.SetXYZM( MomentumPi0CM * TMath::Sin(ThetaPi0CM) * TMath::Cos(PhiPi0CM),   
		    MomentumPi0CM * TMath::Sin(ThetaPi0CM) * TMath::Sin(PhiPi0CM),  
		    MomentumPi0CM * TMath::Cos(ThetaPi0CM), 0.  );

  pi01Vect.Boost(inputmesonVect.BoostVector());

  // Fill ListOfPi0Particles with the 1st pi0 

  Pi0Particle1 = new( (*ListOfPi0Particles)[pi0counter] )  
    TParticle(PDG_gam,0,0,0,0,0,
	      pi01Vect.Px(), pi01Vect.Py(), pi01Vect.Pz(), pi01Vect.E(),
	      0., 0., inputMesonParticle->Vz(), 0.);  
  
  // Check if the 1st track heads towards EMCal
  
  int Impact1 = Impact(Pi0Particle1,Subsystem,Year);
  if ( Impact1 > -1 )
    {
      if (Pi0Particle1) pi0counter++;
      if (verbose) cout << " <I> pi01 hit in Arm = " << Impact1 << " " << endl ;
    }
  else 
    {
      ListOfPi0Particles->RemoveAt(pi0counter);
    }
  
  //2nd pi0
  pi02Vect.SetXYZM( (-1.)*MomentumPi0CM * TMath::Sin(ThetaPi0CM) * TMath::Cos(PhiPi0CM),   
		    (-1.)*MomentumPi0CM * TMath::Sin(ThetaPi0CM) * TMath::Sin(PhiPi0CM),  
		    (-1.)*MomentumPi0CM * TMath::Cos(ThetaPi0CM), 0. );

  pi02Vect.Boost(inputmesonVect.BoostVector());

  // fill ListOfPi0Particles with the 2nd pi0 
  
  Pi0Particle2 = new( (*ListOfPi0Particles)[pi0counter] )  
    TParticle(PDG_gam,0,0,0,0,0,
	      pi02Vect.Px(),  pi02Vect.Py(), pi02Vect.Pz(), pi02Vect.E(),
	      0., 0., inputMesonParticle->Vz() ,0.);
  
  // Check whether the 2nd track heads towards EMCal
  
  int Impact2 = Impact(Pi0Particle2,Subsystem,Year);
  
  // we do NOT need to require both pi0s heading towards the *same* arm
  if ( (Impact2 > -1) )
    {
      if (Pi0Particle2)  pi0counter++;
      if (verbose) cout << " <I> pi02 hit in Arm = " << Impact2 << endl;
    }
  else
    {
      ListOfPi0Particles->RemoveAt(pi0counter);
    }

//       if (verbose) cout << "  Pi0 Entries = " <<  ListOfPi0Particles->GetEntries() 
// 			<< "  pi0counter is " << pi0counter << endl;

}


//**********************************************************************************************





//_____________________________________________________________________________
// Generate MC distributions of mesons and select events with both 
// (2) gammas from pi0 and eta mesons decays *heading* into one or both
// calorimeters (PbSc, PbGl) and for Run-1, Run-2 geometries:
//
// Subsystem == 0 : Full EMCal
// Subsystem == 1 : PbSc
// Subsystem == 2 : PbGl
// Subsystem == 3 : One single PbSc Sector (W0)
//
// Year == 1      : Run-1
// Year == 2      : Run-2;
//
// Particle_type == "pi0
// Particle_type == "eta"
// Particle_type == "k0short"
//

void 
get_acceptance_from_fast_MC( int maxEvents=500000,
			     int Subsystem=1, int Year=2,
			     const char *Particle_type="pi0", 
			     double pTmax = 20.,
			     const char* geo = "real")
{

  // Particle declaration

  TRandom      MesonGenerator(48759581);

  TRandom  Pi0Generator(48759602);
  TClonesArray *ListOfPi0Particles = new TClonesArray("TParticle",10,1);
    
  TParticle    *mesonParticle = 0, *Pi01, *Pi02;
  TClonesArray *ListOfPhotonParticles = new TClonesArray("TParticle",10,1);

  TLorentzVector mesonVect(0.,0.,0.,0.);

  powlaw = new TF1("powlaw","1/x^[0]", 0., pTmax);
  powlaw->SetParameter(0,8.);

  int pisa_id = 0;
  int pdg_id = 0;

  double MesonMass = 0.;
  double twogammaBR = 0.;
  double BR_pi0pi0 = 0.;

  double MesonPt = 0.;
  double MesonEta = 0.;
  double MesonPhi = 0.;
  double MesonRapidity = 0.;
  double MesonMt = 0.;
  double MesonPz = 0.; 
  double MesonTheta = 0.;
  double MesonZVertex = 0.;
 
  // pi0, eta, or K0short
  if (strcmp(Particle_type,"pi0") == 0) 
    {
      pisa_id = 7 ;
      pdg_id = PDG_pi0 ;
      MesonMass = 0.1349764;
      twogammaBR = 0.98798;
    }
  else if (strcmp(Particle_type,"eta") == 0) 
    {
      pisa_id = 17 ;
      pdg_id = PDG_eta ;
      MesonMass = 0.54730;
      twogammaBR = 0.3921;
    }
  else if (strcmp(Particle_type,"k0short") == 0) // to be implemented !
    {
      pisa_id = 16 ;
      pdg_id = PDG_k0short ;
      MesonMass = 0.497672;
      BR_pi0pi0 = 0.3140;
    }
  else 
    { 
      cout << "<E> Unknown particle " << Particle_type << " (should be \'pi0\', \'eta\' or \'k0short\') " << endl ;
      return;
    }

  // 1. Initialize the geometry and book the histos

  init_geom(geo);

  book_histos(pTmax);

  // 2. EVENT LOOP

  int eventNumber = 0;

  while (eventNumber <= maxEvents)
    {
      if (eventNumber == 0) 
	{
	  cout << "<W> Input pT,y,phi range: " << endl;
	  cout << " pTmax  : " << pTmax << endl; 
	  cout << " ymax   : " << ymax  << endl;
	  cout << " phimax : " << phimax << endl;
	}
      
      if (verbose) cout << " event # " << eventNumber << endl;
      if (!(eventNumber%10000)) cout << " event " << eventNumber << flush;
      if (!(eventNumber%1000))  cout << "." << flush;
            
      //_____________________________________________________________________________
      // 3. Generate randomly the initial meson kinematics

      mesonParticle = new TParticle(pdg_id,0,0,0,0,0, 0.,0.,0.,0.,0.,0.,0.,0.);

      MesonZVertex = 60.*(MesonGenerator.Rndm()-0.5); // -30. to +30 cm (flat)
      MesonPt = pTmax*MesonGenerator.Rndm(); // 0. to pTmax (flat)
      MesonRapidity = 0.5-MesonGenerator.Rndm();  // -0.5 to 0.5 (flat)
      MesonPhi = 2.*TMath::Pi()*MesonGenerator.Rndm(); // 0. to 2pi

      MesonMt = TMath::Sqrt( MesonMass*MesonMass + MesonPt*MesonPt);
      MesonPz = MesonMt *TMath::SinH(MesonRapidity);
      MesonTheta = TMath::ACos ( MesonPz / TMath::Sqrt(MesonPt*MesonPt+MesonPz*MesonPz));
      MesonEta = - TMath::Log(TMath::Tan(MesonTheta/2.));

      mesonVect.SetPtEtaPhiM( MesonPt, MesonEta, MesonPhi, MesonMass); 
      mesonParticle->SetMomentum(mesonVect);
      mesonParticle->SetProductionVertex(0.,0., MesonZVertex, 0.);


      // 4. Fill ListOfPhotonParticles 

      if ((strcmp(Particle_type,"eta") == 0 || strcmp(Particle_type,"pi0") == 0) && MesonGenerator.Rndm() < twogammaBR)
	{
	  fill_ListOfPhotonParticles( ListOfPhotonParticles, mesonParticle, false );
	}
      if(strcmp(Particle_type,"k0short") == 0 && MesonGenerator.Rndm() < BR_pi0pi0)
	{
	  fill_ListOfPi0Particles( ListOfPi0Particles, mesonParticle );
	  int numberOfPi0s =  ListOfPi0Particles->GetEntries();
	  twogammaBR = 0.98798;
	  if((numberOfPi0s == 2)&&(Pi0Generator.Rndm() < twogammaBR)&&(Pi0Generator.Rndm() < twogammaBR))
	    {
	      Pi01 = ((TParticle *) (*ListOfPi0Particles)[0]);
	      fill_ListOfPhotonParticles( ListOfPhotonParticles, Pi01, false);
	      int numberOfPhotons = ListOfPhotonParticles->GetEntries();
	      if(numberOfPhotons == 2)
		{
		  Pi02 = ((TParticle *) (*ListOfPi0Particles)[1]);
		  fill_ListOfPhotonParticles( ListOfPhotonParticles, Pi02, true);
		}
	    }

	} 

      
      eventNumber++;  
 
      // 5. Fill the acceptance histos

      fill_histos(ListOfPhotonParticles, mesonParticle, eventNumber, maxEvents, Subsystem, Year, Particle_type, pTmax);
      
      //ListOfPhotonParticles->Print();   
      ListOfPhotonParticles->Delete();
      ListOfPi0Particles->Delete();
      delete mesonParticle;

    }
  // end event loop
  
  return;
}

//_____________________________________________________________________________



void
GammaAcceptance( int Subsystem, int Year)
{

  TRandom GammaRandom(9485761);
  TParticle *PhotonParticle;
  TLorentzVector PhotonMomentum(0.,0.,0.,0.);

  int iphoton;
  double Pt, eta, phi, zvtx;

  static TH1F *hAcc_En_photon = 0 ;
  static TH1F *hAcc_pT_photon = 0 ;
  static TH2F *hAcc_theta_phi_photon = 0 ;
 
  hAcc_En_photon = new TH1F("hAcc_En_photon","hAcc_En_photon",24,0.,12.);
  hAcc_pT_photon = new TH1F("hAcc_pT_photon","hAcc_pT_photon",24,0.,12.);
  hAcc_theta_phi_photon = new TH2F("hAcc_theta_phi_photon","hAcc_theta_phi_photon",360,-180.,180.,180,0.,180.);
  
  PhotonParticle = new TParticle(22,0,0,0,0,0, 0.,0.,0., 0.,0.,0.,0.,0.);

  int nphoton = 500000;
  int maxEvents = nphoton;
 
  for(iphoton=0; iphoton<nphoton; iphoton++) 
    {
      printf("Photon number is %d \n", iphoton);
      eta = GammaRandom.Rndm()-0.5;
      phi = 2.*TMath::Pi()*GammaRandom.Rndm();
      Pt = 12.*GammaRandom.Rndm();
      zvtx = 60.*(GammaRandom.Rndm()-0.5);
      PhotonMomentum.SetPtEtaPhiM(Pt, eta, phi, 0.);
      PhotonParticle->SetMomentum(PhotonMomentum);
      PhotonParticle->SetProductionVertex(0., 0., zvtx, 0);

      int ImpactArm = Impact(PhotonParticle,Subsystem,Year);
      if ( ImpactArm > -1 )
	{
	  printf("Photon kinematics pt %5.2f eta %5.2f phi %5.2f \n",Pt,eta,phi);
	  hAcc_En_photon->Fill(PhotonMomentum.Energy());
	  hAcc_pT_photon->Fill(PhotonMomentum.Pt() );
	  hAcc_theta_phi_photon->Fill( PhotonMomentum.Phi()*TMath::RadToDeg(), PhotonMomentum.Theta()*TMath::RadToDeg() );
	}
    }

  double normalization =  hAcc_En_photon->GetNbinsX()/((double)  maxEvents) ;
  hAcc_En_photon->Sumw2(); 
  hAcc_En_photon->Scale(normalization);
  normalization = hAcc_pT_photon->GetNbinsX()/((double)  maxEvents) ;
  hAcc_pT_photon->Sumw2(); 
  hAcc_pT_photon->Scale(normalization);
  
  char outputfilename1[80];
  char outputfilename2[80];
  
  if(Subsystem==0) sprintf(outputfilename1,"AcceptanceNewGamma_Emcal");
  if(Subsystem==1) sprintf(outputfilename1,"AcceptanceNewGamma_PbSc");
  if(Subsystem==2) sprintf(outputfilename1,"AcceptanceNewGamma_PbGl");
  if(Subsystem==3) sprintf(outputfilename1,"AcceptanceNewGamma_OneSector");
  if(Year==1) sprintf(outputfilename2,"%s_Year1.root",outputfilename1);
  if(Year==2) sprintf(outputfilename2,"%s_Year2.root",outputfilename1);
  
  TFile *outputfile = new TFile(outputfilename2,"recreate");
  hAcc_En_photon->Write();
  hAcc_pT_photon->Write();
  hAcc_theta_phi_photon->Write();
  outputfile->Close();

}

//____________________________________________________________________________
//

TF1* fit_acceptance( char InFile[60]="Acceptance_pi0_Full_EMCal_Run2.root", double pTmax = 20. )
{
  
  TFile *in = new TFile(InFile,"READ");

  TH1F *hAcc_pT = (TH1F *)in->Get("hAcc_pT_meson");
  TH1F *hAcc_pT_noweighted = (TH1F *)in->Get("hAcc_pT_meson_noweighted");

  gROOT->SetStyle("Plain");
  gStyle->SetOptFit(111);
//  gStyle->SetOptStat(0);
//  gStyle->SetOptTitle(1);

  TCanvas *acc_canvas = new TCanvas("acc_canvas","acc_canvas");
  acc_canvas->cd();

  hAcc_pT->SetTitle(InFile);
  hAcc_pT->SetXTitle("p_{T} (GeV/c)");
  hAcc_pT->SetYTitle("Acceptance (|y|<0.5, 0<\\phi<2\\pi)");
  hAcc_pT->GetYaxis()->SetTitleSize(0.05);
  hAcc_pT->Draw();

  const char* myfit = "([0]+[1]*x+[2]*x*x)*(1.0-exp([3]-[4]*x))"; //"([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))";

  TF1 *AcceptanceFit = new TF1("AcceptanceFit",myfit,0.,pTmax);
  AcceptanceFit->SetParameter(0,0.25);
  AcceptanceFit->SetParameter(1,0.);
  AcceptanceFit->SetParameter(2,0.);
  AcceptanceFit->SetParameter(3,-0.2);
  AcceptanceFit->SetParameter(4,1.);
  AcceptanceFit->SetLineColor(2);
  AcceptanceFit->Draw("same");

  hAcc_pT->Fit(AcceptanceFit,"R","",0.7,pTmax);

  TCanvas *acc_canvas0 = new TCanvas("acc_canvas0","acc_canvas0");
  acc_canvas0->cd();
  hAcc_pT_noweighted->SetLineColor(4);
  hAcc_pT_noweighted->Draw();
  TF1 *AcceptanceFit2 = (TF1*)(AcceptanceFit->Clone());
  hAcc_pT_noweighted->Fit(AcceptanceFit2,"R","",0.7,pTmax);
  AcceptanceFit2->Draw("same");

  TH1F* hfit = (TH1F*)(AcceptanceFit->GetHistogram());
  hfit->Draw("same");

  cout << "================================================================================"<<endl;
  cout << InFile << endl;
  cout << "================================================================================"<<endl;
  cout << myfit << endl;

  double RelError_A = TMath::Abs( 100.*AcceptanceFit->GetParError(0)/AcceptanceFit->GetParameter(0));
  double RelError_B = TMath::Abs( 100.*AcceptanceFit->GetParError(1)/AcceptanceFit->GetParameter(1));
  double RelError_a = TMath::Abs( 100.*AcceptanceFit->GetParError(2)/AcceptanceFit->GetParameter(2));
  double RelError_b = TMath::Abs( 100.*AcceptanceFit->GetParError(3)/AcceptanceFit->GetParameter(3));

  cout << "A = " << AcceptanceFit->GetParameter(0) << " +/- " << AcceptanceFit->GetParError(0) 
       <<"  Relative Error = " <<  RelError_A <<"%" << endl;
  cout << "B = " << AcceptanceFit->GetParameter(1) << " +/- " << AcceptanceFit->GetParError(1) 
       <<"  Relative Error = " <<  RelError_B <<"%" << endl;
  cout << "a = " << AcceptanceFit->GetParameter(2) << " +/- " << AcceptanceFit->GetParError(2)
       <<"  Relative Error = " <<  RelError_a <<"%" << endl;
  cout << "b = " << AcceptanceFit->GetParameter(3) << " +/- " << AcceptanceFit->GetParError(3) 
       <<"  Relative Error = " <<  RelError_b <<"%" << endl;

  cout << "================================================================================"<<endl;


  // PPG014 pi0 acceptance:
  
  myfit = "([0]+[1]*x)*(1.0-exp([2]-[3]*x))";
  double pTmax2 = 12.;  
  TF1 *AcceptanceFitPP014 = new TF1("AcceptanceFitPP014",myfit,0.,pTmax2);
  AcceptanceFitPP014->SetParameters(0.2375,0.002884,0.1062,1.042);
  AcceptanceFitPP014->Draw("same");
  TH1F* hfitPPG014 = (TH1F*)(AcceptanceFitPP014->GetHistogram());
  hfitPPG014->Draw("same");

  acc_canvas->Update();

  TCanvas *acc_canvas2 = new TCanvas("acc_canvas2","acc_canvas2");
  acc_canvas2->cd();
  hfitPPG014->Divide(hfit);
  hfitPPG014->Draw();

  return AcceptanceFit;

}
