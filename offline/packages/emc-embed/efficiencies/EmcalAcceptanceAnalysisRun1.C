//************************************************************
// Acceptance Calculations in EMCAL
//        
// Copyright (C) PHENIX collaboration, 2001
//
// Author: Gines MARTINEZ & David D'ENTERRIA - SUBATECH, 2002
//
//************************************************************
/*
_____________________________________________________________________________

Description: Macro to calculate EMCal (Full, PbSc, PbGl) 
             geometrical acceptances for pi0, eta for Run-1 

Important  : At variance with EmcalAcceptanceAnalysis.C this macro
             uses the same hard-coded geometry (EmCalSector.h) as used 
	     i) to generate the input Exodus files, and as used 
	     ii) in the EMCal forced acceptance cut in the production of the
             Run-1 single-particle simulated DSTs (used in the embedding).
 
Input file : Standard single-particle rawrel

_____________________________________________________________________________

Macro compilation:

.includepath $OFFLINE_MAIN/include
.includepath /afs/rhic/phenix/users/enterria/install/include
gSystem->Load("libpreco.so");
gSystem->Load("libEG.so");
.L EmcalAcceptanceAnalysisRun1.C++

Macro run:

gSystem->Load("libpreco.so");
gSystem->Load("libEG.so");
gSystem->Load("EmcalAcceptanceAnalysisRun1_C.so");
reading_from_rawrel(105000,"~/afs/pi0_rawrel_acc_run2_105000evts.root",0,1,"pi0"); // Full EMCal Run-1 pi0

reading_from_rawrel(105000,"~/afs/pi0_rawrel_acc_run2_105000evts.root",1,1,"pi0"); // PbSc Run-1 pi0

and then:

.x AcceptancePlots.C("pi0_Acceptance_Full_EMCal_Run1.root");

to plot your results.

*/

#include "TFile.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TH2.h"
#include "TMath.h"
#include "TF1.h"
#include "TRandom.h"
#include "TString.h"
#include "TParticle.h"
#include "TVector3.h"
#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "PHString.h"
#include "PHNodeIOManager.h"
#include "PHIODataNode.h"
#include "PhRootHistogramFactory.hh"
#include "dEmcGeaTrackWrapper.h"
#include "EmCalSector.h"            // <--- HARD-CODED EMCAL GEOMETRY HERE  
#include "mEmcToolsModule.h"


// A few global variables

// IMPORTANT: The pT,y,phi range below *MUST* be the same as in the input rawrel simulation
//            otherwise the normalization of the acceptance will be wrong.

//static Double_t pTmax = 12.; 
static Double_t ymax = 0.5;
static Double_t phimax = 180.;

//static Int_t npTbin = (Int_t)(pTmax/0.5); // 0.5 GeV/c bins

static Int_t PDG_gam   =  22;
static Int_t PDG_pi0   = 111;
static Int_t PDG_eta   = 221;

static Int_t verbose  = 0;

//_____________________________________________________________________________
// Part of this is a copy of mEmcToolsModule::Hit function
//
Bool_t Impact(TParticle * particle, Int_t Subsystem, Int_t Year )
{

  static int sector_initDone = 0;
  static EmCalSector Sector[8];

  Bool_t bSector[8];

  float v[3];
  float p[3];

  p[0] = particle->Px();
  p[1] = particle->Py();
  p[2] = particle->Pz();
  v[0] = particle->Vx();
  v[1] = particle->Vy();
  v[2] = particle->Vz();

  if(!sector_initDone)
    {
      sector_initDone = 1;
      Sector[0].init("W0");
      Sector[1].init("W1");
      Sector[2].init("W2");
      Sector[3].init("W3");
      Sector[4].init("E3");
      Sector[5].init("E2");
      Sector[6].init("E1");
      Sector[7].init("E0"); 
    }

  for( int i = 0; i<8 ; i++) 
    { 
      if(Sector[i].Hit(v,p)) bSector[i] = kTRUE;
      else bSector[i] = kFALSE;
    }
  
  if ( (Subsystem==0) && (Year==1) )  return (bSector[0]+bSector[1]+bSector[7]) ;
  if ( (Subsystem==0) && (Year==2) )  return (bSector[0]+bSector[1]+bSector[2]+bSector[3]+bSector[4]+bSector[5]+bSector[6]+bSector[7]) ;

  if ( (Subsystem==1) && (Year==1) )  return (bSector[0]+bSector[1]) ;
  if ( (Subsystem==1) && (Year==2) )  return (bSector[0]+bSector[1]+bSector[2]+bSector[3]+bSector[4]+bSector[5]) ;

  if ( (Subsystem==2) && (Year==1) )  return (bSector[7]) ;
  if ( (Subsystem==2) && (Year==2) )  return (bSector[6]+bSector[7]) ;

  if ( (Subsystem==3)  )  return (bSector[0]) ;
  else  return 0;

}

//_____________________________________________________________________________
// Books/Fills/Outputs the acceptance histos

void Acceptance(TClonesArray * ListOfPhotonParticles, TParticle * DecayParticle, 
                Int_t eventNumber, Int_t maxEvents, Int_t Subsystem = 1 , Int_t Year = 1, 
                const char * Particle_type = "pi0", Double_t pTmax = 8. )
{

  Int_t npTbin = (Int_t)(pTmax/0.5); // 0.5 GeV/c bins
  // Histograms booking

  static TH1F * hMinv                = 0 ; 
  static TH1F * hEneAcceptance       = 0 ;
  static TH1F * hPtAcceptance        = 0 ;
  static TH2F * hAcceptancePhoton    = 0 ;
  static TH2F * hAcceptanceMeson1Photon = 0 ;
  static TH2F * hAcceptanceMeson       = 0 ;

  static TH1F * hEnePhotonPlus       = 0 ; 
  static TH2F * hAsymmetryPhotonPlus = 0 ;
  static TH2F * hThetavsPhi          = 0 ;
  static TH2F * hThetavsPhi_EdepCut  = 0 ;

  if( eventNumber==0) 
    {
      delete hMinv;
      delete hEneAcceptance;
      delete hPtAcceptance;
      delete hAcceptancePhoton;
      delete hAcceptanceMeson;
      delete hAcceptanceMeson1Photon;
      delete hEnePhotonPlus;
      delete hAsymmetryPhotonPlus;
      delete hThetavsPhi;
      delete hThetavsPhi_EdepCut;
      hMinv                = new TH1F("hMinv","hMinv",100,0.,1.);
      hThetavsPhi          = new TH2F("hThetavsPhi","hThetavsPhi",360,-phimax,phimax,phimax,0.,phimax);
      hEneAcceptance       = new TH1F("hEneAcceptance","hEneAcceptance",npTbin,0.,pTmax);
      hPtAcceptance        = new TH1F("hPtAcceptance","hPtAcceptance",npTbin,0.,pTmax);
      hAcceptancePhoton    = new TH2F("hAcceptancePhoton","hAcceptancePhoton",npTbin,0.,pTmax,20,-ymax,ymax);
      hAcceptanceMeson1Photon  = new TH2F("hAcceptanceMeson1Photon","hAcceptanceMeson1Photon",npTbin,0.,pTmax,20,-ymax,ymax);
      hAcceptanceMeson     = new TH2F("hAcceptanceMeson","hAcceptanceMeson",npTbin,0.,pTmax,20, -ymax, ymax);
      hEnePhotonPlus       = new TH1F("hPhotonPlus","hPhotonPlus",npTbin, 0., pTmax);
      hAsymmetryPhotonPlus = new TH2F("hAsymmetryPhotonPlus","hAsymmetryPhotonPlus",npTbin,0.,pTmax,20,0.,1.);
      hThetavsPhi_EdepCut  = new TH2F("hThetavsPhi_EdepCut","hThetavsPhi_EdepCut",360,-phimax,phimax,phimax,0.,phimax);
  }

  // Fill Histos

  Int_t iphoton1, iphoton2;
  TParticle * Photon1;
  TParticle * Photon2;
  TLorentzVector Photon1Momentum, Photon2Momentum, Pi0Momentum;

  int numberOfPhotons =  ListOfPhotonParticles->GetEntries();
  if (verbose) cout << ">>> numberOfPhotons is " << numberOfPhotons << endl;

  for( iphoton1=0; iphoton1<numberOfPhotons; iphoton1++) 
    {
      Float_t Asymmetry=0.;
      Photon1 = ((TParticle *) (*ListOfPhotonParticles)[iphoton1]);
      Photon1->Momentum(Photon1Momentum);

      hThetavsPhi->Fill( Photon1Momentum.Phi()*180./TMath::Pi(), Photon1Momentum.Theta()*180./TMath::Pi() );
      hAcceptancePhoton->Fill( Photon1Momentum.Pt(), Photon1Momentum.PseudoRapidity());
      DecayParticle->Momentum(Pi0Momentum);
      hAcceptanceMeson1Photon->Fill( Pi0Momentum.Pt(), Pi0Momentum.PseudoRapidity());

      for( iphoton2=(iphoton1+1); iphoton2<numberOfPhotons; iphoton2++) 
        {
          Photon2 = ((TParticle *) (*ListOfPhotonParticles)[iphoton2]);
          Photon2->Momentum(Photon2Momentum);
          Asymmetry = TMath::Abs(Photon1Momentum.Energy()-Photon2Momentum.Energy());
          Asymmetry /= (Photon1Momentum.Energy()+Photon2Momentum.Energy());
          //cout << ">>>>>    "  << Asymmetry << endl;
     
          if ( Photon1Momentum.Energy() > Photon2Momentum.Energy() ) 
            {
              hEnePhotonPlus->Fill( Photon1Momentum.Energy() );
              hAsymmetryPhotonPlus -> Fill ( Photon1Momentum.Energy(), Asymmetry ); 
            }
          else 
            {
              hEnePhotonPlus->Fill( Photon2Momentum.Energy() );
              hAsymmetryPhotonPlus -> Fill ( Photon2Momentum.Energy(), Asymmetry ); 
            }
          Photon2Momentum+=Photon1Momentum;
          hMinv->Fill(Photon2Momentum.M());
          hEneAcceptance->Fill(Photon2Momentum.Energy());
          hPtAcceptance->Fill(Photon2Momentum.Pt() );
          hAcceptanceMeson->Fill( Photon2Momentum.Pt(), Photon2Momentum.PseudoRapidity());
        }
    }

  // Write histos

  Float_t EneScaling = 0;
  Float_t PtScaling = 0;  
  Float_t AcceptanceScalingMeson = 0;
  Float_t AcceptanceScalingMeson1Photon = 0;
  Float_t AcceptancePhotonScaling = 0;

  if (eventNumber == maxEvents)
    {
      EneScaling = hEneAcceptance->GetNbinsX() / ((Float_t) maxEvents) ;
      hEneAcceptance->Sumw2(); 
      hEneAcceptance->Scale(EneScaling);
      PtScaling = hPtAcceptance->GetNbinsX() / ((Float_t) maxEvents) ;
      hPtAcceptance->Sumw2(); 
      hPtAcceptance->Scale(PtScaling);
      AcceptanceScalingMeson = hAcceptanceMeson->GetNbinsX()*hAcceptanceMeson->GetNbinsY() / ((Float_t)  maxEvents);
      hAcceptanceMeson->Sumw2(); 
      hAcceptanceMeson->Scale(AcceptanceScalingMeson);
      AcceptanceScalingMeson1Photon = hAcceptanceMeson1Photon->GetNbinsX()*hAcceptanceMeson1Photon->GetNbinsY() / ((Float_t) maxEvents);
      hAcceptanceMeson1Photon->Sumw2(); 
      hAcceptanceMeson1Photon->Scale(AcceptanceScalingMeson1Photon);
      AcceptancePhotonScaling = hAcceptancePhoton->GetNbinsX() * hAcceptancePhoton->GetNbinsY() / ((Float_t)  maxEvents);
      hAcceptancePhoton->Sumw2(); hAcceptancePhoton->Scale(AcceptancePhotonScaling);

      char outputfilename1[80];
      char outputfilename2[80];
      char outputfilename3[80];
      
      sprintf(outputfilename1,"Acceptance");
      if(Subsystem==0)  sprintf(outputfilename2,"%s_%s_Full_EMCal",outputfilename1,Particle_type);
      if(Subsystem==1)  sprintf(outputfilename2,"%s_%s_PbSc",outputfilename1,Particle_type);
      if(Subsystem==2)  sprintf(outputfilename2,"%s_%s_PbGl",outputfilename1,Particle_type);
      if(Subsystem==3)  sprintf(outputfilename2,"%s_%s_PbScW0Sector",outputfilename1,Particle_type);

      if(Year==1) sprintf(outputfilename3,"%s_Run1.root",outputfilename2);
      if(Year==2) sprintf(outputfilename3,"%s_Run2.root",outputfilename2);
      
      TFile * outputfile = new TFile(outputfilename3,"recreate");
      hMinv->Write();
      hEneAcceptance->Write();
      hPtAcceptance->Write();
      hAcceptancePhoton->Write();
      hAcceptanceMeson->Write();
      hAcceptanceMeson1Photon->Write();
      hEnePhotonPlus->Write();
      hAsymmetryPhotonPlus->Write();
      hThetavsPhi->Write();
      outputfile->Close();
      cout << endl << "Output acceptance file: " << outputfilename3 << endl;
    }

}

//_____________________________________________________________________________
// Read the input rawrel file and select events with both (2) gammas
// from pi0 and eta mesons decays *heading* into one or both
// calorimeters (PbSc, PbGl) and for Run-1, Run-2 geometries:
// Subsystem == 0 : Full EMCal
// Subsystem == 1 : PbSc
// Subsystem == 2 : PbGl
// Subsystem == 3 : One single PbSc Sector (W0) 
// Year == 1      : Run-1
// Year == 2      : Run-2; 
// Particle_type == "pi0
// Particle_type == "eta"

void reading_from_rawrel(Int_t maxEvents=21000,
                         const char * evaIFile="20000pi0_rawrel.root", 
                         Int_t Subsystem=1, Int_t Year=1,
                         const char * Particle_type="pi0", 
                         Double_t pTmax = 8.)
{
  // Particle declaration
  TParticle    * DecayParticle = 0;;
  TClonesArray * ListOfPhotonParticles = new TClonesArray("TParticle",10,1);
  TParticle    * PhotonParticle;
  TParticle    * PhotonParticle2;
  TLorentzVector  photon1(0.,0.,0.,0.); 
  TLorentzVector  photon2(0.,0.,0.,0.); 

  Int_t photoncounter = 0;
  Int_t pisa_id = 0;
  Int_t pdg_id = 0;
  Int_t ntr;
  Int_t i,j;
  
  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up input and output files
  PHString evaInFile = evaIFile;
  PHNodeIOManager* evaIn = new PHNodeIOManager(evaInFile,PHReadOnly);
  evaIn->selectObjectToRead("*",false);
  evaIn->selectObjectToRead("EVA/dEmcGeaTrack",true);
  evaIn->read(evaNode);

  // Initialize the tables
  PHIODataNode<PHTable>* dEmcGeaTrackNode = 
    (PHIODataNode<PHTable>*) mainIter.findFirst("PHIODataNode","dEmcGeaTrack");
  if (!dEmcGeaTrackNode) 
    {
      cout << "<W> Dst Read: Could not find data node dEmcGeaTrack" << endl;
    }
  dEmcGeaTrackWrapper *dEmcGeaTrack = 0;

  // Total number of events in rawrel file
  TFile f(evaIFile,"");
  Int_t Events = (Int_t)((TTree*)f.Get("T"))->GetEntries();
  cout << "<I> Input file " << evaIFile << " has " << Events << " entries." << endl ;
  maxEvents = (Events < maxEvents) ? Events : maxEvents ;
  maxEvents = (maxEvents == -1 ) ? Events : maxEvents;

  // pi0 or eta
  if (strcmp(Particle_type,"pi0") == 0) {
    pisa_id = 7 ;
    pdg_id = PDG_pi0 ;
  }
  else if (strcmp(Particle_type,"eta") == 0) {
    pisa_id = 17 ;
    pdg_id = PDG_eta ;
  }
  else { 
    cout << "<E> Unknown particle " << Particle_type << " (should be \'pi0\' or \'eta\') " << endl ;
    exit(0);
  }

  // STARTING THE EVENT LOOP
  Int_t eventNumber = 0;
  while (eventNumber <= maxEvents) 
    {
      if (eventNumber == 0) 
        {
          cout << "<W> IMPORTANT: The pT,y,phi range below *MUST* be the same as in the input rawrel simulation"
               << " otherwise the normalization of the acceptance will be wrong: " << endl;
          cout << " pTmax  : " << pTmax << endl; 
          cout << " ymax   : " << ymax  << endl;
          cout << " phimax : " << phimax << endl;
        }
      if (eventNumber >= 1) 
        {
          evaIn->read(evaNode);
        }
      if (!(eventNumber%1000)) cout << " event " << eventNumber << flush;
      if (!(eventNumber%100))  cout << "." << flush;

      if (!dEmcGeaTrackNode) 
        {
          cout << "Dst Read: Could not find data node dEmcGeaTrack" << endl;
        } 
      else 
        {
          dEmcGeaTrack = (dEmcGeaTrackWrapper*)dEmcGeaTrackNode->getData();
        }

      // Identification of the primary parent particle. 
      // It is assumed that only one parent particle (pi0,eta) is present in the simulation
      ntr   = (Int_t) dEmcGeaTrack->RowCount();
      photoncounter = 0;

      for(i=0;i<ntr;i++) 
        {
          if ( ( dEmcGeaTrack->get_idparent(i) == 0 )  &&   // Primary particle
               ( dEmcGeaTrack->get_pid(i)      == pisa_id )     ) // pion or eta
            { 
              DecayParticle = new TParticle(pdg_id,0,0,0,0,0,
                                          dEmcGeaTrack->get_pxyz(0,i),
                                          dEmcGeaTrack->get_pxyz(1,i), 
                                          dEmcGeaTrack->get_pxyz(2,i),
                                          dEmcGeaTrack->get_ekin(i), 
                                          dEmcGeaTrack->get_xyz(0,i),
                                          dEmcGeaTrack->get_xyz(1,i), 
                                          dEmcGeaTrack->get_xyz(2,i),
                                          0.);    
            }
      
          // Identification of *direct-secondary* particle from pi0/eta decay
          if ( ( dEmcGeaTrack->get_idparent(i) == pisa_id ) &&   // Parent is directly a pion/eta (no tertiaries ...)
               ( dEmcGeaTrack->get_pid(i)      == 1 )    ) // photon
            {
              PhotonParticle = new( (*ListOfPhotonParticles)[photoncounter] )  
                                  TParticle(PDG_gam,0,0,0,0,0,
                                            dEmcGeaTrack->get_pxyz(0,i),
                                            dEmcGeaTrack->get_pxyz(1,i),
                                            dEmcGeaTrack->get_pxyz(2,i),
                                            dEmcGeaTrack->get_ekin(i),
                                            dEmcGeaTrack->get_xyz(0,i),
                                            dEmcGeaTrack->get_xyz(1,i), 
                                            dEmcGeaTrack->get_xyz(2,i) ,0.);

              // Check if the track heads to EMCal
              if ( Impact(PhotonParticle,Subsystem,Year) )  
                {
                  // Avoid double counting of particles in ListOfPhotonParticles
                  PhotonParticle->Momentum(photon1);
                  for(j=0; j<photoncounter; j++)
                    {
                      PhotonParticle2 = ((TParticle *) (*ListOfPhotonParticles)[j]);
                      PhotonParticle2->Momentum(photon2);
                      if (photon1 == photon2) 
                        {
                          ListOfPhotonParticles->RemoveAt(j);
                          PhotonParticle= 0;
                        }
                    }
                  if (PhotonParticle)  photoncounter++;
                }
              else 
                {
                  ListOfPhotonParticles->RemoveAt(photoncounter);
                }
            }
        }
      if (verbose) cout << "  Photon Entries = " <<  ListOfPhotonParticles->GetEntries() 
                        << "  photoncounter is " << photoncounter << endl;

      // Fill the histos
      Acceptance(ListOfPhotonParticles, DecayParticle, eventNumber, maxEvents, Subsystem, Year, Particle_type);

      //ListOfPhotonParticles->Print();   
      ListOfPhotonParticles->Delete();
      
      eventNumber++;
      
      // Reset all data for this event
      mainIter.cd();
      if (mainIter.cd("EVA")) 
        {
          mainIter.forEach(reset);
          mainIter.cd();
        } 
    }
  // end event loop
 
  return;
}
