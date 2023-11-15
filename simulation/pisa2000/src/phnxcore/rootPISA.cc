// $Id: rootPISA.cc,v 1.52 2018/07/05 16:50:12 lajoie Exp $

// rootPISA.cc
// Container file for all ROOT-in-PISA C++ routines, except encoders
// These routine use the pisaevent, hfile, and tree static variables.

#include <PISAEvent.h>
#include <encodeRoot.h>

#include <TFile.h>
#include <TNtuple.h>
#include <TTree.h>
#include <TBenchmark.h>

#include <cstdlib>
#include <iostream>

using namespace std;

static TFile *hfile = 0; 
static TTree *tree = 0;
static Int_t isubevent = 1;
static PISAEvent *pisaevent = 0;
static long nbytes = 0;

static TFile *oscarFile = 0;
static TNtuple *EvtParticleNtuple = 0;

static TFile *pairPolarFile = 0;
static TNtuple *EvtPairNtuple = 0;

static Int_t idateDC = 0;
static Int_t idatePC1 = 0;

TBenchmark *gBenchmark2 = new TBenchmark();

using namespace std;

// forward declaration
extern "C" {
  
  //! initialize root
  void rootstart_(Int_t *startflag, const char* filename, int len);
  
  //! write pisa header to root file
  void headrootout_(int i[], float f[]);

  //! write subsystem parameter nodes to root file
  void parrootout_(int *dcode, int i[], float f[]);
  
  //! write "special" output nodes to root file
  /*! 
  used for intra-event staged simulations in the Muon Arm
  Extra array tracks gives the track numbers in the preserved ancestries
  dcode gives the detector subsystem identifier code
  icode gives supplementary identifier information specific to detector subsystem
  */
  void dstrootoutspecial_(int *dcode, int *icode, int *kentries, int i[], float f[], int tracks[]);

  //! write subsystem hits to root file
  void dstrootout_(int *dcode, int *icode, int *kentries, int i[], float f[]);

  //! finish single event in root output file and reset PISAEvent structure
  void endrootout_(int *end_flag);
  
  //! close pisa output file
  void closerootout_();
  
}

//________________________________________________________________________
void rootstart_(Int_t *startflag, const char* filename, int len)
{
  
  // startflag = 0  for subevent output of ROOT clone objects
  // startflag = 1  for full event output of ROOT clone objects
  // startflag = 2  for full event output of PHOOL formatted objects

  if( hfile && hfile->IsOpen() ) 
  { 
    cout << "rootstart_ - TFile output is already opened. This call to rootstart_ does nothing" << endl; 
    return;
  }
  
  string root_filename( filename, len );
  cout << "rootstart_ - pisa output filename: " << root_filename << endl;
  
  // Set up ROOT File
  hfile = new TFile( root_filename.c_str(),"RECREATE","PISA ROOT file");

  // compresses floats in TClones arrays
  Int_t comp = 2;      
  hfile->SetCompressionLevel(comp);

  // Create a PISAevent instance
  pisaevent = new PISAEvent();
  pisaevent->SetStartFlag(*startflag);

  // Set up Tree structure
  Int_t split = 1;
  Int_t branchStyle = 0;
  
  tree = new TTree("T","PISA Hits in a ROOT tree");
  tree->SetMaxTreeSize(10000000000LL); // set max size to ~10 GB
  
  tree->SetAutoSave(30000000);  
  Int_t bufsize = 64000;    
  if (split)  bufsize /= 4;
  TTree::SetBranchStyle(branchStyle);
  tree->Branch("pisaevent", "PISAEvent", &pisaevent, bufsize,split);
  gBenchmark2->Start("PISA Running Times");   

  return;
}

//________________________________________________________________________
void headrootout_(int i[], float f[])
{
  // cout << "headrootout_" << endl;
  Int_t startflag = pisaevent->GetStartFlag();
  if( startflag == 0 || (startflag > 0 && isubevent == 1) ) pisaevent->SetHeader(i, f);   
  return;
  
}

//________________________________________________________________________
void parrootout_(int *dcode, int i[], float f[])
{

  // cout << "parrootout_" << endl;
  
  // check output file
  assert( hfile && hfile->IsOpen() );

  switch (*dcode) 
  {

    case 3:                
    // BBC (beam beam counter)
    pisaevent->AddBbcPara(i,f);
    break;

    case 5:                
    // DC (drift chamber)
    pisaevent->AddDchPara(i,f);
    break;

    case 6:
    // TOF  (Time of Flight)
    pisaevent->AddTofPara(i,f);
    break;

    // In fi_put_dst the PC1/DC call is made for the DC call
    // So we have both the PC1 and DC dates stored
    case -5:                
    // PC1/DC (dates of DC and PC1 last changes)
    idateDC = i[1];       
    idatePC1 = i[5];      
    break;

    case 10:                 
    // EMCal
    pisaevent->AddEmcPara(f);
    break;

    case 11:                
    // Mut  (Muon Tracker)
    pisaevent->AddMutPara(i,f);
    break;
    
    case 12:   
    // Mui  (Muon Id)
    pisaevent->AddMuiPara(i,f);
    break;

    case 14: 
    //  Svx
    pisaevent->AddSvxPara(i,f);
    break;

    default:
    cout << "parrootout_ - unknown code value " << *dcode << endl;
    break;
    
  }
} 

//________________________________________________________________________
void dstrootoutspecial_(int *dcode, int *icode, int *kentries, int i[], float f[], int tracks[])
{

  // cout << "dstrootoutspecial_" << endl;

  // check output file
  assert( hfile && hfile->IsOpen() );
  
  switch (*dcode) 
  {

    case 11:  
    // Mut (Muon trackers)
    encodeRootEvntMut(*kentries, i, f, *icode, tracks, isubevent, pisaevent);
    break;
      
    case 12:
    // Mui (Muon identifier)
    encodeRootEvntMui(*kentries, i, f, *icode, tracks, isubevent, pisaevent);
    break;

    default:
    cout << "dstrootoutspecial_ - unknown code value " << *dcode << endl;
    break;
  }
  return;

}


//________________________________________________________________________
void dstrootout_(int *dcode, int *icode, int *kentries, int i[], float f[])
{

//   cout << "dstrootout_ -"
//     << " dcode: " << *dcode 
//     << " icode: " << *icode 
//     << " entries: " << *kentries
//     << endl;

  // check output file
  assert( hfile && hfile->IsOpen() );
  
  // dcode gives the detector subsystem identifier code
  // icode gives supplementary identifier information specific to detector subsystem
  // tracks array is used only by the Muon Arm encode modules 
  int tracks[1] = {-1}; 
  
  switch (*dcode) 
  {
    // FKIN (vertex kinematics and ancestry)
    case 1:                
    encodeRootEvntKin(*kentries, i, f, isubevent, pisaevent);
    break;

    // PRI (primary particles)
    case 2:               
    encodeRootEvntPri(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // BBC (beam beam counter)
    case 3:                
    encodeRootEvntBbc(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // DCH (drift chamber, West Arm)
    case 5:                 
    encodeRootEvntDch(*icode, *kentries, i, f, isubevent, pisaevent);
    break;
    
    // CRK (Ring Imaging Cerenkov Hodoscope)
    case 6:                
    encodeRootEvntCrk(icode, *kentries, i, isubevent, pisaevent);
    break;
    
    // CTR (Fictitious Cerenkov Trackers)
    case -6:                
    encodeRootEvntCtr(icode, *kentries, i, isubevent, pisaevent);
    break;
    
    // PAD (Pad Chambers)
    case 7:                
    encodeRootEvntPad(*icode, *kentries, i, f, isubevent, pisaevent);
    break;
    
    // TOF (Time of Flight)
    case 8:                
    encodeRootEvntTof(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // Tec (Time Expansion Chamber, or TRD Transition Radiation Detector)
    case 9:                
    encodeRootEvntTec(*icode, *kentries, i, f, isubevent, pisaevent);
    break;
    
    // EMC (Electromagnetic calorimeter)
    case 10:               
    encodeRootEvntEmc(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // Mut (Muon trackers)
    case 11:               
    encodeRootEvntMut(*kentries, i, f, *icode, tracks, isubevent, pisaevent);
    break;
    
    // Mui (Muon identifier)
    case 12:               
    encodeRootEvntMui(*kentries, i, f, *icode,  tracks, isubevent, pisaevent);
    break;
    
    // Zdc (Zero Degree Calorimeter)
    case 13:              
    // ZDC hits data are *not* in a ZEBRA bank
    // The i[] array refers to the track number
    // The f[] array refers to the hits data array
    encodeRootEvntZdc(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // Inner (Test inner tracker)
    case 14:                
    encodeRootEvntSvx(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // HBD (HBD Counter for upgrades, WIS version)
    case 15:                
    encodeRootEvntHbd(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // RXN (Reaction Plane Detector)
    case 18:               
    encodeRootEvntRxn(*icode, *kentries, i, f, isubevent, pisaevent);
    break;

    // FCL (FCL forward calorimeter for Run3)
    case 19:                
    encodeRootEvntFcl(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // AER (AER aerogel counter for Run3)
    case 20:                
    encodeRootEvntAer(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // MuPC (PC for muon trigger upgrade)
    case 22:                
    encodeRootEvntMuPC(*icode, *kentries, i, f, isubevent, pisaevent);
    break;
    
    // RLT (RPC for relative luminosity telescope)
    case 23:                
    encodeRootEvntRLT(*icode, *kentries, i, f, isubevent, pisaevent);
    break;
    
    // NCC (NCC for Nosecone forward calorimeter)
    case 24:                
    encodeRootEvntNCC(*icode, *kentries, i, f, isubevent, pisaevent);
    break;
    
    // MPC (MPC for Muon Piston Calorimeter)
    case 25:                
    encodeRootEvntMpc(*kentries, i, f, isubevent, pisaevent);
    break;
    
    // MPC (MPC for Muon Piston Calorimeter)
    case 26:               
    encodeRootEvntTfw(*icode, *kentries, i, f, isubevent, pisaevent);
    break;

    // MPC-EX Absorber Hits
    case 27:
    encodeRootEvntMPCEXABS(*icode, *kentries, i, f, isubevent, pisaevent);
    break; 

    // MPC-EX Absorber Hits (Al frame and skin)
    case 28:
    encodeRootEvntMPCFPLT(*icode, *kentries, i, f, isubevent, pisaevent);
    break; 

    // MPC Entry Particles
    case 29:
    encodeRootEvntMPCEXEntry(*icode, *kentries, i, f, isubevent, pisaevent);
    break; 
    
    default:
    cout << "dstrootout_ - unknown code value " << *dcode << endl;
    break;
  }

  return;

}

//________________________________________________________________________
void endrootout_(int *end_flag)
{
  pisaevent->SetEndFlag(0);
  if(*end_flag == 1) {
    pisaevent->SetEndFlag(1);
    isubevent++;
  }

  if(*end_flag == 2) {
    pisaevent->SetEndFlag(2);
    isubevent = 1;
  }

  Int_t startflag = pisaevent->GetStartFlag();

  if(startflag == 0) {

    // Subevent output
    nbytes += tree->Fill();
    pisaevent->Clear();
    
  }

  if(startflag == 1 && *end_flag == 2) {

    // Full event output
    nbytes += tree->Fill();
    pisaevent->Clear();
    
  }
  
  return;
}


//________________________________________________________________________
void closerootout_()
{
  
  cout << "closerootout_ - nbytes: " << nbytes << endl;
  if( hfile && hfile->IsOpen() )
  {
    cout << "closerootout_ - closing PISA TFile" << endl;
    hfile->Write();
    hfile->Close();
    delete hfile;
    hfile = 0;
  }
  
  if(oscarFile) {
    cout << "closerootout_ - Oscar File final close" << endl;
    oscarFile->Write();
    oscarFile->Close();
  }

  if(pairPolarFile) {
    cout << "closerootout_ - Pair Decay Polarization File final close" << endl;
    pairPolarFile->Write();
    pairPolarFile->Close();
  }

  gBenchmark2->Show("PISA Running Times");  // show timing information
  
  return;
}

//________________________________________________________________________
extern "C" void openoscarfile_(int *nStage, float *zBoundary)
{

  // Originally written in February 2004 for Muon Arm staged simulations
  // Adopted for general use in April 2004
  static char oscarFileName[200];
  static Int_t lastStage = -1;

  if(oscarFile) {

    if(*nStage - lastStage != 1 && *nStage != 9) {
      cerr << "openoscarfile_ - new stage number " << *nStage;
      cerr << " is not one more than the old stage number " << lastStage;
      cerr << endl;
      exit(1);
    }

    cout << "openoscarfile_ - Closing " << oscarFileName << " output file" << endl;
    oscarFile->Write();
    oscarFile->Close();
    oscarFile = 0;

    if(*nStage == 9) return;

  } 

  char oscarFileTitle[200];

  if(*nStage > 0) {
    lastStage = *nStage;
    int iZ;
    iZ = static_cast<int>(*zBoundary);
    sprintf(oscarFileName, "oscarfilez%dstage%d.root", iZ, *nStage);
    sprintf(oscarFileTitle, "Oscar File zBoundary %d Stage %d", iZ,  *nStage);
  }

  if(*nStage == 0) {
    sprintf(oscarFileName, "oscarPrimary.root");
    sprintf(oscarFileTitle, "OSCAR NTUPLE for Primary Particles");
  }

  oscarFile = new TFile(oscarFileName, "RECREATE", oscarFileTitle);

  if(oscarFile) 
  {
    oscarFile->SetCompressionLevel(2);
    cout << "openoscarfile_ - Opened " << oscarFileName << endl;

    // pnum is the number of particles in the event counting from 0 as one particle
    // the (x,y,z) vertex values are in fm units (10**-13 cm)
    if(*nStage > 0) 
    {
      EvtParticleNtuple = new TNtuple("particle", "Event Particles",
              "event:pnum:pid:px:py:pz:E:xvtx:yvtx:zvtx:"// 10
              "ivert:pxvert:pyvert:pzvert:xvtxanc:yvtxanc:"// 16
              "zvtxanc:itparent:idparent:pxorig:pyorig:pzorig:"// 22
              "xorig:yorig:zorig:itorig:idorig:itra:ksearch:mxtot"); // 30
    } 
    // Expanded OSCAR particle list for Muon Arm

    if(*nStage == 0) {
      EvtParticleNtuple = new TNtuple("particle", "Event Particles",
              "event:pnum:pid:px:py:pz:E:xvtx:yvtx:zvtx"); // 10      
    } 
    // Standard OSCAR particle list 

  } else cerr << "\n Failure to open " << oscarFileName << endl;
    
  return;

}

//________________________________________________________________________
extern "C" void filloscarfile_(float f[])
{

  // Event-by-event fill of Oscar File NTUPLE
  EvtParticleNtuple->Fill(f);
  return;

}

//_______________________________________________________
extern "C" void openpairpolarfile_(int *openClose)
{

  // Code to store pair decay polarization information
  if(!pairPolarFile && *openClose == 1) 
  {
    pairPolarFile = new TFile("pairPolarFile.root", "RECREATE", "Polarized Pair Decay Data");
    pairPolarFile->SetCompressionLevel(2);
    cout << "\n openPairPolarFile function <I>: Opened pairPolarFile.root" << endl;
    EvtPairNtuple = new TNtuple("particle", "Pair Decay Particles",
        "Pxreac1r:Pyreac1r:Pzreac1r:Pxreac2r:Pyreac2r:Pzreac2r:" // 6
        "Alpha1:Beta1:Phi1r:Theta1r:Alpha2:Beta2:Phi2r:Theta2r:" // 14
        "Pxreac1l:Pyreac1l:Pzreac1l:Phi1l:Theta1l:" // 19
        "Pxreac2l:Pyreac2l:Pzreac2l:Phi2l:Theta2l:" // 24
        "Pxparntl:Pyparntl:Pzparntl:Ptparnl:Rho00:V2const:Rapidity:RestMass:Event"); // 32
        
  } else {
    cerr << "openpairpolarfile_ - pair file has already been opened" << endl;
    exit(1);
  }  // check for file open request

}

//________________________________________________________________________
extern "C" void fillpairpolarfile_(float f[])
{
  // Event-by-event fill of Pair Decay Polarization File NTUPLE
  EvtPairNtuple->Fill(f);
  return;

}
