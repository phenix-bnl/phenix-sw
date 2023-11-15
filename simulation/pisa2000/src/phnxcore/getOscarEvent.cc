// $Id: getOscarEvent.cc,v 1.3 2014/02/06 21:00:58 bbannier Exp $

/*!
   \file getOscarEvent.cc
   \brief oscar events stored in root files
   \author Mickey Chiu
   \version $Revision: 1.3 $
   \date $Date: 2014/02/06 21:00:58 $
*/

#include <TROOT.h>
#include <TFile.h>
#include <TNtuple.h>
#include <TTree.h>

#include <cstdlib>
#include <iostream>
#include <string>

using namespace std;

//________________________________________________________________________
extern "C" 
{
  //! get one oscar event
  void getoscarevent_(
    int *mxtot, int idtot[], float pptot[], float xyzmv[], 
    int *istart, int *nClone, int *clength,  char *oscarFileName);
}

//_______________________________________________________________
void getoscarevent_(
  int *mxtot, int idtot[], float pptot[], float xyzmv[], 
  int *istart, int *nClone, int *clength,  char *oscarFileName){

  // convert fermi to cm
  const float FMTOCM = 1.0e-13; 
  static int iFirst = 0;
  static int kClone = 0;

  // mxtot is the number of particles in the event
  // idtot is the PDG particle ID  (will be converted to GEANT ID in FORTRAN)
  // pptot is the momentum 4-vector particle in GeV
  // xyzmv is the vertex of the particle in cm

  static TTree *particle;
  static Int_t nentries;
  static Int_t iCall = 0;
  static Int_t iStore = 0;
  static TFile *f1 = 0;
  if (iFirst == 0) {
    // Open the OSCAR input file
    const std::string rootFile(oscarFileName, *clength);

    if (f1) delete f1;
    f1 =  new TFile(rootFile.c_str());
    if(!f1) {
      cerr << "getoscarevent_  - Unable to find OSCAR input file " << rootFile
           << endl;
      exit(1);
    }
    
    particle = (TTree*)f1->Get("particle");
    if(!particle) 
    {
      cout << "getoscarevent_ - Cannot find  particle " << endl;
      exit(1);
    }

    nentries = (Int_t)particle->GetEntries();
    cout << "getoscarevent_ - OSCAR input file " << rootFile << " has "
         << nentries << " entries " << endl;

  } // check on first call

  //Declaration of leaves types
  Float_t event;
  Float_t pnum;
  Float_t pid;
  Float_t px;
  Float_t py;
  Float_t pz;
  Float_t E;
  Float_t xvtx;
  Float_t yvtx;
  Float_t zvtx;

  
  // Set branch addresses
  particle->SetBranchAddress("event",&event); 
  particle->SetBranchAddress("pnum",&pnum); 
  particle->SetBranchAddress("pid",&pid); 
  particle->SetBranchAddress("px",&px); 
  particle->SetBranchAddress("py",&py); 
  particle->SetBranchAddress("pz",&pz); 
  particle->SetBranchAddress("E",&E); 
  particle->SetBranchAddress("xvtx",&xvtx); 
  particle->SetBranchAddress("yvtx",&yvtx); 
  particle->SetBranchAddress("zvtx",&zvtx); 
      
  if(iFirst==0) 
  {
    iFirst = 1;
    int eventTest = -1;
    int eventStart = *istart;
    while(eventTest < eventStart) 
    {
      if(iStore >= nentries) 
      {
        cerr << "getoscarevent_ - Trying to get non-existent entry in ROOT file " << endl;
        cerr << "getoscarevent_ - At iCall = " << iCall << ", iStore = " << iStore << endl;
        exit(1);
      }
      
      particle->GetEvent(iStore++);
      if(pnum<0) {
        cerr << "getoscarevent_ - At iCall = " << iCall << ", iStore = " << iStore-1;
        cerr << ",  number of particle in events = " << pnum << endl;
        exit(1);
      }
      
      eventTest = int(event);
      
    } 
  } else {
    
    if(iStore >= nentries) 
    {
      
      cerr << "getoscarevent_ - Trying to get non-existent entry in ROOT file " << endl;
      cerr << "getoscarevent_ - At iCall = " << iCall << ", iStore = " << iStore;
      cerr << " , nentries = " << nentries << endl;
      exit(1);
    }
    particle->GetEvent(iStore++);
    if(pnum<0) {
      cerr << "\n getoscarevent <E>: At iCall = " << iCall << ", iStore = " << iStore-1;
      cerr << ",  number of particle in events = " << pnum << endl;
      exit(1);
    }
  }

  *mxtot = int(pnum) + 1;  // number of particles in this event (EXODUS counts from 0)
  if(mxtot==0) return;

  Int_t kpart = 0;
  Int_t kpart3 = 0;
  Int_t kpart4 = 0;

  idtot[kpart] = Int_t(pid);
  pptot[kpart4] = E;
  pptot[kpart4+1] = px;
  pptot[kpart4+2] = py;
  pptot[kpart4+3] = pz;
  xyzmv[kpart3] = FMTOCM*xvtx;
  xyzmv[kpart3+1] = FMTOCM*yvtx;
  xyzmv[kpart3+2] = FMTOCM*zvtx;

  Float_t pnumSave = pnum;
  Float_t eventSave = event;
  Int_t nMore = *mxtot - 1;

  // Retrieve the remaining particles in this event
  for(Int_t kMore=0; kMore<nMore; kMore++) 
  {
    
    if(iStore >= nentries) 
    {
      
      cerr << "getoscarevent - Trying to get non-existent entry in ROOT file " << endl;
      cerr << "getoscarevent - at iCall = " << iCall << ", iStore = " << iStore;
      cerr << " , nentries = " << nentries << endl;
      exit(1);
    }
    
    particle->GetEvent(iStore++);
    if(pnum != pnumSave || event != eventSave) 
    {
      cerr << "getoscarevent - event or pnum mismatch " << endl;
      cerr << "getoscarevent - At iCall = " << iCall << ", iStore = " << iStore-1 << endl;
      cerr << "getoscarevent - pnum = " << pnum  << " pnumSave = " << pnumSave << endl;
      cerr << "getoscarevent - event = " << event  << " eventSave = " << eventSave << endl;
      exit(1);
    }
    kpart++;
    kpart4 += 4;
    kpart3 += 3;
    idtot[kpart] = int(pid);
    pptot[kpart4] = E;
    pptot[kpart4+1] = px;
    pptot[kpart4+2] = py;
    pptot[kpart4+3] = pz;
    xyzmv[kpart3] = FMTOCM*xvtx;
    xyzmv[kpart3+1] = FMTOCM*yvtx;
    xyzmv[kpart3+2] = FMTOCM*zvtx;

  }

  if(iStore >= nentries) 
  {
    // There are no more entries in this OSCAR file
    kClone++;
    iStore = 0;

    // Check if there are more clones to be done
    if(kClone < *nClone) 
    {
      cout << "getoscarevent - Finished clone cycle " << kClone;
      cout << ", iCall " << iCall;
      cout << ",  proceeding to the next clone cycle with max cycles = " << *nClone;
      cout << endl;
      iFirst = 1;
    } else {
     
      // Change the mxtot to be negative in order to signal oscarRootInput
      *mxtot = -(int(pnum) + 1);
      cout << "getoscarevent - This is the last event in this oscar.root file";
      cout << ", iCall " << iCall << endl;
      kClone = 0;
      iFirst = 0;
    } // check on kClone

  }  // check on iStore  
 
  iCall++;
  return;

}
