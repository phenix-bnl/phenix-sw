#include <cstdlib>
#include <iostream>
#include <fstream>

#include <TFile.h>
#include <TNtuple.h>

using namespace std;

extern "C" void read3droot_(float vector3d[], int *n3dtotal) 
{

  TFile *f = 0; 
  //
  // Check if the map file exists, locally first, and then check AFS
  // 
  ifstream fp("Sim3D.root",ios::in);
  if(!fp || fp.bad()){
    cout << "read3droot - Unable to find local file Sim3D.root." << endl;
    cout << "read3droot - Will try /afs/rhic.bnl.gov/phenix/software/simulation area" << endl;
    ifstream fp("/afs/rhic.bnl.gov/phenix/software/simulation/Sim3D.root",ios::in);
    if(!fp || fp.bad()){
      cout << "read3droot - Unable to find AFS file Sim3D.root" << endl;
      cout << "read3droot - Exiting " << endl;
      exit(1);
    }  // check on missing AFS copy
    fp.close();
    f = new TFile("/afs/rhic.bnl.gov/phenix/software/simulation/Sim3D.root");
  }  // missing local copy
  
  if(!f) {
    fp.close();
    f = new TFile("Sim3D.root");
  }  // local copy is present

   TTree *SimAll = (TTree*)gDirectory->Get("SimAll");
   if(!SimAll) {
     cout << "read3droot - Cannot find  SimAll " << endl;
     exit(1);
   }

//Declaration of leaves types
   Float_t  r;
   Float_t  z;
   Float_t  phi;
   Float_t  bz;
   Float_t  br;
   Float_t  bphi;
   Int_t    key;

//Set branch addresses
   SimAll->SetBranchAddress("r",&r);
   SimAll->SetBranchAddress("z",&z);
   SimAll->SetBranchAddress("phi",&phi);
   SimAll->SetBranchAddress("bz",&bz);
   SimAll->SetBranchAddress("br",&br);
   SimAll->SetBranchAddress("bphi",&bphi);
   SimAll->SetBranchAddress("key",&key);

   Int_t nentries = (Int_t)SimAll->GetEntries();
   if(nentries != *n3dtotal){
     cout 
      << "read3droot - File number entries = " << nentries
      << " does not match requested number = " << *n3dtotal << endl;
     exit(1);
   }
   Int_t nbytes = 0;

   Int_t ip = 0;
   for (Int_t i=0; i<nentries;i++) {
      nbytes += SimAll->GetEvent(i);
      vector3d[ip++] = bz;
      vector3d[ip++] = br;
      vector3d[ip++] = bphi;
   }

   cout << "read3drooot - 3D map file read for " << nentries << " space points" << endl;
   cout << endl;

  f->Close();

  return;
}
