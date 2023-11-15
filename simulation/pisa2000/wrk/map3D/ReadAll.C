{
gROOT->Reset();

Float_t  z,r,phi,bz,br,bphi,btot;
Float_t bkey;
Int_t ikey;

Int_t iRmaxAll = 101;
Int_t iZmaxAll = 101;
Int_t iPhimaxAll = 120; 

Int_t iR;
Int_t iZ;
Int_t iPhi;

Float_t rStart = 0.0;
Float_t zStart = -200.0;
Float_t phiStart = 0.0;
Float_t rDel = 4.0;
Float_t zDel = 4.0;
Float_t phiDel = 3.0;

TFile *f = new TFile("SimAll3D.root", "RECREATE");
TNtuple *ntuple = new TNtuple("SimAll","Data from two simulation files","z:r:phi:bz:br:bphi:key");

ifstream fp("simuAll.dat");
if(fp.bad()){
  cout << "\n Cannot open input file" << endl;
  exit(1);
}

Int_t irec = 0;
for(iR=0; iR<iRmaxAll; iR++){
  r = rStart + float(iR)*rDel;

  for(iZ=0; iZ<iZmaxAll; iZ++){
    z = zStart + float(iZ)*zDel;

    for(iPhi=0; iPhi<iPhimaxAll; iPhi++){
      phi = phiStart + float(iPhi)*phiDel;

      fp >> bz >> br >> bphi >> bkey;
      irec++;

      ikey = bkey;
      ntuple->Fill(z,r,phi,bz,br,bphi,ikey);
    } // loop over Phi
  }  // loop over Z
}  // loop over R

f->Write();
f->Close();

}


