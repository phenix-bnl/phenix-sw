#define EmbedMcRecoTrack_cxx
#include "EmbedMcRecoTrack.hh"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TNtuple.h"
#include <iostream>
void EmbedMcRecoTrack::Loop(char *name)
{
  // This version was copied and modified from the one in the nov2008 directory.  
  bool debug = false;

  if (fChain == 0) return;

   TFile *f = new TFile(name,"RECREATE");
   TNtuple *nt = new TNtuple("EmbedMcRecoTrack",
			     "EmbedMcRecoTrack",

			     "pc3dphiS:pc3dzS:pc3dphiR:pc3dzR:"
			     "pc3sdphiS:pc3sdzS:pc3sdphiR:pc3sdzR:"

			     "QS:QR:zedS:zedR:nx1hS:nx2hS:nx1hR:nx2hR:"
			     "phiS:phiR:thetaS:thetaR:alphaS:alphaR:"
			     "momG:momS:momR:type:gen:bbccent:crknpmt0S:crknpmt0R");

//    TNtuple *nt = new TNtuple("EmbedMcRecoTrack",
// 			     "EmbedMcRecoTrack",
// 			     "pc2pS:pc2zS:pc3pS:pc3zS:pc2pR:pc2zR:pc3pR:pc3zR"
// 			     ":napS:brpS:nazS:brzS:napR:brpR:nazR:brzR:"
// 			     "QS:QR:zedS:zedR:nx1hS:nx2hS:nx1hR:nx2hR:phiS:phiR:"
// 			     "ntrkwBR:ntrkeBR:ntrkwAR:ntrkeAR:centp:centc:"
// 			     "momG:w1:w2:momS:momR:type:gen:bbccent");

   float array[100];

   Int_t nentries;

   if (debug) 
     nentries = 10000;
   else
     nentries = Int_t(fChain->GetEntries());
   cout << nentries << " entries" << endl;
   int m;
   Int_t nbytes = 0, nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
     
     // in case of a TChain, ientry is the entry number in the current file
      Int_t ientry = LoadTree(jentry); 
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      if(jentry%100000==1)cout<<jentry<<endl;
      m=0;

      array[m++] = pc3dphiS;
      array[m++] = pc3dzS;
      array[m++] = pc3dphiR;
      array[m++] = pc3dzR;
      array[m++] = pc3sdphiS;
      array[m++] = pc3sdzS;
      array[m++] = pc3sdphiR;
      array[m++] = pc3sdzR;
      array[m++] = dctrkQualS;
      array[m++] = dctrkQualR;
      array[m++] = zedS;
      array[m++] = zedR;
      array[m++] = x1hS;
      array[m++] = x2hS;
      array[m++] = x1hR;
      array[m++] = x2hR;
      array[m++] = phiS;
      array[m++] = phiR;
      array[m++] = thetaS;
      array[m++] = thetaR;
      array[m++] = alphaS;
      array[m++] = alphaR;
      array[m++] = momG;
      array[m++] = momS;
      array[m++] = momR;
      array[m++] = type;
      array[m++] = genG;
      array[m++] = bbccent;
      array[m++] = crknpmt0S;
      array[m++] = crknpmt0R;

      if (bbccent != -1)  nt->Fill(array);
   } // entry loop

   f->Write();
   f->Close();
}
