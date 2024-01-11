#include "OccEff.h"
#include "Treeder.h"
#include "TSystem.h"
#include "TROOT.h"
#include "TStyle.h"
#include "TLegend.h"
#include <cmath>
#include <iostream>

using namespace std;

OccEff::OccEff(int ncb, double cb[], int npt, double pt[]) :
  nPid(4),
  nType(4),
  nCentBins(ncb),
  nPtBins(npt)
{
  char scen[500];
  char smom[500];
  outFile = new TFile(Form("embEff_2sigPC3_qual3163_RScuts.root"), "recreate");
  cout << "OccEff::Occeff(): initializing with these bins:" << endl;

  // Set up private containers to store the binning
  hCentBin = new TH1F("hCentBin", "hCentBin", ncb, cb);
  hPtBin = new TH1F("hPtBin", "hPtBin", npt, pt);
  for (int i=0; i<ncb; i++) {
    centBin.push_back(cb[i]);
    sprintf(scen, "%.0f-%.0f %%", cb[i], cb[i+1]);
    sCentBin.push_back(string(scen));
    cout<<sCentBin.at(i)<<endl;
  }
  for (int i=0; i<npt; i++) {
    ptBin.push_back(pt[i]);
    sprintf(smom,"%.1f-%.1f GeV/c", pt[i], pt[i+1]);
    sPtBin.push_back(string(smom));
    cout<<sPtBin.at(i)<<endl;
  }
  
  sPid.push_back("pi");
  sPid.push_back("k");
  sPid.push_back("p");
  sPid.push_back("all");

  sType.push_back("S"); // single MC, ~same as fun4pisa output
  sType.push_back("R"); // MC reconstructed w/ real bg hits
  sType.push_back("E"); // efficiency (R/S)
  sType.push_back("G"); // GEANT
}

int OccEff::loopTree(TFile* inFile, string treeName, ePid pid)
{
  Treeder t;
  TTree* nt = (TTree*)inFile->Get(treeName.c_str());
  t.set_branchnames(nt);

  int nent = nt->GetEntries();

  cout << Form("Looping thru %.2g entries...", double(nent)) << endl;
  int counter=0;
  for (int i=0; i<nent; i++) {

    if (i%10000==0) { // print progress
      cout << " " << i << flush;
      counter++;
      if (counter==10) {
	cout << endl;
	counter=0;
      }
    }
    t.getentry(nt, i);

    // Variables that exist for both S and R
    double mom[2], pc3dphi[2], pc3dz[2], pc3sdphi[2], pc3sdz[2], zed[2], Q[2], n0[2];
    double nx1h[2], nx2h[2];
    double theta[2], pt[2];

    // Cuts
    bool passPC3match[2], zedOK[2], qualOK[2], ptOK[2];
    bool passRICHveto[2];
    bool nxOK[2]; // Are there enough x1 and x2 hits?
    bool passAllCuts[2];
    
    // Assign pt, cent bins
    int ipt[2], icb; 
    // Get the common variables
    double type = t.value("type");
    double gen = t.value("gen");
    double bbccent = t.value("bbccent");
    icb= hCentBin->FindBin(bbccent) - 1;
    bool isType2 = (type==2);    
    bool isCentOK = (icb >=0 && icb < nCentBins);

    // This cut has no effect, at least not in addition to the others.
    bool isMomOK = fabs(t.value("momG")-t.value("momS")) < 0.1 * t.value("momG");
    // New 9/21/2009: This does not change the embedding efficiency by any noticeable amount.
    // It does introduce larger scatter in the points and increase the errors though.
    // I will not use it.
    //    double wt = spectralWeight(t.value("momG"));
    double wt = 1.0;    

    for (int k=0; k<2; k++) { // 0="S", 1="R"
      mom[k]      = t.value(Form("mom%s", sType[k]));
      theta[k]    = t.value(Form("theta%s", sType[k]));
      pc3dphi[k]  = t.value(Form("pc3dphi%s", sType[k]));
      pc3dz[k]    = t.value(Form("pc3dz%s", sType[k]));
      pc3sdphi[k] = t.value(Form("pc3sdphi%s", sType[k]));
      pc3sdz[k]   = t.value(Form("pc3sdz%s", sType[k]));
      zed[k]      = t.value(Form("zed%s", sType[k]));
      Q[k]        = t.value(Form("Q%s", sType[k]));
      n0[k]       = t.value(Form("crknpmt0%s", sType[k]));
      nx1h[k]     = t.value(Form("nx1h%s", sType[k]));
      nx2h[k]     = t.value(Form("nx2h%s", sType[k]));

      pt[k]   = mom[k]*sin(theta[k]);   
      ipt[k]  = hPtBin->FindBin(pt[k]) - 1;
      
      nSigma = 2.0; // pt[k]<5.0? 3.0 : 2.0;
 
      // ---Define cuts---
      passPC3match[k] = pow(pc3sdz[k],2)+pow(pc3sdphi[k],2) < nSigma*nSigma;
      zedOK[k]        = fabs(zed[k]) < zedCut;
      qualOK[k]       = Q[k]==31 || Q[k]==63; // pt[k]<5.0? Q[k]>7 : Q[k]==31 || Q[k]==63;
      ptOK[k]         = ipt[k] >= 0 && ipt[k] < nPtBins;  
      passRICHveto[k] = (mom[k] < 4.8) ? n0[k] < 1 : true;
      nxOK[k]       = nx1h[k] >= 2 && nx2h[k] >= 2;
      
      passAllCuts[k]  = 
	gen==1 
	&& isType2 
	&& zedOK[k] 
	&& qualOK[k] 
	&& nxOK[k] 
	&& passPC3match[k]; 
	//	&& passRICHveto[k];
      
    } // k=S,R loop
    
    if (ptOK[S]==false || ptOK[R]==false) continue;
    if (isCentOK==false) continue;
    
    // ---Fill histos---
    // Yields vs. momentum and centrality
    if (passAllCuts[S]) {
      hMom.at(pid).at(S).at(icb)->Fill(mom[S], wt);
      hCen.at(pid).at(S).at(ipt[S])->Fill(bbccent, wt);
    }
    if (passAllCuts[R] && passAllCuts[S]) {
      hMom.at(pid).at(R).at(icb)->Fill(mom[R], wt);
      hCen.at(pid).at(R).at(ipt[R])->Fill(bbccent, wt);
    }

  } // End entry loop i.
  cout << endl;
  

  // ---Now divide R/S to make E histos---
  for(int ipt=0; ipt<nPtBins; ipt++) {
    hCen[pid][E][ipt]->Divide(hCen[pid][R][ipt], hCen[pid][S][ipt]);
  }
  for(int icb=0; icb<nCentBins; icb++) {
    hMom[pid][E][icb]->Divide(hMom[pid][R][icb], hMom[pid][S][icb]);
  }
  
  outFile->Write();
  
  return 0;
}

// Clipped straight from SingPartEff
double OccEff::spectralWeight(double pt) {

  double a[4] =
    {1.28055e+08, -2.50683e+00, 2.30999e+05, -2.11475e+00};

  double wt = a[0]*exp(a[1]*pt) + a[2]*pow(pt, a[3]);

  return wt;
}

void OccEff::bookHistos()
{

 for (int ip=0; ip<nPid; ip++) {
    vector<vector<TH1F*> > vvpt;
    vector<vector<TH1F*> > vvcb;
    for (int it=0; it<nType; it++) {
      
      // vpt = Centrality distributions for each pt bin
      // vcb = pT distributions for each centrality bin
      vector<TH1F*> vpt, vcb;
      for(int ipt=0; ipt<nPtBins; ipt++) {
       	vcb.push_back((TH1F*)hCentBin->Clone(Form("hCen_%i_%i_%i", ip,it,ipt))); 
      }
      for(int icb=0; icb<nCentBins; icb++) {
       	vpt.push_back((TH1F*)hPtBin->Clone(Form("hMom_%i_%i_%i", ip,it,icb))); 
      }
      vvpt.push_back(vpt);
      vvcb.push_back(vcb);
    }
    hMom.push_back(vvpt);
    hCen.push_back(vvcb);
  }

 // Now set them up...
 for (int ip=0; ip<nPid; ip++) {
   for (int it=0; it<nType; it++) {
     for(int ipt=0; ipt<nPtBins; ipt++) {
       assert(hCen[ip][it][ipt]);
       hCen[ip][it][ipt]->SetTitle(Form("%s, Type %s, %s;Centrality (%%)",
					sPid.at(ip),
					sType.at(it),
					sPtBin.at(ipt).c_str()));
       hCen[ip][it][ipt]->Sumw2();
     }
     for(int icb=0; icb<nCentBins; icb++) {
       assert(hMom[ip][it][icb]);
       hMom[ip][it][icb]->SetTitle(Form("%s, Type %s, %s;Momentum (GeV/c)",
					sPid.at(ip),
					sType.at(it),
					sCentBin.at(icb).c_str()));
       hMom[ip][it][icb]->Sumw2();
     }
   }
 }
 
 return;
}

int main(int argc, char* argv[]) 
{
  const int nFiles = 3;
  enum eFile {PION, KAON, PROT};

  char* inFileDir =
    "/phenix/u/workarea/adare/offline/packages/embed/post";
  char* pidStr[nFiles] = {"pions_n0", "kaons", "ppbar"};

  TFile* f[3];

  //  for (int i=0; i<nFiles; i++) {
  for (int i=0; i<1; i++) {
    f[i] = new TFile(Form("%s/recal_%s.root", inFileDir, pidStr[i]), "read");
  }

  const int ncentbins = 9;
  double centbin[ncentbins+1] = {0, 10, 20, 30, 40, 50, 60, 70, 80, 93};
  const int nptbins = 7;
  double ptbin[nptbins+1] = {0.0, 0.5, 1, 2, 3, 5, 7, 10};

  OccEff occ(ncentbins, centbin, nptbins, ptbin);
  occ.setDchZedCut(80.0);        // |zed| < 80cm
  
  // The set function below is not being used now...
  //  occ.setPC3sigmaCut(2.0);  // PC3 matching in units of sigma

  cout << "bookHistos()"<< endl;
  occ.bookHistos();

  cout << "loopTree()"<< endl;
  occ.loopTree(f[PION], "zip");


  return 0;
}
