#define Recal_cxx
#include "Recal.h"
#include <TF1.h>
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TNtuple.h>
#include <iostream>

bool debug = false;

Recal::Recal(TTree *tree, char* outFileName, int ncb, double cb[], int npt, double pb[]) :
  nTypes(6),
  nPar(4),
  nCentBins(ncb),
  nPtBins(npt),
  pc3dzMin(-10.0),
  pc3dzMax(10.0),
  pc3dpMin(-0.01),
  pc3dpMax(0.01),
  zedCut(80),
  fitPtMin(0.5),
  fitPtMax(10.0)
{
  // if parameter tree is not specified (or zero), connect the file
  // used to generate this class and read the Tree.
  if (tree == 0) {
    TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("../embedcom_pions.root");
    if (!f) {
      f = new TFile("../embedcom_pions.root");
    }
    tree = (TTree*)gDirectory->Get("EmbedMcRecoTrack");
    
  }
  Init(tree);
  
  outFile = new TFile(outFileName, "recreate");
  
  // eType {S, R, SP, SN, RP, RN};
  sType[S] = "S";
  sType[R] = "R";
  sType[SP] = "S+";
  sType[SN] = "S-";
  sType[RP] = "R+";
  sType[RN] = "R-";

  color[S] = color[SP] = color[SN] = kBlack;
  color[R] = color[RP] = color[RN] = kRed;

  sPar[PMU] = "pc3#Delta#phi #mu";
  sPar[PSG] = "pc3#Delta#phi #sigma";
  sPar[ZMU] = "pc3#Delta z #mu";
  sPar[ZSG] = "pc3#Delta z #sigma";

  cout << "Recal::Recal(): " << endl;
  cout << nCentBins << " centrality bins: " << flush;
  for (int icb=0; icb<nCentBins+1; icb++) {
    centBin[icb] = cb[icb];
    cout << centBin[icb] << " " << flush;
  }
  cout << endl;
  cout << nPtBins << " momentum bins:   " << flush;
  for (int ipt=0; ipt<nPtBins+1; ipt++) {
    ptBin[ipt] = pb[ipt];  
    cout << ptBin[ipt] << " " << flush;
  }
  cout << endl;
}

void Recal::matchRecal()
{
  Long64_t nentries;
  nentries = fChain->GetEntriesFast();

  cout << nentries << " entries." << endl;
 
  Long64_t nbytes = 0, nb = 0;
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   
    nbytes += nb;
    if(jentry%100000==1)
      cout << jentry << endl;

    // ---Start here---
    double dz[2], dphi[2];
    int ipt[2], icb;
    bool isType2, isCentOK;
    bool isPositive[2], isNegative[2];
    bool zedOK[2], qualOK[2], ptOK[2];
    bool passAllCuts[2];
    dz[S]           = pc3dzS;
    dz[R]           = pc3dzR;
    dphi[S]         = pc3dphiS;
    dphi[R]         = pc3dphiR;
    ipt[S]          = hPtBin->FindBin(momS) - 1;
    ipt[R]          = hPtBin->FindBin(momR) - 1;
    icb             = hCentBin->FindBin(bbccent) - 1;

    isType2         = (type==2);
    isCentOK        = (icb >=0 && icb < nCentBins);
    zedOK[S]        = fabs(zedS) < zedCut;
    zedOK[R]        = fabs(zedR) < zedCut;
    qualOK[S]       = QS==31 || QS==63;
    qualOK[R]       = QR==31 || QR==63;
    ptOK[S]         = ipt[S] >= 0 && ipt[S] < nPtBins;
    ptOK[R]         = ipt[R] >= 0 && ipt[R] < nPtBins;

    isPositive[S]   = alphaS > 0;
    isPositive[R]   = alphaR > 0;
    isNegative[S]   = alphaS > -99 && alphaS < 0;
    isNegative[R]   = alphaR > -99 && alphaR < 0;
 
    passAllCuts[S]  = gen==1 && isType2 && isCentOK && zedOK[S] && qualOK[S] && ptOK[S];
    passAllCuts[R]  = gen==1 && isType2 && isCentOK && zedOK[R] && qualOK[R] && ptOK[R];

    for (int it=0; it<2; it++) {

      // Fill PC3 delta phi distributions
      if (passAllCuts[it] && dz[it] > pc3dzMin && dz[it] < pc3dzMax) {
	assert(	hp[it][icb][ipt[it]] );
	
	// combined + and - tracks
	hp[it][icb][ipt[it]]->Fill(dphi[it]);
	
	// individual charges
	if (it==S && isPositive[it]) hp[SP][icb][ipt[it]]->Fill(dphi[it]);
	if (it==S && isNegative[it]) hp[SN][icb][ipt[it]]->Fill(dphi[it]);
	if (it==R && isPositive[it]) hp[RP][icb][ipt[it]]->Fill(dphi[it]);
	if (it==R && isNegative[it]) hp[RN][icb][ipt[it]]->Fill(dphi[it]);

      }

      // Fill PC3 delta z distributions
      if (passAllCuts[it] && dphi[it] > pc3dpMin && dphi[it] < pc3dpMax) {
	assert(	hz[it][icb][ipt[it]] );

	// combined + and - tracks
	hz[it][icb][ipt[it]]->Fill(dz[it]);

	// individual charges
	if (it==S && isPositive[it]) hz[SP][icb][ipt[it]]->Fill(dz[it]);
	if (it==S && isNegative[it]) hz[SN][icb][ipt[it]]->Fill(dz[it]);
	if (it==R && isPositive[it]) hz[RP][icb][ipt[it]]->Fill(dz[it]);
	if (it==R && isNegative[it]) hz[RN][icb][ipt[it]]->Fill(dz[it]);
      }
    }

  } // end jentry loop
  

  // Fit the matching distributions, and 
  // fill the hf histos with the fit params.  
  for (int it=0; it<nTypes; it++) { // 0=S, 1=R
    for (int icb=0; icb<nCentBins; icb++) {
      for (int ip=0; ip<nPtBins; ip++) {
        assert(hp[it][icb][ip]);
        assert(hz[it][icb][ip]);	
	hp[it][icb][ip]->SetLineColor(color[it]);
	hz[it][icb][ip]->SetLineColor(color[it]);	

	hp[it][icb][ip]->Fit("gaus");
	hz[it][icb][ip]->Fit("gaus");

	TF1* fp = (TF1*) hp[it][icb][ip]->GetFunction("gaus");
	TF1* fz = (TF1*) hz[it][icb][ip]->GetFunction("gaus");
	fp->SetLineColor(color[it]);
	fz->SetLineColor(color[it]);
	
	double par[nPar]; // means and sigmas
	double err[nPar];

	par[PMU] = fp->GetParameter(1);
	par[PSG] = fp->GetParameter(2);
	par[ZMU] = fz->GetParameter(1);
	par[ZSG] = fz->GetParameter(2);

	err[PMU] = fp->GetParError(1);
	err[PSG] = fp->GetParError(2);
	err[ZMU] = fz->GetParError(1);
	err[ZSG] = fz->GetParError(2);

	for (int ic=0; ic<nPar; ic++) { // PMU, PSG, ZMU, ZSG
	hf[ic][it][icb]->SetBinContent(ip+1, par[ic]);
	hf[ic][it][icb]->SetBinError(ip+1, err[ic]);
	}

      }
    }
  }


  for (int it=0; it<nTypes; it++) {
    for (int icb=0; icb<nCentBins; icb++) {
      for (int ic=0; ic<nPar; ic++) { // PMU, PSG, ZMU, ZSG
	fitPC3VsPt(hf[ic][it][icb]);
      }
    }
  }

  return;
}


void Recal::Loop()
{
  // u stands for un-recalibrated
  TNtuple *nt = new TNtuple("zip", "zip", 
			    "pc3dphiS:pc3dzS:pc3dphiR:pc3dzR:"
			    "pc3sdphiS:pc3sdzS:pc3sdphiR:pc3sdzR:"
			    "u_pc3sdphiS:u_pc3sdzS:u_pc3sdphiR:u_pc3sdzR:"
			    "QS:QR:zedS:zedR:nx1hS:nx2hS:nx1hR:nx2hR:"
			    "phiS:phiR:thetaS:thetaR:"
			    "momG:w1:w2:momS:momR:type:gen:bbccent:crknpmt0S:crknpmt0R");
  float array[100]; 
  int m;
  
  if (fChain == 0) return;
    
  Long64_t nentries;
  if (debug) 
    nentries = 1000;
  else 
    nentries = fChain->GetEntries();
 
  cout << nentries << " entries." << endl;
 
  Long64_t nbytes = 0, nb = 0;
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   
    nbytes += nb;
    if(jentry%100000==1)
      cout << jentry << endl;
  
    m = 0;

    // Stuff for recalibration calcs
    double p_sigS = -9999, z_sigS = -9999, p_muS = -9999, z_muS = -9999;
    double p_sigR = -9999, z_sigR = -9999, p_muR = -9999, z_muR = -9999;
    // eType: {S, R, SP, SN, RP, RN}
    int icb = hCentBin->FindBin(bbccent) - 1;
    bool isCentOK        = (icb >=0 && icb < nCentBins);
    bool momS_ok = momS >= fitPtMin && momS < fitPtMax;
    bool momR_ok = momR >= fitPtMin && momR < fitPtMax;

    if (!isCentOK) {
      cout << "Bad centrality bin " << icb << ", bbccent= " << bbccent << endl;
      continue;
    }

    // Look up the correction values...
    // These are fits to the Gaussian fit parameters vs. pT.
    if (alphaS > 0) { // Positive
      if (momS_ok) {
	for (int ic=0; ic<nPar; ic++) assert(hf[ic][SP][icb]);
 
	p_muS  = hf[PMU][SP][icb]->GetFunction("fexp")->Eval(momS);
	p_sigS = hf[PSG][SP][icb]->GetFunction("fexp")->Eval(momS);
	z_muS  = hf[ZMU][SP][icb]->GetFunction("fexp")->Eval(momS);
	z_sigS = hf[ZSG][SP][icb]->GetFunction("fexp")->Eval(momS);
      }
      
      if (momR_ok) {
	for (int ic=0; ic<nPar; ic++) assert(hf[ic][RP][icb]);

	p_muR  = hf[PMU][RP][icb]->GetFunction("fexp")->Eval(momR);
	p_sigR = hf[PSG][RP][icb]->GetFunction("fexp")->Eval(momR);
	z_muR  = hf[ZMU][RP][icb]->GetFunction("fexp")->Eval(momR);
	z_sigR = hf[ZSG][RP][icb]->GetFunction("fexp")->Eval(momR);
      }
    }
    
    if (alphaS > -99 && alphaS < 0) { // Negative
      if (momS_ok) {
	for (int ic=0; ic<nPar; ic++) assert(hf[ic][SN][icb]);

	p_muS  = hf[PMU][SN][icb]->GetFunction("fexp")->Eval(momS);
	p_sigS = hf[PSG][SN][icb]->GetFunction("fexp")->Eval(momS);
	z_muS  = hf[ZMU][SN][icb]->GetFunction("fexp")->Eval(momS);
	z_sigS = hf[ZSG][SN][icb]->GetFunction("fexp")->Eval(momS);
      }
      
      if (momR_ok) {
	for (int ic=0; ic<nPar; ic++) assert(hf[ic][RN][icb]);

	p_muR  = hf[PMU][RN][icb]->GetFunction("fexp")->Eval(momR);
	p_sigR = hf[PSG][RN][icb]->GetFunction("fexp")->Eval(momR);
	z_muR  = hf[ZMU][RN][icb]->GetFunction("fexp")->Eval(momR);
	z_sigR = hf[ZSG][RN][icb]->GetFunction("fexp")->Eval(momR);
      }
    }

    bool pS_ok = (p_sigS != -9999 && p_muS != -9999);
    bool zS_ok = (z_sigS != -9999 && z_muS != -9999);

    bool pR_ok = (p_sigR != -9999 && p_muR != -9999);
    bool zR_ok = (z_sigR != -9999 && z_muR != -9999);

    //    if (!zR_ok) cout << "z_sigR, z_muR: " << z_sigR << ", " << z_muR << endl;

    //     "pc3dphiS:pc3dzS:pc3dphiR:pc3dzR:"
    //     "pc3sdphiS:pc3sdzS:pc3sdphiR:pc3sdzR:"
    //     "u_pc3sdphiS:u_pc3sdzS:u_pc3sdphiR:u_pc3sdzR:"
    //     "QS:QR:zedS:zedR:nx1hS:nx2hS:nx1hR:nx2hR:"
    //     "phiS:phiR:thetaS:thetaR:"
    //     "momG:w1:w2:momS:momR:type:gen:bbccent");
    
    // Raw matching variables
    array[m++] = pc3dphiS;
    array[m++] = pc3dzS;
    array[m++] = pc3dphiR;
    array[m++] = pc3dzR;
    
    // recalibrated!
    array[m++] = pS_ok ? (pc3dphiS - p_muS) / p_sigS : -9999;
    array[m++] = zS_ok ? (pc3dzS   - z_muS) / z_sigS : -9999;
    array[m++] = pR_ok ? (pc3dphiR - p_muR) / p_sigR : -9999;
    array[m++] = zR_ok ? (pc3dzR   - z_muR) / z_sigR : -9999;

    // un-recalibrated
    array[m++] = pc3sdphiS;
    array[m++] = pc3sdzS;
    array[m++] = pc3sdphiR;
    array[m++] = pc3sdzR;

    array[m++] = QS;
    array[m++] = QR;
    array[m++] = zedS;
    array[m++] = zedR;
    array[m++] = nx1hS;
    array[m++] = nx2hS;
    array[m++] = nx1hR;
    array[m++] = nx2hR;
    array[m++] = phiS;
    array[m++] = phiR;
    array[m++] = thetaS;
    array[m++] = thetaR;
    array[m++] = momG;
    array[m++] = w1;
    array[m++] = w2;
    array[m++] = momS;
    array[m++] = momR;
    array[m++] = type;
    array[m++] = gen;
    array[m++] = bbccent;
    array[m++] = crknpmt0S;
    array[m++] = crknpmt0R;

    nt->Fill(array);

  } // jentry loop

  outFile->Write();
  outFile->Close();
  return;
}

void Recal::bookHistos()
{
  hCentBin = new TH1F("hCentBin", "hCentBin", nCentBins, centBin);
  hPtBin = new TH1F("hPtBin", "hPtBin", nPtBins, ptBin);

  for (int it=0; it<nTypes; it++) { // 0=S, 1=R
    for (int icb=0; icb<nCentBins; icb++) {
      for (int ipt=0; ipt<nPtBins; ipt++) {
	hp[it][icb][ipt] = new TH1F(Form("hp_%i_%i_%i", it, icb, ipt), 
				    Form("PC3#Delta#phi, %.0f-%.0f %%, %.1f-%.1f GeV", 
					 centBin[icb], centBin[icb+1],
					 ptBin[ipt], ptBin[ipt+1] ),
				    100, pc3dpMin, pc3dpMax); 
	hz[it][icb][ipt] = new TH1F(Form("hz_%i_%i_%i", it, icb, ipt), 
				    Form("PC3#Delta z, %.0f-%.0f %%, %.1f-%.1f GeV", 
					 centBin[icb], centBin[icb+1],
					 ptBin[ipt], ptBin[ipt+1] ),
				    100, pc3dzMin, pc3dzMax);
      }
    }
  }
   
  double ymin[4], ymax[4];
  ymin[PMU] = 0.05*pc3dpMin;  ymax[PMU] = 0.05*pc3dpMax;
  ymin[PSG] = 0.0005;         ymax[PSG] = 0.3*pc3dpMax;
  ymin[ZMU] = 0.02*pc3dzMin;  ymax[ZMU] = 0.02*pc3dzMax;
  ymin[ZSG] = 0.0;            ymax[ZSG] = 0.2*pc3dzMax;

  for (int ic=0; ic<nPar; ic++) { // PMU, PSG, ZMU, ZSG
    for (int it=0; it<nTypes; it++) { // 0=S, 1=R
      for (int icb=0; icb<nCentBins; icb++) {
	hf[ic][it][icb] = (TH1F*) hPtBin->Clone(Form("hf_%i_%i_%i", ic, it, icb));
	hf[ic][it][icb]->SetTitle( Form("PC3 %s, Type %s, %.0f-%.0f %%;p_{T};",
					sPar[ic], sType[it], centBin[icb], centBin[icb+1]));
        if (it==S || it==R)
	  hf[ic][it][icb]->SetMarkerStyle(kFullCircle);
	else if (it==SP || it==RP)
	  hf[ic][it][icb]->SetMarkerStyle(kOpenCircle);
	else if (it==SN || it==RN)
	  hf[ic][it][icb]->SetMarkerStyle(kOpenSquare);

        hf[ic][it][icb]->SetLineWidth(2);
        hf[ic][it][icb]->SetLineColor(color[it]);
        hf[ic][it][icb]->SetMarkerColor(color[it]);
	hf[ic][it][icb]->SetMarkerSize(0.5);
        hf[ic][it][icb]->GetYaxis()->SetRangeUser(ymin[ic], ymax[ic]);
      }
    }
  }
  

  /*
    for (int it=0; it<ntypes; it++) { // 0=S, 1=R
    vector<vector<TH1F*> > vvptp, vvptz;
    for (int icb=0; icb<ncentbins; icb++) {
    vector<TH1F*> vptp, vptz;
    for (int ipt=0; ipt<nptbins; ipt++) {
    cout << "before " << flush;
    vptp.push_back(new TH1F(Form("hp_%i_%i_%i", it, icb, ipt), 
    Form("PC3#Delta#phi, %.0f-%.0f %%, %.1f-%.1f GeV", 
    centbin[icb], centbin[icb+1],
    ptbin[ipt], ptbin[ipt+1] ),
    100, pc3dpMin, pc3dpMax)); 
    vptz.push_back(new TH1F(Form("hz_%i_%i_%i", it, icb, ipt), 
    Form("PC3#Delta z, %.0f-%.0f %%, %.1f-%.1f GeV", 
    centbin[icb], centbin[icb+1],
    ptbin[ipt], ptbin[ipt+1] ),
    100, pc3dzMin, pc3dzMax)); 
    
    
    cout <<	Form("PC3#Delta#phi, %.0f-%.0f %%, %.1f-%.1f GeV", 
    centbin[icb], centbin[icb+1],
    ptbin[ipt], ptbin[ipt+1] ) << endl;
    
    }
    cout << " vv.push_back " << endl;
    vvptp.push_back(vptp);
    vvptz.push_back(vptz);
    cout << " after " << endl;
    }
    hp.push_back(vvptp);
    hz.push_back(vvptz);
    }
  */
  
  return;
}

void Recal::fitPC3VsPt(TH1F* h)
{
  //  Fit widths and means vs. pT with offset exponential + pedestal
  TF1* fexp = new TF1("fexp", "[0]*exp([1]+[2]*x) + [3]", fitPtMin, fitPtMax);
  fexp->SetParameters(1e-5, 1.0, -1.0, h->GetBinContent(h->FindBin(5.5)));
  fexp->SetParLimits(2, -5.0, 5.0);
  fexp->SetLineColor(h->GetLineColor());
  h->Fit(fexp, "R", "goff");
  return;
}
