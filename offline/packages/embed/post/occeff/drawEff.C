TH1F* hCenRich;

void drawEff()
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);

  bool printCanvases = true;
  bool includeRich = false;

  //  char* pdraft = "/phenix/WWW/p/draft/adare/embedding/eff/";
  char* pdraft = "";
  const int nPid = 3;
  enum ePid {PION, KAON, PROT, ALL3};
  const int nType = 4;
  enum eType {S, R, E, G};
  TLatex ltx;
  ltx.SetNDC();

  TFile* inFile[3];
  TFile* outFile[3];

  // Centrality weighting for combinations.
  // 1st 2 for 0-20, 2nd 4 for 20-60.
  double wts[6] = {0.55, 0.45, 0.42, 0.29, 0.18, 0.11};

  if (includeRich) {
    inFile[PION] = new TFile("embEff_dch_pc32_rich_RScuts.root", "read");
    outFile[PION] = new TFile("embEffout_dch_pc32_rich_RScuts_pi.root", 
			      "recreate");
  }
  else {
    inFile[PION] = new TFile("embEff_2sigPC3_qual3163_RScuts.root", "read");
    outFile[PION] = new TFile("embEffout_2sigPC3_qual3163_RScuts.root", 
			      "recreate");

//     inFile[PION] = new TFile("embEff_dch_pc32_qual7_3163_RScuts.root", "read");
//     outFile[PION] = new TFile("embEffout_dch_pc32_qual7_3163_RScuts.root", 
// 			      "recreate");

//     inFile[PION] = new TFile("embEff_dch_pc_RScuts.root", "read");
//     outFile[PION] = new TFile("embEffout_dch_pc_RScuts_pi.root", 
// 			      "recreate");
  }

  TH1F* hCentBin[nPid];
  TH1F* hPtBin[nPid];

  hCentBin[PION] = (TH1F*)inFile[PION]->Get("hCentBin");
  hPtBin[PION] =   (TH1F*)inFile[PION]->Get("hPtBin");

  const int nCentBinsTmp = hCentBin[PION]->GetNbinsX();
  const int nPtBinsTmp =   hPtBin[PION]->GetNbinsX();
  const int nCentBins = nCentBinsTmp;
  const int nPtBins =   nPtBinsTmp;

  int momColor[nCentBins] = {kBlack, kRed+2, kRed, 
			     kOrange+1, kOrange, kGreen+2, 
			     kCyan+1, kBlue, kViolet};
  
  int cenColor[nPtBins] = {kBlack, kRed, kOrange, kGreen+2, 
			   kCyan+1, kBlue, kViolet};

  cout << nCentBins << " centrality bins, " << nPtBins << " pT bins." << endl;

  TH1F* hCen[nPid][nType][nPtBins];  
  TH1F* hMom[nPid][nType][nCentBins];  
  TH1F* hMom2[nPid][nType][2]; // 2 cent bins: 0-20 and 20-60%  

  //  if (includeRich) getRichEff();

  for (int it=0; it<nType; it++) {
    for(int ipt=0; ipt<nPtBins; ipt++) {
      hCen[PION][it][ipt] = 
	(TH1F*) inFile[PION]->Get(Form("hCen_%i_%i_%i", PION, it, ipt));
      assert(hCen[PION][it][ipt]);
      hCen[PION][it][ipt]->SetLineWidth(2);
      hCen[PION][it][ipt]->SetMarkerStyle(8);
      hCen[PION][it][ipt]->SetMarkerColor(cenColor[ipt]);
      hCen[PION][it][ipt]->SetLineColor(cenColor[ipt]);

      /*
      if (includeRich) {
	//	hCen[PION][it][ipt]->SetTitle("DC, PC, and RICH Occupancy efficiency");
	hCen[PION][it][ipt]->Multiply(hCenRich);
	hCen[PION][it][ipt]->GetYaxis()->SetRangeUser(0.5, 1.1);
      }
      else
      */
      hCen[PION][it][ipt]->GetYaxis()->SetRangeUser(0.5, 1.1);
    }
    for(int icb=0; icb<nCentBins; icb++) {
      hMom[PION][it][icb] = 
	(TH1F*) inFile[PION]->Get(Form("hMom_%i_%i_%i", PION, it, icb));
      assert(hMom[PION][it][icb]);
      hMom[PION][it][icb]->SetLineWidth(2);
      hMom[PION][it][icb]->SetMarkerStyle(8);
      hMom[PION][it][icb]->SetMarkerColor(momColor[icb]);
      hMom[PION][it][icb]->SetLineColor(momColor[icb]);
      hMom[PION][it][icb]->GetYaxis()->SetRangeUser(0.55, 1.3);

      // There should be no data below 500 MeV
      hMom[PION][it][icb]->SetBinContent(1, 0.);
      /*
      if (includeRich) {
	hMom[PION][it][icb]->Scale(hCenRich->GetBinContent(icb+1));
	hMom[PION][it][icb]->GetYaxis()->SetRangeUser(0.5, 1.4);
      }
      else
      */
      hMom[PION][it][icb]->GetYaxis()->SetRangeUser(0.55, 1.3);

      // Create combined centrality bins here
      if (icb==1) { // 0-20 %
	hMom2[PION][it][0] =
	  (TH1F*)hMom[PION][it][0]->Clone(Form("hMom2_%i_%i_%i", PION, it, 0));
	hMom2[PION][it][0]->Scale(wts[0]);
	hMom2[PION][it][0]->Add(hMom[PION][it][1], wts[1]);

     }
      if (icb==5) { // 20-60 %
	hMom2[PION][it][1] =
	  (TH1F*)hMom[PION][it][2]->Clone(Form("hMom2_%i_%i_%i", PION, it, 1));
	hMom2[PION][it][1]->Scale(wts[2]);
	hMom2[PION][it][1]->Add(hMom[PION][it][3], wts[3]);
	hMom2[PION][it][1]->Add(hMom[PION][it][4], wts[4]);
	hMom2[PION][it][1]->Add(hMom[PION][it][5], wts[5]);
      }

    }
  }
  TCanvas* cMom, *cMom2;
  TCanvas* cCen;
  cMom = new TCanvas("cMom", "cMom", 1);
  cCen = new TCanvas("cCen", "cCen", 1);
  cMom2 = new TCanvas("cMom2", "cMom2", 1);

  TLegend* lMom = new TLegend(0.6, 0.6, 0.99, 0.99);
  TLegend* lCen = new TLegend(0.5, 0.15, 0.89, 0.5);
  lMom->SetBorderSize(1);
  lCen->SetBorderSize(1);
  lMom->SetFillColor(kNone);
  lCen->SetFillColor(kNone);

  // Now draw, print, write everything
  outFile[PION]->cd();

  // Embed eff vs. momentum  
  cMom->cd();
  for(int icb=0; icb<nCentBins; icb++) {
    char* gopt = icb==0 ? "" : "same";
    hMom[PION][E][icb]->Draw(gopt);

    lMom->AddEntry(hMom[PION][E][icb], 
		   hMom[PION][E][icb]->GetTitle(), "epl");
    hMom[PION][E][icb]->Write();
  }

  if (includeRich)
    ltx.DrawLatex(0.14, 0.8, "#splitline{Occupancy efficiency}{for DC, PC, & RICH}");
  else
    ltx.DrawLatex(0.14, 0.8, "#splitline{Occupancy efficiency}{for DC & PC}");
  lMom->Draw();
  cMom->Write();

  // Embed eff vs. pt for combined centrality bins
  cMom2->cd();
  TF1* eFit[2];
  for (int k=0; k<2; k++) {
    eFit[k] = new TF1(Form("eFit%i", k), "[0]+[1]*exp(-[2]*x)", 0.7, 10.0);
    eFit[k]->SetParameters(1, 1, 1);
    eFit[k]->SetLineColor(hMom2[PION][E][k]->GetLineColor());
  }
  hMom2[PION][E][0]->Fit(eFit[0], "R", "goff");
  hMom2[PION][E][1]->Fit(eFit[1], "R", "goff");

  hMom2[PION][E][0]->GetYaxis()->SetRangeUser(0.6, 1.1);
  hMom2[PION][E][0]->Draw();
  hMom2[PION][E][1]->Draw("same");
  ltx.DrawLatex(0.15, 0.82, Form("Run 7 occupancy efficiency fit: A + Be^{-Cp_{T}}"));
  ltx.DrawLatex(0.15, 0.75, Form("0-20%%: (A, B, C) = (%4.3f, %4.3f, %4.3f)",
				 eFit[0]->GetParameter(0),
				 eFit[0]->GetParameter(1),
				 eFit[0]->GetParameter(2)));
  ltx.DrawLatex(0.15, 0.7, Form("20-60%%: (A, B, C) = (%4.3f, %4.3f, %4.3f)",
				 eFit[1]->GetParameter(0),
				 eFit[1]->GetParameter(1),
				 eFit[1]->GetParameter(2)));
  ltx.DrawLatex(0.15, 0.22, Form("31 or 63, 2#sigma PC3 match, no RICH veto"));
  // Embed eff vs. centrality
  cCen->cd();
  for(int ipt=1; ipt<nPtBins; ipt++) {
    char* gopt = ipt==1 ? "" : "same";
    hCen[PION][E][ipt]->Draw(gopt);
    lCen->AddEntry(hCen[PION][E][ipt],
		   hCen[PION][E][ipt]->GetTitle(), "epl");
    hCen[PION][E][ipt]->Write();
  }

  if (includeRich)
    ltx.DrawLatex(0.14, 0.8, 
		  "#splitline{Occupancy efficiency}{for DC, PC, & RICH}");
  else
    ltx.DrawLatex(0.14, 0.8, 
		  "#splitline{Occupancy efficiency}{for DC & PC}");

  lCen->Draw();
  cCen->Write();

  if (printCanvases) {
    if (includeRich){
      cMom->Print(Form("%scMom_n0.pdf", pdraft));
      cCen->Print(Form("%scCen_n0.pdf", pdraft));
    }
    else {
      cMom->Print(Form("%scMom.pdf", pdraft));
      cCen->Print(Form("%scCen.pdf", pdraft));
    }
  }
  
  return;
}

void getRichEff()
{
  TFile* f = new TFile("richOccEff.root", "read");
  hCenRich = (TH1F*)f->Get("he0");
  assert(hCenRich);
}
