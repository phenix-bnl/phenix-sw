
TH2F *frame(const char *title="frame", 
	    const int xbins=100, const double xmin=0., const double xmax=100.,
	    const int ybins=100, const double ymin=0., const double ymax=100.,
	    const char *xtitle="p_{T} (GeV/c)", 
	    const char *ytitle="d^{3}N/d^{3}p (GeV/c)^{-2}")
{

  TH2F *myframe = new TH2F(title, title, xbins, xmin, xmax, ybins, ymin, ymax);

  myframe->SetStats(0);
  myframe->SetTitle(title);
  myframe->SetXTitle(xtitle);
  myframe->SetYTitle(ytitle);

  myframe->GetXaxis()->SetTitleSize(0.065);
  myframe->GetXaxis()->SetTitleOffset(1.3);
  myframe->GetXaxis()->SetLabelSize(0.065);

  myframe->GetYaxis()->SetTitleSize(0.065);
  myframe->GetYaxis()->SetTitleOffset(1.);
  myframe->GetYaxis()->SetLabelSize(0.065);

  return myframe;
}

void ppgamma_vs_NLO()
{

  TH2F *myframe = (TH2F*)frame("pp_gamma", 7, 3., 10, 100, 1e-11, 1e-05);
  myframe->Draw();

  TFile f("dir_phot_pt_pp_emcal_tag_meth_gl76_1_sc76_1.root");
  TH1D *gamma_exp = (TH1D*)f.Get("gDirGamSpecP0");
  //gamma_exp->SetMinimum(1.e-11);
  gamma_exp->Draw("P");
  gamma_exp->SetMarkerSize(2.);

  ifstream  inQCD("pQCD_vogelsang_pp_gamma_200GeV_cteq6_sc1.dat");
  const Int_t nBins = 17 ;  
  Double_t pT[nBins] ;
  Double_t nQCD[nBins] ;
  Double_t adump,edump ;

  for(Int_t ipt = 0; ipt < nBins; ipt++)
    { 
      inQCD >> pT[ipt] >> adump >> edump >>  nQCD[ipt] ; 
      nQCD[ipt]*=1.e-9/42.;
    }

  inQCD.close() ;

  TGraph * gQCDvogel = new TGraph(nBins,pT,nQCD) ;
  gQCDvogel->SetMarkerStyle(29);
  gQCDvogel->SetMarkerSize(2.5);
  gQCDvogel->SetMarkerColor(2);
  gQCDvogel->Draw("P");

  ifstream  inQCD("pQCD_mwerlen_pp_gamma_200GeV_cteq5_sc1.dat");
  const Int_t nBins2 = 13 ;  

  for(Int_t ipt = 0; ipt < nBins2; ipt++)
    { 
      inQCD >> pT[ipt] >> nQCD[ipt] ; 
      nQCD[ipt]/=42.;
    }
 inQCD.close() ;
 
 TGraph * gQCDwerl = new TGraph(nBins2,pT,nQCD) ;
 gQCDwerl->SetMarkerStyle(28);
 gQCDwerl->SetMarkerSize(2.5);
 gQCDwerl->SetMarkerColor(2);
 gQCDwerl->Draw("P");
 //gQCDcen->Draw("L");
}
