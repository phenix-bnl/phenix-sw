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
  myframe->GetXaxis()->SetTitleOffset(1.);
  myframe->GetXaxis()->SetLabelSize(0.065);

  myframe->GetYaxis()->SetTitleSize(0.065);
  myframe->GetYaxis()->SetTitleOffset(1.);
  myframe->GetYaxis()->SetLabelSize(0.065);

  return myframe;
}

void makeFig4(char* inasciifile = "sumvarvol_tau015_v01.dat"){

  
  //==========================Var Vol ==============================

  const int Npoints = 100;

  Double_t EinVarVol[Npoints] ;
  Double_t TinVarVol[Npoints] ;
  Double_t EtVarVol[Npoints] ;
  Double_t EtV[Npoints] ;
  Double_t slope1V[Npoints] ;
  Double_t slope1ErrV[Npoints] ;
  Double_t slope2V[Npoints] ;
  Double_t slope2ErrV[Npoints] ;
  Double_t slope3V[Npoints] ;
  Double_t slope3ErrV[Npoints] ;

  Int_t chiMinPiV[11] ;
  Int_t chiMinKV[11] ;
  Int_t chiMinPrV[11] ;

  ifstream in(inasciifile) ;

  Int_t k ;
  Int_t j;
 
  for(Int_t i=0;i<Npoints;i++){
    j = i;
    in >> k  >>  EinVarVol[j]  >>  TinVarVol[j]  >> EtVarVol[j]  >>  EtV[j]  
       >> slope1V[j]  >>  slope1ErrV[j]  >> slope2V[j]  >> slope2ErrV[j]  >> slope3V[j]  >> slope3ErrV[j] ;
  }

  //for(Int_t cen=0;cen<11;cen++)
    //in >> k >> chiMinPiV[k] >> chiMinKV[k] >> chiMinPrV[k] ;
  
  in.close() ;

  //================================================================
  Double_t xe[Npoints] ;
  for(Int_t i=0;i<Npoints;i++)xe[i]=0. ;

  Double_t Emax = 300.;
   
  char title[100];
  sprintf(title,"Ein_vs_Teff");
  TCanvas *c1 = new TCanvas(title,title,700,500);
  TH2F *myframe1 = (TH2F*)frame(title, 20, 0.15, 0.35, 100, 0., 200, "T_{eff} (GeV)", "\epsilon_{0} (GeV/fm^{3})");
  myframe1->Draw();

  sprintf(title,"ET_vs_Teff");
  TCanvas *c2 = new TCanvas(title,title,700,500);
  TH2F *myframe2 = (TH2F*)frame(title, 20,0.15,0.35, 100, 0., 500, "T_{eff} (GeV)", "E_{T} (GeV)");
  myframe2->Draw();

   
  c1->Update() ;

  TGraphErrors * Ein_vs_Teff1 = new TGraphErrors(Npoints,slope1V,EinVarVol,slope1ErrV,xe) ;
  TGraphErrors * ET_vs_Teff1  = new TGraphErrors(Npoints,slope1V,EtVarVol,slope1ErrV,xe) ;

  TGraphErrors * Ein_vs_Teff2 = new TGraphErrors(Npoints,slope2V,EinVarVol,slope2ErrV,xe) ;
  TGraphErrors * ET_vs_Teff2  = new TGraphErrors(Npoints,slope2V,EtVarVol,slope2ErrV,xe) ;

  TGraphErrors * Ein_vs_Teff3 = new TGraphErrors(Npoints,slope3V,EinVarVol,slope3ErrV,xe) ;
  TGraphErrors * ET_vs_Teff3  = new TGraphErrors(Npoints,slope3V,EtVarVol,slope3ErrV,xe) ;
  

  c1->cd();
  Ein_vs_Teff1->SetMarkerStyle(20); 
  Ein_vs_Teff1->SetMarkerColor(2) ;
  Ein_vs_Teff1->SetMarkerSize(0.8) ;
  Ein_vs_Teff1->Draw("P") ;
  
  Ein_vs_Teff2->SetMarkerStyle(21); 
  Ein_vs_Teff2->SetMarkerColor(4) ;
  Ein_vs_Teff2->SetMarkerSize(0.8) ;
  Ein_vs_Teff2->Draw("P") ;

  Ein_vs_Teff3->SetMarkerStyle(22); 
  Ein_vs_Teff3->SetMarkerColor(6) ;
  Ein_vs_Teff3->SetMarkerSize(1.) ;
  Ein_vs_Teff3->Draw("P") ;

  c2->cd();
  ET_vs_Teff1->SetMarkerStyle(20); 
  ET_vs_Teff1->SetMarkerColor(2) ;
  ET_vs_Teff1->SetMarkerSize(0.8) ;
  ET_vs_Teff1->Draw("P") ;

  ET_vs_Teff2->SetMarkerStyle(21); 
  ET_vs_Teff2->SetMarkerColor(4) ;
  ET_vs_Teff2->SetMarkerSize(0.8) ;
  ET_vs_Teff2->Draw("P") ;

  ET_vs_Teff3->SetMarkerStyle(22); 
  ET_vs_Teff3->SetMarkerColor(6) ;
  ET_vs_Teff3->SetMarkerSize(1.) ;
  ET_vs_Teff3->Draw("P") ;

}
