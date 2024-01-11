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

void makeFig3(char* inasciifile = "sumvarvol_qgp_tau015_v00.dat" )
{

  //==========================Var Vol ==============================
  Double_t EinVarVol[100] ;
  Double_t TinVarVol[100] ;
  Double_t EtVarVol[100] ;
  Double_t EtV[100] ;
  Double_t slope1V[100] ;
  Double_t slope1ErrV[100] ;
  Double_t slope2V[100] ;
  Double_t slope2ErrV[100] ;
  Double_t slope3V[100] ;
  Double_t slope3ErrV[100] ;
  Int_t chiMinPiV[11] ;
  Int_t chiMinKV[11] ;
  Int_t chiMinPrV[11] ;

  ifstream in(inasciifile) ;
  Int_t k ;
  
  for(Int_t i=0;i<100;i++){
    in >> k  >>  EinVarVol[i]  >>  TinVarVol[i]  >> EtVarVol[i]  >>  EtV[i]  
       >> slope1V[i]  >>  slope1ErrV[i]  >> slope2V[i]  >> slope2ErrV[i]  >> slope3V[i]  >> slope3ErrV[i] ;
  }

  for(Int_t cen=0;cen<11;cen++)
    in >> k >> chiMinPiV[k] >> chiMinKV[k] >> chiMinPrV[k] ;
  
  in.close() ;
  //================================================================
  Double_t xe[100] ;
  for(Int_t i=0;i<100;i++)xe[i]=0. ;

  char title[100];
  sprintf(title,"Teff_vs_Tin");
  TCanvas *c1 = new TCanvas(title,title,700,500);
  TH2F *myframe = (TH2F*)frame(title, 14, 0.25, 0.6, 14, 0.1,0.35,"T_{in} (GeV)","T_{eff} (GeV)");
  myframe->Draw();

  TGraphErrors *ge2V1 = new TGraphErrors(100,TinVarVol,slope1V,xe,slope2ErrV) ;
  TGraphErrors *ge2V2 = new TGraphErrors(100,TinVarVol,slope2V,xe,slope2ErrV) ;
  TGraphErrors *ge2V3 = new TGraphErrors(100,TinVarVol,slope3V,xe,slope2ErrV) ;

  ge2V1->SetMarkerStyle(20); 
  ge2V1->SetMarkerColor(2) ;
  ge2V1->SetMarkerSize(0.8) ;
  ge2V1->Draw("P") ;

  ge2V2->SetMarkerStyle(21); 
  ge2V2->SetMarkerColor(4) ;
  ge2V2->SetMarkerSize(0.8) ;
  ge2V2->Draw("P") ;

  ge2V3->SetMarkerStyle(22); 
  ge2V3->SetMarkerColor(6) ;
  ge2V3->SetMarkerSize(1.) ;
  ge2V3->Draw("P") ;

  TF1 *f1= new TF1("f1","pol1",0.1,0.7);
  f1->SetLineWidth(1) ;
  f1->SetLineColor(2) ;
  ge2V1->Fit("f1","RQ","",0.1,0.7) ;
  
  f1->SetLineColor(4) ;
  ge2V2->Fit("f1","RQ","",0.1,0.7) ;

  f1->SetLineColor(6) ;
  ge2V3->Fit("f1","RQ","",0.1,0.7) ;

  
  TText * t = new TText() ;
  t->SetTextSize(0.03) ;
  t->SetTextAngle(90) ;
  t->SetTextAlign(21) ;
  for(Int_t cen = 0; cen<11; cen++){
    TString txt ;
    txt+=10*(cen-1) ; txt +="-" ; txt+=10*cen ; txt += " %" ;
    if(cen!=0){
      t->SetTextColor(1) ;
      t->SetTextAlign(12) ;
      t->DrawText(TinVarVol[chiMinPiV[cen]],0.11,txt) ;
    }
  }
  
  TLegend * l = new TLegend(0.8,0.7,0.99,0.99) ;
  l->AddEntry(ge2V1,"0.5 < P_{t} < 1 GeV/c","p") ;
  l->AddEntry(ge2V2,"1 < P_{t} < 2 GeV/c","p") ;
  l->AddEntry(ge2V3,"2 < P_{t} < 5 GeV/c","p") ;
  l->Draw() ;
}
