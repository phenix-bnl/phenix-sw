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
  myframe->SetTitle("");
  myframe->SetStats(0);

  myframe->GetXaxis()->SetTitleSize(0.05);
  myframe->GetXaxis()->SetTitleOffset(1.2);
  myframe->GetXaxis()->SetLabelSize(0.05);

  myframe->GetYaxis()->SetTitleSize(0.05);
  myframe->GetYaxis()->SetTitleOffset(1.2);
  myframe->GetYaxis()->SetLabelSize(0.05);

  myframe->Draw();

  return myframe;
}

void makeFig3v2(char* filename = "sumvarvol_hadr_tau06_v00.dat")
{

  //____________________________________________________________________________
  // Read hydro data file

  const int Ncent = 100;

  double E_0[Ncent];
  double T_0[Ncent];
  double eT_0[Ncent];
  double E_T[Ncent];
  double E0_av[Ncent];
  double T0_av[Ncent];
  double S_0[Ncent];
  double S0_av[Ncent];
  double Vol[Ncent];
  double dNdy[Ncent];


  double slopeV[10][Ncent];
  double slopeErrV[10][Ncent];

  Int_t chiMinPiV[11];
  Int_t chiMinKV[11];
  Int_t chiMinPrV[11];

  const double hc = 0.197; // GeV fm
  const double  B=  0.378534779 ; 
  //ifstream in("sumvarvol_qgp_tau015_v00.dat");
  ifstream in(filename);
  Int_t k;

  for(Int_t i=0;i<Ncent;i++)
    {
      in >> k  >>  E_0[i] >> E0_av[i]>> T_0[i] >> T0_av[i] >> S_0[i] >> S0_av[i]>> Vol[i]>>dNdy[i] >> E_T[i] ;
      for(Int_t j=0;j<10;j++){ in >> slopeV[j][i]  >>  slopeErrV[j][i]; }

      // GeV/fm^3 --> GeV^4
      E_0[i]-=B ;
      E_0[i]*=TMath::Power(hc,3);
      E0_av[i]*=TMath::Power(hc,3);
      E_T[i]*=TMath::Power(hc,3);
    }

  // last lines of file are for chi-square values ...
  for(Int_t cen=0;cen<11;cen++)
    in >> k >> chiMinPiV[k] >> chiMinKV[k] >> chiMinPrV[k];
 
  in.close();


  Double_t x[100] ;
  for(Int_t i=0;i<100;i++)x[i]=1.*i ;
  TGraph * g = new TGraph(100,x,Vol) ;
  g->Fit("pol8") ;
  TF1 * fVol = g->GetFunction("pol8") ;
  TCanvas *dd = new TCanvas();
  fVol->Draw();

  //____________________________________________________________________________
  // T_eff vs T_0

  char title[100];
  sprintf(title,"Teff_vs_T_0_%s",filename);
  TCanvas *c1 = new TCanvas(title,title,700,500);
  c1->Range(0.0822335,-28.6458,0.612183,206.771);
  c1->SetLeftMargin(0.127874);
  c1->SetRightMargin(0.0229885);
  c1->SetTopMargin(0.0287611);
  c1->SetBottomMargin(0.121681);

  double Teff_min = 0.100, T_0_min = 0.150;
  double Teff_max = 0.500, T_0_max = 0.610;

  TH2F *myframe = (TH2F*)frame(title, 14,T_0_min,T_0_max, 14,Teff_min,Teff_max,"T_{0} (GeV)","T_{eff} (GeV)");

  TLegend *l = new TLegend(0.17,0.65,0.48,0.95,"Local #gamma slopes:","brNDC");
  l->SetMargin(0.2);
  l->SetFillStyle(0);
  l->SetBorderSize(4);

  // Reduced number of selected slopes: 1,3,5,7,9
  // j= 0  1      2      3      4      5      6      7      8      9  
  // 0-0.5 0.5-1. 1-1.5  1.5-2  2-2.5  2.5-3  3-3.5  3.5-4  4-4.5  4.5-5

  const int Nslopes = 5;

  TGraphErrors *T0_vs_Teff[Nslopes];
  int col[Nslopes] = {2,96,91,14,52} ; //{52,14,91,96,2};
  int is = 0;

  for(Int_t i=9; i>0;i=i-2)
    {
      T0_vs_Teff[is] = new TGraphErrors(Ncent,T_0,slopeV[i],eT_0,slopeErrV[i]);
      //T0_vs_Teff[i]->SetLineStyle(i); 
      T0_vs_Teff[is]->SetMarkerStyle(8); 
      T0_vs_Teff[is]->SetMarkerColor(col[is]); 
      T0_vs_Teff[is]->SetLineWidth(4);
      T0_vs_Teff[is]->SetLineColor(col[is]);
      T0_vs_Teff[is]->Draw("L");
      char titl[50];
      sprintf(titl,"%3.1f < p_{T} < %3.1f GeV/c",i*0.5,(i+1)*0.5);
      l->AddEntry(T0_vs_Teff[is],titl,"l");
      is++;
    }
  l->Draw();

  // Print centrality labels

  TText *t = new TText();
  t->SetTextSize(0.031);
  t->SetTextAngle(90);
  t->SetTextAlign(21);
  for(Int_t cen = 0; cen<11; cen++){
    TString txt;
    txt+= " "; 
    txt+=10*(cen-1); 
    if (cen==1) txt = "   0";
    txt +="-"; txt+=10*cen; txt += " %";
    if (cen==10) txt = " >90 %";
    if(cen!=0){
      t->SetTextColor(1);
      t->SetTextAlign(12);
      t->DrawText(T_0[chiMinPiV[cen]],0.11,txt);
    }
  }

  TText *t2 = new TText(0.2,0.13,"Au+Au centrality:");
  t2->SetTextSize(0.03);
  t2->SetTextAlign(21);
  t2->Draw();

  //  return;

  //____________________________________________________________________________
  // Equation-of-State

  const double PI_2 = TMath::Pi()*TMath::Pi();
  double gamma4 = PI_2/30.;

  // Theoretical EoS

  double deg[Ncent];

  for(Int_t i=0;i<Ncent;i++)
    {
      deg[i]=E_0[i]/(gamma4*TMath::Power(T_0[i],4.));
    }

  TGraph *deg_vs_T0 = new TGraph(Ncent,T_0,deg);

  // Computed EoS

  const double tau0 = 0.15;// fm/c

  TF1 *A_T = new TF1("A_T","gaus",0.,100.);
  //A_T->SetParameters(14./100.*150.77,0.30867,7.1);
  A_T->SetParameters(150.77,0.30867,7.1);


  double slope1V[Ncent];
  double slope2V[Ncent];
  double slope3V[Ncent];
  double slope4V[Ncent];
  double slope5V[Ncent];
  double slopeErr1[Ncent];
  double slopeErr2[Ncent];
  double slopeErr3[Ncent];
  double slopeErr4[Ncent];
  double slopeErr5[Ncent];

  double EinNorm1[Ncent];
  double EinNorm2[Ncent];
  double EinNorm3[Ncent];
  double EtNorm1[Ncent];
  double EtNorm2[Ncent];
  double EtNorm3[Ncent];
  double EtNorm4[Ncent];
  double EtNorm5[Ncent];
  double E_err[Ncent];

  for(Int_t i=0;i<Ncent;i++){
    slope1V[i]  =slopeV[1][i];
    slopeErr1[i]=slopeErrV[1][i]; 
    slope2V[i]=slopeV[3][i];
    slopeErr2[i]=slopeErrV[2][i]; 
    slope3V[i]=slopeV[5][i];
    slopeErr3[i]=slopeErrV[5][i]; 
    slope4V[i]=slopeV[7][i];
    slopeErr4[i]=slopeErrV[7][i]; 
    slope5V[i]=slopeV[9][i];
    slopeErr5[i]=slopeErrV[9][i]; 

    // Energies normalized by Steffan-Boltzmann law: gamma4*T^4
    // and by volume: V = [Glauber A_T]x[tau0]

    double V_0 = tau0*fVol->Eval(1.*i) ; 
    //V_0 = A_T->Eval(14./100.*i)*tau0;//*pow(0.7,4.);
    
//     EinNorm1[i] = E_0[i]/(gamma4*TMath::Power(slope1V[i],4.));
//     EinNorm2[i] = E_0[i]/(gamma4*TMath::Power(slope2V[i],4.));
//     EinNorm3[i] = E_0[i]/(gamma4*TMath::Power(T_0[i],4.));

    EinNorm1[i] = E_0[i]/(gamma4*TMath::Power(T_0[i],4.));
    EinNorm2[i] = (E_T[i]/V_0)/(gamma4*TMath::Power(T_0[i],4.));
    //EinNorm3[i] = E0_av[i]/(gamma4*TMath::Power(T0[i],4.));
    EinNorm3[i] = E0_av[i]/(gamma4*TMath::Power(T0_av[i],4.));

    EtNorm1[i]  = (E_T[i]/V_0)/(gamma4*TMath::Power(slope1V[i],4.));
    EtNorm2[i]  = (E_T[i]/V_0)/(gamma4*TMath::Power(slope2V[i],4.));
    EtNorm3[i]  = (E_T[i]/V_0)/(gamma4*TMath::Power(slope3V[i],4.));
    EtNorm4[i]  = (E_T[i]/V_0)/(gamma4*TMath::Power(slope4V[i],4.));
    EtNorm5[i]  = (E_T[i]/V_0)/(gamma4*TMath::Power(slope5V[i],4.));
    E_err[i]= EtNorm1[i]*0.0002;
  }
  
  TGraph *Ein_vs_Teff1 = new TGraph(Ncent,T_0,EinNorm1);
  TGraph *Ein_vs_Teff2 = new TGraph(Ncent,T_0,EinNorm2);
  TGraph *Ein_vs_Teff3 = new TGraph(Ncent,T_0,EinNorm3);

//   TCanvas * ccc = new TCanvas() ;
  
//   TGraph *g1 = new TGraph(Ncent,E_0,E0_av);
//   g1->Draw("APl") ;
//   return ;

  TGraph *ET_vs_Teff1 = new TGraph(Ncent,slope1V,EtNorm1);
  TGraph *ET_vs_Teff2 = new TGraph(Ncent,slope2V,EtNorm2);
  TGraph *ET_vs_Teff3 = new TGraph(Ncent,slope3V,EtNorm3);
  TGraph *ET_vs_Teff4 = new TGraph(Ncent,slope4V,EtNorm4);
  TGraph *ET_vs_Teff5 = new TGraph(Ncent,slope5V,EtNorm5);


//   TLegend *l2 = new TLegend(0.15,0.50,0.40,0.89,NULL,"brNDC");
//   l2->SetMargin(0.2);
//   l2->SetFillStyle(0);
//   l2->SetBorderSize(4);


//____________________________________________________________________________
  // Plot degOfFreedom_vs_T

  sprintf(title,"degOfFreedom_vs_T_%s",filename);
  TCanvas *c2 = new TCanvas(title,title,700,500);
  c2->Range(0.0822335,-28.6458,0.612183,206.771);
  c2->SetLeftMargin(0.127874);
  c2->SetRightMargin(0.0229885);
  c2->SetTopMargin(0.0287611);
  c2->SetBottomMargin(0.121681);

  double Teff_min = 0.150, Teff_max = 0.450;

  double E_T_min = 0., E_0_min = 0.;
  double E_T_max = 100., E_0_max = 120.;

  TH2F *myframe2 = (TH2F*)frame(title, 14,Teff_min,Teff_max, 14,E_T_min,E_T_max,"T_{eff} (GeV)","g_{eff}");
  myframe2->Draw();

  ET_vs_Teff1->SetLineColor(52) ;
  ET_vs_Teff1->SetLineWidth(5) ;
  ET_vs_Teff1->Draw("C");
  
  ET_vs_Teff2->SetLineColor(14) ;
  ET_vs_Teff2->SetLineWidth(5) ;
  ET_vs_Teff2->Draw("C");
  
  ET_vs_Teff3->SetLineColor(91) ;
  ET_vs_Teff3->SetLineWidth(5) ;
  ET_vs_Teff3->Draw("C");
  
  ET_vs_Teff4->SetLineColor(96) ;
  ET_vs_Teff4->SetLineWidth(5) ;
  ET_vs_Teff4->Draw("C");
  
  ET_vs_Teff5->SetLineColor(2) ;
  ET_vs_Teff5->SetLineWidth(5) ;
  ET_vs_Teff5->Draw("C");
  
  Ein_vs_Teff1->SetMarkerStyle(20); 
  Ein_vs_Teff1->SetMarkerColor(3);
  Ein_vs_Teff1->SetMarkerSize(0.8);
  Ein_vs_Teff1->Draw("P");

  Ein_vs_Teff2->SetMarkerStyle(21); 
  Ein_vs_Teff2->SetMarkerColor(4);
  Ein_vs_Teff2->SetMarkerSize(0.8);
  Ein_vs_Teff2->Draw("P");

  Ein_vs_Teff3->SetMarkerStyle(22); 
  Ein_vs_Teff3->SetMarkerColor(8);
  Ein_vs_Teff3->SetMarkerSize(1.);
  Ein_vs_Teff3->Draw("P");


 

  TLegend *l2 = new TLegend(0.62,0.59,0.97,0.98);
  l2->SetMargin(0.2);
  //  l2->SetFillStyle(0);
  l2->SetBorderSize(4);

  l2->AddEntry(Ein_vs_Teff1,"#epsilon_{0}/(#pi^{2}T_{0}^{4}/30)","p")  ;
  l2->AddEntry(Ein_vs_Teff3,"<#epsilon_{0}>/(#pi^{2}T_{0}^{4}/30)","p")  ;
  l2->AddEntry(Ein_vs_Teff2,"E_{T}/(#pi^{2}T_{0}^{4}/30)","p")  ;
  l2->AddEntry(0,"","") ; 
  l2->AddEntry(0,"E_{T}/(#pi^{2}T_{eff}^{4}/30) for p_{T} in","")  ;
  l2->AddEntry(0,"","") ;
  l2->AddEntry(ET_vs_Teff1,"0.5 < p_{T} < 1.0 GeV/c","l");
  l2->AddEntry(ET_vs_Teff2,"1.5 < p_{T} < 2.0 GeV/c","l");
  l2->AddEntry(ET_vs_Teff3,"2.5 < p_{T} < 3.0 GeV/c","l");
  l2->AddEntry(ET_vs_Teff4,"3.5 < p_{T} < 4.0 GeV/c","l");
  l2->AddEntry(ET_vs_Teff5,"4.5 < p_{T} < 5.0 GeV/c","l");

  l2->Draw() ;


  return;

//   // Print centrality labels
//   TText *t2 = new TText(0.201776,100.,"Au+Au centrality:");
//   t2->SetTextSize(0.03);
//   t2->SetTextAlign(21);
//   t2->Draw();

//   TText *t = new TText();
//   t->SetTextSize(0.031);
//   t->SetTextAngle(90);
//   t->SetTextAlign(21);
//   for(Int_t cen = 0; cen<11; cen++){
//     TString txt;
//      txt+= " "; 
//      txt+=10*(cen-1); 
//      if (cen==1) txt = "   0";
//      txt +="-"; txt+=10*cen; txt += " %";
//     if (cen==10) txt = " >90 %";
//     if(cen!=0){
//       t->SetTextColor(1);
//       t->SetTextAlign(12);
//       t->DrawText(slopeV[9][chiMinPiV[cen]],100.,txt);
//     }
//   }

//   TText *t2 = new TText(0.201776,100.,"Au+Au centrality:");
//   t2->SetTextSize(0.03);
//   t2->SetTextAlign(21);
//   t2->Draw();


}
