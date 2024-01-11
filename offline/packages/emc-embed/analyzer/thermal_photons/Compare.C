
const int phnx_marker = 20;
const int phnx_col = 1;

//____________________________________________________________________________
void Compare( bool weightImpParam = false )
{

  //Jyvaskyla group
  double xJ[6]={1.,  2.,   3.,     4.,     5.,       6.} ;
  //double yJ[6]={0.25,0.008,0.00042,0.00004,0.0000042,0.00000055} ;
  double yJ[6]={0.25,0.008,0.00042,0.000035,0.000003,0.0000004} ;
  TGraph *gJyv = new TGraph(6,xJ,yJ) ;
  
  //Gale et al.,
  double xG[7] ={0.2,0.4,0.6,1.,2.,3.,4.} ;
  double yQG[7]={5.5,1.5,0.6,0.105,0.0035,0.00014,0.000007} ;
  double yHG[7]={7.5,3.2,1.05,0.11,0.0005,0.0000024,0.} ;
  for(Int_t i=0; i<7; i++) yQG[i]+=yHG[i];
  TGraph *gGale = new TGraph(7,xG,yQG) ;
  
  //Srivastava 0.2
  double xS[8] = {0.2,0.4,0.6,1.,2.,3.,4.,5.} ;
  double yQS[8]= {18.,4.,1.05,0.15,0.003,0.00015,0.00001,0.0000007} ;
  double yHS[8]= {13.,2.,1.,0.2,0.0025,0.00006,0.,0.,} ;
  for(Int_t i=0; i<8; i++) yQS[i]+=yHS[i];
  TGraph *gSriv = new TGraph(8,xS,yQS) ;
  
  //Alam
  double xA[]= {0.6,1.,2.,3.} ;
  double yA[]= {20.,1.05,0.004,0.000034} ;
  TGraph *gAlam = new TGraph(4,xA,yA) ;
  
  //____________________________________________________________________________
  // Hydro rates

  //char *file="sumvarvol_tau015_v00.root";
  char *file="sumvarvol_qgp_tau015_v00_Glau_Au_200GeV.root";
  TFile *f = new TFile(file);

  TH2F *hQGP = (TH2F*)f->Get("h102");  // QGP
  TH2F *hMixQ= (TH2F*)f->Get("h103");  // Mix QGP
  TH2F *hMixH= (TH2F*)f->Get("h105");  // Mix Hadr
  TH2F *hHadr =(TH2F*)f->Get("h104");  // Hadr
  TH2F *hTotHydro = (TH2F*)f->Get("h100");  // Total

//   hQGP->Scale(2);
//   hMixQ->Scale(2);
//   hHadr->Scale(2);
//   hMixH->Scale(2);
//   hTotHydro->Scale(2);

  hQGP->Add(hMixQ); 
  hHadr->Add(hMixH);

  //____________________________________________________________________________
  // Hydro yields
  // Calculate impact parameter ranges assuming b_{max}=14
  // CC:  0    1     2     3     4     5     6     7     8     9    10    11   (12) 
  // CC:  0%  0-5%,5-10,10-15,15-20,20-30,30-40,40-50,50-60,60-70,70-80,80-90, (90-100)
  // b:   0   2.3, 4.1,  5.2,  6.2,  7.4,  8.7,  9.9, 11.0, 11.9, 12.8,  14.,  (15.1)

  TString CentClass[12] = {"0-0%","0-5%","5-10%","10-15%","15-20%","20-30%","30-40%",
			   "40-50%","50-60%","60-70%","70-80%","80-90%"};

  // Choose centrality class in hydro histos

  int icenB = 1; 
  int iperB = 9;

  cout << "<I> Will plot central [" << CentClass[icenB] << "] and peripheral [" << CentClass[iperB] << "] Au+Au" << endl;

  // Calculate impact parameter ranges assuming b_{max}=14
  double impParam[12]=   {0.0,2.3,4.1,5.2,6.2,7.4,8.8, 9.9,10.9,11.9,12.8,14.1};
  double impParamMin[12]={0.0,0.0,3.3,4.8,5.7,6.7,8.2, 9.2,10.5,11.6,12.4,13.3};
  double impParamMax[12]={0.0,3.3,4.8,5.7,6.7,8.2,9.2,10.5,11.6,12.4,13.3,14.0};

  double bmax = 14.;  

  cout << "<I> Projecting hydro histos for impact parameters: <b> = " 
       << impParam[icenB] << " fm (central),  <b> = " << impParam[iperB] << " fm (periph)" << endl; 

  TAxis *bAxis = hQGP->GetYaxis() ;

  //____________________________________________________________________________
  // Hydro yields central

  int iBmin = 16.; // Let's fix it a b = 2.3 fm (0-5%)
  int iBmax = 16.;

  cout << "<I> Hydro central yields projected between bins: " << iBmin << " - " << iBmax << endl;

  TH1D *hQGPCen = hQGP->ProjectionX("QGPC",iBmin,iBmin);
  hQGPCen->SetName("hQGPCen");
  TH1D *hHadrCen= hHadr->ProjectionX("HadrC",iBmin,iBmin);
  hHadrCen->SetName("hHadrCen");
  TH1D *hTotHydroCen= hTotHydro->ProjectionX("hTotHydroCen",iBmin,iBmin);
  hTotHydroCen->SetName("hTotHydroCen");

  if (weightImpParam)
    {

      iBmin = bAxis->FindBin(impParamMin[icenB]/bmax*100.) ;
      iBmax = bAxis->FindBin(impParamMax[icenB]/bmax*100.) ;

      cout << "<I> Hydro central yields projected between bins: " << iBmin << " - " << iBmax << endl;

      double bSurf = (impParamMin[icenB]-0.5)*bmax/100.;
      hQGPCen->Scale(bSurf) ;
      hHadrCen->Scale(bSurf) ;
      hTotHydroCen->Scale(bSurf) ;
      
      TH1D *tmp;
      for(int i=iBmin+1;i<=iBmax;i++)
    {
      if(i==21)continue ;
      double dS =(impParamMin[icenB]+i+0.5-iBmin)*bmax/100.;
      //double dS =(impParamMin[icenB]+i-0.5)*bmax/100.;
      tmp = hQGP->ProjectionX("tmp",i,i) ;
      hQGPCen->Add(tmp,dS) ;
      tmp = hHadr->ProjectionX("tmp",i,i) ;
      hHadrCen->Add(tmp,dS) ;
      tmp = hTotHydro->ProjectionX("tmp",i,i) ;
      hTotHydroCen->Add(tmp,dS) ;
      bSurf+=dS ;
    }
      
      hQGPCen->Scale(1./bSurf) ;
      hHadrCen->Scale(1./bSurf) ;
      hTotHydroCen->Scale(1./bSurf) ;
    }

  TH1D *hTotHydroCen2 =(TH1D*)hQGPCen->Clone();
  hTotHydroCen2->Add(hHadrCen,1);

  //____________________________________________________________________________
  // Hydro yields peripheral

  iBmin = 80.; // Let's fix it a b = 11.2 fm (60-70% should be b ~ 11.9 fm)
  iBmax = 80.; 

  cout << "<I> Hydro periph yields projected between bins: " << iBmin << " - " << iBmax << endl;

  TH1D *hQGPPer = hQGP->ProjectionX("QGPP",iBmin,iBmin);
  hQGPPer->SetName("hQGPPer");
  TH1D *hHadrPer= hHadr->ProjectionX("HadrP",iBmin,iBmin);
  hHadrPer->SetName("hHadrPer");
  TH1D *hTotHydroPer= hTotHydro->ProjectionX("hTotHydroPer",iBmin,iBmin);
  hTotHydroPer->SetName("hTotHydroPer");

  if (weightImpParam)
    {

      iBmin = bAxis->FindBin(impParamMin[iperB]/bmax*100.) ;
      iBmax = bAxis->FindBin(impParamMax[iperB]/bmax*100.) ;

      cout << "<I> Hydro periph yields projected between bins: " << iBmin << " - " << iBmax << endl;

      for(int i=iBmin+1;i<=iBmax;i++)
    {
      //if(i==21)continue ;
      //double dS =(impParamMin[iperB-1]+i+0.5-iBmin)*bmax/100.;
      double dS =(impParamMin[iperB]+i-0.5)*bmax/100.;
      tmp = hQGP->ProjectionX("tmp",i,i) ;
      hQGPPer->Add(tmp,dS) ;
      tmp = hHadr->ProjectionX("tmp",i,i) ;
      hHadrPer->Add(tmp,dS) ;
      tmp = hTotHydro->ProjectionX("tmp",i,i) ;
      hTotHydroPer->Add(tmp,dS) ;
      bSurf+=dS ;
    }
      
      hQGPPer->Scale(1./bSurf) ;
      hHadrPer->Scale(1./bSurf) ;
      hTotHydroPer->Scale(1./bSurf) ;
    }

  //____________________________________________________________________________
  // TOTAL yields: HYDRO + NLO pQCD 

  // fill Total spectrum with Hydro alone below pT = 1.5 GeV/c

  const int ntotBins = 27; // 15 bins hydro alone + 17 bins (pQCD+Hydro)

  double pt = 0.;
  int binHydro = 0;
  double yieldCenHydro = 0, yieldPerHydro = 0.;

  double totPt[ntotBins];
  double pQCDplusHydroNPhotCen[ntotBins];
  double pQCDplusHydroNPhotPer[ntotBins];

  int counter = 0;

  for(int ipt = 0; ipt < 10; ipt++)
    {
      pt = ipt*0.1; // 0 -- 1.0 GeV/c
      binHydro = hTotHydroCen->GetXaxis()->FindBin(pt);
      yieldCenHydro = hTotHydroCen->GetBinContent(binHydro);
      yieldPerHydro = hTotHydroPer->GetBinContent(binHydro);
      
      totPt[counter]=pt+0.05;
      pQCDplusHydroNPhotCen[counter]=yieldCenHydro;
      //cout << counter << " filling bin: " << binHydro << " w/ value: " << yieldCenHydro << endl;
      pQCDplusHydroNPhotPer[counter++]=yieldPerHydro;
    }

  //____________________________________________________________________________
  // pQCD rates scaled by Glauber Ncoll
  // 

  TF1 *fNcoll_vs_b= new TF1("fNcoll_vs_b","pol6/([7]+exp((x-[8])/[9]))",0.,20.); 
  fNcoll_vs_b->SetParameters(9449.06,-187.61,-143.892,-6.29555,2.96372,-0.196842,0.00408522,7.73342,10.9962,1.07787);

//  ifstream inpQCD("/afs/rhic.bnl.gov/phenix/users/enterria/thermal_photons/data/pQCD_vogelsang_pp_gamma_200GeV_cteq6_sc1.dat");
  ifstream inpQCD("pQCD_vogelsang_pp_gamma_200GeV_cteq6_sc1.dat");

  //Normalize

  double sigma_pp = 42.;

  double nBCcen = fNcoll_vs_b->Eval(impParam[icenB]);
  double nBCper = fNcoll_vs_b->Eval(impParam[iperB]);

  const int nBins = 17;
  double px[nBins];
  double epx[nBins];
  double npQCDCen[nBins];
  double npQCDPer[nBins];

  double ratioTotOverpQCDCen[nBins];
  double eratioTotOverpQCDCen[nBins];

  double adump,edump;

  for(int ipt = 0; ipt < nBins; ipt++)
    {
      inpQCD >> px[ipt] >> adump >> edump >>  npQCDCen[ipt];
      npQCDCen[ipt]*=1.e-9;
      npQCDPer[ipt]= npQCDCen[ipt];
      
      // normalize to Ncoll
      npQCDCen[ipt]*=nBCcen/sigma_pp;
      npQCDPer[ipt]*=nBCper/sigma_pp;
      
      // fill total w/ pQCD+hydro 
      binHydro = hTotHydroCen->GetXaxis()->FindBin(px[ipt]);
      yieldCenHydro = hTotHydroCen->GetBinContent(binHydro);
      yieldPerHydro = hTotHydroPer->GetBinContent(binHydro);
      
      totPt[counter]=px[ipt];
      pQCDplusHydroNPhotCen[counter]=npQCDCen[ipt]+yieldCenHydro;
      //cout << counter << " filling bin: " << binHydro << "(pT = " 
      // << px[ipt] << " GeV/c) w/ value: " << pQCDplusHydroNPhotCen[counter] << endl;

      ratioTotOverpQCDCen[ipt]=pQCDplusHydroNPhotCen[counter]/npQCDCen[ipt];
      eratioTotOverpQCDCen[ipt]=pQCDplusHydroNPhotCen[counter]*0.2;
      epx[ipt]=0.;

      pQCDplusHydroNPhotPer[counter++]=npQCDPer[ipt]+yieldPerHydro;
    } 
  inpQCD.close();

  TGraph *gpQCDcen = new TGraph(nBins,px,npQCDCen);
  gpQCDcen->SetName("gpQCDcen");
  TGraph *gpQCDper = new TGraph(nBins,px,npQCDPer);
  gpQCDper->SetName("gpQCDper");

  TGraph *pQCDplusHydroCen = new TGraph(ntotBins,totPt,pQCDplusHydroNPhotCen);
  pQCDplusHydroCen->SetName("pQCDplusHydroCen");
  TGraph *pQCDplusHydroPer = new TGraph(ntotBins,totPt,pQCDplusHydroNPhotPer);
  pQCDplusHydroPer->SetName("pQCDplusHydroPer");

  TGraphErrors *hratioTotOverpQCDCen = new TGraphErrors(nBins,px,ratioTotOverpQCDCen,epx,eratioTotOverpQCDCen);
  hratioTotOverpQCDCen->SetName("hratioTotOverpQCDCen");

  //____________________________________________________________________________
  // PHENIX data

  TH1F* phnx_AuAugamma_cent = (TH1F*)phnx_AuAugamma_cent();
  TGraphAsymmErrors *AuAugamma_cent_lowpt1 = (TGraphAsymmErrors*)phnx_AuAugamma_cent_lowpt(1);
  TGraphAsymmErrors *AuAugamma_cent_lowpt2 = (TGraphAsymmErrors*)phnx_AuAugamma_cent_lowpt(2);

  TH1F* phnx_AuAugamma_per = (TH1F*)phnx_AuAugamma_per();
  TGraphAsymmErrors *AuAugamma_per_lowpt1 = (TGraphAsymmErrors*)phnx_AuAugamma_per_lowpt(1);
  TGraphAsymmErrors *AuAugamma_per_lowpt2 = (TGraphAsymmErrors*)phnx_AuAugamma_per_lowpt(2);
  TGraphAsymmErrors *AuAugamma_per_lowpt3 = (TGraphAsymmErrors*)phnx_AuAugamma_per_lowpt(3);
  TGraphAsymmErrors *AuAugamma_per_lowpt4 = (TGraphAsymmErrors*)phnx_AuAugamma_per_lowpt(4);

  //____________________________________________________________________________
  // Plot CENTRAL

  char title[255];
  sprintf(title,"photon_spec_AuAu200GeV_cent%d",icenB);
  TCanvas *cCen = new TCanvas(title,title,720,720);
  cCen->Range(-1.45368,-8.12827,8.64133,1.27398);
  cCen->SetLeftMargin(0.144);
  cCen->SetRightMargin(0.014);
  cCen->SetTopMargin(0.02914);
  cCen->SetBottomMargin(0.12);
  cCen->SetLogy(1);

  double ptmin = 0.;
  double ptmax = 8.5;  
  double ymax = 1.e+1;
  double ymin = 1.e-7;

  int xbins=100, ybins=100;
  TH2F *myframe = (TH2F*)frame("",xbins,ptmin,ptmax, ybins,ymin,ymax,"p_{T} (GeV/c)","d^{2}N/(#pi dp_{T}^{2}dy) (GeV/c)^{-2}");
  myframe->Draw();
  myframe->GetYaxis()->SetTitleOffset(1.35);
  myframe->GetYaxis()->SetTitleSize(0.05);
  myframe->GetXaxis()->SetTitleOffset(1.1);
  myframe->GetXaxis()->SetTitleSize(0.05);
  cCen->Update();

  //hTotHydroCen->SetTitle("");
  //hTotHydroCen->SetStats(0);
  hTotHydroCen->GetXaxis()->SetRangeUser(0.4,6.5);
  hTotHydroCen->SetMinimum(ymin);
  hTotHydroCen->SetMaximum(ymax);
  //hTotHydroCen->SetXTitle("p_{T} (GeV/c)");
  //hTotHydroCen->GetXaxis()->SetTitleSize(0.05);
  //hTotHydroCen->SetYTitle("d^{2}N/(#pi dp_{T}^{2}dy) (GeV/c)^{-2}");
  //hTotHydroCen->GetYaxis()->SetTitleOffset(1.25);
  //hTotHydroCen->GetYaxis()->SetTitleSize(0.05);
  hTotHydroCen->SetLineStyle(2);
  hTotHydroCen->SetLineColor(2);
  hTotHydroCen->SetLineWidth(4); //(15);
  hTotHydroCen->Draw("csame");
  //hTotHydroCen2->Draw("hsame");
  //hTotHydroCen2->SetLineColor(8);

  hQGPCen->GetXaxis()->SetRangeUser(0.4,6.5);
  hQGPCen->SetLineStyle(2);
  hQGPCen->SetLineColor(2);
  hQGPCen->SetLineWidth(4);//(15);
  //hQGPCen->Draw("csame");

  gpQCDcen->SetLineColor(4); //(52);
  gpQCDcen->SetLineWidth(4);//(15);
  //gpQCDcen->SetLineStyle(2);
//  gpQCDcen->Draw("c");

  hHadrCen->SetLineStyle(2);
  hHadrCen->SetLineColor(1);
  hHadrCen->SetLineWidth(3);
  hHadrCen->SetMarkerStyle(20);
  hHadrCen->SetMarkerSize(0.8);
  hHadrCen->SetMarkerColor(1);
//  hHadrCen->Draw("psame");

  pQCDplusHydroCen->SetLineStyle(1);
  pQCDplusHydroCen->SetLineColor(6);
  pQCDplusHydroCen->SetLineWidth(3);
  pQCDplusHydroCen->SetLineStyle(2);
//  pQCDplusHydroCen->Draw("c");

  gpQCDcen->Draw("c");

  //Jyvalys results
  gJyv->SetLineStyle(1);
  gJyv->SetLineColor(93);
  gJyv->SetLineWidth(3);
  gJyv->SetMarkerStyle(28);
  gJyv->SetMarkerColor(93);
  gJyv->SetMarkerSize(2.);
  gJyv->SetMaximum(ymax) ;
  gJyv->SetMinimum(ymin) ;
  gJyv->Draw("cp") ;

  //Srivastava
  gSriv->SetLineColor(50) ;
  gSriv->SetLineWidth(3) ;
  gSriv->SetMarkerStyle(25);
  gSriv->SetMarkerColor(50);
  gSriv->SetMarkerSize(1.4);
  gSriv->Draw("cp") ;

  //Gale
  gGale->SetLineStyle(1);
  gGale->SetLineColor(7);
  gGale->SetLineWidth(3);
  gGale->SetMarkerStyle(26);
  gGale->SetMarkerColor(7);
  gGale->SetMarkerSize(1.4);
  gGale->Draw("cp") ;

  //Alam
  gAlam->SetLineColor(6);
  gAlam->SetLineWidth(3) ;
  gAlam->SetMarkerStyle(30);
  gAlam->SetMarkerColor(6);
  gAlam->SetMarkerSize(2.0);
  gAlam->Draw("cp") ;

  // replot
  //hQGPCen->Draw("csame");
  hTotHydroCen->Draw("csame");

  // PHENIX data
  phnx_AuAugamma_cent->Draw("same");
  AuAugamma_cent_lowpt2->Draw(">");
  AuAugamma_cent_lowpt1->Draw("p");
  phnx_AuAugamma_cent->Draw("same");

  TLegend *l = new TLegend(0.42,0.63,0.977,0.964,"Thermal photons: Au+Au #rightarrow #gamma+X  [0-10% central]","bNDC");
  l->SetMargin(0.2);
  l->SetFillStyle(0);
  l->SetBorderSize(4);
//  l->AddEntry(pQCDplusHydroCen,"Total #gamma: Prompt + Thermal","l");
  l->AddEntry(hTotHydroCen,"D.d'Enterria-D.Peressounko. T_{0} = 590 MeV, #tau_{0}=0.15 fm/c","L");
  l->AddEntry(gJyv,"S.Rasanen et al. T_{0} = 580 MeV, #tau_{0}=0.17 fm/c","PL");
  l->AddEntry(gSriv,"D.K.Srivastava. T_{0} = 450--600 MeV, #tau_{0}=0.2 fm/c","PL");
  l->AddEntry(gGale,"S.Turbide et al. T_{0} = 370 MeV, #tau_{0}=0.33 fm/c","PL");
  l->AddEntry(gAlam,"J.Alam et al. T_{0} = 300 MeV, #tau_{0}=0.5 fm/c","PL");
//  l->AddEntry(hQGPCen,"QGP","l");
//  l->AddEntry(hHadrCen,"HRG","l");
  l->AddEntry(phnx_AuAugamma_cent,"PHENIX Au+Au [0-10% central]","P");
  l->AddEntry(gpQCDcen,"Prompt #gamma: NLO pQCD #times T_{AA}[0-10%]","L");
  l->Draw();

//   TLegend *l2 = new TLegend(0.42,0.63,0.977,0.964,"","bNDC");
//   l2->SetMargin(0.2);
//   l2->SetFillStyle(0);
//   l2->SetBorderSize(4);
//   l2->AddEntry(gpQCDcen,"Prompt: NLO pQCD #times T_{AA}[0-10%]","L");
//   l2->Draw();

  cCen->Update();
  return ;

}


//____________________________________________________________________________
TH1F* phnx_AuAugamma_cent()
{

   double xAxis[21] = {0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 9, 10, 12, 14}; 

   TH1F *dirgsyspt0C2Sc = new TH1F("dirgsyspt0C2Sc","",20,xAxis);
   //for (int i=1;i<=7;i++) dirgsyspt0C2Sc->SetBinContent(i,0.000105372);
   dirgsyspt0C2Sc->SetBinContent(8,0.000105372);
   dirgsyspt0C2Sc->SetBinContent(9,8.96282e-05);
   dirgsyspt0C2Sc->SetBinContent(10,4.64596e-05);
   dirgsyspt0C2Sc->SetBinContent(11,2.08558e-05);
   dirgsyspt0C2Sc->SetBinContent(12,1.36532e-05);
   dirgsyspt0C2Sc->SetBinContent(13,5.91448e-06);
   dirgsyspt0C2Sc->SetBinContent(14,4.55948e-06);
   dirgsyspt0C2Sc->SetBinContent(15,3.3129e-06);
   dirgsyspt0C2Sc->SetBinContent(16,1.96383e-06);
   dirgsyspt0C2Sc->SetBinContent(17,8.99538e-07);
   dirgsyspt0C2Sc->SetBinContent(18,4.59361e-07);
   dirgsyspt0C2Sc->SetBinContent(19,1.56568e-07);
   dirgsyspt0C2Sc->SetBinContent(20,6.12889e-08);
   //for (int i=1;i<=7;i++) dirgsyspt0C2Sc->SetBinError(i,8.70924e-05);
   dirgsyspt0C2Sc->SetBinError(8,8.70924e-05);
   dirgsyspt0C2Sc->SetBinError(9,3.52972e-05);
   dirgsyspt0C2Sc->SetBinError(10,1.62224e-05);
   dirgsyspt0C2Sc->SetBinError(11,7.77575e-06);
   dirgsyspt0C2Sc->SetBinError(12,4.19404e-06);
   dirgsyspt0C2Sc->SetBinError(13,2.12723e-06);
   dirgsyspt0C2Sc->SetBinError(14,1.42955e-06);
   dirgsyspt0C2Sc->SetBinError(15,1.03045e-06);
   dirgsyspt0C2Sc->SetBinError(16,6.59879e-07);
   dirgsyspt0C2Sc->SetBinError(17,2.96453e-07);
   dirgsyspt0C2Sc->SetBinError(18,1.78795e-07);
   dirgsyspt0C2Sc->SetBinError(19,6.29225e-08);
   dirgsyspt0C2Sc->SetBinError(20,3.02399e-08);
   //dirgsyspt0C2Sc->SetEntries(23);
   dirgsyspt0C2Sc->SetMarkerColor(phnx_col);
   dirgsyspt0C2Sc->SetMarkerStyle(phnx_marker);
   dirgsyspt0C2Sc->SetMarkerSize(1.4);
   //dirgsyspt0C2Sc->Draw("p");
   
   return dirgsyspt0C2Sc;
}

//____________________________________________________________________________
TGraphAsymmErrors* phnx_AuAugamma_cent_lowpt(int type=1)
{

  TGraphAsymmErrors  *AuAugamma_lowpt1 = new TGraphAsymmErrors(5);
  AuAugamma_lowpt1->SetMarkerColor(phnx_col);
  AuAugamma_lowpt1->SetMarkerStyle(phnx_marker);
  AuAugamma_lowpt1->SetMarkerSize(1.4);
  AuAugamma_lowpt1->SetPoint(0,1.25,0.234751);
  AuAugamma_lowpt1->SetPointError(0,0,0,0,0.264571);
  AuAugamma_lowpt1->SetPoint(1,1.75,0.0239043);
  AuAugamma_lowpt1->SetPointError(1,0,0,0,0.0282684);
  AuAugamma_lowpt1->SetPoint(2,2.25,0.00140969);
  AuAugamma_lowpt1->SetPointError(2,0,0,0,0.00481325);
  AuAugamma_lowpt1->SetPoint(3,2.75,0.00027178);
  AuAugamma_lowpt1->SetPointError(3,0,0,0,0.00106237);
  AuAugamma_lowpt1->SetPoint(4,3.25,0.000240848);
  AuAugamma_lowpt1->SetPointError(4,0,0,0,0.000301077);
  //AuAugamma_lowpt1->Draw("p");

  TGraphAsymmErrors *AuAugamma_lowpt2 = new TGraphAsymmErrors(5);
  AuAugamma_lowpt2->SetMarkerColor(phnx_col);
  AuAugamma_lowpt2->SetMarkerStyle(phnx_marker);
  AuAugamma_lowpt2->SetMarkerSize(2);
  AuAugamma_lowpt2->SetPoint(0,1.25,0.234751);
  AuAugamma_lowpt2->SetPointError(0,0,0,0.211276,0);
  AuAugamma_lowpt2->SetPoint(1,1.75,0.0239043);
  AuAugamma_lowpt2->SetPointError(1,0,0,0.0215139,0);
  AuAugamma_lowpt2->SetPoint(2,2.25,0.00140969);
  AuAugamma_lowpt2->SetPointError(2,0,0,0.00126872,0);
  AuAugamma_lowpt2->SetPoint(3,2.75,0.00027178);
  AuAugamma_lowpt2->SetPointError(3,0,0,0.000244602,0);
  AuAugamma_lowpt2->SetPoint(4,3.25,0.000240848);
  AuAugamma_lowpt2->SetPointError(4,0,0,0.000216763,0);
  //AuAugamma_lowpt2->Draw(">");

  if (type==1) return AuAugamma_lowpt1;
  if (type==2) return AuAugamma_lowpt2;

}

//____________________________________________________________________________
TH1F* phnx_AuAugamma_per()
{
   double xAxis[21] = {0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 9, 10, 12, 14}; 

   double scale_factor=1e+12;   

   TH1F *dirgsyspt6C2Sc = new TH1F("dirgsyspt6C2Sc","",20,xAxis);
   dirgsyspt6C2Sc->SetBinContent(3,5.31341e-16*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(4,1.02623e-17*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(6,8.01448e-18*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(8,2.04185e-18*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(10,2.37179e-18*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(12,3.41176e-19*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(15,5.07998e-20*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(16,9.74906e-20*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(17,6.27733e-20*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(18,2.13621e-20*scale_factor);
   dirgsyspt6C2Sc->SetBinContent(20,2.74136e-21*scale_factor);

   dirgsyspt6C2Sc->SetBinError(3,1e-10); // arbitrarily small
   dirgsyspt6C2Sc->SetBinError(4,1e-10); // arbitrarily small
   dirgsyspt6C2Sc->SetBinError(6,1e-10); // arbitrarily small
   dirgsyspt6C2Sc->SetBinError(8,1e-10); // arbitrarily small
   dirgsyspt6C2Sc->SetBinError(10,1.50412e-18*scale_factor);
   dirgsyspt6C2Sc->SetBinError(12,1e-10); // arbitrarily small
   dirgsyspt6C2Sc->SetBinError(15,1e-10); // arbitrarily small
   dirgsyspt6C2Sc->SetBinError(16,1e-10); // arbitrarily small
   dirgsyspt6C2Sc->SetBinError(17,4.56906e-20)*scale_factor;
   dirgsyspt6C2Sc->SetBinError(20,1e-10); // arbitrarily small

//    dirgsyspt6C2Sc->SetBinContent(5,3.99565e-16*scale_factor);
//    dirgsyspt6C2Sc->SetBinContent(7,2.8311e-17*scale_factor);
//    dirgsyspt6C2Sc->SetBinContent(9,3.61552e-18*scale_factor);
//    dirgsyspt6C2Sc->SetBinContent(10,2.37179e-18*scale_factor);
//    dirgsyspt6C2Sc->SetBinContent(11,6.4569e-19*scale_factor);
//    dirgsyspt6C2Sc->SetBinContent(13,1.53239e-19*scale_factor);
//    dirgsyspt6C2Sc->SetBinContent(14,8.7691e-20*scale_factor);
//    dirgsyspt6C2Sc->SetBinContent(17,6.27733e-20*scale_factor);
//    dirgsyspt6C2Sc->SetBinContent(19,2.62864e-21*scale_factor);

//    dirgsyspt6C2Sc->SetBinError(5,1e-10); // arbitrarily small
//    dirgsyspt6C2Sc->SetBinError(7,1e-10); // arbitrarily small
//    dirgsyspt6C2Sc->SetBinError(9,1e-10); // arbitrarily small
//    dirgsyspt6C2Sc->SetBinError(10,1.50412e-18*scale_factor);
//    dirgsyspt6C2Sc->SetBinError(11,1e-10); // arbitrarily small
//    dirgsyspt6C2Sc->SetBinError(13,1e-10); // arbitrarily small
//    dirgsyspt6C2Sc->SetBinError(14,1e-10); // arbitrarily small
//    dirgsyspt6C2Sc->SetBinError(17,4.56906e-20*scale_factor);
//    dirgsyspt6C2Sc->SetBinError(19,1e-10); // arbitrarily small

   //dirgsyspt6C2Sc->SetEntries(23);
   dirgsyspt6C2Sc->SetMarkerStyle(phnx_marker);
   dirgsyspt6C2Sc->SetMarkerColor(phnx_col);
   dirgsyspt6C2Sc->SetMarkerStyle(phnx_marker);
   dirgsyspt6C2Sc->SetMarkerSize(1.4);

   //TCanvas *cper = new TCanvas();
   //dirgsyspt6C2Sc->Draw("p");

   return dirgsyspt6C2Sc;
}

//____________________________________________________________________________
TGraphAsymmErrors* phnx_AuAugamma_per_lowpt(int type=1)
{

  double scale_factor=1e+12;   

  TGraphAsymmErrors *AuAugamma_lowpt1 = new TGraphAsymmErrors(7);
  AuAugamma_lowpt1->SetName("AuAugamma_lowpt1");
  AuAugamma_lowpt1->SetTitle("AuAugamma_lowpt1");
  AuAugamma_lowpt1->SetMarkerStyle(1);
  AuAugamma_lowpt1->SetPoint(0,2.25,3.99565e-16*scale_factor);
  AuAugamma_lowpt1->SetPointError(0,0.25,0.25,0,0);
  AuAugamma_lowpt1->SetPoint(1,3.25,2.8311e-17*scale_factor);
  AuAugamma_lowpt1->SetPointError(1,0.25,0.25,0,0);
  AuAugamma_lowpt1->SetPoint(2,4.25,3.61552e-18*scale_factor);
  AuAugamma_lowpt1->SetPointError(2,0.25,0.25,0,0);
  AuAugamma_lowpt1->SetPoint(3,5.25,6.4569e-19*scale_factor);
  AuAugamma_lowpt1->SetPointError(3,0.25,0.25,0,0);
  AuAugamma_lowpt1->SetPoint(4,6.25,1.53239e-19*scale_factor);
  AuAugamma_lowpt1->SetPointError(4,0.25,0.25,0,0);
  AuAugamma_lowpt1->SetPoint(5,6.75,8.7691e-20*scale_factor);
  AuAugamma_lowpt1->SetPointError(5,0.25,0.25,0,0);
  AuAugamma_lowpt1->SetPoint(6,11,2.62864e-21*scale_factor);
  AuAugamma_lowpt1->SetPointError(6,0.25,0.25,0,0);
  //AuAugamma_lowpt1->Draw("z");
   
  TGraphAsymmErrors *AuAugamma_lowpt2 = new TGraphAsymmErrors(7);
  AuAugamma_lowpt2->SetName("AuAugamma_lowpt2");
  AuAugamma_lowpt2->SetTitle("AuAugamma_lowpt2");
  AuAugamma_lowpt2->SetMarkerStyle(1);
  AuAugamma_lowpt2->SetMarkerSize(2);
  AuAugamma_lowpt2->SetPoint(0,2.25,3.99565e-16*scale_factor);
  AuAugamma_lowpt2->SetPointError(0,0,0,3.59608e-16*scale_factor,0);
  AuAugamma_lowpt2->SetPoint(1,3.25,2.8311e-17*scale_factor);
  AuAugamma_lowpt2->SetPointError(1,0,0,2.54799e-17*scale_factor,0);
  AuAugamma_lowpt2->SetPoint(2,4.25,3.61552e-18*scale_factor);
  AuAugamma_lowpt2->SetPointError(2,0,0,3.25396e-18*scale_factor,0);
  AuAugamma_lowpt2->SetPoint(3,5.25,6.4569e-19*scale_factor);
  AuAugamma_lowpt2->SetPointError(3,0,0,5.81121e-19*scale_factor,0);
  AuAugamma_lowpt2->SetPoint(4,6.25,1.53239e-19*scale_factor);
  AuAugamma_lowpt2->SetPointError(4,0,0,1.37915e-19*scale_factor,0);
  AuAugamma_lowpt2->SetPoint(5,6.75,8.7691e-20*scale_factor);
  AuAugamma_lowpt2->SetPointError(5,0,0,7.89219e-20*scale_factor,0);
  AuAugamma_lowpt2->SetPoint(6,11,2.62864e-21*scale_factor);
  AuAugamma_lowpt2->SetPointError(6,0,0,2.36578e-21*scale_factor,0);
  //AuAugamma_lowpt2->Draw(">");

  TGraphAsymmErrors *AuAugamma_lowpt3 = new TGraphAsymmErrors(9);
  AuAugamma_lowpt3->SetName("AuAugamma_lowpt3");
  AuAugamma_lowpt3->SetTitle("AuAugamma_lowpt3");
  AuAugamma_lowpt3->SetMarkerStyle(1);
  AuAugamma_lowpt3->SetPoint(0,1.25,5.31341e-16*scale_factor);
  AuAugamma_lowpt3->SetPointError(0,0,0,0,1.03082e-14*scale_factor);
  AuAugamma_lowpt3->SetPoint(1,1.75,1.02623e-17*scale_factor);
  AuAugamma_lowpt3->SetPointError(1,0,0,0,1.36154e-15*scale_factor);
  AuAugamma_lowpt3->SetPoint(2,2.75,8.01448e-18*scale_factor);
  AuAugamma_lowpt3->SetPointError(2,0,0,0,6.73927e-17*scale_factor);
  AuAugamma_lowpt3->SetPoint(3,3.75,2.04185e-18*scale_factor);
  AuAugamma_lowpt3->SetPointError(3,0,0,0,6.63883e-18*scale_factor);
  AuAugamma_lowpt3->SetPoint(4,5.75,3.41176e-19*scale_factor);
  AuAugamma_lowpt3->SetPointError(4,0,0,0,3.6808e-19*scale_factor);
  AuAugamma_lowpt3->SetPoint(5,7.25,5.07998e-20*scale_factor);
  AuAugamma_lowpt3->SetPointError(5,0,0,0,9.33259e-20*scale_factor);
  AuAugamma_lowpt3->SetPoint(6,7.75,9.74906e-20*scale_factor);
  AuAugamma_lowpt3->SetPointError(6,0,0,0,1.02573e-19*scale_factor);
  AuAugamma_lowpt3->SetPoint(7,9.5,2.13621e-20*scale_factor);
  AuAugamma_lowpt3->SetPointError(7,0,0,0,2.42398e-20*scale_factor);
  AuAugamma_lowpt3->SetPoint(8,13,2.74136e-21*scale_factor);
  AuAugamma_lowpt3->SetPointError(8,0,0,0,5.7081e-21*scale_factor);
  //AuAugamma_lowpt3->Draw("p");
   
  TGraphAsymmErrors *AuAugamma_lowpt4 = new TGraphAsymmErrors(9);
  AuAugamma_lowpt4->SetName("AuAugamma_lowpt4");
  AuAugamma_lowpt4->SetTitle("AuAugamma_lowpt4");
  AuAugamma_lowpt4->SetMarkerStyle(1);
  AuAugamma_lowpt4->SetMarkerSize(2);
  AuAugamma_lowpt4->SetPoint(0,1.25,5.31341e-16*scale_factor);
  AuAugamma_lowpt4->SetPointError(0,0,0,4.78207e-16*scale_factor,0);
  AuAugamma_lowpt4->SetPoint(1,1.75,1.02623e-17*scale_factor);
  AuAugamma_lowpt4->SetPointError(1,0,0,9.23611e-18*scale_factor,0);
  AuAugamma_lowpt4->SetPoint(2,2.75,8.01448e-18*scale_factor);
  AuAugamma_lowpt4->SetPointError(2,0,0,7.21303e-18*scale_factor,0);
  AuAugamma_lowpt4->SetPoint(3,3.75,2.04185e-18*scale_factor);
  AuAugamma_lowpt4->SetPointError(3,0,0,1.83766e-18*scale_factor,0);
  AuAugamma_lowpt4->SetPoint(4,5.75,3.41176e-19*scale_factor);
  AuAugamma_lowpt4->SetPointError(4,0,0,3.07058e-19*scale_factor,0);
  AuAugamma_lowpt4->SetPoint(5,7.25,5.07998e-20*scale_factor);
  AuAugamma_lowpt4->SetPointError(5,0,0,4.57199e-20*scale_factor,0);
  AuAugamma_lowpt4->SetPoint(6,7.75,9.74906e-20*scale_factor);
  AuAugamma_lowpt4->SetPointError(6,0,0,8.77416e-20*scale_factor,0);
  AuAugamma_lowpt4->SetPoint(7,9.5,2.13621e-20*scale_factor);
  AuAugamma_lowpt4->SetPointError(7,0,0,1.92259e-20*scale_factor,0);
  AuAugamma_lowpt4->SetPoint(8,13,2.74136e-21*scale_factor);
  AuAugamma_lowpt4->SetPointError(8,0,0,2.46722e-21*scale_factor,0);
  //AuAugamma_lowpt4->Draw(">");


  if (type==1) return AuAugamma_lowpt1;
  if (type==2) return AuAugamma_lowpt2;
  if (type==3) return AuAugamma_lowpt3;
  if (type==4) return AuAugamma_lowpt4;

}

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
