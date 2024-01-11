
void makeFig1(char *file="sumvarvol_qgp_tau015_v00_Glau.root"){

  TFile * f = new TFile(file);
  if (!f) return;
  cout << "<I> Reading hydro file: " << file << endl;

  //____________________________________________________________________________
  // Hydro yields
  // Calculate impact parameter ranges assuming b_{max}=14
  // CC:  0    1     2     3     4     5     6     7     8     9    10    (11) 
  // CC: 0-5%,5-10,10-15,15-20,20-30,30-40,40-50,50-60,60-70,70-80,80-90, (90-100)
  // b:   2.3, 4.1,  5.2,  6.2,  7.4,  8.7,  9.9, 11.0, 11.9, 12.8,  14.,  (15.1)

  TString CentClass[12] = {"--","0-5%","5-10%","10-15%","15-20%","20-30%","30-40%",
			   "40-50%","50-60%","60-70%","70-80%","80-90%"};

  // Choose which data sets read

  int icenB = 1; 
  int iperB = 9;

  cout << "<I> Will plot central [" << CentClass[icenB] << "] and peripheral [" << CentClass[iperB] << "] Au+Au" << endl;

  // Calculate impact parameter ranges assuming b_{max}=14
  // Obtained from parametrization: TF1 *b_vs_fcc_percentile= new TF1("b_vs_fcc_percentile","pol9",0,100);
  // b_vs_fcc_percentile->SetParameters(0.319777,1.02717,-0.11322,0.00840246,-0.000368604,9.77327e-06,-1.58187e-07,1.52723e-09,-8.07674e-12,1.80031e-14);

  double impParam[12]=   {0.0,2.3,4.1,5.2,6.2,7.4,8.8, 9.9,10.9,11.9,12.8,14.1};
  double impParamMin[12]={0.0,0.0,3.3,4.8,5.7,6.7,8.2, 9.2,10.5,11.6,12.4,13.3};
  double impParamMax[12]={0.0,3.3,4.8,5.7,6.7,8.2,9.2,10.5,11.6,12.4,13.3,14.0};

  // Choose centrality class in hydro histos

  double scaleImpParam = 100./14.; // "100." = 14 fm

  cout << "<I> Projecting hydro histos for impact parameters: <b> = " 
       << impParam[icenB] << " fm (central),  <b> = " << impParam[iperB] << " fm (periph)" << endl; 

  TH2F * hydroPi0 = (TH2F*)f->Get("h2");  // Pi0
  TH2F * hydroPim = (TH2F*)f->Get("h3");  // Pi-
  TH2F * hydroKp  = (TH2F*)f->Get("h5");  // K+
  TH2F * hydroPnonfd = (TH2F*)f->Get("h19"); // P
  TH2F * hydroPNoL= (TH2F*)f->Get("h36"); // P without Lambda
  TH2F * hydroP= (TH2F*)f->Get("h35"); // P without Sigma and Lambda feed

  TAxis * bAxis = hydroPi0->GetYaxis() ;

  //____________________________________________________________________________
  // Hydro yields central

  Int_t iBmin = bAxis->FindBin(impParamMin[icenB]*scaleImpParam) ;
  Int_t iBmax = bAxis->FindBin(impParamMax[icenB]*scaleImpParam) ;

  cout << "<I> Hydro central yields projected between bins: " << iBmin << " - " << iBmax << endl;

  TH1D * hydroPi0Cen = hydroPi0->ProjectionX("hydroPi0Cen",iBmin,iBmin);
  TH1D * hydroPimCen = hydroPim->ProjectionX("hydroPimCen",iBmin,iBmin);
  TH1D * hydroKCen = hydroKp->ProjectionX("hydroKCen",iBmin,iBmin);
  TH1D * hydroPnonfd_Cen = hydroPnonfd->ProjectionX("hydroPnonfd",iBmin,iBmin);
  TH1D * hydroPnoLCen= hydroPNoL->ProjectionX("hydroPnoLCen",iBmin,iBmin);
  TH1D * hydroPCen= hydroP->ProjectionX("hydroPCen",iBmin,iBmin);

  Double_t bSurf = (impParamMin[icenB]-0.5)*scaleImpParam;
  hydroPi0Cen->Scale(bSurf) ;
  hydroPimCen->Scale(bSurf) ;
  hydroKCen->Scale(bSurf) ;
  hydroPnonfd_Cen->Scale(bSurf) ;
  hydroPnoLCen->Scale(bSurf) ;
  hydroPCen->Scale(bSurf) ;

  TH1D * tmp;
  for(Int_t i=iBmin+1;i<=iBmax;i++){
    //if(i==21)continue ;
    Double_t dS =(impParamMin[icenB]+i+0.5-iBmin)*scaleImpParam;
    //Double_t dS =(impParamMin[icenB]+i-0.5)*scaleImpParam;
    tmp = hydroPi0->ProjectionX("hydroPi0tmp",i,i) ;
    hydroPi0Cen->Add(tmp,dS) ;
    tmp =  hydroPim->ProjectionX("hydroPimtmp",i,i);
    hydroPimCen->Add(tmp,dS) ;
    tmp =  hydroKp->ProjectionX("hydroKCentmp",i,i);
    hydroKCen->Add(tmp,dS) ;
    tmp =  hydroPnonfd->ProjectionX("hydroPnonfdtmp",i,i) ;
    hydroPnonfd_Cen->Add(tmp,dS) ;
    tmp = hydroPNoL->ProjectionX("hydroPnoLCentmp",i,i);
    hydroPnoLCen->Add(tmp,dS) ;
    tmp = hydroP->ProjectionX("hydroPCentmp",i,i) ;
    hydroPCen->Add(tmp,dS) ;
    bSurf+=dS ;
  }

  hydroPi0Cen->Scale(1./bSurf) ;
  hydroPimCen->Scale(1./bSurf) ;
  hydroKCen->Scale(1./bSurf) ;
  hydroPnonfd_Cen->Scale(1./bSurf) ;
  hydroPnoLCen->Scale(1./bSurf) ;
  hydroPCen->Scale(1./bSurf) ;

  //____________________________________________________________________________
  // Hydro yields peripheral

  iBmin = bAxis->FindBin(impParamMin[iperB]*scaleImpParam) ;
  iBmax = bAxis->FindBin(impParamMax[iperB]*scaleImpParam) ;

  iBmin = 80.;

  cout << "<I> Hydro periph yields projected between bins: " << iBmin << " - " << iBmax << endl;

  TH1D * hydroPi0Per = hydroPi0->ProjectionX("hydroPi0Per",iBmin,iBmin);
  TH1D * hydroPimPer = hydroPim->ProjectionX("hydroPimPer",iBmin,iBmin);
  TH1D * hydroKPer   = hydroKp->ProjectionX("hydroKPer",iBmin,iBmin);
  TH1D * hydroPnonfd_Per = hydroPnonfd->ProjectionX("hydroPnonfd_Per",iBmin,iBmin);
  TH1D * hydroPnoLPer= hydroPNoL->ProjectionX("hydroPnoLPer",iBmin,iBmin);
  TH1D * hydroPPer= hydroP->ProjectionX("hydroPPer",iBmin,iBmin);

  bSurf = (impParamMin[iperB]-0.5)*scaleImpParam;
  hydroPi0Per->Scale(bSurf) ;
  hydroPimPer->Scale(bSurf) ;
  hydroKPer->Scale(bSurf) ;
  hydroPnonfd_Per->Scale(bSurf) ;
  hydroPnoLPer->Scale(bSurf) ;
  hydroPPer->Scale(bSurf) ;

  for(Int_t i=iBmin+1;i<=iBmax;i++){
    Double_t dS =(impParamMin[iperB]+i+0.5-iBmin)*scaleImpParam;
    //Double_t dS =(impParamMin[iperB]+i-0.5)*scaleImpParam;
    tmp = hydroPi0->ProjectionX("hydroPi0tmp",i,i) ;
    hydroPi0Per->Add(tmp,dS) ;
    tmp =  hydroPim->ProjectionX("hydroPimtmp",i,i);
    hydroPimPer->Add(tmp,dS) ;
    tmp =  hydroKp->ProjectionX("hydroKPertmp",i,i);
    hydroKPer->Add(tmp,dS) ;
    tmp =  hydroPnonfd->ProjectionX("hydroPnonfdtmp",i,i) ;
    hydroPnonfd_Per->Add(tmp,dS) ;
    tmp = hydroPNoL->ProjectionX("hydroPnoLPertmp",i,i);
    hydroPnoLPer->Add(tmp,dS) ;
    tmp = hydroP->ProjectionX("hydroPPertmp",i,i) ;
    hydroPPer->Add(tmp,dS) ;
    bSurf+=dS ;
  }
  hydroPi0Per->Scale(1./bSurf) ;
  hydroPimPer->Scale(1./bSurf) ;
  hydroKPer->Scale(1./bSurf) ;
  hydroPnonfd_Per->Scale(1./bSurf) ;
  hydroPnoLPer->Scale(1./bSurf) ;
  hydroPPer->Scale(1./bSurf) ;

  //const double kScaleExp = 0;
  //const double pScaleExp = 0;

  const double kScaleExp = -2; // power shift 
  const double pScaleExp = -4; // to resolve curves

  double kScale = pow(10,kScaleExp); // power shift 
  double pScale = pow(10,pScaleExp); // to resolve curves

  hydroKCen->Scale(kScale);
  hydroPnonfd_Cen->Scale(pScale);
  hydroPnoLCen->Scale(pScale);
  hydroPCen->Scale(pScale);

  hydroKPer->Scale(kScale);
  hydroPnonfd_Per->Scale(pScale);
  hydroPnoLPer->Scale(pScale);
  hydroPPer->Scale(pScale);

  //____________________________________________________________________________
  // TOTAL yields: HYDRO + NLO pQCD 

  // fill Total spectrum with Hydro alone below pT = 1.5 GeV/c

  double ptmin = 0.;
  double ptmax = 5.5;  

  const int ntotBins = 25; // 15 bins hydro alone + 10 bins (pQCD+Hydro)

  int binHydro = 0;
  double yieldCenHydro = 0, yieldPerHydro = 0.;
  double pt = 0.;

  double totptPi[ntotBins];
  double errtotptPi[ntotBins];
  double pQCDplusHydroNPiCen[ntotBins];
  double errpQCDplusHydroNPiCen[ntotBins];
  double pQCDplusHydroNPiPer[ntotBins];
  int picounter = 0;

  for(int ipt = 0; ipt < 15; ipt++){
    pt = ipt*0.1;
    binHydro = hydroPimCen->GetXaxis()->FindBin(pt);
    yieldCenHydro = hydroPimCen->GetBinContent(binHydro);
    yieldPerHydro = hydroPimPer->GetBinContent(binHydro);

    totptPi[picounter]=pt;
    errtotptPi[picounter]=0.01*pt;
    pQCDplusHydroNPiCen[picounter]=yieldCenHydro;
    errpQCDplusHydroNPiCen[picounter]=0.1*yieldCenHydro;
    //cout << picounter << " filling bin: " << binHydro << " w/ value: " << yieldCenHydro << endl;
    pQCDplusHydroNPiPer[picounter]=yieldPerHydro;

    picounter++;
  }

  double totptK[ntotBins];
  double errtotptK[ntotBins];
  double pQCDplusHydroNKCen[ntotBins];
  double errpQCDplusHydroNKCen[ntotBins];
  double pQCDplusHydroNKPer[ntotBins];
  int kcounter = 0;

  for(int ipt = 0; ipt < 15; ipt++){
    pt = ipt*0.1;
    binHydro = hydroKCen->GetXaxis()->FindBin(pt);
    yieldCenHydro = hydroKCen->GetBinContent(binHydro);
    yieldPerHydro = hydroKPer->GetBinContent(binHydro);

    totptK[kcounter]=pt;
    errtotptK[kcounter]=0.01*pt;
    pQCDplusHydroNKCen[kcounter]=yieldCenHydro;
    errpQCDplusHydroNKCen[kcounter]=0.1*yieldCenHydro;
    //cout << kcounter << " filling bin: " << binHydro << " w/ value: " << yieldCenHydro << endl;
    pQCDplusHydroNKPer[kcounter]=yieldPerHydro;

    kcounter++;
  }

  double totptP[ntotBins];
  double errtotptP[ntotBins];
  double pQCDplusHydroNPnonfdCen[ntotBins];
  double errpQCDplusHydroNPnonfdCen[ntotBins];
  double pQCDplusHydroNPnonfdPer[ntotBins];
  double pQCDplusHydroNPCen[ntotBins];
  double errpQCDplusHydroNPCen[ntotBins];
  double pQCDplusHydroNPPer[ntotBins];
  int pcounter = 0;

  for(int ipt = 0; ipt < 15; ipt++){
    pt = ipt*0.1;
    binHydro = hydroPnonfd_Cen->GetXaxis()->FindBin(pt);
    yieldCenHydro = hydroPnonfd_Cen->GetBinContent(binHydro);
    yieldPerHydro = hydroPnonfd_Per->GetBinContent(binHydro);

    totptP[pcounter]=pt;
    errtotptP[pcounter]=0.01*pt;
    pQCDplusHydroNPnonfdCen[pcounter]=yieldCenHydro;
    errpQCDplusHydroNPnonfdCen[pcounter]=0.1*yieldCenHydro;
    //cout << pcounter << " filling bin: " << binHydro << " w/ value: " << yieldCenHydro << endl;
    pQCDplusHydroNPnonfdPer[pcounter]=yieldPerHydro;

    binHydro = hydroPCen->GetXaxis()->FindBin(pt);
    yieldCenHydro = hydroPCen->GetBinContent(binHydro);
    yieldPerHydro = hydroPPer->GetBinContent(binHydro);

    pQCDplusHydroNPCen[pcounter]=yieldCenHydro;
    errpQCDplusHydroNPCen[pcounter]=0.1*yieldCenHydro;
    //cout << pcounter << " filling bin: " << binHydro << " w/ value: " << yieldCenHydro << endl;
    pQCDplusHydroNPPer[pcounter]=yieldPerHydro;

    pcounter++;
  }

  //____________________________________________________________________________
  // NLO pQCD scaled by Glauber Ncoll
  // 

  TF1 *fNpart_vs_b= new TF1("fNpart_vs_b","pol6/([7]+exp((x-[8])/[9]))",0.,20.); 
  fNpart_vs_b->SetParameters(1142.49,-17.4869,-6.84553,-2.34426,0.414572,-0.0231652,0.000458723,2.98342,11.9418,1.22045);
  
  TF1 *fNcoll_vs_b= new TF1("fNcoll_vs_b","pol6/([7]+exp((x-[8])/[9]))",0.,20.); 
  fNcoll_vs_b->SetParameters(9449.06,-187.61,-143.892,-6.29555,2.96372,-0.196842,0.00408522,7.73342,10.9962,1.07787);
  
  double suppr[12] = {0.18,0.18,0.22,0.23,0.27,0.4,0.5,0.6,0.7,0.75,0.8,1.0};

  double sigma_pp = 42.;
  double sigma_cen = 1.;//0.05; // 5-10% yields
  double sigma_per = 1.;//0.10; // 60-70% yields

  double pQCDerr = 0.20; // 20% scale uncertainty

  double scale_factor_cent = fNcoll_vs_b->Eval(impParam[icenB])*suppr[icenB];
  double scale_factor_peri = fNcoll_vs_b->Eval(impParam[iperB])*suppr[iperB];

  cout << "<I> NLO yields will be scaled by: Ncoll*R_AA(central) = " 
       << fNcoll_vs_b->Eval(impParam[icenB]) << "*" << suppr[icenB] 
       << " = " << scale_factor_cent << endl
       << "    and by:  Ncoll*R_AA(periph) = " << fNcoll_vs_b->Eval(impParam[iperB]) << "*" << suppr[iperB]  
       << " = " << scale_factor_peri << endl;

  
  //____________________________________________________________________________
  // NLO Pions

  const char *path="/afs/rhic.bnl.gov/phenix/users/enterria/thermal_photons/data";

  char filename[300];

  const int nBins = 10;

  sprintf(filename,"%s/pQCD_vogelsang_pp_pi0_200GeV_cteq6_sc1_kkp.dat",path);
  ifstream inpQCD(filename);

  double pQCDptPi[nBins];
  double pQCDEptPi[nBins];
  double pQCDNPiCen[nBins];
  double pQCDNPiPer[nBins];
  double pQCDEPiCen[nBins];
  //double pQCDEPiPer[nBins];

  for(int ipt = 0; ipt < nBins; ipt++){
    inpQCD >> pQCDptPi[ipt] >> pQCDNPiCen[ipt];
    pQCDEptPi[ipt]=0.001;

    pQCDNPiCen[ipt]*=1.e-9;
    pQCDNPiCen[ipt]/=sigma_pp;
    pQCDNPiPer[ipt] = pQCDNPiCen[ipt];

    // Experimental pi0 centrality is a bit higher (0-10%) ...
    pQCDNPiCen[ipt]*=scale_factor_cent*sigma_cen;
    pQCDNPiPer[ipt]*=scale_factor_peri*sigma_per;

    // fill total w/ pQCD+hydro 
    binHydro = hydroPimCen->GetXaxis()->FindBin(pQCDptPi[ipt]);
    yieldCenHydro = hydroPimCen->GetBinContent(binHydro);
    yieldPerHydro = hydroPimPer->GetBinContent(binHydro);

    totptPi[picounter]=pQCDptPi[ipt];
    errtotptPi[picounter]=0.01*pQCDptPi[ipt];
    pQCDplusHydroNPiCen[picounter]=pQCDNPiCen[ipt]+yieldCenHydro;
    errpQCDplusHydroNPiCen[picounter]=0.2*pQCDplusHydroNPiCen[picounter];
    pQCDplusHydroNPiPer[picounter]=pQCDNPiPer[ipt]+yieldPerHydro;
    picounter++;

    //cout << picounter << " filling bin: " << binHydro << " w/ value: " << pQCDplusHydroNPiCen[picounter] << endl;

    //pQCDEPiCen[ipt]=pQCDerr*pQCDNPiCen[ipt];
    //pQCDEPiPer[ipt]=pQCDerr*pQCDNPiPer[ipt];
  }
  inpQCD.close();

  TGraph *pQCDPiCen = new TGraph(nBins,pQCDptPi,pQCDNPiCen);
  TGraph *pQCDPiPer = new TGraph(nBins,pQCDptPi,pQCDNPiPer);
  pQCDPiCen->SetTitle("pQCDPiCen");
  pQCDPiPer->SetTitle("pQCDPiPer");

  TGraphErrors *pQCDplusHydroPiCen = new TGraphErrors(ntotBins,totptPi,pQCDplusHydroNPiCen,errtotptPi,errpQCDplusHydroNPiCen);
  TGraph *pQCDplusHydroPiPer = new TGraph(ntotBins,totptPi,pQCDplusHydroNPiPer);
  pQCDplusHydroPiCen->SetTitle("pQCDplusHydroPiCen");
  pQCDplusHydroPiPer->SetTitle("pQCDplusHydroPiPer");

  TString formula = "pol4*(x<=0.5)+[5]*(exp([6]*x)+[7]*x)^[8]*(x>0.5)";
  TF1 *pQCDplusHydroPiCenFit = new TF1("pQCDplusHydroPiCenFit",formula.Data(), ptmin, ptmax);
  pQCDplusHydroPiCenFit->SetParameters(500.,-600.,-7000.,2.2e+04,-1.9e+04,500.,-1.,1.,-10.);
  //TString formula = "([0]*exp(x/[1]))*(x<1.)+[2]*(exp([3]*x)+[4]*x)^[5]*(x>=1)";
  //TF1 *pQCDplusHydroPiCenFit = new TF1("pQCDplusHydroPiCenFit",formula.Data(), ptmin, ptmax);
  //pQCDplusHydroPiCenFit->SetParameters(1000.,-1.,100.,-1.,1.,-10.);
  //TF1 *pQCDplusHydroPiCenFit = new TF1("pQCDplusHydroPiCenFit","[0]*(exp([1]*x)+[2]*x)^[3]", ptmin, ptmax);
  //pQCDplusHydroPiCenFit->SetParameters(500.,-1.,1.,-10.);
  pQCDplusHydroPiCen->Fit("pQCDplusHydroPiCenFit","EL");
  
  //pQCDplusHydroPiCen->Print();

  //TGraphErrors *pQCDPiCen = new TGraphErrors(nBins,pQCDptPi,pQCDNPiCen,pQCDEptPi,pQCDEPiCen);
  //TGraphErrors *pQCDPiPer = new TGraphErrors(nBins,pQCDptPi,pQCDNPiPer,pQCDEptPi,pQCDEPiPer);

  //____________________________________________________________________________
  // NLO Kaons

  sprintf(filename,"%s/pQCD_vogelsang_pp_k+-_200GeV_cteq6_sc1_kkp.dat",path);
  ifstream inpQCD(filename);

  double pQCDptK[nBins];
  double pQCDEptK[nBins];
  double pQCDNKCen[nBins];
  double pQCDNKPer[nBins];
  //double pQCDEKCen[nBins];
  //double pQCDEKPer[nBins];

  for(int ipt = 0; ipt < nBins; ipt++){
    inpQCD >> pQCDptK[ipt] >> pQCDNKCen[ipt];
    pQCDEptK[ipt]=0.001;

    pQCDNKCen[ipt]*=1.e-9;
    pQCDNKCen[ipt]/=sigma_pp;
    pQCDNKPer[ipt] = pQCDNKCen[ipt];

    // Experimental K0s centrality is a bit higher (0-10%) ...
    pQCDNKCen[ipt]*=kScale*scale_factor_cent*sigma_cen;
    pQCDNKPer[ipt]*=kScale*scale_factor_peri*sigma_per;

    // fill total w/ pQCD+hydro 
    binHydro = hydroKCen->GetXaxis()->FindBin(pQCDptK[ipt]);
    yieldCenHydro = hydroKCen->GetBinContent(binHydro);
    yieldPerHydro = hydroKPer->GetBinContent(binHydro);

    totptK[kcounter]=pQCDptK[ipt];
    errtotptK[kcounter]=0.01*pQCDptK[ipt];
    pQCDplusHydroNKCen[kcounter]=pQCDNKCen[ipt]+yieldCenHydro;
    errpQCDplusHydroNKCen[kcounter]=0.1*pQCDplusHydroNKCen[kcounter];
    pQCDplusHydroNKPer[kcounter]=pQCDNKPer[ipt]+yieldPerHydro;
    kcounter++;

    //cout << kcounter << " filling bin: " << binHydro << " w/ value: " << pQCDplusHydroNKCen[kcounter] << endl;

    //pQCDEKCen[ipt]=pQCDerr*pQCDNKCen[ipt];
    //pQCDEKPer[ipt]=pQCDerr*pQCDNKPer[ipt];
  }
  inpQCD.close();

  TGraph *pQCDKCen = new TGraph(nBins,pQCDptK,pQCDNKCen);
  TGraph *pQCDKPer = new TGraph(nBins,pQCDptK,pQCDNKPer);
  pQCDKCen->SetTitle("pQCDKCen");
  pQCDKPer->SetTitle("pQCDKPer");

  TGraphErrors *pQCDplusHydroKCen = new TGraphErrors(ntotBins,totptK,pQCDplusHydroNKCen,errtotptK,errpQCDplusHydroNKCen);
  TGraph *pQCDplusHydroKPer = new TGraph(ntotBins,totptK,pQCDplusHydroNKPer);
  pQCDplusHydroPiCen->SetTitle("pQCDplusHydroKCen");
  pQCDplusHydroPiPer->SetTitle("pQCDplusHydroKPer");

  //formula = "[0]*(exp([1]*x)+[2]*x)^[3]*(x<=1.)+[4]*(exp([5]*x)+[6]*x)^[7]*(x>1.)";
  //TF1 *pQCDplusHydroKCenFit = new TF1("pQCDplusHydroKCenFit",formula.Data(), ptmin, ptmax);
  //pQCDplusHydroKCenFit->SetParameters(kScale*500.,-1.,1.,-10.,kScale*500.,-1.,1.,-10.);
  //TF1 *pQCDplusHydroKCenFit = new TF1("pQCDplusHydroKCenFit","[0]*exp(sqrt(x*x+[1])/[2])", ptmin, ptmax);
  //pQCDplusHydroKCenFit->SetParameters(1.,1.,-0.3);
  TF1 *pQCDplusHydroKCenFit = new TF1("pQCDplusHydroKCenFit","[0]*(exp([1]*x)+[2]*x)^[3]", ptmin, ptmax);
  pQCDplusHydroKCenFit->SetParameters(kScale*500.,-1.,1.,-10.);
  pQCDplusHydroKCen->Fit("pQCDplusHydroKCenFit","E");

  //TGraphErrors *pQCDKCen = new TGraphErrors(nBins,pQCDptK,pQCDNKCen,pQCDEptK,pQCDEKCen);
  //TGraphErrors *pQCDKPer = new TGraphErrors(nBins,pQCDptK,pQCDNKPer,pQCDEptK,pQCDEKPer);

  //____________________________________________________________________________
  // NLO protons

  sprintf(filename,"%s/pQCD_vogelsang_pp_proton_200GeV_cteq6_sc1_kkp.dat",path);
  ifstream inpQCD(filename);

  double pQCDptP[nBins];
  double pQCDEptP[nBins];
  double pQCDNPCen[nBins];
  double pQCDNPPer[nBins];
  //double pQCDEPCen[nBins];
  //double pQCDEPPer[nBins];

  for(int ipt = 0; ipt < nBins; ipt++){
    inpQCD >> pQCDptP[ipt] >> pQCDNPCen[ipt];
    pQCDEptP[ipt]=0.001;

    pQCDNPCen[ipt]*=1.e-9;
    pQCDNPCen[ipt]/=sigma_pp;
    pQCDNPPer[ipt] = pQCDNPCen[ipt];

    pQCDNPCen[ipt]*=pScale*scale_factor_cent*sigma_cen;
    pQCDNPPer[ipt]*=pScale*scale_factor_peri*sigma_per;

    // fill total w/ pQCD+hydro 

    binHydro = hydroPnonfd_Cen->GetXaxis()->FindBin(pQCDptP[ipt]);
    yieldCenHydro = hydroPnonfd_Cen->GetBinContent(binHydro);
    yieldPerHydro = hydroPnonfd_Per->GetBinContent(binHydro);

    totptP[pcounter]=pQCDptP[ipt];
    errtotptP[pcounter]=0.01*pQCDptP[ipt];
    pQCDplusHydroNPnonfdCen[pcounter]=pQCDNPCen[ipt]+yieldCenHydro;
    errpQCDplusHydroNPnonfdCen[pcounter]=0.1*pQCDplusHydroNPnonfdCen[pcounter];
    //cout << pcounter << " filling bin: " << binHydro << " w/ value: " << pQCDplusHydroNPnonfdCen[kcounter] << endl;
    pQCDplusHydroNPnonfdPer[pcounter]=pQCDNPPer[ipt]+yieldPerHydro;

    binHydro = hydroPCen->GetXaxis()->FindBin(pQCDptP[ipt]);
    yieldCenHydro = hydroPCen->GetBinContent(binHydro);
    yieldPerHydro = hydroPPer->GetBinContent(binHydro);

    pQCDplusHydroNPCen[pcounter]=pQCDNPCen[ipt]+yieldCenHydro;
    errpQCDplusHydroNPCen[pcounter]=0.2*pQCDplusHydroNPCen[pcounter];
    //cout << pcounter << " filling bin: " << binHydro << " w/ value: " << pQCDplusHydroNPCen[kcounter] << endl;
    pQCDplusHydroNPPer[pcounter]=pQCDNPPer[ipt]+yieldPerHydro;
    pcounter++;

    //pQCDEPCen[ipt]=pQCDerr*pQCDNPCen[ipt];
    //pQCDEPPer[ipt]=pQCDerr*pQCDNPPer[ipt];
  }
  inpQCD.close();

  TGraph *pQCDPCen = new TGraph(nBins,pQCDptP,pQCDNPCen);
  TGraph *pQCDPPer = new TGraph(nBins,pQCDptP,pQCDNPPer);
  pQCDPCen->SetTitle("pQCDPCen");
  pQCDPPer->SetTitle("pQCDPPer");

  //TGraphErrors *pQCDPCen = new TGraphErrors(nBins,pQCDptP,pQCDNPnonfdCen,pQCDEptP,pQCDEPCen);
  //TGraphErrors *pQCDPPer = new TGraphErrors(nBins,pQCDptP,pQCDNPnonfdPer,pQCDEptP,pQCDEPPer);

  TGraphErrors *pQCDplusHydroPnonfdCen = new TGraphErrors(ntotBins,totptP,pQCDplusHydroNPnonfdCen,errtotptP,errpQCDplusHydroNPnonfdCen);
  TGraph *pQCDplusHydroPnonfdPer = new TGraph(ntotBins,totptP,pQCDplusHydroNPnonfdPer);
  pQCDplusHydroPnonfdCen->SetTitle("pQCDplusHydroPnonfdCen");
  pQCDplusHydroPnonfdPer->SetTitle("pQCDplusHydroPnonfdPer");

  TGraphErrors *pQCDplusHydroPCen = new TGraphErrors(ntotBins,totptP,pQCDplusHydroNPCen,errtotptP,errpQCDplusHydroNPCen);
  TGraph *pQCDplusHydroPPer = new TGraph(ntotBins,totptP,pQCDplusHydroNPPer);
  pQCDplusHydroPCen->SetTitle("pQCDplusHydroPCen");
  pQCDplusHydroPPer->SetTitle("pQCDplusHydroPPer");

  TF1 *pQCDplusHydroPCenFit = new TF1("pQCDplusHydroPCenFit","[0]*exp(sqrt(x*x+[1])/[2])", ptmin, ptmax);
  pQCDplusHydroPCenFit->SetParameters(0.05,2.,-0.3);
  pQCDplusHydroPCen->Fit("pQCDplusHydroPCenFit","EL");


  //____________________________________________________________________________
  // PHENIX pi+/-

  const int nPiBins = 28;
  double phenixPtPim[nPiBins];
  double phenixEptPim[nPiBins];
  double phenixNPimCen[nPiBins];
  double phenixEPimCen[nPiBins];
  double phenixNPimPer[nPiBins];
  double phenixEPimPer[nPiBins];
  double adump,edump,idump,odump;

  char cdamp[200];
  char centPiCen[200];
  char centPiPer[200];

  sprintf(filename,"%s/pim.dat",path);
  ifstream pim(filename);

  for(int i=0;i<12;i++){
    pim >> cdamp;
    if(i==icenB)
      {
	sprintf(centPiCen,cdamp);
	cout <<"<I> Plotting PHENIX pi+/- spectra for [" << centPiCen << "] centrality class" <<  endl;
      }
    if(i==iperB)
      {
	sprintf(centPiPer,cdamp);
	cout <<"<I> Plotting PHENIX pi+/- spectra for [" << centPiPer << "] centrality class" <<  endl;
      }
  }

  for(int ipt = 0; ipt < nPiBins; ipt++){
    phenixEptPim[ipt]=0.001;
    pim >> phenixPtPim[ipt];
    for(int icen=0; icen<11;icen++){
      if(icen==icenB-1)
	pim >> phenixNPimCen[ipt] >> phenixEPimCen[ipt];
      else
	if(icen==iperB-1)
	  pim >> phenixNPimPer[ipt] >> phenixEPimPer[ipt];
	else
	  pim >> adump >> edump;
    }
  } 
  pim.close();

  TGraphErrors * phenixPimCen = new TGraphErrors(nPiBins,phenixPtPim,phenixNPimCen,phenixEptPim,phenixEPimCen);
  TGraphErrors * phenixPimPer = new TGraphErrors(nPiBins,phenixPtPim,phenixNPimPer,phenixEptPim,phenixEPimPer);
  phenixPimCen->SetTitle("phenixPimCen");
  phenixPimPer->SetTitle("phenixPimPer");

  //____________________________________________________________________________
  // PHENIX pi0

  const int nPi0Bins = 9;
  double phenixPtPi0[nPi0Bins];
  double phenixEptPi0[nPi0Bins];
  double phenixNPi0Cen[nPi0Bins];
  double phenixEPi0Cen[nPi0Bins];
  double phenixNPi0Per[nPi0Bins];
  double phenixEPi0Per[nPi0Bins];

  char centPi0Cen[200];
  char centPi0Per[200];

  if(icenB==1 || icenB==2)
    {
      sprintf(filename,"%s/pi0_emcal_0_10_chisq2_finaljustin.txt",path); 
      sprintf(centPi0Cen,"0-10%");
    }
  else 
    {
      cout << " <W> Centrality " << icenB << " not available for pi0 " << endl;
      exit(0);
    }
  cout << "<I> Reading: " << filename << endl;

  ifstream pi0(filename);
  for(int ipt = 0; ipt < nPi0Bins; ipt++){
    pi0 >> phenixPtPi0[ipt] >>  phenixEptPi0[ipt] >> phenixNPi0Cen[ipt] >> phenixEPi0Cen[ipt];
    //pi0 >> adump >> edump >> idump >> odump;
    //cout << phenixPtPi0[ipt] << " " <<  phenixEptPi0[ipt] << " " << phenixNPi0Cen[ipt] << " " << phenixEPi0Cen[ipt] << endl;
  } 
  pi0.close();

  TGraphErrors * phenixPi0Cen = new TGraphErrors(nPi0Bins,phenixPtPi0,phenixNPi0Cen,phenixEptPi0,phenixEPi0Cen);
  phenixPi0Cen->SetTitle("phenixPi0Cen");
  //phenixPi0Cen->Print();

  if(iperB==9)
    {
      sprintf(filename,"%s/pi0_emcal_60_70_chisq2_finaljustin.txt",path); 
      sprintf(centPi0Per,"60-70%");
    }
  else if(iperB==10)
    {
      sprintf(filename,"%s/pi0_emcal_70_80_chisq2_finaljustin.txt",path); 
      sprintf(centPi0Per,"70-80%");
    }
  else
    {
      cout << "<W> Centrality " << iperB << " not available for pi0 " << endl;
      exit(0);
    }
  cout << "<I> Reading: " << filename << endl;

  ifstream p(filename);
  for(int ipt = 0; ipt < nPi0Bins; ipt++){
    p >> phenixPtPi0[ipt] >> phenixEptPi0[ipt] >> phenixNPi0Per[ipt] >> phenixEPi0Per[ipt];
  } 
 pi0.close();

 TGraphErrors * phenixPi0Per = new TGraphErrors(nPi0Bins,phenixPtPi0,phenixNPi0Per,phenixEptPi0,phenixEPi0Per);
 phenixPi0Per->SetTitle("phenixPi0Per");
 //phenixPi0Per->Print();

  //____________________________________________________________________________
  // PHENIX K+/-

  const int nKBins = 16;
  double phenixPtK[nKBins];
  double phenixEptK[nKBins];
  double phenixNKCen[nKBins];
  double phenixEKCen[nKBins];
  double phenixNKPer[nKBins];
  double phenixEKPer[nKBins];

  char centKCen[255];
  char centKPer[255];

  sprintf(filename,"%s/kpl.dat",path);
  ifstream kpl(filename);

  for(int i=0;i<12;i++){
    kpl >> cdamp;
    if(i==icenB)
      {
	sprintf(centKCen,cdamp);
	cout <<"<I> Plotting PHENIX K+/- spectra for [" << centKCen << "] centrality class" <<  endl;
      }
    if(i==iperB)
      {
	sprintf(centKPer,cdamp);
	cout <<"<I> Plotting PHENIX K+/- spectra for [" << centKPer << "] centrality class" <<  endl;
      }
  }

  for(int ipt = 0; ipt < nKBins; ipt++){
    phenixEptK[ipt]=0.001;
    kpl >> phenixPtK[ipt];
    for(int icen=0; icen<11;icen++){
      if(icen==icenB-1)
	kpl >> phenixNKCen[ipt] >> phenixEKCen[ipt];
      else
	if(icen==iperB-1)
	  kpl >> phenixNKPer[ipt] >> phenixEKPer[ipt];
	else
	  kpl >> adump >> edump;
    }
    phenixNKCen[ipt]*=kScale;
    phenixEKCen[ipt]*=kScale;
    phenixNKPer[ipt]*=kScale;
    phenixEKPer[ipt]*=kScale;
  } 
  kpl.close();

  TGraphErrors * phenixKmCen = new TGraphErrors(nKBins,phenixPtK,phenixNKCen,phenixEptK,phenixEKCen);
  TGraphErrors * phenixKmPer = new TGraphErrors(nKBins,phenixPtK,phenixNKPer,phenixEptK,phenixEKPer);
  phenixKmCen->SetTitle("phenixKmCen");
  phenixKmPer->SetTitle("phenixKmPer");

  //____________________________________________________________________________
  // PHENIX p,pbar *feed-down* corrected

  const int nPBins = 22;
  double phenixPtP[nPBins];
  double phenixEptP[nPBins];
  double phenixNPCen[nPBins];
  double phenixEPCen[nPBins];
  double phenixNPPer[nPBins];
  double phenixEPPer[nPBins];

  char centPCen[255];
  char centPPer[255];

  sprintf(filename,"%s/p.dat",path);
  ifstream p(filename);

  for(int i=0;i<12;i++){
    p >> cdamp;
    if(i==icenB )
      {
	sprintf(centPCen,cdamp);
	cout <<"<I> Plotting PHENIX p,pbar *feed-down* corr. spectra for [" << centPCen << "] centrality class" <<  endl;
      }
    if(i==iperB )
      {
	sprintf(centPPer,cdamp);
	cout <<"<I> Plotting PHENIX p,pbar spectra for [" << centPPer << "] centrality class" <<  endl;
      }
  }

  for(int ipt = 0; ipt < nPBins; ipt++){
    phenixEptP[ipt]=0.001;
    p >> phenixPtP[ipt];
    for(int icen=0; icen<11;icen++){
      if(icen==icenB-1)
	p >> phenixNPCen[ipt] >> phenixEPCen[ipt];
      else
	if(icen==iperB-1)
	  p >> phenixNPPer[ipt] >> phenixEPPer[ipt];
	else
	  p >> adump >> edump;
    }
    phenixNPCen[ipt]*=pScale;
    phenixEPCen[ipt]*=pScale;
    phenixNPPer[ipt]*=pScale;
    phenixEPPer[ipt]*=pScale;
  } 
  p.close();

  TGraphErrors * phenixPCen = new TGraphErrors(nPBins,phenixPtP,phenixNPCen,phenixEptP,phenixEPCen);
  TGraphErrors * phenixPPer = new TGraphErrors(nPBins,phenixPtP,phenixNPPer,phenixEptP,phenixEPPer);
  phenixPCen->SetTitle("phenixPCen");
  phenixPPer->SetTitle("phenixPPer");

  //____________________________________________________________________________
  // PHENIX proton no feed-down correction
  // https://www.phenix.bnl.gov/phenix/WWW/p/info/ppg/026/DataTable/03.0313/spectra/

  //   // 5-10%
  //   double phenixPtPnoL[nPBins]={0.650, 0.750, 0.850, 0.950, 1.050, 1.150, 1.250, 1.350, 1.450, 1.550, 1.650, 1.750, 
  // 		      1.850, 1.950, 2.100, 2.300, 2.500, 2.700, 2.900, 3.250, 3.750, 4.250};
  //   double phenixEptPnoL[nPBins]={0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 
  // 		       0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00};
  
  //   double phenixNPCennoL[nPBins]={4.04, 3.59, 2.99, 2.55, 2.17, 1.83, 1.42, 1.18, 
  // 			8.93e-01, 7.37e-01, 5.43e-01, 3.95e-01, 3.25e-01, 2.28e-01, 1.59e-01, 9.47e-02, 
  // 			5.15e-02, 3.18e-02, 1.62e-02, 7.13e-03, 2.19e-03, 9.44e-04};
  //   double phenixEnPCennoL[nPBins]={7.34e-02, 6.77e-02, 5.83e-02, 5.19e-02, 4.58e-02, 4.06e-02, 3.28e-02, 2.90e-02, 
  // 			 2.32e-02, 2.04e-02, 1.61e-02, 1.27e-02, 1.13e-02, 8.74e-03, 3.99e-03, 2.93e-03, 
  // 			 2.05e-03, 1.59e-03, 1.12e-03, 4.59e-04, 2.60e-04, 1.76e-04};
  
  //   // PHENIX proton ??-??%, no feed-down correction
  
  //   double phenixNPPernoL[nPBins]={3.38e-01, 2.65e-01, 1.98e-01, 1.54e-01, 1.13e-01, 8.83e-02, 5.93e-02, 5.05e-02, 
  // 			 3.53e-02, 2.46e-02, 1.87e-02, 1.40e-02, 1.02e-02, 7.58e-03, 4.90e-03, 3.21e-03, 
  // 			 1.39e-03, 8.83e-04, 6.43e-04, 2.29e-04, 7.50e-05, 3.22e-05};
  //   double phenixEnPPernoL[nPBins]={8.94e-03, 7.55e-03, 6.12e-03, 5.18e-03, 4.21e-03, 3.62e-03, 2.80e-03, 2.57e-03, 
  // 			 2.06e-03, 1.70e-03, 1.44e-03, 1.21e-03, 1.04e-03, 8.74e-04, 4.02e-04, 3.23e-04, 
  // 			 2.08e-04, 1.67e-04, 1.43e-04, 5.30e-05, 3.07e-05, 2.08e-05};


  const int nPBins = 22;
  double phenixPtP_nonfd[nPBins];
  double phenixEptP_nonfd[nPBins];
  double phenixNP_nonfd_Cen[nPBins];
  double phenixEP_nonfd_Cen[nPBins];
  double phenixNP_nonfd_Per[nPBins];
  double phenixEP_nonfd_Per[nPBins];

  char centP_nonfd_Cen[255];
  char centP_nonfd_Per[255];

  if(icenB==1)
    {
      sprintf(filename,"%s/pr_phnx_cent_0-5_nofeeddown.dat",path); 
      sprintf(centP_nonfd_Cen,"0-5%");
    }
  else if(icenB==2)
    {
      sprintf(filename,"%s/pr_phnx_cent_5-10_nofeeddown.dat",path); 
      sprintf(centP_nonfd_Cen,"5-15%");
    }
  else if(icenB==3)
    {
      sprintf(filename,"%s/pr_phnx_cent_10-15_nofeeddown.dat",path); 
      sprintf(centP_nonfd_Cen,"10-15%");
    }
  else
    {
      cout << "<W> Centrality " << icenB << " not available for protons NON-feeddown. "
	   << "check: https://www.phenix.bnl.gov/phenix/WWW/p/info/ppg/026/DataTable/03.0313/spectra/" << endl;
      exit(0);
    }
  cout <<"<I> Reading " << filename << " --> PHENIX p,pbar NON feed-down spectra for [" 
       << centP_nonfd_Cen << "] centrality class" <<  endl;

  ifstream p(filename);
  for(int ipt = 0; ipt < nPBins; ipt++){
    phenixEptP_nonfd[ipt]=0.001;
    p >> phenixPtP_nonfd[ipt] >> phenixNP_nonfd_Cen[ipt] >> phenixEP_nonfd_Cen[ipt];
    phenixNP_nonfd_Cen[ipt]*=pScale;
    phenixEP_nonfd_Cen[ipt]*=pScale;
  } 
  p.close();

  if(iperB==9)
    {
      sprintf(filename,"%s/pr_phnx_cent_60-70_nofeeddown.dat",path); 
      sprintf(centP_nonfd_Per,"60-70%");
    }
  else if(iperB==11) 
    {
      sprintf(filename,"%s/pr_phnx_cent_80-91_nofeeddown.dat",path); 
      //if(iperB==12) sprintf(filename,"pr_phnx_cent_60-91_nofeeddown.dat",path); 
      sprintf(centP_nonfd_Per,"80-91%");
    }
  else
    {
      cout << "<W> Centrality " << iperB << " not available for protons non-feeddown. "
	   << "check: https://www.phenix.bnl.gov/phenix/WWW/p/info/ppg/026/DataTable/03.0313/spectra/" << endl;
      exit(0);
    }
  cout <<"<I> Reading " << filename << " --> PHENIX p,pbar NON feed-down spectra for [" 
       << centP_nonfd_Per << "] centrality class" <<  endl;

  ifstream p(filename);
  for(int ipt = 0; ipt < nPBins; ipt++){
    phenixEptP_nonfd[ipt]=0.001;
    p >> phenixPtP_nonfd[ipt] >> phenixNP_nonfd_Per[ipt] >> phenixEP_nonfd_Per[ipt];
    phenixNP_nonfd_Per[ipt]*=pScale;
    phenixEP_nonfd_Per[ipt]*=pScale;
  } 
  p.close();

  TGraphErrors * phenixP_nonfd_Cen = 
    new TGraphErrors(nPBins,phenixPtP_nonfd,phenixNP_nonfd_Cen,phenixEptP_nonfd,phenixEP_nonfd_Cen);
  TGraphErrors * phenixP_nonfd_Per = 
    new TGraphErrors(nPBins,phenixPtP_nonfd,phenixNP_nonfd_Per,phenixEptP_nonfd,phenixEP_nonfd_Per);
  phenixP_nonfd_Cen->SetTitle("phenixP_nonfd_Cen");
  phenixP_nonfd_Per->SetTitle("phenixP_nonfd_Per");

  //____________________________________________________________________________
  // STAR spectra

  // pi+ , 5-10% Au+Au
  double pi_pt_11[11]={0.225, 0.275, 0.325, 0.375, 0.425, 0.476, 0.525, 0.575, 0.625, 0.675, 0.725};
  double epi_pt_11[11]={0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00};
  double Npip_AuAu_5_10[11]={324.10, 241.00, 187.80, 147.20, 115.60, 90.51, 71.59, 56.30, 44.60, 35.05, 28.10};
  double eNpip_AuAu_5_10[11]={9.80, 4.80, 3.80, 1.50, 1.20, 0.93, 0.74, 1.00, 1.30, 0.99, 1.10};
  //double ey[12]={1.70e+01, 8.56, 6.72, 2.69, 2.12, 1.70, 1.31, 1.98, 2.33, 1.84, 1.98};

  // pi+, 60-70% Au+Au
  double pi_pt_12[12]={0.225, 0.275, 0.325, 0.375, 0.425, 0.476, 0.525, 0.575, 0.625, 0.675, 0.725, 0.775};
  double epi_pt_12[12]={0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00};
  double Npip_AuAu_60_70[12]={30.51, 21.96, 16.44, 12.33, 9.45, 7.22, 5.52, 4.30, 3.29, 2.58, 2.09, 1.61};
  double eNpip_AuAu_60_70[12]={0.93, 0.45, 0.34, 0.13, 0.10, 0.08, 0.06, 0.06, 0.08, 0.06, 0.07, 0.06};

  // K+, 5-10% Au+Au
  double k_pt_10[10]={0.225, 0.276, 0.326, 0.376, 0.425, 0.476, 0.525, 0.575, 0.625, 0.676};
  double ek_pt_10[10]={0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00};
  double Nkp_AuAu_5_10[10]={2.3e+01, 2.1e+01, 2.0e+01, 1.8e+01, 1.7e+01, 1.5e+01, 1.3e+01, 1.2e+01, 1.1e+01, 8.9};
  double eNkp_AuAu_5_10[10]={1.6, 6.5e-01, 3.9e-01, 5.2e-01, 7.9e-01, 1.7, 1.6, 1.4, 8.3e-01, 8.5e-01};

  for (int i=0;i<10;i++)
    { 
      Nkp_AuAu_5_10[i]*=kScale; 
      eNkp_AuAu_5_10[i]*=kScale; 
    }

  // K+, 60-70% Au+Au
  double k_pt_11[11]={0.225, 0.276, 0.326, 0.376, 0.425, 0.476, 0.525, 0.575, 0.625, 0.676, 0.724};
  double ek_pt_11[11]={0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00};
  double Nkp_AuAu_60_70[11]={2.4, 2.2, 1.9, 1.7, 1.4, 1.2, 1.0, 8.7e-01, 7.3e-01, 6.1e-01, 4.9e-01};
  double eNkp_AuAu_60_70[11]={1.8e-01,7.9e-02,4.0e-02, 4.5e-02, 5.4e-02, 1.3e-01, 1.1e-01, 9.1e-02, 5.4e-02, 4.9e-02, 5.5e-02};

  for (int i=0;i<11;i++)
    { 
      Nkp_AuAu_60_70[i]*=kScale; 
      eNkp_AuAu_60_70[i]*=kScale; 
    }

  // K0s, 0-5% Au+Au
  double k0s_pt_0_5[19]={0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 1.9, 2.1, 2.3,
			 2.5, 2.7, 2.9, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75};
  double Nk0s_AuAu_0_5[19]={12.0, 9.05, 3.77, 2.75, 1.39, 8.03e-01, 4.48e-01, 2.18e-01, 
			    1.23e-01, 7.03e-02, 3.75e-02, 1.98e-02, 1.16e-02, 4.79e-03, 
			    1.42e-03, 4.84e-04, 1.52e-04, 6.06e-05, 1.88e-05};
  double ek0s_pt_0_5[19]={0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
  double eNk0s_AuAu_0_5[19]={1.20, 9.07e-01, 3.77e-01, 2.76e-01, 1.40e-01, 8.08e-02, 4.52e-02, 
			     2.21e-02, 1.26e-02, 7.24e-03, 3.92e-03, 2.01e-03, 1.19e-03, 4.87e-04, 
			     1.49e-04, 5.41e-05, 1.75e-05, 7.75e-06, 3.44e-06};
  for (int i=0;i<19;i++)
    { 
      Nk0s_AuAu_0_5[i]*=kScale; 
      eNk0s_AuAu_0_5[i]*=kScale; 
    }

  // K0s, 60-80% Au+Au
  double k0s_pt_60_80[16]={0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 1.9, 2.1, 2.3, 2.5, 2.7, 2.9, 3.250, 3.750, 4.250};
  double Nk0s_AuAu_60_80[16]={6.84e-01, 3.47e-01, 1.86e-01, 1.01e-01, 5.37e-02, 2.59e-02, 
			 1.38e-02, 8.60e-03, 5.17e-03, 2.49e-03, 1.51e-03, 1.01e-03, 
			 5.72e-04, 2.35e-04, 9.16e-05, 2.36e-05};
  double ek0s_pt_60_80[16]={0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};
  double eNk0s_AuAu_60_80[16]={7.02e-02, 3.54e-02, 1.89e-02, 1.03e-02, 5.55e-03, 2.72e-03, 
			  1.48e-03, 9.54e-04, 5.55e-04, 2.79e-04, 1.78e-04, 1.26e-04, 
			  7.74e-05, 3.13e-05, 1.27e-05, 4.57e-06};
  
  for (int i=0;i<16;i++)
    { 
      Nk0s_AuAu_60_80[i]*=kScale; 
      eNk0s_AuAu_60_80[i]*=kScale; 
    }
  
  // p, 0-5% Au+Au
  double p_pt_16[16]={0.425,0.476,0.525,0.575,0.625,0.676,0.726,0.775,0.825,
		      0.875, 0.925, 0.975, 1.026, 1.076, 1.125, 1.176};
  double ep_pt_16[16]={0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 
		       0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00};

  double Np_nonfd_AuAu_5_10[16]={5.70,5.62,5.38,5.09,4.89,4.56,4.30, 4.09, 3.83, 
			   3.67, 3.42, 3.17, 2.95, 2.71, 2.57, 2.34};
  double eNp_nonfd_AuAu_5_10[16]={2.80e-01, 2.20e-01, 1.70e-01, 1.30e-01, 1.10e-01, 1.10e-01, 1.00e-01, 9.20e-02, 
			    8.40e-02, 7.90e-02, 1.00e-01, 9.30e-02, 9.30e-02, 8.70e-02, 1.00e-01, 1.10e-01};

  double Np_AuAu_5_10[16];
  double eNp_AuAu_5_10[16];

  for (int i=0;i<16;i++)
    { 
      Np_nonfd_AuAu_5_10[i]*=pScale;
      eNp_nonfd_AuAu_5_10[i]*=pScale;

      // for fd-uncorrected spectra
      double feeddown = 0.6;
      Np_AuAu_5_10[i]=Np_nonfd_AuAu_5_10[i]*feeddown;
      eNp_AuAu_5_10[i]=eNp_nonfd_AuAu_5_10[i]*feeddown;
    }

  // p, 60-70% Au+Au
  double Np_nonfd_AuAu_60_70[16]={7.12e-01, 6.65e-01, 6.31e-01, 5.56e-01, 5.10e-01, 4.46e-01, 4.03e-01, 3.71e-01, 
				  3.17e-01, 2.84e-01, 2.56e-01, 2.29e-01, 2.14e-01, 1.76e-01, 1.60e-01, 1.38e-01};
  double eNp_nonfd_AuAu_60_70[16]={3.60e-02, 2.70e-02, 2.20e-02, 1.60e-02, 1.30e-02, 1.10e-02, 1.10e-02, 1.00e-02, 
				   8.70e-03, 7.80e-03, 7.60e-03, 6.90e-03, 7.90e-03, 6.70e-03, 6.80e-03, 7.50e-03};

  double Np_AuAu_60_70[16];
  double eNp_AuAu_60_70[16];

  for (int i=0;i<16;i++)
    { 
      Np_nonfd_AuAu_60_70[i]*=pScale;
      eNp_nonfd_AuAu_60_70[i]*=pScale;

      // for fd-uncorrected spectra
      double feeddown = 0.6;
      Np_AuAu_60_70[i]=Np_nonfd_AuAu_60_70[i]*feeddown;
      eNp_AuAu_60_70[i]=eNp_nonfd_AuAu_60_70[i]*feeddown ;
    }
  
  TGraphErrors *starPimCen = new TGraphErrors(11,pi_pt_11,Npip_AuAu_5_10,epi_pt_11,eNpip_AuAu_5_10);
  TGraphErrors *starKplCen = new TGraphErrors(10,k_pt_10,Nkp_AuAu_5_10,ek_pt_10,eNkp_AuAu_5_10);
  TGraphErrors *starK0sCen = new TGraphErrors(19,k0s_pt_0_5,Nk0s_AuAu_0_5,ek0s_pt_0_5,eNk0s_AuAu_0_5);
  TGraphErrors *starPCen   = new TGraphErrors(16,p_pt_16,Np_AuAu_5_10,ep_pt_16,eNp_AuAu_5_10);
  TGraphErrors *starP_nonfd_Cen = new TGraphErrors(16,p_pt_16,Np_nonfd_AuAu_5_10,ep_pt_16,eNp_nonfd_AuAu_5_10);

  TGraphErrors *starPimPer = new TGraphErrors(11,pi_pt_11,Npip_AuAu_60_70,epi_pt_11,eNpip_AuAu_60_70);
  TGraphErrors *starKplPer = new TGraphErrors(10,k_pt_10,Nkp_AuAu_60_70,ek_pt_10,eNkp_AuAu_60_70);
  TGraphErrors *starK0sPer = new TGraphErrors(16,k0s_pt_60_80,Nk0s_AuAu_60_80,ek0s_pt_60_80,eNk0s_AuAu_60_80);
  TGraphErrors *starPPer   = new TGraphErrors(16,p_pt_16,Np_AuAu_60_70,ep_pt_16,eNp_AuAu_60_70);
  TGraphErrors *starP_nonfd_Per = new TGraphErrors(16,p_pt_16,Np_nonfd_AuAu_60_70,ep_pt_16,eNp_nonfd_AuAu_60_70);

  starPimCen->SetTitle("starPimCen");
  starKplCen->SetTitle("starKplCen");
  starK0sCen->SetTitle("starK0sCen");
  starPCen  ->SetTitle("starPCen");
  starP_nonfd_Cen->SetTitle("starP_nonfd_Cen");

  starPimPer->SetTitle("starPimPer");
  starKplPer->SetTitle("starKplPer");
  starK0sPer->SetTitle("starK0sPer");
  starPPer  ->SetTitle("starPPer");
  starP_nonfd_Per->SetTitle("starP_nonfd_Per");

  //____________________________________________________________________________
  // BRAHMS spectra
  // http://www4.rcf.bnl.gov/brahms/WWW/publication_data/mesonAuAu200/

  // pi, 0-5% Au+Au
  double pi_x[18]={0.250, 0.350, 0.450, 0.550, 0.650, 0.750, 0.850, 0.950, 1.050, 
		   1.150, 1.250, 1.350, 1.450, 1.550, 1.650, 1.750, 1.850, 1.950};
  double pi_ex[18]={0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 
		    0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00};

  // pi+ , 0-5% Au+Au
  double pip_y[18]={3.20e+02, 2.03e+02, 1.17e+02, 7.71e+01, 4.78e+01, 3.13e+01, 2.00e+01, 1.31e+01, 
		    9.41, 6.10, 4.27, 3.03, 1.89, 1.34, 9.50e-01, 6.21e-01, 4.62e-01, 2.90e-01};
  double pip_ey[18]={1.74, 5.25, 2.42, 1.23, 7.36e-01, 5.08e-01, 3.59e-01, 2.65e-01, 2.08e-01, 1.59e-01, 
		     1.27e-01, 1.00e-01, 7.63e-02, 6.11e-02, 5.00e-02, 3.91e-02, 3.30e-02, 2.54e-02};
  // pi- , 0-5% Au+Au
  double pim_y[18]={2.98e+02, 2.03e+02, 1.19e+02, 7.79e+01, 4.68e+01, 3.15e+01, 2.15e+01, 1.34e+01, 
		    9.03, 5.97, 4.21, 2.57, 1.86, 1.26, 8.15e-01, 6.68e-01, 4.30e-01, 3.09e-01};
  double pim_ey[18]={1.12e+01, 5.27, 2.76, 1.75, 1.11, 7.69e-01, 5.22e-01, 3.70e-01, 2.71e-01, 1.98e-01, 
		     1.52e-01, 1.12e-01, 8.76e-02, 6.90e-02, 5.34e-02, 4.60e-02, 3.60e-02, 2.90e-02};

  // K , 0-5% Au+Au
  double K_x[18]={0.250, 0.350, 0.450, 0.550, 0.650, 0.750, 0.850, 0.950, 1.050, 
		  1.150, 1.250, 1.350, 1.450, 1.550, 1.650, 1.750, 1.850, 1.950};
  double K_ex[18]={0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 
		   0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00};

  // K+ , 0-5% Au+Au
  double Kp_y[18]={1.71e+01, 1.92e+01, 1.73e+01, 1.50e+01, 1.14e+01, 8.16, 6.37, 4.27, 3.46, 
		   2.64, 1.81, 1.39, 8.17e-01, 7.23e-01, 5.34e-01, 3.65e-01, 2.47e-01, 2.20e-01};
  double Kp_ey[18]={8.54, 3.57, 1.76, 9.76e-01, 5.73e-01, 3.90e-01, 2.86e-01, 2.01e-01, 1.65e-01, 1.31e-01, 
		    1.01e-01, 8.06e-02, 5.86e-02, 5.12e-02, 4.21e-02, 3.33e-02, 2.65e-02, 2.42e-02};
  // K- , 0-5% Au+Au
  double Km_y[18]={2.13e+01, 1.34e+01, 1.60e+01, 1.53e+01, 9.90, 6.79, 5.90, 4.13, 3.42, 2.15, 
		   1.70, 1.17, 8.49e-01, 6.07e-01, 4.63e-01, 3.68e-01, 2.22e-01, 1.71e-01};
  double Km_ey[18]={9.53, 2.99, 1.88, 1.25, 6.97e-01, 4.58e-01, 3.36e-01, 2.29e-01, 1.81e-01, 1.29e-01, 
		    1.04e-01, 7.85e-02, 6.24e-02, 4.96e-02, 4.16e-02, 3.49e-02, 2.62e-02, 2.21e-02};
  
  // protons , 0-10% Au+Au

  double p_x[27]={0.35,0.45,0.55,0.65,0.75,0.85,0.95,1.05,1.15,1.25,1.35,1.45,1.55,1.65,
		  1.75,1.85,1.95,2.05,2.15,2.25,2.35,2.45,2.55,2.65,2.75,2.85,2.95};

  double p_ex[27]={0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
		   0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00};

  double p_y[27]={3.56,3.42,3.28,2.79,2.68,2.21,2.09,1.78,1.27,1.21,0.93,0.82,0.62,0.48,
		  0.43,0.31,0.18,0.16,0.10,0.099,0.073,0.061,0.043,0.036,0.025,0.015,0.015};

  double p_ey[27]={2.98,0.44,0.20,0.13,0.10,0.08,0.07,0.06,0.05,0.04,0.03,0.03,0.02,0.02,0.018,
		   0.015,0.011,0.011,0.008,0.008,0.007,0.006,0.005,0.005,0.004,0.003,0.003};


  for (int i=0;i<18;i++)
    { 
      Kp_y[i]*=kScale; 
      Kp_ey[i]*=kScale; 

      Km_y[i]*=kScale; 
      Km_ey[i]*=kScale; 
    }

  for (int i=0;i<27;i++)
    { 
      p_y[i]*=pScale; 
      p_ey[i]*=pScale; 
    }

  TGraphErrors *brahmsPiplusCen = new TGraphErrors(18,pi_x,pip_y,pi_ex,pip_ey);
  TGraphErrors *brahmsPiminusCen = new TGraphErrors(18,pi_x,pim_y,pi_ex,pim_ey);
  TGraphErrors *brahmsKplusCen  = new TGraphErrors(18,K_x,Kp_y,K_ex,Kp_ey);
  TGraphErrors *brahmsKminusCen  = new TGraphErrors(18,K_x,Km_y,K_ex,Km_ey);
  TGraphErrors *brahmsProtonCen = new TGraphErrors(27,p_x,p_y,p_ex,p_ey);
  brahmsPiplusCen->SetTitle("brahmsPiplusCen");
  brahmsPiminusCen->SetTitle("brahmsPiminusCen");
  brahmsKplusCen->SetTitle("brahmsKplusCen");
  brahmsKminusCen->SetTitle("brahmsKminusCen");
  brahmsPiplusCen->SetTitle("brahmsProtonCen");
  
  //____________________________________________________________________________
  // PHOBOS spectra (very low pT)
  // http://www.phobos.bnl.gov/Publications/Physics/Stopping/stopping_data.txt
  // Data for invariant yields of charged particles in 0-15% 
  // most central Au + Au collisions at sqrt{s_{NN}}=200 GeV.
  // Yield = 1/(2*pi*p_T*N_{ev}) d^2N/dydp_T [c^2/GeV^2]

  double dtmp = 0.;

  // (pi^+ + pi^-) 
  double pi_lowpt[4] = {0.0311, 0.0435, 0.0484, 0.0529};
  double e_lowpt[4] = {0.01, 0.01, 0.01, 0.01}; // arbitrary ...
  double pi_yield_phobos[4] = { 1478.1, 1288.1, 1387.9, 1525.4};
  double e_pi_yield_phobos[4] = { 106.0, 51.4, 50.8 , 53.9}; // stat. error
  double esyst_pi_yield_phobos[4] = { 177.4, 154.6, 166.5, 183.0}; // syst. error.

  // stat.+syst. error in quadrature
  for (int i=0;i<4;i++)
    { 
      dtmp = pow(e_pi_yield_phobos[i],2.)+pow(esyst_pi_yield_phobos[i],2.);
      e_pi_yield_phobos[i]=sqrt(dtmp);
      pi_yield_phobos[i]/=2.;
      e_pi_yield_phobos[i]/=2.;
    }

   //(K^+ + K^-) 
  double K_lowpt[4] = { 0.096, 0.116, 0.123, 0.128};
  double K_yield_phobos[4] = { 47.38, 50.98, 58.27, 62.94};
  double e_K_yield_phobos[4] = { 3.72, 4.89, 5.61, 6.15}; // stat. error
  double esyst_K_yield_phobos[4] = { 8.53, 9.18, 10.49, 11.33}; // syst. error.

  for (int i=0;i<4;i++)
    { 
      K_yield_phobos[i]*=kScale; 
      e_K_yield_phobos[i]*=kScale; 
      esyst_K_yield_phobos[i]*=kScale; 

      // stat.+syst. error in quadrature
      dtmp = pow(e_K_yield_phobos[i],2.)+pow(esyst_K_yield_phobos[i],2.);
      e_K_yield_phobos[i]=sqrt(dtmp);

      K_yield_phobos[i]/=2.;
      e_K_yield_phobos[i]/=2.;
    }

  //(p + pbar) 
  double p_lowpt[4] = { 0.143, 0.172, 0.190, 0.206};
  double p_yield_phobos[4] = { 7.518, 7.064, 6.546, 6.722};
  double e_p_yield_phobos[4] = { 0.899, 0.773, 0.791, 0.823}; // stat. error
  double esyst_p_yield_phobos[4] = { 2.406, 2.261, 2.095, 2.151}; // syst. error.

  double p_nonfd_yield_phobos[4];
  double e_p_nonfd_yield_phobos[4];
  
  for (int i=0;i<4;i++)
    { 

      p_yield_phobos[i]*=pScale;
      e_p_yield_phobos[i]*=pScale;
      esyst_p_yield_phobos[i]*=pScale;

      // stat.+syst. error in quadrature
      dtmp = pow(e_p_yield_phobos[i],2.)+pow(esyst_p_yield_phobos[i],2.);
      e_p_yield_phobos[i]=sqrt(dtmp);

      double pbar_over_p = 0.73;

      p_yield_phobos[i]/=(1+pbar_over_p);
      e_p_yield_phobos[i]/=(1+pbar_over_p);

      // for fd-uncorrected spectra
      double feeddown =1.3;
      p_nonfd_yield_phobos[i]= p_yield_phobos[i]*feeddown;
      e_p_nonfd_yield_phobos[i]= e_p_yield_phobos[i]*feeddown;

     }
  
  TGraphErrors *phobosPiCen = new TGraphErrors(4,pi_lowpt,pi_yield_phobos,e_lowpt,e_pi_yield_phobos);
  TGraphErrors *phobosKCen  = new TGraphErrors(4,K_lowpt,K_yield_phobos,e_lowpt,e_K_yield_phobos);
  TGraphErrors *phobosPCen  = new TGraphErrors(4,p_lowpt,p_yield_phobos,e_lowpt,e_p_yield_phobos);
  TGraphErrors *phobosP_nonfd_Cen  = new TGraphErrors(4,p_lowpt,p_nonfd_yield_phobos,e_lowpt,e_p_nonfd_yield_phobos);
  phobosPiCen->SetTitle("phobosPiCen");
  phobosKCen->SetTitle("phobosKCen");
  phobosPCen->SetTitle("phobosPCen");
  phobosP_nonfd_Cen->SetTitle("phobosP_nonfd_Cen");

  //____________________________________________________________________________
  // Plot CENTRAL spectra

  TCanvas * cCen = new TCanvas("hadron_spec_AuAu200GeV_cent","Hadron spectrum Au+Au 200 GeV central",650,700);
  cCen->Range(-0.830387,-7.30357,5.12994,4.33929);
  cCen->SetLeftMargin(0.139319);
  cCen->SetRightMargin(0.0201238);
  cCen->SetTopMargin(0.0291411);
  cCen->SetBottomMargin(0.111963);
  cCen->SetLogy(1);
  cCen->SetFillColor(0);
  cCen->SetFillStyle(0);

  double ymax = 1.e+5;
  double ymin = 1.e-9;

  hydroPimCen->GetXaxis()->SetRangeUser(ptmin,ptmax);
  hydroPimCen->SetMinimum(ymin);
  hydroPimCen->SetMaximum(ymax);
  hydroPimCen->SetXTitle("p_{T} (GeV/c)");
  hydroPimCen->GetXaxis()->SetTitleSize(0.05);
  hydroPimCen->SetYTitle("d^{2}N/(#pi dp_{T}^{2}dy) (GeV/c)^{-2}");
  hydroPimCen->GetYaxis()->SetTitleOffset(1.2);
  hydroPimCen->GetYaxis()->SetTitleSize(0.05);
  hydroPimCen->SetTitle("");
  hydroPimCen->SetStats(0);

  int hydroLine = 2;
  int hydroWidth = 2;

  hydroPimCen->SetLineStyle(hydroLine);
  hydroPimCen->SetLineColor(2);
  hydroPimCen->SetLineWidth(hydroWidth);
  hydroPi0Cen->Draw("c");
  hydroPimCen->Draw("c");

  hydroKCen->SetLineStyle(hydroLine);
  hydroKCen->SetLineColor(4);
  hydroKCen->SetLineWidth(hydroWidth);
  hydroKCen->Draw("csame");

//   hydroPnonfd_Cen->GetXaxis()->SetRangeUser(ptmin,ptmax-0.5);
//   hydroPnonfd_Cen->SetLineStyle(hydroLine);
//   hydroPnonfd_Cen->SetLineColor(1);
//   hydroPnonfd_Cen->SetLineWidth(hydroWidth);
//   hydroPnonfd_Cen->Draw("csame");

  hydroPCen->GetXaxis()->SetRangeUser(ptmin,ptmax-0.5);
  hydroPCen->SetLineStyle(hydroLine);
  hydroPCen->SetLineColor(1);  
  hydroPCen->SetLineWidth(hydroWidth);
  hydroPCen->Draw("csame");

  //=============================================================================
  // NLO pQCD 

  int nloLine = 3;
  int nloWidth = 2;

  pQCDPiCen->SetLineStyle(nloLine);
  pQCDPiCen->SetLineColor(2);
  pQCDPiCen->SetLineWidth(nloWidth);
  pQCDPiCen->Draw("l");

  pQCDKCen->SetLineStyle(nloLine);
  pQCDKCen->SetLineColor(4);
  pQCDKCen->SetLineWidth(nloWidth);
  pQCDKCen->Draw("l");

  pQCDPCen->SetLineStyle(nloLine);
  pQCDPCen->SetLineColor(1);
  pQCDPCen->SetLineWidth(nloWidth);
  pQCDPCen->Draw("l");

  int totLine = 1;
  int totWidth = 3;

  pQCDplusHydroPiCen->SetLineStyle(totLine);
  pQCDplusHydroPiCen->SetLineColor(2);
  pQCDplusHydroPiCen->SetLineWidth(totWidth);
  pQCDplusHydroPiCen->Draw("l");

  pQCDplusHydroKCen->SetLineStyle(totLine);
  pQCDplusHydroKCen->SetLineColor(4);
  pQCDplusHydroKCen->SetLineWidth(totWidth);
  pQCDplusHydroKCen->Draw("l");

  pQCDplusHydroPCen->SetLineStyle(totLine);
  pQCDplusHydroPCen->SetLineColor(1);
  pQCDplusHydroPCen->SetLineWidth(totWidth);
  pQCDplusHydroPCen->Draw("l");

  //=============================================================================
  // DATA
  //=============================================================================

  int colkaons = 4;
  int colprotons = 93;

  //=============================================================================
  // PHENIX 

  phenixPimCen->SetMarkerStyle(20);
  phenixPimCen->SetMarkerColor(2);
  phenixPimCen->SetMarkerSize(1.2);
  phenixPimCen->Draw("p");

  phenixPi0Cen->SetMarkerStyle(20);
  phenixPi0Cen->SetMarkerColor(6);//(2);
  phenixPi0Cen->SetMarkerSize(1.2);
  phenixPi0Cen->Draw("p");

  phenixKmCen->SetMarkerStyle(21);
  phenixKmCen->SetMarkerColor(colkaons);
  phenixKmCen->SetMarkerSize(1.);
  phenixKmCen->Draw("p");

  phenixPCen->SetMarkerStyle(22);
  phenixPCen->SetMarkerColor(colprotons);
  phenixPCen->SetMarkerSize(1.3);
  phenixPCen->Draw("p");

//   phenixP_nonfd_Cen->SetMarkerStyle(22);
//   phenixP_nonfd_Cen->SetMarkerColor(30);
//   phenixP_nonfd_Cen->SetMarkerSize(1.2);
//   phenixP_nonfd_Cen->Draw("p");

  //=============================================================================
  // STAR is for 5-15% !!

  starPimCen->SetMarkerStyle(24); 
  starPimCen->SetMarkerColor(2);
  starPimCen->SetMarkerSize(1.2);
  starPimCen->Draw("p");
 
  starKplCen->SetMarkerStyle(30);
  starKplCen->SetMarkerColor(colkaons);
  starKplCen->SetMarkerSize(1.2);
  starKplCen->Draw("p");

  starK0sCen->SetMarkerStyle(29);
  starK0sCen->SetMarkerColor(66);
  starK0sCen->SetMarkerSize(1.6);
  starK0sCen->Draw("p");

  starPCen->SetMarkerStyle(30);
  starPCen->SetMarkerColor(colprotons);
  starPCen->SetMarkerSize(1.2);
  starPCen->Draw("p");

//   starP_nonfd_Cen->SetMarkerStyle(26);
//   starP_nonfd_Cen->SetMarkerColor(3);
//   starP_nonfd_Cen->SetMarkerSize(1.2);
//   starP_nonfd_Cen->Draw("p");

  //=============================================================================
  // BRAHMS is for 0-5% !!

  brahmsPiminusCen->SetMarkerStyle(27);
  brahmsPiminusCen->SetMarkerColor(98);
  brahmsPiminusCen->SetMarkerSize(1.2);
  brahmsPiminusCen->Draw("p");

  brahmsKminusCen->SetMarkerStyle(26);
  brahmsKminusCen->SetMarkerColor(colkaons);
  brahmsKminusCen->SetMarkerSize(1.2);
  brahmsKminusCen->Draw("p");

  // BRAHMS protons are for 0-10%

  brahmsProtonCen->SetMarkerStyle(26);
  brahmsProtonCen->SetMarkerColor(colprotons);
  brahmsProtonCen->SetMarkerSize(1.3);
  brahmsProtonCen->Draw("p");

  //=============================================================================
  // PHOBOS is for 0-15% !!

  phobosPiCen->SetMarkerStyle(28);
  phobosPiCen->SetMarkerColor(2);
  phobosPiCen->SetMarkerSize(1.5);
  phobosPiCen->Draw("p");

  phobosKCen->SetMarkerStyle(28);
  phobosKCen->SetMarkerColor(51);
  phobosKCen->SetMarkerSize(1.5);
  phobosKCen->Draw("p");

  phobosPCen->SetMarkerStyle(28);
  phobosPCen->SetMarkerColor(colprotons);
  phobosPCen->SetMarkerSize(1.5);
  phobosPCen->Draw("p");

//   phobosP_nonfd_Cen->SetMarkerStyle(28);
//   phobosP_nonfd_Cen->SetMarkerColor(8);
//   phobosP_nonfd_Cen->SetMarkerSize(1.5);
//   phobosP_nonfd_Cen->Draw("p");

  //____________________________________________________________________________
  // Legends

  char label[300];
  TLegend * l = new TLegend(0.306502,0.773006,0.647059,0.98773,"Au+Au (central) calculations:","brNDC");
  l->SetFillColor(0);
  sprintf(label,"Hydro #pi^{-} [%s]",CentClass[icenB].Data()); 
  l->AddEntry(hydroPimCen,label,"l");
  sprintf(label,"Hydro K^{-} [%s]  #times10^{%i}",CentClass[icenB].Data(),kScaleExp); 
  l->AddEntry(hydroKCen,label,"l");
  //sprintf(label,"Hydro p [%s] no #Lambda feed, #times10^{%i}",CentClass[icenB].Data(),pScaleExp);  
  //l->AddEntry(hydroPnonfd_Cen,label,"l");
  sprintf(label,"Hydro p [%s] #times10^{%i}",CentClass[icenB].Data(),pScaleExp);
  l->AddEntry(hydroPCen,label,"l");

  sprintf(label,"[NLO pQCD] #times T_{AA}[%s]*0.2, #pi^{0}",CentClass[icenB].Data()); 
  l->AddEntry(pQCDPiCen,label,"l");
  sprintf(label,"[NLO pQCD] #times T_{AA}[%s]*0.2, K^{-} #times 10^{%i}",CentClass[icenB].Data(),kScaleExp); 
  l->AddEntry(pQCDKCen,label,"l");
  sprintf(label,"[NLO pQCD] #times T_{AA}[%s]*0.2, p #times 10^{%i}",CentClass[icenB].Data(),pScaleExp); 
  l->AddEntry(pQCDPCen,label,"l");
  l->Draw();

  TLegend * l2 = new TLegend(0.664087,0.659509,0.986068,0.98773,"Au+Au (central) data:","brNDC");
  l2->SetFillColor(0);
  sprintf(label,"PHENIX #pi^{-} [%s%%]",centPiCen); 
  l2->AddEntry(phenixPimCen,label,"p");
  sprintf(label,"PHENIX #pi^{0} [%s%%]",centPi0Cen); 
  l2->AddEntry(phenixPi0Cen,label,"p");
  sprintf(label,"STAR #pi^{+} [5-10%]"); 
  l2->AddEntry(starPimCen,label,"p");
  sprintf(label,"PHOBOS #pi [0-15%]"); 
  l2->AddEntry(phobosPiCen,label,"p");
  sprintf(label,"BRAHMS #pi^{-} [0-5%]");
  l2->AddEntry(brahmsPiminusCen,label,"p");

  sprintf(label,"PHENIX K^{+} [%s%%] #times 10^{%i}",centKCen,kScaleExp); 
  l2->AddEntry(phenixKmCen,label,"p");
  sprintf(label,"STAR K^{+} [5-10%] #times 10^{%i}",kScaleExp); 
  l2->AddEntry(starKplCen,label,"p");
  sprintf(label,"STAR K^{0}_{s} [0-5%] #times 10^{%i}",kScaleExp); 
  l2->AddEntry(starK0sCen,label,"p");
  sprintf(label,"PHOBOS K [0-15%] #times 10^{%i}",kScaleExp); 
  l2->AddEntry(phobosKCen,label,"p");
  sprintf(label,"BRAHMS K^{-} [0-5%] #times 10^{%i}",kScaleExp); 
  l2->AddEntry(brahmsKminusCen,label,"p");

  sprintf(label,"PHENIX p [%s%%] #times 10^{%i}",centPCen,pScaleExp); 
  l2->AddEntry(phenixPCen,label,"p");
  //sprintf(label,"PHENIX p non-fd [%s] #times 10^{%i}",centP_nonfd_Cen,pScaleExp); 
  //l2->AddEntry(phenixP_nonfd_Cen,label,"p");
  sprintf(label,"STAR p [0-5%] #times 10^{%i}",pScaleExp); 
  l2->AddEntry(starPCen,label,"p");
  //sprintf(label,"STAR p non-fd [0-5%] #times 10^{%i}",pScaleExp); 
  //l2->AddEntry(starP_nonfd_Cen,label,"p");
  sprintf(label,"PHOBOS p [0-15%] #times 10^{%i}",pScaleExp); 
  l2->AddEntry(phobosPCen,label,"p");
  //sprintf(label,"PHOBOS p non-fd [0-15%] #times 10^{%i}",pScaleExp); 
  //l2->AddEntry(phobosP_nonfd_Cen,label,"p");
  sprintf(label,"BRAHMS p [0-10%] #times 10^{%i}",pScaleExp); 
  l2->AddEntry(brahmsProtonCen,label,"p");
	    
  l2->Draw();

  //____________________________________________________________________________
  // Plot PION RATIOS Data/Model (CENTRAL)

  gSystem->Load("libemcAnalyzer");
  char *title="ratio_pi_data_model_AuAu_cent";
  int xbins=100; 
  int ybins=100; double ymin=0., ymax=2.;

  char *xtitle="p_{T} (GeV/#font[72]{c})";
  //char *ytitle="ratio #gamma_{tot} /#gamma_{pQCD}";
  char *ytitle="Data/[Hydro+pQCD]";
  
  TCanvas *c1 = new TCanvas(title,title,40,106,800,490);
  c1->Range(-0.761691,-0.319783,7.1223,2.07588);
  c1->SetLeftMargin(0.11111);
  c1->SetRightMargin(0.205772);
  c1->SetTopMargin(0.0316742);
  c1->SetBottomMargin(0.1334);
  //c1->SetLogx();

  TH2F *myframe = (TH2F*)emcAnalyzer::frame(title, xbins, ptmin, ptmax, ybins, ymin, ymax, xtitle, ytitle);
  myframe->Draw();
  myframe->GetYaxis()->SetTitleOffset(1.0);
  c1->Update();
  
  TLine *line = new TLine(ptmin,1,ptmax,1);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();

  TGraphErrors *ratio_phenixPimCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(phenixPimCen,pQCDplusHydroPiCenFit,0,1);
  c1->cd();
  ratio_phenixPimCen_model_cent->SetMarkerStyle(20);
  ratio_phenixPimCen_model_cent->SetMarkerColor(2);
  ratio_phenixPimCen_model_cent->SetMarkerSize(1.2);
  ratio_phenixPimCen_model_cent->Draw("P");

  TGraphErrors *ratio_phenixPi0Cen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(phenixPi0Cen,pQCDplusHydroPiCenFit,0,1);
  c1->cd();
  ratio_phenixPi0Cen_model_cent->SetMarkerStyle(20);
  ratio_phenixPi0Cen_model_cent->SetMarkerColor(6);//(2);
  ratio_phenixPi0Cen_model_cent->SetMarkerSize(1.2);
  ratio_phenixPi0Cen_model_cent->Draw("P");

  TGraphErrors *ratio_starPimCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(starPimCen,pQCDplusHydroPiCenFit,0,1);
  c1->cd();
  ratio_starPimCen_model_cent->SetMarkerStyle(24); 
  ratio_starPimCen_model_cent->SetMarkerColor(2);
  ratio_starPimCen_model_cent->SetMarkerSize(1.2);
  ratio_starPimCen_model_cent->Draw("P");

  TGraphErrors *ratio_phobosPiCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(phobosPiCen,pQCDplusHydroPiCenFit,0,1);
  c1->cd();
  ratio_phobosPiCen_model_cent->SetMarkerStyle(28);
  ratio_phobosPiCen_model_cent->SetMarkerColor(2);
  ratio_phobosPiCen_model_cent->SetMarkerSize(1.5);
  ratio_phobosPiCen_model_cent->Draw("P");

  TGraphErrors *ratio_brahmsPiminusCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(brahmsPiminusCen,pQCDplusHydroPiCenFit,0,1);
  c1->cd();
  ratio_brahmsPiminusCen_model_cent->SetMarkerStyle(27);
  ratio_brahmsPiminusCen_model_cent->SetMarkerColor(98);
  ratio_brahmsPiminusCen_model_cent->SetMarkerSize(1.8);
  ratio_brahmsPiminusCen_model_cent->Draw("P");

  TLegend *r1 = new TLegend(0.797992,0.140271,0.994981,0.970588,"Au+Au (central) data:","brNDC");
  sprintf(label,"PHENIX #pi^{-} [%s%%]",centPiCen); 
  r1->AddEntry(ratio_phenixPimCen_model_cent,label,"p");
  sprintf(label,"PHENIX #pi^{0} [%s%%]",centPi0Cen); 
  r1->AddEntry(ratio_phenixPi0Cen_model_cent,label,"p");
  sprintf(label,"STAR #pi^{+} [5-10%]"); 
  r1->AddEntry(ratio_starPimCen_model_cent,label,"p");
  sprintf(label,"PHOBOS #pi [0-15%]"); 
  r1->AddEntry(ratio_phobosPiCen_model_cent,label,"p");
  sprintf(label,"BRAHMS #pi^{-} [0-5%]");
  r1->AddEntry(ratio_brahmsPiminusCen_model_cent,label,"p");
  r1->SetFillColor(0);
  c1->cd();
  r1->Draw();

  //____________________________________________________________________________
  // Plot KAON RATIOS Data/Model (CENTRAL)

  sprintf(title,"ratio_kaon_data_model_AuAu_cent");
  TCanvas *c2 = new TCanvas(title,title,36,84,697,503);
  c2->Range(-1.77595,-1.01385,14.2076,1.374);
  c2->SetLeftMargin(0.11111);
  c2->SetRightMargin(0.012987);
  c2->SetTopMargin(0.03516);
  c2->SetBottomMargin(0.1318);
  //c2->SetLogx();

  myframe->Draw();
  line->Draw();
  c2->Update();

  TGraphErrors *ratio_phenixKmCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(phenixKmCen,pQCDplusHydroKCenFit,0,1);
  ratio_phenixKmCen_model_cent->SetMarkerStyle(21);
  ratio_phenixKmCen_model_cent->SetMarkerColor(colkaons);
  ratio_phenixKmCen_model_cent->SetMarkerSize(1.);
  c2->cd(); ratio_phenixKmCen_model_cent->Draw("P");
  c1->cd(); ratio_phenixKmCen_model_cent->Draw("P");
  TGraphErrors *ratio_starKplCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(starKplCen,pQCDplusHydroKCenFit,0,1);
  ratio_starKplCen_model_cent->SetMarkerStyle(30);
  ratio_starKplCen_model_cent->SetMarkerColor(colkaons);
  ratio_starKplCen_model_cent->SetMarkerSize(1.2);
  c2->cd(); ratio_starKplCen_model_cent->Draw("P");
  c1->cd(); ratio_starKplCen_model_cent->Draw("P");
  TGraphErrors *ratio_starK0sCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(starK0sCen,pQCDplusHydroKCenFit,0,1);
  ratio_starK0sCen_model_cent->SetMarkerStyle(29);
  ratio_starK0sCen_model_cent->SetMarkerColor(66);
  ratio_starK0sCen_model_cent->SetMarkerSize(1.6);
  c2->cd(); ratio_starK0sCen_model_cent->Draw("P");
  c1->cd(); ratio_starK0sCen_model_cent->Draw("P");
  TGraphErrors *ratio_phobosKCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(phobosKCen,pQCDplusHydroKCenFit,0,1);
  ratio_phobosKCen_model_cent->SetMarkerStyle(28);
  ratio_phobosKCen_model_cent->SetMarkerColor(51);
  ratio_phobosKCen_model_cent->SetMarkerSize(1.5);
  c2->cd(); ratio_phobosKCen_model_cent->Draw("P");
  c1->cd(); ratio_phobosKCen_model_cent->Draw("P");
  TGraphErrors *ratio_brahmsKminusCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(brahmsKminusCen,pQCDplusHydroKCenFit,0,1);
  ratio_brahmsKminusCen_model_cent->SetMarkerStyle(26);
  ratio_brahmsKminusCen_model_cent->SetMarkerColor(colkaons);
  ratio_brahmsKminusCen_model_cent->SetMarkerSize(1.2);
  c2->cd(); ratio_brahmsKminusCen_model_cent->Draw("P");
  c1->cd(); ratio_brahmsKminusCen_model_cent->Draw("P");

  TLegend *r2 = new TLegend(0.664087,0.659509,0.986068,0.98773,"Au+Au (central) data:","brNDC");
  sprintf(label,"PHENIX K^{+} [%s%%]",centKCen); 
  r1->AddEntry(ratio_phenixKmCen_model_cent,label,"p");
  r2->AddEntry(ratio_phenixKmCen_model_cent,label,"p");
  sprintf(label,"STAR K^{+} [5-10%]"); 
  r1->AddEntry(ratio_starKplCen_model_cent,label,"p");
  r2->AddEntry(ratio_starKplCen_model_cent,label,"p");
  sprintf(label,"STAR K^{0}_{s} [0-5%]"); 
  r1->AddEntry(ratio_starK0sCen_model_cent,label,"p");
  r2->AddEntry(ratio_starK0sCen_model_cent,label,"p");
  sprintf(label,"PHOBOS K [0-15%]"); 
  r1->AddEntry(ratio_phobosKCen_model_cent,label,"p");
  r2->AddEntry(ratio_phobosKCen_model_cent,label,"p");
  sprintf(label,"BRAHMS K^{-} [0-5%]"); 
  r1->AddEntry(ratio_brahmsKminusCen_model_cent,label,"p");
  r2->AddEntry(ratio_brahmsKminusCen_model_cent,label,"p");
  r2->SetFillColor(0);
  c2->cd();
  r2->Draw();

  //____________________________________________________________________________
  // Plot PROTON RATIOS Data/Model (CENTRAL)

  sprintf(title,"ratio_proton_data_model_AuAu_cent");
  TCanvas *c3 = new TCanvas(title,title,36,84,697,503);
  c3->Range(-1.77595,-1.01385,14.2076,1.374);
  c3->SetLeftMargin(0.11111);
  c3->SetRightMargin(0.012987);
  c3->SetTopMargin(0.03516);
  c3->SetBottomMargin(0.1318);
  //c3->SetLogx();

  myframe->Draw();
  line->Draw();
  c3->Update();

  TGraphErrors *ratio_phenixPCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(phenixPCen,pQCDplusHydroPCenFit,0,1);
  ratio_phenixPCen_model_cent->SetMarkerStyle(22);
  ratio_phenixPCen_model_cent->SetMarkerColor(colprotons);
  ratio_phenixPCen_model_cent->SetMarkerSize(1.3);
  c3->cd(); ratio_phenixPCen_model_cent->Draw("P");
  c1->cd(); ratio_phenixPCen_model_cent->Draw("P");
  TGraphErrors *ratio_starPCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(starPCen,pQCDplusHydroPCenFit,0,1);
  ratio_starPCen_model_cent->SetMarkerStyle(30);
  ratio_starPCen_model_cent->SetMarkerColor(colprotons);
  ratio_starPCen_model_cent->SetMarkerSize(1.2);
  c3->cd(); ratio_starPCen_model_cent->Draw("P");
  c1->cd(); ratio_starPCen_model_cent->Draw("P");
  TGraphErrors *ratio_phobosPCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(phobosPCen,pQCDplusHydroPCenFit,0,1);
  ratio_phobosPCen_model_cent->SetMarkerStyle(26);
  ratio_phobosPCen_model_cent->SetMarkerColor(colprotons);
  ratio_phobosPCen_model_cent->SetMarkerSize(1.3);
  c3->cd(); ratio_phobosPCen_model_cent->Draw("P");
  c1->cd(); ratio_phobosPCen_model_cent->Draw("P");
  TGraphErrors *ratio_brahmsProtonCen_model_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(brahmsProtonCen,pQCDplusHydroPCenFit,0,1);
  ratio_brahmsProtonCen_model_cent->SetMarkerStyle(26);
  ratio_brahmsProtonCen_model_cent->SetMarkerColor(colprotons);
  ratio_brahmsProtonCen_model_cent->SetMarkerSize(1.3);
  c3->cd(); ratio_brahmsProtonCen_model_cent->Draw("P");
  c1->cd(); ratio_brahmsProtonCen_model_cent->Draw("P");

  sprintf(label,"PHENIX p [%s%%]",centPCen); 
  r1->AddEntry(ratio_phenixPCen_model_cent,label,"p");
  sprintf(label,"STAR p [0-5%]"); 
  r1->AddEntry(ratio_starPCen_model_cent,label,"p");
  sprintf(label,"PHOBOS p [0-15%]"); 
  r1->AddEntry(ratio_phobosPCen_model_cent,label,"p");
  sprintf(label,"BRAHMS p [0-10%]"); 
  r1->AddEntry(ratio_brahmsProtonCen_model_cent,label,"p");

  return;

  //____________________________________________________________________________
  // Plot PERIPHERAL spectra

  TCanvas * cPer = new TCanvas("hadron_spec_AuAu200GeV_periph","Hadron spectrum Au+Au 200 GeV peripheral",650,700);
  cPer->Range(-0.830387,-7.30357,5.12994,4.33929);
  cPer->SetLeftMargin(0.139319);
  cPer->SetRightMargin(0.0201238);
  cPer->SetTopMargin(0.0291411);
  cPer->SetBottomMargin(0.111963);
  cPer->SetLogy(1);
  cPer->SetFillColor(0);
  cPer->SetFillStyle(0);

  ymax *= 0.1;
  ymin *= 0.1;
  
  hydroPimPer->GetXaxis()->SetRangeUser(ptmin,ptmax);
  hydroPimPer->SetMinimum(ymin);
  hydroPimPer->SetMaximum(ymax);
  hydroPimPer->SetXTitle("p_{T} (GeV/c)");
  hydroPimPer->GetXaxis()->SetTitleOffset(1.);
  hydroPimPer->GetXaxis()->SetTitleSize(0.05);
  hydroPimPer->SetYTitle("d^{2}N/(#pi dp_{T}^{2}dy (GeV/c)^{-2}");
  hydroPimPer->GetYaxis()->SetTitleOffset(1.2);
  hydroPimPer->GetYaxis()->SetTitleSize(0.05);
  hydroPimPer->SetTitle("");
  hydroPimPer->SetStats(0);

  hydroPimPer->SetLineStyle(hydroLine);
  hydroPimPer->SetLineColor(2);
  hydroPimPer->SetLineWidth(hydroWidth);
  hydroPimPer->Draw("c");

  hydroKPer->GetXaxis()->SetRangeUser(0,4.5);
  hydroKPer->SetLineStyle(hydroLine);
  hydroKPer->SetLineColor(colkaons);
  hydroKPer->SetLineWidth(hydroWidth);
  hydroKPer->Draw("csame");

//   hydroPnonfd_Per->GetXaxis()->SetRangeUser(ptmin,ptmax-2);
//   hydroPnonfd_Per->SetLineStyle(hydroLine);
//   hydroPnonfd_Per->SetLineColor(1);
//   hydroPnonfd_Per->SetLineWidth(hydroWidth);
//   hydroPnonfd_Per->Draw("csame");

  hydroPPer->GetXaxis()->SetRangeUser(ptmin,ptmax-1);
  hydroPPer->SetLineStyle(hydroLine);
  hydroPPer->SetLineColor(1);
  hydroPPer->SetLineWidth(hydroWidth);
  hydroPPer->Draw("csame");

  //____________________________________________________________________________
  // Peripheral NLO pQCD

  pQCDPiPer->SetLineStyle(nloLine);
  pQCDPiPer->SetLineColor(2);
  pQCDPiPer->SetLineWidth(nloWidth);
  pQCDPiPer->Draw("l");

  pQCDKPer->SetLineStyle(nloLine);
  pQCDKPer->SetLineColor(colkaons);
  pQCDKPer->SetLineWidth(nloWidth);
  pQCDKPer->Draw("l");

  pQCDPPer->SetLineStyle(nloLine);
  pQCDPPer->SetLineColor(1);
  pQCDPPer->SetLineWidth(nloWidth);
  pQCDPPer->Draw("l");

  pQCDplusHydroPiPer->SetLineStyle(totLine);
  pQCDplusHydroPiPer->SetLineColor(2);
  pQCDplusHydroPiPer->SetLineWidth(totWidth);
  pQCDplusHydroPiPer->Draw("l");

  pQCDplusHydroKPer->SetLineStyle(totLine);
  pQCDplusHydroKPer->SetLineColor(colkaons);
  pQCDplusHydroKPer->SetLineWidth(totWidth);
  pQCDplusHydroKPer->Draw("l");

  pQCDplusHydroPPer->SetLineStyle(totLine);
  pQCDplusHydroPPer->SetLineColor(1);
  pQCDplusHydroPPer->SetLineWidth(totWidth);
  pQCDplusHydroPPer->Draw("l");

  //____________________________________________________________________________
  // Peripheral PHENIX

  phenixPimPer->SetMarkerStyle(20);
  phenixPimPer->SetMarkerColor(2);
  phenixPimPer->SetMarkerSize(1.2);
  phenixPimPer->Draw("p");

  phenixPi0Per->SetMarkerStyle(20);
  phenixPi0Per->SetMarkerColor(6); //(2);
  phenixPi0Per->SetMarkerSize(1.2);
  phenixPi0Per->Draw("p");

  phenixKmPer->SetMarkerStyle(21);
  phenixKmPer->SetMarkerColor(colkaons);
  phenixKmPer->SetMarkerSize(1.2);
  phenixKmPer->Draw("p");

  phenixPPer->SetMarkerStyle(22);
  phenixPPer->SetMarkerColor(colprotons);
  phenixPPer->SetMarkerSize(1.3);
  phenixPPer->Draw("p");

//   phenixP_nonfd_Per->SetMarkerStyle(22);
//   phenixP_nonfd_Per->SetMarkerColor(30);
//   phenixP_nonfd_Per->SetMarkerSize(1.2);
//   phenixP_nonfd_Per->Draw("p");

  //____________________________________________________________________________
  // Peripheral STAR

  starPimPer->SetMarkerStyle(24);
  starPimPer->SetMarkerColor(2);
  starPimPer->SetMarkerSize(1.2);
  starPimPer->Draw("p");

  starKplPer->SetMarkerStyle(30);
  starKplPer->SetMarkerColor(colkaons);
  starKplPer->SetMarkerSize(1.2);
  starKplPer->Draw("p");

  starK0sPer->SetMarkerStyle(29);
  starK0sPer->SetMarkerColor(66);
  starK0sPer->SetMarkerSize(1.6);
  starK0sPer->Draw("p");

  starPPer->SetMarkerStyle(30);
  starPPer->SetMarkerColor(colprotons);
  starPPer->SetMarkerSize(1.2);
  starPPer->Draw("p");

//   starP_nonfd_Per->SetMarkerStyle(26);
//   starP_nonfd_Per->SetMarkerColor(3);
//   starP_nonfd_Per->SetMarkerSize(1.2);
//   starP_nonfd_Per->Draw("p");

  //____________________________________________________________________________
  // Peripheral Legends

  TLegend * l3 = new TLegend(0.306502,0.773006,0.647059,0.98773,"Au+Au (peripheral) calculations:","brNDC");
  l3->SetFillColor(0);
  sprintf(label,"Hydro #pi^{-} [%s]",CentClass[iperB].Data()); 
  l3->AddEntry(hydroPimPer,label,"l");
  sprintf(label,"Hydro K^{-} [%s]  #times10^{%i}",CentClass[iperB].Data(),kScaleExp); 
  l3->AddEntry(hydroKPer,label,"l");
  //sprintf(label,"Hydro p non-fd [%s] #times10^{%i}",CentClass[iperB].Data(),pScaleExp);  
  //l3->AddEntry(hydroPnonfd_Per,label,"l");
  sprintf(label,"Hydro p [%s] #times10^{%i}",CentClass[iperB].Data(),pScaleExp);
  l3->AddEntry(hydroPPer,label,"l");

  sprintf(label,"[NLO pQCD] #times T_{AA}[%s], #pi^{0}",CentClass[iperB].Data()); 
  l3->AddEntry(pQCDPiPer,label,"l");
  sprintf(label,"[NLO pQCD] #times T_{AA}[%s], K^{-} #times 10^{%i}",CentClass[iperB].Data(),kScaleExp); 
  l3->AddEntry(pQCDKPer,label,"l");
  sprintf(label,"[NLO pQCD] #times T_{AA}[%s], p #times 10^{%i}",CentClass[iperB].Data(),pScaleExp); 
  l3->AddEntry(pQCDPPer,label,"l");
  l3->Draw();

  TLegend * l4 = new TLegend(0.664087,0.659509,0.986068,0.98773,"Au+Au (peripheral) data:","brNDC");
  l4->SetFillColor(0);
  sprintf(label,"PHENIX #pi^{-} [%s%%]",centPiPer); 
  l4->AddEntry(phenixPimPer,label,"p");
  sprintf(label,"PHENIX #pi^{0} [%s%%]",centPi0Per); 
  l4->AddEntry(phenixPi0Per,label,"p");
  sprintf(label,"STAR #pi^{+} [60-70%]"); 
  l4->AddEntry(starPimPer,label,"p");

  sprintf(label,"PHENIX K^{+} [%s%%] #times 10^{%i}",centKPer,kScaleExp); 
  l4->AddEntry(phenixKmPer,label,"p");
  sprintf(label,"STAR K^{+} [60-70%] #times 10^{%i}",kScaleExp); 
  l4->AddEntry(starKplPer,label,"p");
  sprintf(label,"STAR K^{0}_{s} [60-80%] #times 10^{%i}",kScaleExp); 
  l4->AddEntry(starK0sPer,label,"p");

  sprintf(label,"PHENIX p [%s%%] #times 10^{%i}",centPPer,pScaleExp); 
  l4->AddEntry(phenixPPer,label,"p");
  //sprintf(label,"PHENIX p non-fd [%s] #times 10^{%i}",centPPer,pScaleExp); 
  //l4->AddEntry(phenixP_nonfd_Per,label,"p");
  sprintf(label,"STAR p [60-70%] #times 10^{%i}",pScaleExp); 
  l4->AddEntry(starPPer,label,"p");
  //sprintf(label,"STAR p [60-70%] #times 10^{%i}",pScaleExp); 
  //l4->AddEntry(starP_nonfd_Per,label,"p");
  l4->Draw();

}

