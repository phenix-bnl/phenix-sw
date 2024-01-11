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

void makeFigs3_4_old(char* inasciifile = "sumvarvol_qgp_tau015_v00_Et.dat" , 
		     bool useEntropy = true,
		     bool useTeffoverTmax = false)

  //(char* inasciifile = "sumvarvol_tau015_v00.dat" )
  //(char* inasciifile = "sumvarvol_30GeV_tau06_v023.dat" ) 
  //(char* inasciifile = "sumvarvol_tau06_v025.dat" )
{

  cout << "Reading " << inasciifile << " ... " << endl ;

  TString file_str = inasciifile;
  
  bool QGP_EoS = true;
  if (file_str.Contains("qgp",TString::kIgnoreCase)) QGP_EoS = true;
  else if (file_str.Contains("had",TString::kIgnoreCase)) QGP_EoS = false ;

  //____________________________________________________________________________
  // Read hydro data file

  const int Ncent = 100;
  const int Nslopes = 10;

  double E0[Ncent];
  double E0_av[Ncent];
  double T0[Ncent];
  double T0_av[Ncent];
  double S0[Ncent];
  double S0_av[Ncent];
  double Vol[Ncent];
  double dNdy[Ncent];
  double E_T[Ncent];

  double E_TV[Ncent];
  double errT0[Ncent];
  double errET[Ncent];

  double slopeV[Nslopes][Ncent];
  double slopeErrV[Nslopes][Ncent];

  int chiMinPiV[11];
  int chiMinKV[11];
  int chiMinPrV[11];

  double T0_norm[Ncent];

  const double hc = 0.197; // GeV fm
  const double hc3 = TMath::Power(hc,3.); // GeV/fm^2 --> GeV^4, fm^-3 --> GeV^3
  const double B =  0.379 ; // GeV/fm^3 

  double impParam9[9]={ 3.2, 5.7, 7.4, 8.7, 9.9, 11.0, 11.9, 12.8, 14.0}; // 0-10%,10-20%, ... 80-90%
  double T0sel[9]; // T0 for 0-10%, 10-20%, ... 80-90% centralities
  int isel = 1;

  ifstream in(inasciifile);
  int k;

   // icen - step in impact parameter (100 steps up to 14 fm)
  for(int icen=0; icen<Ncent; icen++)
    {
      // old format
      //in >> k >>  E0[icen] >> T0[icen] >> E_T[icen] >> E_TV[icen];

      // new format
      // E0, T0, S0 - initial MAXIMAL energy density, temperature and entropy densities, 
      // xx_Av - averaged values over transv. area, 
      // dNdy - estimated multiplicity of massless particles (a la Bjorken), 
      // E_T - transverse energy. 

      in >> k >> E0[icen] >> E0_av[icen] >> T0[icen] >> T0_av[icen] 
	 >> S0[icen] >> S0_av[icen] >> Vol[icen] >> dNdy[icen] >> E_T[icen] ;

      cout << " " << k << " " << E0[icen] << " " << E0_av[icen] << " " << T0[icen] << " " << T0_av[icen] 
           << " " << S0[icen] << " " << S0_av[icen] << " " << Vol[icen] << " " << dNdy[icen] << " " << E_T[icen] << endl ;;

      // in the same file line 10 effective slopes with errors.
      for(int is=0; is<Nslopes; is++){ 
	in >> slopeV[is][icen] >> slopeErrV[is][icen]; 	
	//cout << " " << slopeV[is][icen] << " " << slopeErrV[is][icen]; 
      }
      //cout << endl;

      // subtract Bag-constant energy
      E0[icen]-=B;
      S0[icen]-=B;

      // Normalized T0 = T0/T0max
      T0_norm[icen]=T0[icen]/T0[0];

      errT0[icen]=0;
      errET[icen]=0;

      if ( (14./100.*icen) >= impParam9[isel-1] && 
	   (14./100.*icen) <= impParam9[isel] )
	{
	  T0sel[isel-1] = T0[icen-1];
	  isel++;
	  cout << "--------> isel:" << icen-1 << " " << (14./100.*(icen-1))  << endl;
	}
    }

  T0sel[isel-1] = T0[icen-1];
  cout << "--------> isel:" << icen-1 << " " << (14./100.*(icen-1))  << endl;

  // last lines of file are for chi-square values ...
  for(int cen=0; cen<11; cen++){
    cout << "Cen " << cen << " " << k << endl ;
    in >> k >> chiMinPiV[k] >> chiMinKV[k] >> chiMinPrV[k];
  }
  in.close();

  //____________________________________________________________________________
  // E_T, dN/deta data and Npart versus b

  const double ET_phnx[20] = { 606, 493., //   0-10%
			       402, 328,  //  10-20%
			       266, 216.,  //  20-30%
			       173, 137,  //  30-40%
			       107,  82,  //  40-50%
			        60,  44,  //  50-60%
			        31,  21,  //  60-70% // below this are invented ...
			        17,14, //  70-80%
			        12,9, //  80-90%
			         5,2}; //  90-100%

  const double dNdeta_ch_phnx[20] = { 687, 560, //  0-10%
				      457, 372, // 10-20%
				      302, 246, // 20-30% 
				      197, 156, // 30-40%
				      124,  95.3, // 40-50%
				      70.9,  52.2, // 50-60%
				      37.5,  25.6, // 60-70% // below this are invented ...
				      12,9, //  70-80%
				      12,9, //  80-90%
				      5,2}; //  90-100%

  const double impParam[20] =  { 2.3,  4.1, //  0- 5%,  5-10%
				 5.2,  6.2, // 10-15%, 15-20%
				 7.0,  7.8, // 20-25%, 25-30%
				 8.4,  9.1, // 30-35%, 35-40%
				 9.7, 10.2, // 40-45%, 45-50% 
				 10.7, 11.2, // 50-55%, 55-60%
				 11.7, 12.1, // 60-65%, 65-70%
				 12.6, 13.0, // 70-75%, 75-80%
				 13.3, 13.7, // 80-85%, 85-90%
				 13.9, 14};// 90-95%, 95-100% // real distrib. goes higher ~15 fm. we chop it at 14 fm
  
  const double cc[20] = {  3,  7,  //   0-10%
			   13, 17,  //  10-20%
			   23, 27,  //  20-30%
			   33, 37,  //  30-40%
			   43, 47,  //  40-50%
			   53, 57,  //  50-60%
			   63, 67,  //  60-70%
			   73, 77, //  70-80%
			   83, 87, //  80-90%
			   93, 97}; //  90-100%

  TGraph *tmp = new TGraph(20,impParam,ET_phnx) ;
  TF1 *et_vs_b_phnx = new TF1("et_vs_b_phnx","pol7",0.,14.);
  tmp->SetMinimum(1);
  tmp->Fit("et_vs_b_phnx","QEIM");
  TCanvas *c0 = new TCanvas("c0","c0",700,600);
  tmp->SetMarkerStyle(29);
  tmp->Draw("AP");
  et_vs_b_phnx->Draw("same");

  TGraph *tmp2 = new TGraph(20,impParam,dNdeta_ch_phnx) ;
  TF1 *dndeta_vs_b_phnx = new TF1("dndeta_vs_b_phnx","pol7",0.,14.);
  tmp2->SetMinimum(1);
  tmp2->Fit("dndeta_vs_b_phnx","QEIM");
  c0->cd();
  tmp2->SetMarkerStyle(29);
  tmp2->Draw("P");
  dndeta_vs_b_phnx->Draw("same");

  TGraph *tmp3 = new TGraph(20,impParam,cc) ;
  TF1 *centclass_vs_b = new TF1("centclass_vs_b","pol7",0.,14.);
  tmp3->SetMinimum(1);
  tmp3->Fit("centclass_vs_b","QEIM");
  c0->cd();
  tmp3->SetMarkerStyle(29);
  tmp3->Draw("P");
  centclass_vs_b->Draw("same");

  //double npart[12] = {2*197, 351.4, 299.0, 253.9, 215.3, 166.6, 114.2,  74.4, 45.5, 25.6, 13.4, 6.3};
  double npart[13] = {2*197, 351.4, 299.0, 253.9, 215.3, 166.6, 114.2,  74.4, 45.5, 25.6, 13.4, 6.3,2.};
  double enpart[13] = {3, 3, 4, 5, 5, 5, 4,  4, 4, 4, 3, 1, 1.};
  double impParam2[13]={0.,2.3,4.1,5.2,6.2,7.4,8.8,9.9,10.9,11.9,12.8,14.1,14.6};

  TGraph *Npartvsb = new TGraph(13,impParam2,npart);
  c0->cd();
  Npartvsb->Draw("*");
  Npartvsb->SetMarkerColor(2);
  Npartvsb->Fit("pol6","QEIM");
  TF1 *Npart_vs_b = (TF1*)Npartvsb->GetFunction("pol6");
  Npart_vs_b->SetName("Npart_vs_b");

//   TF1 *Npart_vs_b_0_9 = new TF1("Npart_vs_b_0_9","gaus",0.,9.);
//   Npart_vs_b_0_9->SetParameters(3.92731e+02,-2.84002e-01,5.85478);
//   TF1 *Npart_vs_b_9_15 = new TF1("Npart_vs_b_9_15","gaus",9.,15.);
//   Npart_vs_b_9_15->SetParameters(2.93297e+02,3.92958,3.60589);
//   TF1 *Npart_vs_b = new TF1("Npart_vs_b","Npart_vs_b_0_9*(x<=9)+Npart_vs_b_9_15*(x>9)",0.,15.);

  // transverse Area versus b(fm)
  TF1 *A_T_vs_b = new TF1("A_T_vs_b","gaus",0.,15.);
  //A_T_vs_b->SetParameters(14./100.*150.77,0.30867,7.1);
  A_T_vs_b->SetParameters(150.77,0.30867,7.1);
  c0->cd();
  A_T_vs_b->Draw("same");
  A_T_vs_b->SetLineColor(2);

  //____________________________________________________________________________
  // volume2 (Dmitri's code ...)

  Double_t x[100] ;
  for(Int_t i=0;i<100;i++)x[i]=1.*i ;
  TGraph * g = new TGraph(100,x,Vol) ;
  g->Fit("pol8","QEIM") ;
  TF1 * Vol2 = (TF1*)g->GetFunction("pol8");

  //____________________________________________________________________________
  // Reduced number of centralities: 100 --> 20
  // Reduced number of selected slopes: 1,3,5,7,9
  // j= 0  1      2      3      4      5      6      7      8      9  
  // 0-0.5 0.5-1. 1-1.5  1.5-2  2-2.5  2.5-3  3-3.5  3.5-4  4-4.5  4.5-5

  const int Ncent2 = 100;
  int step = (int)Ncent/Ncent2;  

  double T0bis[Ncent2];
  double T0bis_norm[Ncent2];
  double E_Tbis[Ncent2];
  double dNdybis[Ncent2];
  double S0bis[Ncent2];

  double errT0bis[Ncent2];
  double errE_Tbis[Ncent2];
  double errdNdybis[Ncent2];
  double errS0bis[Ncent2];

  const int Nslopes2 = 5;

  double slope[Nslopes2][Ncent2];
  double slope_norm[Nslopes2][Ncent2];
  double slopeErr[Nslopes2][Ncent2];

  // icen2 - step in impact parameter (20 steps up to 14 fm)
  int icen2 = 0;
  int is = 0;

  for(int icen=0; icen<Ncent; icen+=step)
    {
      T0bis[icen2]=T0[icen];
      T0bis_norm[icen2]=T0[icen]/T0[0]; // T0/T0max
      errT0bis[icen2]=errT0[icen];
	  
      E_Tbis[icen2]=E_T[icen];
      errE_Tbis[icen2]=errET[icen];	  

      S0bis[icen2]=S0[icen];
      errS0bis[icen2]=0.;

      dNdybis[icen2]=dNdy[icen];
      errdNdybis[icen2]=0.;

      for(int is2=0; is2<Nslopes2; is2++)
	{
	  is = 2*is2+1;
	  //cout << "Refilling slopes array: old(cent,slope) = (" << icen << "," << is << ") ---> "
	  //     << " new(cent,slope) = (" << icen2 << "," << is2 << ")" << endl;
	  slope[is2][icen2]=slopeV[is][icen];
	  slope_norm[is2][icen2]=slopeV[is][icen]/slopeV[is][0]; // T0/T0max
	  slopeErr[is2][icen2]=slopeErrV[is][icen];	  
	}
      icen2++;
    }

  //____________________________________________________________________________
  // T_eff vs T_0

  char title[100];
  sprintf(title,"Teff_vs_T0");
  char xtitle[100];
  sprintf(xtitle,"T_{0} (GeV)");
  char ytitle[100];
  sprintf(ytitle,"T_{eff} (GeV)");

  TCanvas *c1 = new TCanvas(title,title,700,500);
  c1->Range(0.0822335,-28.6458,0.612183,206.771);
  c1->SetLeftMargin(0.127874);
  c1->SetRightMargin(0.0229885);
  c1->SetTopMargin(0.0287611);
  c1->SetBottomMargin(0.121681);

  double Teff_min = 0.100, T0_min = 0.150;
  double Teff_max = 0.500, T0_max = 0.660;

  if (!QGP_EoS) { T0_min = 0.120; Teff_max = 0.290; T0_max = 0.290; }

  TH2F *myframe = (TH2F*)frame(title, 14,T0_min,T0_max, 14,Teff_min,Teff_max,xtitle,ytitle);

  TLegend *l = new TLegend(0.142,0.781,0.405,0.987,"Local thermal #gamma slopes:","brNDC");
  l->SetMargin(0.2);
  l->SetFillStyle(0);
  l->SetBorderSize(4);

  TGraphErrors *T0_vs_Teff[Nslopes2];

  int col[Nslopes2] = {4,7,91,6,96};

  for(int is2=0; is2<Nslopes2; is2++)
    {      
      T0_vs_Teff[is2] = new TGraphErrors(Ncent2,T0bis,slope[is2],errT0bis,slopeErr[is2]);
      T0_vs_Teff[is2] = new TGraphErrors(Ncent2,slope[is2],T0bis,slopeErr[is2],errT0bis);
      sprintf(title,"T0_vs_Teff%i",is2);
      cout << "plotting graph: " << title << endl;
      T0_vs_Teff[is2]->SetName(title); 
      //T0_vs_Teff[is2]->SetLineStyle(i); 
      T0_vs_Teff[is2]->SetMarkerStyle(28); 
      T0_vs_Teff[is2]->SetMarkerSize(1.6); 
      T0_vs_Teff[is2]->SetMarkerColor(col[is2]);
      T0_vs_Teff[is2]->SetLineWidth(3);
      T0_vs_Teff[is2]->SetLineColor(col[is2]);

      c1->cd();
      T0_vs_Teff[is2]->Draw("L"); //("PC");

      is = 2*is2+1;
  
      sprintf(title,"%3.1f < p_{T} < %3.1f GeV/c",is*0.5,(is+1)*0.5);
      l->AddEntry(T0_vs_Teff[is2],title,"l");
    }
  l->Draw();

  // Print centrality labels

  TText *t = new TText();
  t->SetTextSize(0.031);
  t->SetTextAngle(90);
  t->SetTextAlign(21);
  for(int cen = 0; cen<10; cen++){
    TString txt;
    txt+= " "; 
    txt+=10*cen; 
    if (cen==0) txt = "   0";
    txt +="-"; txt+=10*(cen+1); txt += " %";
    if (cen==9) txt = " >90 %";
    t->SetTextColor(1);
    t->SetTextAlign(12);
    t->DrawText(T0sel[cen],0.11,txt);
    cout << "----> plotting text at: " << cen  << " " << T0sel[cen] << endl;
    //t->DrawText(T0[int(100./14.*impParam[2*(cen-1)])],0.11,txt);
    //cout << "----> plotting text at: " << int(100./14.*impParam[2*(cen-1)]) 
    //<< " " << 100./14.*impParam[2*(cen-1)] << endl;
    //t->DrawText(T0[chiMinPiV[cen]],0.11,txt);
  }

  TText *t2 = new TText(0.2,0.13,"Au+Au centrality:");
  t2->SetTextSize(0.03);
  t2->SetTextAlign(21);
  t2->Draw();

  //return;

  //____________________________________________________________________________
  // Equation-of-State

  const double PI_2 = TMath::Pi()*TMath::Pi();
  double zetafactor_E = PI_2/30.; //zeta(2)/5
  double zetafactor_S = 4.*PI_2/90.;

  // Theoretical EoS

  double ndf0[Ncent];
  double ndf0_S[Ncent];

  for(int icen=0; icen<Ncent; icen++)
    {
      ndf0[icen]=E0[icen]*hc3/(zetafactor_E*TMath::Power(T0[icen],4.));
      ndf0_S[icen]=S0[icen]*hc3/(zetafactor_S*TMath::Power(T0[icen],3.));
    }

  // Computed EoS

  double ndf_E0_Teff[Nslopes2][Ncent2];
  double ndf_ET_Teff[Nslopes2][Ncent2];
  double ndf_ET_T0[Ncent2];

  double ndf_S0_Teff[Nslopes2][Ncent2];
  double ndf_dNdy_Teff[Nslopes2][Ncent2];
  double ndf_dNdy_T0[Ncent2];

  double Teff_reduced[Ncent2];

  double EBjork[Ncent2];

  const double tau0 = 0.15;// fm/c
  cout << "<I> Remember normalizing E_T's by tau0 = " << tau0 << " fm/c (only valid @ 200 GeV/c) ..." << endl;

  double Ntot_to_Nch = 1. ; //already dN_ch/deta  2./3.;
  double dy_to_deta = 1.; //already in data   /1.25.;

  TF1 *slopefit = new TF1("slopefit","pol1",0.,500.);
  slopefit->SetParameters(6.9436e-02,6.3735e-01);

  icen2 = 0;

  for(int icen=0; icen<Ncent; icen+=step)
    {

      // Energies normalized by Stefan-Boltzmann law: zeta(2)/5.*T^4
      // and by volume: V = [Glauber A_T]x[tau0]

      // Entropies normalized by Stefan-Boltzmann law: 12./45.*zeta(2)*T^3
      // and by volume: V = [Glauber A_T]x[tau0]
      // Note: dN/dy = rho*V = s/4*V
      
      double b = 14./100.*icen; // idem to: 14./20.*icen2
      double V_0 = A_T_vs_b->Eval(b)*tau0;//*pow(0.7,4.);
      //V_0 = Vol2->Eval(1.*icen)*tau0; 

      for(int is2=0; is2<Nslopes2; is2++)
	{
	  ndf_E0_Teff[is2][icen2] = E0[icen]*hc3/(zetafactor_E*TMath::Power(slope[is2][icen2],4.));
	  //ndf_ET_Teff[is2][icen2]  = (et_vs_b_phnx->Eval(b)/V_0)*hc3/(zetafactor_E*TMath::Power(slope[is2][icen2],4.));
	  ndf_ET_Teff[is2][icen2]  = (E_T[icen]/V_0)*hc3/(zetafactor_E*TMath::Power(slope[is2][icen2],4.));

	  double dndy = dNdy[icen]/V_0;
	  dndy = dndeta_vs_b_phnx->Eval(b)/(V_0*Ntot_to_Nch*dy_to_deta);

	  ndf_S0_Teff[is2][icen2] = S0[icen]*hc3/(zetafactor_S*TMath::Power(slope[is2][icen2],3.));
	  ndf_dNdy_Teff[is2][icen2]=(4.*dndy)*hc3/(zetafactor_S*TMath::Power(slope[is2][icen2],3.));
	}

      ndf_ET_T0[icen2] = (E_Tbis[icen2]/V_0)*hc3/(zetafactor_E*TMath::Power(T0bis[icen2],4.));
      ndf_dNdy_T0[icen2] = (4.*dNdybis[icen2]/V_0)*hc3/(zetafactor_S*TMath::Power(T0bis[icen2],3.));

      EBjork[icen2] = (E_Tbis[icen2]/V_0);

      icen2++; 
    }

  //____________________________________________________________________________
  // Plot "pure" (hydro) degsOfFreedom_ET_vs_T0

  TGraph *ndf0_vs_T0 = 0;
  if (useTeffoverTmax) ndf0_vs_T0 = new TGraph(Ncent,T0_norm,ndf0);
  else ndf0_vs_T0 = new TGraph(Ncent,T0,ndf0);
  ndf0_vs_T0->SetName("ndf0_vs_T0");
  ndf0_vs_T0->SetLineColor(1);
  ndf0_vs_T0->SetLineWidth(4);
  ndf0_vs_T0->SetLineStyle(2);

  TGraph *ndf0S_vs_T0 = 0;
  if (useTeffoverTmax) ndf0S_vs_T0 = new TGraph(Ncent,T0_norm,ndf0_S);
  else ndf0S_vs_T0 = new TGraph(Ncent,T0,ndf0_S);
  ndf0S_vs_T0->SetName("ndf0S_vs_T0");
  ndf0S_vs_T0->SetLineColor(2);
  ndf0S_vs_T0->SetLineWidth(4);
  ndf0S_vs_T0->SetLineStyle(2);

  //____________________________________________________________________________
  // Plot "semi-empirical" (hydro-T0 and dN/dy or E_T) degsOfFreedom_ET_vs_T0

  TGraphErrors *ndf_vs_T0 = 0;
  if (useEntropy)
    {
      if (useTeffoverTmax) ndf_vs_T0 = new TGraphErrors(Ncent2,T0bis_norm,ndf_dNdy_T0,errT0bis,errdNdybis);
      else ndf_vs_T0 = new TGraphErrors(Ncent2,T0bis,ndf_dNdy_T0,errT0bis,errdNdybis);
      sprintf(title,"ndfdNdy_vs_T0");
    }
  else
    {
      if (useTeffoverTmax) ndf_vs_T0 = new TGraphErrors(Ncent2,T0bis_norm,ndf_ET_T0,errT0bis,errE_Tbis);
      else ndf_vs_T0 = new TGraphErrors(Ncent2,T0bis,ndf_ET_T0,errT0bis,errE_Tbis);
      sprintf(title,"ndfET_vs_T0");
    }  
  ndf_vs_T0->SetName(title); 
  ndf_vs_T0->SetMarkerStyle(28); 
  ndf_vs_T0->SetMarkerSize(1.6); 
  ndf_vs_T0->SetMarkerColor(1); 
  ndf_vs_T0->SetLineWidth(4);
  ndf_vs_T0->SetLineStyle(4);
  ndf_vs_T0->SetLineColor(1);

  //____________________________________________________________________________
  // Plot fully "empirical" degsOfFreedom_ET_vs_Teff

  sprintf(xtitle,"T_{eff} (GeV)");

  if (useTeffoverTmax)
    {
      sprintf(xtitle,"T_{eff}/T_{eff}^{max}");
      Teff_min = 0.2,  Teff_max = 1.01;
    }

  char label1[200];
  char label2[200];
  char label3[200];

  if (useEntropy)
    {
      sprintf(title,"ndf_dNdy_vs_Teff");
      sprintf(ytitle,"g_{eff}(dN/d#eta, T_{eff})"); 
      //sprintf(ytitle,"dN/d#eta/(#pi^{2}/90 T_{eff}^{3})"); //"g_{eff}"); 
      ET_max = 100.;

      sprintf(label1,"g_{hydro}(s_{0},T_{0}) = s_{0}/(4#pi^{2}/90 T_{0}^{3})");
      sprintf(label2,"g_{eff}^{'}(dN/d#eta,T_{0}) = (4 dN/d#eta/V)/(4#pi^{2}/90 T_{0}^{3})");
      sprintf(label3,"g_{eff}(dN/d#eta,T_{eff}), with local thermal slope T_{eff} :");
    }
  else
    {
      sprintf(title,"ndf_ET_vs_Teff");
      sprintf(ytitle,"g_{eff}(#epsilon_{T}, T_{eff})"); 
      //sprintf(ytitle,"#epsilon_{T}/(#pi^{2}/30 T_{eff}^{4})");
      //"#epsilon_{0}/[#zeta(2)/5 T_{eff}^{4}]");//"#epsilon_{0}/(#pi^{2}/30 T_{eff}^{4})");
      ET_max = 60.;

      sprintf(label1,"g_{hydro}(#epsilon_{0},T_{0}) = #epsilon_{0}/(#pi^{2}/30 T_{0}^{4})");
      sprintf(label2,"g_{eff}^{'}(#epsilon_{Bj},T_{0}) = #epsilon_{Bj}/(#pi^{2}/30 T_{0}^{4})");
      sprintf(label3,"g_{eff}(#epsilon_{Bj},T_{eff}) , with local thermal slope T_{eff} :");
    }

  TCanvas *c2 = new TCanvas(title,title,700,500);
  c2->Range(0.0822335,-28.6458,0.612183,206.771);
  c2->SetLeftMargin(0.127874);
  c2->SetRightMargin(0.0229885);
  c2->SetTopMargin(0.0287611);
  c2->SetBottomMargin(0.121681);

  double ET_min = 0.;
  Teff_min = 0.150, Teff_max = 0.450;

  if (!QGP_EoS) { Teff_min = 0.120; Teff_max = 0.290; ET_max = 180; }

  TH2F *myframe2 = (TH2F*)frame(title, 14,Teff_min,Teff_max, 14,ET_min,ET_max,xtitle,ytitle);
  myframe2->Draw();

  TLegend *l2 = new TLegend(0.65,0.57,0.95,0.89,NULL,"brNDC");
  l2->SetMargin(0.2);
  l2->SetFillStyle(0);
  l2->SetBorderSize(4);

  l2->AddEntry(ndf0_vs_T0,label1,"l");
  l2->AddEntry(ndf_vs_T0,label2,"l");
  l2->AddEntry("",label3,"");

  c2->cd();
  ndf0_vs_T0->Draw("L");
  //ndf0S_vs_T0->Draw("L");
  ndf_vs_T0->Draw("L"); //Draw("PC");

  TGraphErrors *ndf_vs_Teff[Nslopes2];

  for(int is2=0; is2<Nslopes2; is2++)
    {
      if (useEntropy)
	{
	  if (useTeffoverTmax) ndf_vs_Teff[is2] = new TGraphErrors(Ncent2,slope_norm[is2],ndf_dNdy_Teff[is2],slopeErr[is2],errdNdybis);
	  else ndf_vs_Teff[is2] = new TGraphErrors(Ncent2,slope[is2],ndf_dNdy_Teff[is2],slopeErr[is2],errdNdybis);
	  sprintf(title,"ndf_dNdeta_vs_Teff%i",is2);
	}
      else
	{
	  if (useTeffoverTmax) ndf_vs_Teff[is2] = new TGraphErrors(Ncent2,slope_norm[is2],ndf_ET_Teff[is2],slopeErr[is2],errE_Tbis);
	  else ndf_vs_Teff[is2] = new TGraphErrors(Ncent2,slope[is2],ndf_ET_Teff[is2],slopeErr[is2],errE_Tbis);
	  sprintf(title,"ndf_dETdeta_vs_Teff%i",is2);
	}
      cout << "plotting graph: " << title << endl;
      ndf_vs_Teff[is2]->SetName(title); 
      //ndf_vs_Teff[is2]->Print("all");
      ndf_vs_Teff[is2]->SetMarkerStyle(25); 
      ndf_vs_Teff[is2]->SetMarkerSize(1.6); 
      ndf_vs_Teff[is2]->SetMarkerColor(col[is2]); 
      ndf_vs_Teff[is2]->SetLineWidth(4);
      ndf_vs_Teff[is2]->SetLineColor(col[is2]);

      c2->cd();
      ndf_vs_Teff[is2]->Draw("L"); //("PC");

      is = 2*is2+1;

      sprintf(title,"%3.1f < p_{T} < %3.1f GeV/c",is*0.5,(is+1)*0.5);
      l2->AddEntry(ndf_vs_Teff[is2],title,"l");
    }

  c2->cd();
  l2->Draw();

  TGraphErrors *ndf_vs_Teff_red[5];

  for(int is2=0; is2<Nslopes2; is2++)
    {
      ndf_vs_Teff_red[is2]=(TGraphErrors*)reduced_graph_10centralities(ndf_vs_Teff[is2]);
      c2->cd();
      ndf_vs_Teff_red[is2]->SetMarkerColor(col[is2]);
      ndf_vs_Teff_red[is2]->SetMarkerStyle(25);
      ndf_vs_Teff_red[is2]->Draw("P");
    }

//   for(int cen = 0; cen<11; cen++){
//     TString txt;
//     txt+= " "; 
//     txt+=10*(cen-1); 
//     if (cen==1) txt = "   0";
//     txt +="-"; txt+=10*cen; txt += " %";
//     if (cen==10) txt = " >90 %";
//     if(cen!=0){
//       t->SetTextColor(1);
//       t->SetTextAlign(12);
//       t->DrawText(T0sel[cen]/T0sel[0],ET_max+2.,txt);
//       //t->DrawTextNDC(slope[4][cen]/slopeV[4][0],ET_max+2.,txt);
//       //t->DrawText(slopeV[9][chiMinPiV[cen]]/slopeV[9][0],ET_max+2.,txt);
//       //t->DrawText(slopeV[9][chiMinPiV[cen]],ET_max+2.,txt);
//     }
//   }
//   t2->DrawText(0.201776,ET_max+2.,"Au+Au centrality:");

//   if (useEntropy)
//     {
//       sprintf(title,"g_{hydro}(s_{0},T_{0})=s_{0}/(4#pi^{2}/90 T_{0}^{3})");
//     }
//   else
//     {
//       sprintf(title,"g_{hydro}(#epsilon_{0},T_{0})=#epsilon_{0}/#pi^{2}/30 T_{0}^{4})");
//     } 
//   TLatex *tt = new TLatex(0.32,48.125,title);
//   tt->SetTextSize(0.044);
//   tt->SetTextFont(42);
//   c2->cd();
//   tt->Draw();

  c2->Update();

  //return;

  //____________________________________________________________________________
  // Plot "semi-empirical" degOfFreedom_E0_vs_Teff

  if (useEntropy)
    {
      sprintf(title,"ndf_S0_vs_Teff");
      sprintf(ytitle,"s_{0}/(4#pi^{2}/90 T_{eff}^{3})"); //"g_{eff}"); 
      ET_max = 100.;
    }
  else
    {
      sprintf(title,"ndf_E0_vs_Teff");
      sprintf(ytitle,"#epsilon_{0}/(#pi^{2}/30 T_{eff}^{4})"); //"g_{eff}");
    }

  TCanvas *c3 = new TCanvas(title,title,700,500);
  c3->Range(0.0822335,-28.6458,0.612183,206.771);
  c3->SetLeftMargin(0.127874);
  c3->SetRightMargin(0.0229885);
  c3->SetTopMargin(0.0287611);
  c3->SetBottomMargin(0.121681);

  double E0_min = 0., E0_max = 550.;
  if (useTeffoverTmax)
    {
      sprintf(xtitle,"T_{eff}/T_{eff}^{max}");
      T0_min = 0.2,  T0_max = 1.01;
    }

  if (!QGP_EoS) { Teff_min = 0.120; Teff_max = 0.290; E0_max = 200; }

  TH2F *myframe3 = (TH2F*)frame(title, 14,T0_min,T0_max, 14,E0_min,E0_max,xtitle,ytitle);
  myframe3->Draw();

  TLegend *l3 = new TLegend(0.65,0.57,0.95,0.89,"Local thermal #gamma slopes:","brNDC");
  l3->SetMargin(0.2);
  l3->SetFillStyle(0);
  l3->SetBorderSize(4);

  c3->cd();
  ndf0_vs_T0->Draw("L");
  //ndf0S_vs_T0->Draw("L");
  //tt->Draw();
  ndf_vs_T0->Draw("L"); //("PC");

  TGraphErrors *ndf0_vs_Teff[Nslopes2];

  for(int is2=0; is2<Nslopes2; is2++)
    {
      if (useEntropy)
	{
	  if (useTeffoverTmax) ndf0_vs_Teff[is2] = new TGraphErrors(Ncent2,slope_norm[is2],ndf_S0_Teff[is2],slopeErr[is2],errS0bis);
	  else ndf0_vs_Teff[is2] = new TGraphErrors(Ncent2,slope[is2],ndf_S0_Teff[is2],slopeErr[is2],errS0bis);
	  sprintf(title,"ndfS0_vs_Teff%i",is2);
	}
      else
	{
	  if (useTeffoverTmax) ndf0_vs_Teff[is2] = new TGraphErrors(Ncent2,slope_norm[is2],ndf_E0_Teff[is2],slopeErr[is2],errE_Tbis);
	  else ndf0_vs_Teff[is2] = new TGraphErrors(Ncent2,slope[is2],ndf_E0_Teff[is2],slopeErr[is2],errE_Tbis);
	  sprintf(title,"ndfE0_vs_Teff%i",is2);
	}
      cout << "plotting graph: " << title << endl;
      ndf0_vs_Teff[is2]->SetName(title); 
      //ndf0_vs_Teff[is2]->Print("all");
      ndf0_vs_Teff[is2]->SetMarkerStyle(28); 
      ndf0_vs_Teff[is2]->SetMarkerSize(1.6); 
      ndf0_vs_Teff[is2]->SetMarkerColor(col[is2]); 
      ndf0_vs_Teff[is2]->SetLineWidth(2);
      ndf0_vs_Teff[is2]->SetLineColor(col[is2]);

      c3->cd();
      ndf0_vs_Teff[is2]->Draw("L"); //Draw("PC");

      is = 2*is2+1;

      sprintf(title,"%3.1f < p_{T} < %3.1f GeV/c",is*0.5,(is+1)*0.5);
      l3->AddEntry(ndf0_vs_Teff[is2],title,"l");
    }
  c3->cd();
  l3->Draw();

  //return;

  //____________________________________________________________________________
  // Plot E_T, dN/deta versus Npart

  double Npart[Ncent2];
  double eNpart[Ncent2];
  double EtNormNpart2[Ncent2];
  double dNdetaNormNpart2[Ncent2];

//  Cent = 00-05% -->  <E_T>[pQCD] = 56.5057 GeV
//  Cent = 05-10% -->  <E_T>[pQCD] = 54.8014 GeV
//  Cent = 10-15% -->  <E_T>[pQCD] = 45.5683 GeV
//  Cent = 15-20% -->  <E_T>[pQCD] = 42.3793 GeV
//  Cent = 20-30% -->  <E_T>[pQCD] = 44.0443 GeV
//  Cent = 30-40% -->  <E_T>[pQCD] = 32.3821 GeV
//  Cent = 40-50% -->  <E_T>[pQCD] = 21.2679 GeV
//  Cent = 50-60% -->  <E_T>[pQCD] = 12.5816 GeV
//  Cent = 60-70% -->  <E_T>[pQCD] = 5.87828 GeV
//  Cent = 70-80% -->  <E_T>[pQCD] = 2.89936 GeV
//  Cent = 80-92% -->  <E_T>[pQCD] = 1.44379 GeV

  double ET_pQCD[12]={70.,56.5,54.8,45.5,42.4,44.0,32.4,21.3,12.6,5.9,2.9,1.5};
  double errET_pQCD[12];

  for(int icen=0;icen<12;icen++){
    ET_pQCD[icen]/=(npart[icen]/2.);
    errET_pQCD[icen]=ET_pQCD[icen]*0.2;
  }

  TGraphErrors *ETpQCD_vs_Npart = new TGraphErrors(12,npart,ET_pQCD,enpart,errET_pQCD);

  TF1 * fitETpQCD = new TF1("fitETpQCD","pol4",0.,1000.) ;
  ETpQCD_vs_Npart->Fit("fitETpQCD") ;
  

  icen2 = 0;
  for(int icen=0; icen<Ncent; icen+=step)
    {

      double b = 14./100.*icen; // idem to: 14./20.*icen2

      Npart[icen2] = Npart_vs_b->Eval(b);
      eNpart[icen2] = 0.;//0.1*Npart[icen2];
      //cout << "Npart = " << Npart[icen2] << " for centrality: " << icen << endl;

      //EtNormNpart2[icen2] = etvs_b_phnx->Eval(b)/(Npart[icen2]/2.);
      dy_to_deta = 1.;
      EtNormNpart2[icen2] = dy_to_deta*E_Tbis[icen2]/(Npart[icen2]/2.);
      EtNormNpart2[icen2]+= fitETpQCD->Eval(Npart[icen2]) ;
      dNdetaNormNpart2[icen2] = dndeta_vs_b_phnx->Eval(b)/(Npart[icen2]/2.);
      dNdetaNormNpart2[icen2] = Ntot_to_Nch*dy_to_deta*dNdybis[icen2]/(Npart[icen2]/2.);

      icen2++;
    }

  //____________________________________________________________________________
  // T_eff versus Npart 

  sprintf(title,"Teff_T0_vs_Npart");
  TCanvas *c4 = new TCanvas(title,title,700,500);
  c4->Range(0.0822335,-28.6458,0.612183,206.771);
  c4->SetLeftMargin(0.127874);
  c4->SetRightMargin(0.0229885);
  c4->SetTopMargin(0.0287611);
  c4->SetBottomMargin(0.121681);

  T0_min = 0.;
  if (!QGP_EoS) { T0_max = 0.290; }

  TH2F *myframe4 = (TH2F*)frame(title, 14,0.,400., 14,T0_min,T0_max,"N_{part}","T_{eff} (GeV)"); 
  myframe4->Draw();

  TLegend *l4 = new TLegend(0.65,0.57,0.95,0.89,"Local thermal #gamma slopes:","brNDC");
  l4->SetMargin(0.2);
  l4->SetFillStyle(0);
  l4->SetBorderSize(4);

  TGraphErrors *T0_vs_Npart = new TGraphErrors(Ncent2,Npart,T0bis,eNpart,errT0bis);
  sprintf(title,"T0_vs_Npart");
  cout << "plotting graph: " << title << endl;
  T0_vs_Npart->SetName(title); 
  //T0_vs_Npart->Print("all");
  T0_vs_Npart->SetMarkerStyle(28); 
  T0_vs_Npart->SetMarkerSize(1.6); 
  T0_vs_Npart->SetMarkerColor(1); 
  T0_vs_Npart->SetLineWidth(2);
  T0_vs_Npart->SetLineColor(1);

  c4->cd();
  T0_vs_Npart->Draw("L"); // PC");

  TGraphErrors *Teff_vs_Npart[Nslopes2];

  for(int is2=0; is2<Nslopes2; is2++)
    {      
      Teff_vs_Npart[is2] = new TGraphErrors(Ncent2,Npart,slope[is2],eNpart,slopeErr[is2]);
      sprintf(title,"Teff_vs_Npart%i",is2);
      cout << "plotting graph: " << title << endl;
      Teff_vs_Npart[is2]->SetName(title); 
      //Teff_vs_Npart[is2]->SetLineStyle(i); 
      Teff_vs_Npart[is2]->SetMarkerStyle(28); 
      Teff_vs_Npart[is2]->SetMarkerSize(1.6); 
      Teff_vs_Npart[is2]->SetMarkerColor(col[is2]);
      Teff_vs_Npart[is2]->SetLineWidth(3);
      Teff_vs_Npart[is2]->SetLineColor(col[is2]);

      c4->cd();
      Teff_vs_Npart[is2]->Draw("L"); //("PC");

      is = 2*is2+1;

      sprintf(title,"%3.1f < p_{T} < %3.1f GeV/c",is*0.5,(is+1)*0.5);
      l4->AddEntry(Teff_vs_Npart[is2],title,"l");
    }
  l4->Draw();

  //____________________________________________________________________________
  // dE_T/deta, dN/deta versus Npart 

  if (useEntropy)
    {
      ET_max = 5.9. ;
      sprintf(title,"dNdeta_vs_Npart");
    }
  else
    {
      ET_max = 3.9. ;
      sprintf(title,"dETdeta_vs_Npart");
    }
  TCanvas *c5 = new TCanvas(title,title,700,500);
  c5->Range(0.0822335,-28.6458,0.612183,206.771);
  c5->SetLeftMargin(0.127874);
  c5->SetRightMargin(0.0229885);
  c5->SetTopMargin(0.0287611);
  c5->SetBottomMargin(0.121681);

  TH2F *myframe5 = 0;
  TGraphErrors *ET_vs_Npart = 0;

  if (useEntropy)
    {
      ET_vs_Npart = new TGraphErrors(Ncent2,Npart,dNdetaNormNpart2,eNpart,errdNdybis);
      sprintf(title,"dNdeta_vs_Npart");
      myframe5 = (TH2F*)frame(title, 14,0.,400., 14,ET_min,ET_max, "N_{part}","dN_{ch}/d#eta/(0.5 N_{part}) (GeV)"); 
      myframe5->Draw();
      int v = plot_dNdeta_Npart_RHIC(c5);
    }
  else
    {
      ET_vs_Npart = new TGraphErrors(Ncent2,Npart,EtNormNpart2,eNpart,errE_Tbis);
      sprintf(title,"dETdeta_vs_Npart");
      myframe5 = (TH2F*)frame(title, 14,0.,400., 14,ET_min,ET_max, "N_{part}","(dE_{T}/d#eta)/(0.5 N_{part}) (GeV)"); 
      myframe5->Draw();
      int v = plot_dET_Npart_RHIC(c5);
    }
  cout << "plotting graph: " << title << endl;
  ET_vs_Npart->SetName(title); 
  //ET_vs_Npart->Print("all");
  ET_vs_Npart->SetMarkerStyle(28); 
  ET_vs_Npart->SetMarkerSize(1.6); 
  ET_vs_Npart->SetMarkerColor(1); 
  ET_vs_Npart->SetLineWidth(2);
  ET_vs_Npart->SetLineColor(1);

  c5->cd();
  ET_vs_Npart->Draw("PL");
  ETpQCD_vs_Npart->SetMarkerStyle(20);
  ETpQCD_vs_Npart->Draw("PL");
  //ETpQCD_vs_Npart->Print();

  return;


//   //____________________________________________________________________________
//   // Plot Teff,T0 vs Npart

//   T0_vs_Npart->Draw("C");

//   TGraphErrors *ndf_vs_T0 = new TGraphErrors(Ncent2,T0bis,EtNorm2,errT0bis,errE_Tbis);
//   sprintf(title,"ndf_vs_T0");
//   cout << "plotting graph: " << title << endl;
//   ndf_vs_T0->SetName(title); 
//   //ndf_vs_T0->Print("all");
//   ndf_vs_T0->SetMarkerStyle(28); 
//   ndf_vs_T0->SetMarkerSize(1.6); 
//   ndf_vs_T0->SetMarkerColor(1); 
//   ndf_vs_T0->SetLineWidth(2);
//   ndf_vs_T0->SetLineColor(1);

//   c5->cd();
//   ndf_vs_T0->Draw("PC");

//   c5->Update();

//   return;

}

int plot_dET_Npart_RHIC( TCanvas *c1)
{

   TGraphErrors *gre = new TGraphErrors(14);
   gre->SetName("Graph");
   gre->SetTitle("Graph");
   gre->SetFillColor(1);
   gre->SetMarkerStyle(8);
   gre->SetMarkerSize(1.3);
   gre->SetPoint(0,353.4,3.427);
   gre->SetPointError(0,10.1,0.207);
   gre->SetPoint(1,300.2,3.283);
   gre->SetPointError(1,9,0.203);
   gre->SetPoint(2,254.3,3.163);
   gre->SetPointError(2,8.1,0.203);
   gre->SetPoint(3,215.2,3.046);
   gre->SetPointError(3,7.3,0.205);
   gre->SetPoint(4,181,2.945);
   gre->SetPointError(4,6.6,0.21);
   gre->SetPoint(5,151.3,2.858);
   gre->SetPointError(5,6,0.22);
   gre->SetPoint(6,125.2,2.757);
   gre->SetPointError(6,5.5,0.231);
   gre->SetPoint(7,102.7,2.664);
   gre->SetPointError(7,5.1,0.25);
   gre->SetPoint(8,83.3,2.567);
   gre->SetPointError(8,4.7,0.27);
   gre->SetPoint(9,66.7,2.453);
   gre->SetPointError(9,4.3,0.29);
   gre->SetPoint(10,52.5,2.301);
   gre->SetPointError(10,4.1,0.328);
   gre->SetPoint(11,40.2,2.184);
   gre->SetPointError(11,3.8,0.359);
   gre->SetPoint(12,30.2,2.06);
   gre->SetPointError(12,3.6,0.407);
   gre->SetPoint(13,22,1.918);
   gre->SetPointError(13,3.4,0.469);
   c1->cd();
   gre->Draw("p");
   
   Double_t *dum = 0;
   TPolyLine *pline = new TPolyLine(14,dum,dum,"");
   pline->SetFillColor(19);
   pline->SetPoint(0,353.4,3.441);
   pline->SetPoint(1,300.2,3.317);
   pline->SetPoint(2,254.3,3.218);
   pline->SetPoint(3,215.2,3.118);
   pline->SetPoint(4,181,3.038);
   pline->SetPoint(5,151.3,2.974);
   pline->SetPoint(6,125.2,2.894);
   pline->SetPoint(7,102.7,2.83);
   pline->SetPoint(8,83.3,2.762);
   pline->SetPoint(9,66.7,2.677);
   pline->SetPoint(10,52.5,2.569);
   pline->SetPoint(11,40.2,2.492);
   pline->SetPoint(12,30.2,2.416);
   pline->SetPoint(13,22,2.346);
   pline->Draw("L");
   
   Double_t *dum = 0;
   pline = new TPolyLine(14,dum,dum,"");
   pline->SetFillColor(19);
   pline->SetPoint(0,353.4,3.413);
   pline->SetPoint(1,300.2,3.249);
   pline->SetPoint(2,254.3,3.108);
   pline->SetPoint(3,215.2,2.974);
   pline->SetPoint(4,181,2.852);
   pline->SetPoint(5,151.3,2.742);
   pline->SetPoint(6,125.2,2.62);
   pline->SetPoint(7,102.7,2.498);
   pline->SetPoint(8,83.3,2.372);
   pline->SetPoint(9,66.7,2.229);
   pline->SetPoint(10,52.5,2.033);
   pline->SetPoint(11,40.2,1.876);
   pline->SetPoint(12,30.2,1.704);
   pline->SetPoint(13,22,1.49);
   pline->Draw("L");


}

int plot_dNdeta_Npart_RHIC( TCanvas *c1)
{

   TGraph *graph = new TGraph(11);
   graph->SetName("Graph");
   graph->SetTitle("Graph");
   graph->SetFillColor(1);
   graph->SetMarkerStyle(21);
   graph->SetMarkerSize(1.3);
   graph->SetPoint(0,357.8,3.908);
   graph->SetPoint(1,330.9,3.804);
   graph->SetPoint(2,298.1,3.671);
   graph->SetPoint(3,255.9,3.553);
   graph->SetPoint(4,217.3,3.479);
   graph->SetPoint(5,182.7,3.405);
   graph->SetPoint(6,151.7,3.316);
   graph->SetPoint(7,123.6,3.242);
   graph->SetPoint(8,103.1,3.183);
   graph->SetPoint(9,83.2,3.138);
   graph->SetPoint(10,65,2.894);
   c1->cd();
   graph->Draw("p");
   
   TGraphErrors *gre = new TGraphErrors(6);
   gre->SetName("Graph");
   gre->SetTitle("Graph");
   gre->SetFillColor(1);
   gre->SetMarkerStyle(29);
   gre->SetMarkerSize(1.8);
   gre->SetPoint(0,48.9,2.997);
   gre->SetPointError(0,0,0.506);
   gre->SetPoint(1,129.4,3.242);
   gre->SetPointError(1,0,0.261);
   gre->SetPoint(2,185.2,3.35);
   gre->SetPointError(2,0,0.299);
   gre->SetPoint(3,248.5,3.541);
   gre->SetPointError(3,0,0.299);
   gre->SetPoint(4,294,3.641);
   gre->SetPointError(4,0,0.299);
   gre->SetPoint(5,362.8,3.917);
   gre->SetPointError(5,0,0.303);
   //gre->SetPoint(6,0,0);
   //gre->SetPointError(6,0,0);
   gre->Draw("p");
   
   gre = new TGraphErrors(14);
   gre->SetName("Graph");
   gre->SetTitle("Graph");
   gre->SetFillColor(1);
   gre->SetMarkerStyle(8);
   gre->SetMarkerSize(1.3);
   gre->SetPoint(0,353.4,3.89);
   gre->SetPointError(0,10.1,0.235);
   gre->SetPoint(1,300.2,3.734);
   gre->SetPointError(1,9,0.217);
   gre->SetPoint(2,254.3,3.593);
   gre->SetPointError(2,8.1,0.209);
   gre->SetPoint(3,215.2,3.453);
   gre->SetPointError(3,7.3,0.206);
   gre->SetPoint(4,181,3.343);
   gre->SetPointError(4,6.6,0.213);
   gre->SetPoint(5,151.3,3.247);
   gre->SetPointError(5,6,0.223);
   gre->SetPoint(6,125.2,3.15);
   gre->SetPointError(6,5.5,0.239);
   gre->SetPoint(7,102.7,3.046);
   gre->SetPointError(7,5.1,0.261);
   gre->SetPoint(8,83.3,2.965);
   gre->SetPointError(8,4.7,0.285);
   gre->SetPoint(9,66.7,2.858);
   gre->SetPointError(9,4.3,0.317);
   gre->SetPoint(10,52.5,2.701);
   gre->SetPointError(10,4.1,0.358);
   gre->SetPoint(11,40.2,2.597);
   gre->SetPointError(11,3.8,0.406);
   gre->SetPoint(12,30.2,2.483);
   gre->SetPointError(12,3.6,0.464);
   gre->SetPoint(13,22,2.327);
   gre->SetPointError(13,3.4,0.545);
   gre->Draw("p");
   
   gre = new TGraphErrors(6);
   gre->SetName("Graph");
   gre->SetTitle("Graph");
   gre->SetFillColor(1);
   gre->SetPoint(0,357,3.501);
   gre->SetPointError(0,8,0.318);
   gre->SetPoint(1,306,3.275);
   gre->SetPointError(1,11,0.311);
   gre->SetPoint(2,239,3.155);
   gre->SetPointError(2,10,0.306);
   gre->SetPoint(3,168,3.06);
   gre->SetPointError(3,9,0.319);
   gre->SetPoint(4,114,3.053);
   gre->SetPointError(4,9,0.37);
   gre->SetPoint(5,73,3.014);
   gre->SetPointError(5,8,0.429);
   gre->Draw("p");

   return 0;
}

TGraphErrors *reduced_graph_10centralities( TGraphErrors *g )
{

  double impParam10[10]={ 22, 40, 52, 62, 70, 78, 84, 91, 96, 99};
  double x[10],y[10],ex[10],ey[10];

  int N = g->GetN();
  int j = 0;

  for ( int i = 0; i < N; i++ )
    {
      if (i==impParam10[j])
	{
	  g->GetPoint(i,x[j],y[j]);
	  ex[j] = g->GetErrorX(i);
	  ey[j] = g->GetErrorY(i);
	  j++;
	}
    }

  TGraphErrors *gred = new TGraphErrors(10,x,y,ex,ey);

  return gred;

}
