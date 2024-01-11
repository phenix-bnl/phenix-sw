
void makeFigs3_4_new(char* inasciifile = "sumvarvol_qgp_tau015_v00_Glau_Et.dat" , 
		     bool useEntropy = true)

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

  double e0[Ncent];
  double e0_av[Ncent];
  double T0[Ncent];
  double T0_av[Ncent];
  double s0[Ncent];
  double s0_av[Ncent];
  double Vol[Ncent];
  double dNchdeta[Ncent];
  double E_T[Ncent];

  double errT0[Ncent];
  double errs0[Ncent];
  double errET[Ncent];
  double errdNchdeta[Ncent];

  double TeffV[Nslopes][Ncent];
  double TeffErrV[Nslopes][Ncent];

  int chiMinPiV[11];
  int chiMinKV[11];
  int chiMinPrV[11];

  const double hc = 0.197; // GeV fm
  const double hc3 = TMath::Power(hc,3.); // GeV/fm^3 --> GeV^4, fm^-3 --> GeV^3
  const double B =  0.379 ; // GeV/fm^3 

  double b_sel[9]={ 3.2, 5.7, 7.4, 8.7, 9.9, 11.0, 11.9, 12.8, 14.0}; // 0-10%,10-20%, ... 80-90%
  double T0_sel[9]; // T0 for 0-10%, 10-20%, ... 80-90% centralities
  int isel = 1;

  ifstream in(inasciifile);
  int k;

   // icen - step in impact parameter (100 steps up to 14 fm)
  for(int icen=0; icen<Ncent; icen++)
    {

      // e0, T0, s0 - initial MAXIMAL energy density, temperature and entropy densities, 
      // xx_Av - averaged values over transv. area, 
      // dNchdeta - pseudo-rapidity density of _charged_ hadrons [post-computed with T.C]
      // E_T - transverse energy [post-computed with T.C]

      in >> k >> e0[icen] >> e0_av[icen] >> T0[icen] >> T0_av[icen] 
	 >> s0[icen] >> s0_av[icen] >> Vol[icen] >> dNchdeta[icen] >> E_T[icen] ;

      cout << " " << k << " " << e0[icen] << " " << e0_av[icen] << " " << T0[icen] << " " << T0_av[icen] 
           << " " << s0[icen] << " " << s0_av[icen] << " " << Vol[icen] << " " << dNchdeta[icen] << " " << E_T[icen] << endl ;;

      // 10 effective slopes (with errors) per line.
      for(int is=0; is<Nslopes; is++)
	{ 
	  in >> TeffV[is][icen] >> TeffErrV[is][icen]; 	
	  //cout << " " << TeffV[is][icen] << " " << TeffErrV[is][icen]; 
	}
      //cout << endl;

      // subtract Bag-constant energy
      e0[icen]-=B;
      s0[icen]-=(4./3.)*B/T0[icen];

      errT0[icen]=0;
      errs0[icen]=0;
      errET[icen]=0;
      errdNchdeta[icen]=0;

      if ( (14./100.*icen) >= b_sel[isel-1] && 
	   (14./100.*icen) <= b_sel[isel] )
	{
	  T0_sel[isel-1] = T0[icen-1];
	  isel++;
	  cout << "--------> isel:" << icen-1 << " " << (14./100.*(icen-1))  << endl;
	}
    }

  T0_sel[isel-1] = T0[Ncent-1];
  cout << "--------> isel:" << icen-1 << " " << (14./100.*(Ncent-1))  << endl;

  // last lines of file are for chi-square values ...
  for(int cen=0; cen<11; cen++){
    cout << "Cen " << cen << " " << k << endl ;
    in >> k >> chiMinPiV[k] >> chiMinKV[k] >> chiMinPrV[k];
  }
  in.close();

  //____________________________________________________________________________
  // Reduced number of selected slopes: 1,3,5,7,9
  // j= 0  1      2      3      4      5      6      7      8      9  
  // 0-0.5 0.5-1. 1-1.5  1.5-2  2-2.5  2.5-3  3-3.5  3.5-4  4-4.5  4.5-5

  const int Nslopes2 = 5;

  double Teff[Nslopes2][Ncent];
  double TeffErr[Nslopes2][Ncent];

  int is = 0;

  for(int icen=0; icen<Ncent; icen++)
    {
      for(int is2=0; is2<Nslopes2; is2++)
	{
	  is = 2*is2+1;
	  //cout << "Refilling slopes array: old(cent,slope) = (" << icen << "," << is << ") ---> "
	  //     << " new(cent,slope) = (" << icen << "," << is2 << ")" << endl;
	  Teff[is2][icen]=TeffV[is][icen];
	  TeffErr[is2][icen]=TeffErrV[is][icen];	  
	}
    }

  //____________________________________________________________________________
  // T_eff vs T_0

  char title[100];
  char xtitle[100];
  char ytitle[100];
  sprintf(title,"Teff_vs_T0");
  sprintf(xtitle,"T_{0} (GeV)");
  sprintf(ytitle,"T_{eff} (GeV)");

  TCanvas *c1 = new TCanvas(title,title,700,500);
  c1->Range(0.0822335,-28.6458,0.612183,206.771);
  c1->SetLeftMargin(0.127874);
  c1->SetRightMargin(0.0229885);
  c1->SetTopMargin(0.0287611);
  c1->SetBottomMargin(0.121681);

  double Teff_min = 0.090, T0_min = 0.140;
  double Teff_max = 0.500, T0_max = 0.640;
  double Tsel_pos = 0.100;

  char text[200];
  sprintf(text,"Local thermal #gamma slopes (QGP+HRG EoS):");

  if (!QGP_EoS) { 
    Teff_min = 0.110; Teff_max = 0.300;
    T0_min = 0.140; T0_max = 0.300; 
    Tsel_pos = 0.117;
    sprintf(text,"Local thermal #gamma slopes (HRG EoS):");
  }

  TH2F *myframe = (TH2F*)frame(title, 14,T0_min,T0_max, 14,Teff_min,Teff_max,xtitle,ytitle);

  TLegend *l = new TLegend(0.168103,0.665957,0.507184,0.948936,text,"brNDC");
  l->SetTextSize(0.033);
  l->SetMargin(0.2);
  l->SetFillStyle(0);
  l->SetBorderSize(4);

  TGraphErrors *T0_vs_Teff[Nslopes2];

  int col[Nslopes2] = {4,7,91,6,96};

  for(int is2=Nslopes2-1; is2>=0; is2--)
    //for(int is2=0; is2<Nslopes2; is2++)
    {      
      T0_vs_Teff[is2] = new TGraphErrors(Ncent,T0,Teff[is2],errT0,TeffErr[is2]);
      sprintf(title,"T0_vs_Teff%i",is2);
      //cout << "plotting graph: " << title << endl;
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

  print_centrality_label( T0_sel, Tsel_pos, c1);
  
  TText *t2 = new TText(0.553706,0.175031,"Au+Au centrality:");
  t2->SetTextSize(0.03);
  t2->SetTextAlign(21);
  t2->Draw();

  //return;

  //____________________________________________________________________________
  // Equation-of-State

  const double pi2 = TMath::Pi()*TMath::Pi();
  double zetafactor_E = pi2/30.; //zeta(4)*3/pi^2
  double zetafactor_S = 4.*pi2/90.; // zeta(4)*4/pi^2

  //____________________________________________________________________________
  // Theoretical EoS: "pure" (hydro) degsOfFreedom_ET_vs_T0

  double ndf0_e[Ncent];
  double ndf0_s[Ncent];

  for(int icen=0; icen<Ncent; icen++)
    {
      ndf0_e[icen]=e0[icen]*hc3/(zetafactor_E*TMath::Power(T0[icen],4.));
      ndf0_s[icen]=s0[icen]*hc3/(zetafactor_S*TMath::Power(T0[icen],3.));
    }

  TGraph *ndf0_vs_T0 = ndf0_vs_T0 = new TGraph(Ncent,T0,ndf0_e);
  ndf0_vs_T0->SetName("ndf0_vs_T0");
  ndf0_vs_T0->SetLineColor(1);
  ndf0_vs_T0->SetLineWidth(4);
  ndf0_vs_T0->SetLineStyle(2);

  TGraph *ndf0S_vs_T0 = new TGraph(Ncent,T0,ndf0_s);
  ndf0S_vs_T0->SetName("ndf0S_vs_T0");
  ndf0S_vs_T0->SetLineColor(1);
  ndf0S_vs_T0->SetLineWidth(4);
  ndf0S_vs_T0->SetLineStyle(2);

  //____________________________________________________________________________
  // Computed EoS

  double ndf_e0_Teff[Nslopes2][Ncent];
  double ndf_ET_Teff[Nslopes2][Ncent];
  double ndf_ET_T0[Ncent];

  double ndf_s0_Teff[Nslopes2][Ncent];
  double ndf_dNchdeta_Teff[Nslopes2][Ncent];
  double ndf_dNchdeta_T0[Ncent];

  double Teff_reduced[Ncent];

  double EBjork[Ncent];

  //____________________________________________________________________________
  // Geometrical factors

  const double tau0 = 0.15;// fm/c
  cout << "<I> Remember normalizing E_T's by tau0 = " << tau0 << " fm/c (only valid @ 200 GeV/c) ..." << endl;

  // Area versus b

  TF1 *fAT_vs_b= new TF1("fAT_vs_b","pol6/([7]+exp((x-[8])/[9]))",0,20);
  fAT_vs_b->SetParameters(3755.7,-22.6065,-6.78784,-11.8696,2.1149,-0.141457,0.00344969,25.0349,14.0565,0.606894);

  // volume2 (Dmitri's code ...)

  Double_t x[100] ;
  for(Int_t i=0;i<100;i++)x[i]=1.*i ;
  TGraph * g = new TGraph(100,x,Vol) ;
  g->Fit("pol8","QEIM") ;
  TF1 * Vol2 = (TF1*)g->GetFunction("pol8");

  //____________________________________________________________________________

  TF1 *dndeta_vs_b_phnx= new TF1("dndeta_vs_b_phnx","pol9",2.1,14);
  dndeta_vs_b_phnx->SetParameters(-1593.53,3148.35,-1758.14,532.874,-99.8061,
				  12.0101,-0.93009,0.0447964,-0.00122018,1.43491e-05);

  double Nch_to_Ntot = 3./2.; // hydro computes dN_ch/deta already
  double deta_to_dy = 0.8;  //

  double dSdy_to_tot_entropy = 1.;// 6.;

  for(int icen=0; icen<Ncent; icen++)
    {

      // Energies normalized by Stefan-Boltzmann law: g*3/pi^2*zeta(4)*T^4
      // and by volume: V = [Glauber A_T]x[tau0]

      // Entropies normalized by Stefan-Boltzmann law: g*4/pi^2*zeta(4)*T^3
      // and by volume: V = [Glauber A_T]x[tau0]
      // Note: dN/dy = rho*V = s/4*V
      
      double b = 14./100.*icen;

      double V_0 = fAT_vs_b->Eval(b)*tau0;//*pow(0.7,4.);
      //V_0 = Vol2->Eval(1.*icen)*tau0; 

      double prefactor = dSdy_to_tot_entropy*3.6*Nch_to_Ntot*deta_to_dy;

      for(int is2=0; is2<Nslopes2; is2++)
	{
	  ndf_e0_Teff[is2][icen] = e0[icen]*hc3/(zetafactor_E*TMath::Power(Teff[is2][icen],4.));
	  //ndf_ET_Teff[is2][icen]  = (et_vs_b_phnx->Eval(b)/V_0)*hc3/(zetafactor_E*TMath::Power(Teff[is2][icen],4.));
	  ndf_ET_Teff[is2][icen]  = dSdy_to_tot_entropy*(E_T[icen]/V_0)*hc3/(zetafactor_E*TMath::Power(Teff[is2][icen],4.));

	  ndf_s0_Teff[is2][icen] = s0[icen]*hc3/(zetafactor_S*TMath::Power(Teff[is2][icen],3.));

	  //ndf_dNchdeta_Teff[is2][icen]=prefactor*dndeta_vs_b_phnx->Eval(b)*hc3/(V_0*zetafactor_S*TMath::Power(Teff[is2][icen],3.));
	  ndf_dNchdeta_Teff[is2][icen]=prefactor*dNchdeta[icen]*hc3/(V_0*zetafactor_S*TMath::Power(Teff[is2][icen],3.));

	  if (is2==Nslopes2-1) 
	    cout << prefactor/zetafactor_S << "*" << dNchdeta[icen] << "/(" << V_0 << "*pow(" 
		 << Teff[is2][icen] << ",3.) = "<< ndf_dNchdeta_Teff[is2][icen] << endl;
	}

      ndf_ET_T0[icen] = dSdy_to_tot_entropy*(E_T[icen]/V_0)*hc3/(zetafactor_E*TMath::Power(T0[icen],4.));
      ndf_dNchdeta_T0[icen] = prefactor*dNchdeta[icen]*hc3/(V_0*zetafactor_S*TMath::Power(T0[icen],3.));
      cout << prefactor/zetafactor_S << "*" << dNchdeta[icen] << "/(" << V_0 << "*pow(" 
	   << T0[icen] << ",3.) = "<< ndf_dNchdeta_T0[icen] << endl;

      EBjork[icen] = (E_T[icen]/V_0);

    }

  //____________________________________________________________________________
  // plot semi- and fully- empirical degrees of freedom

  char label1[200];
  char label2[200];
  char label3[200];

  double ET_min = 0.;
  double ET_max = 0.;

  sprintf(xtitle,"T_{eff} (GeV)");
  TLatex *hbar = new TLatex(0.238416,106.087,"/"); // for hbar
  hbar->SetTextSize(0.0240385);
  hbar->SetLineWidth(2);

  if (useEntropy)
    {
      if (QGP_EoS) sprintf(title,"ndf_dNchdeta_vs_Teff_qgp");
      else sprintf(title,"ndf_dNchdeta_vs_Teff_qgp+hrg");
      sprintf(ytitle,"g_{eff}(dN_{ch}/d#eta, T_{eff})"); 
      //sprintf(ytitle,"dN_{ch}/d#eta/(#pi^{2}/90 T_{eff}^{3})"); //"g_{eff}"); 
      ET_max = 80.;

      sprintf(label1," g_{hydro}(s_{0},T_{0}) =  #frac{45}{2#pi^{2}}  #frac{s_{0}}{T_{0}^{3}} (h c)^{3}");
      sprintf(label2," g_{eff}(dN_{ch}/d#eta,T_{0}) =  #frac{100}{#pi^{2}}  #frac{dN_{ch}/d#eta}{A_{T}#tau_{0} T_{0}^{3}} (h c)^{3}");
      sprintf(label3," g_{eff}(dN_{ch}/d#eta,T_{eff}). Local thermal #gamma slope T_{eff} :");
      //sprintf(label1," g_{hydro}(s_{0},T_{0}) = #frac{s_{0}}{4#pi^{2}/90 T_{0}^{3}}");
      //sprintf(label2," g_{eff}^{'}(dN_{ch}/d#eta,T_{0}) = #frac{4 dN_{ch}/d#eta/V}{4#pi^{2}/90 T_{0}^{3}}");
      //sprintf(label3," g_{eff}(dN_{ch}/d#eta,T_{eff}). Local thermal #gamma slope T_{eff} :");
    }
  else
    {
      sprintf(title,"ndf_ET_vs_Teff");
      sprintf(ytitle,"g_{eff}(#epsilon_{T}, T_{eff})"); 
      //sprintf(ytitle,"#epsilon_{T}/(#pi^{2}/30 T_{eff}^{4})");
      //"#epsilon_{0}/[#zeta(2)/5 T_{eff}^{4}]");//"#epsilon_{0}/(#pi^{2}/30 T_{eff}^{4})");
      ET_max = 120.;

      sprintf(label1," g_{hydro}(#epsilon_{0},T_{0}) = #frac{#epsilon_{0}}{#pi^{2}/30 T_{0}^{4}}");
      sprintf(label2," g_{eff}^{'}(#epsilon_{Bj},T_{0}) = #frac{#epsilon_{Bj}}{#pi^{2}/30 T_{0}^{4}}");
      sprintf(label3," g_{eff}(#epsilon_{Bj},T_{eff}). Local thermal #gamma slope T_{eff} :");
    }

  TCanvas *c2 = new TCanvas(title,title,700,500);
  c2->Range(0.104514,-15.942,0.460221,115.072);
  //c2->Range(0.0822335,-28.6458,0.612183,206.771);
  c2->SetLeftMargin(0.127874);
  c2->SetRightMargin(0.02874);
  c2->SetTopMargin(0.2677);
  c2->SetBottomMargin(0.121681);

  Teff_min = 0.150, Teff_max = 0.450;
  double ypos = 62.;

  if (!QGP_EoS) {
    Teff_min = 0.144; Teff_max = 0.304;
    ET_max = 180.;
    ypos = 144.;
  }

  TH2F *myframe2 = (TH2F*)frame(title, 14,Teff_min,Teff_max, 14,ET_min,ET_max,xtitle,ytitle);
  myframe2->Draw();

  TLegend *l2 = new TLegend(0.126437,0.767699,0.54454,0.980088,NULL,"brNDC");
  l2->SetTextSize(0.03);
  l2->SetMargin(0.15);
  l2->SetFillStyle(0);
  l2->SetBorderSize(4);

  //____________________________________________________________________________
  // "Semi-empirical" (hydro-T0 and dN/dy or E_T) degsOfFreedom_ET_vs_T0

  TGraphErrors *ndf_vs_T0 = 0;

  if (useEntropy)
    {
      ndf_vs_T0 = new TGraphErrors(Ncent,T0,ndf_dNchdeta_T0,errT0,errdNchdeta);
      sprintf(title,"ndfdNchdeta_vs_T0");
    }
  else
    {
      ndf_vs_T0 = new TGraphErrors(Ncent,T0,ndf_ET_T0,errT0,errET);
      sprintf(title,"ndfET_vs_T0");
    }  
  ndf_vs_T0->SetName(title); 
  ndf_vs_T0->SetMarkerStyle(28); 
  ndf_vs_T0->SetMarkerSize(1.6); 
  ndf_vs_T0->SetMarkerColor(1); 
  ndf_vs_T0->SetLineWidth(4);
  ndf_vs_T0->SetLineStyle(4);
  ndf_vs_T0->SetLineColor(1);

  l2->AddEntry(ndf0_vs_T0,label1,"l");
  l2->AddEntry(ndf_vs_T0,label2,"l");


  TLegend *l22 = new TLegend(0.566092,0.761062,0.969828,0.984513,label3,"brNDC");
  l22->SetTextSize(0.03);
  l22->SetMargin(0.2);
  l22->SetFillStyle(0);
  l22->SetBorderSize(4);

  c2->cd();
  ndf0_vs_T0->Draw("L");
  //ndf0S_vs_T0->Draw("L");
  ndf_vs_T0->Draw("L"); //Draw("PC");

  //____________________________________________________________________________
  // Fully "empirical" degsOfFreedom_ET_vs_Teff

  TGraphErrors *ndf_vs_Teff[Nslopes2];

  for(int is2=1; is2<Nslopes2; is2++)
    {
      if (useEntropy)
	{
	  ndf_vs_Teff[is2] = new TGraphErrors(Ncent,Teff[is2],ndf_dNchdeta_Teff[is2],TeffErr[is2],errdNchdeta);
	  sprintf(title,"ndf_dNchdeta_vs_Teff%i",is2);
	}
      else
	{
	  ndf_vs_Teff[is2] = new TGraphErrors(Ncent,Teff[is2],ndf_ET_Teff[is2],TeffErr[is2],errET);
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
      //ndf_vs_Teff[is2]->Draw("L"); //("PC");

      is = 2*is2+1;

      sprintf(title,"%3.1f < p_{T} < %3.1f GeV/c",is*0.5,(is+1)*0.5);
      l22->AddEntry(ndf_vs_Teff[is2],title,"l");
    }

  c2->cd();

  if (QGP_EoS) {
    l2->Draw();
    l22->Draw();
  }
  hbar->Draw();
  //TLatex *hbar2 =(TLatex*)hbar->Clone();
  hbar->DrawLatex(0.264481,91.8842,"/");

  //____________________________________________________________________________
  // reduced # of centralities

  TGraphErrors *ndf_vs_Teff_red[5];

  for(int is2=1; is2<Nslopes2; is2++)
    {
      if (is2==Nslopes2-1) ndf_vs_Teff_red[is2]=(TGraphErrors*)reduced_graph_10centralities(ndf_vs_Teff[is2], c2, ypos);
      else ndf_vs_Teff_red[is2]=(TGraphErrors*)reduced_graph_10centralities(ndf_vs_Teff[is2], 0, 0);
      c2->cd();
      ndf_vs_Teff_red[is2]->SetMarkerColor(col[is2]);
      ndf_vs_Teff_red[is2]->SetMarkerStyle(25);
      ndf_vs_Teff_red[is2]->SetMarkerSize(1.8);
      ndf_vs_Teff_red[is2]->SetLineWidth(3);
      ndf_vs_Teff_red[is2]->SetLineColor(col[is2]);
      if (is2==Nslopes2-1) ndf_vs_Teff_red[is2]->Draw("PL");
      else ndf_vs_Teff_red[is2]->Draw("L");
    }

  c2->Update();

  //return;

  //____________________________________________________________________________
  // Plot "semi-empirical" degOfFreedom_e0_vs_Teff

  if (useEntropy)
    {
      sprintf(title,"ndf_s0_vs_Teff");
      sprintf(ytitle,"s_{0}/(4#pi^{2}/90 T_{eff}^{3})"); //"g_{eff}"); 
      ET_max = 100.;
    }
  else
    {
      sprintf(title,"ndf_e0_vs_Teff");
      sprintf(ytitle,"#epsilon_{0}/(#pi^{2}/30 T_{eff}^{4})"); //"g_{eff}");
    }

  TCanvas *c3 = new TCanvas(title,title,700,500);
  c3->Range(0.0822335,-28.6458,0.612183,206.771);
  c3->SetLeftMargin(0.127874);
  c3->SetRightMargin(0.0229885);
  c3->SetTopMargin(0.0287611);
  c3->SetBottomMargin(0.121681);

  double e0_min = 0., e0_max = 550.;
  if (useEntropy)
    {
      e0_max = 200.;
    }

  if (!QGP_EoS) { Teff_min = 0.120; Teff_max = 0.290; e0_max = 200; }

  TH2F *myframe3 = (TH2F*)frame(title, 14,T0_min,T0_max, 14,e0_min,e0_max,xtitle,ytitle);
  myframe3->Draw();

  TLegend *l3 = new TLegend(0.65,0.57,0.95,0.89,"Local thermal #gamma slopes:","brNDC");
  l3->SetMargin(0.2);
  l3->SetFillStyle(0);
  l3->SetBorderSize(4);

  c3->cd();
  ndf0_vs_T0->Draw("L");
  ndf0S_vs_T0->Draw("L");
  //tt->Draw();
  ndf_vs_T0->Draw("L"); //("PC");

  TGraphErrors *ndf0_vs_Teff[Nslopes2];

  for(int is2=0; is2<Nslopes2; is2++)
    {
      if (useEntropy)
	{
	  ndf0_vs_Teff[is2] = new TGraphErrors(Ncent,Teff[is2],ndf_s0_Teff[is2],TeffErr[is2],errs0);
	  sprintf(title,"ndfs0_vs_Teff%i",is2);
	}
      else
	{
	  ndf0_vs_Teff[is2] = new TGraphErrors(Ncent,Teff[is2],ndf_e0_Teff[is2],TeffErr[is2],errET);
	  sprintf(title,"ndfe0_vs_Teff%i",is2);
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

  return;

  //____________________________________________________________________________
  // E_T, dN/deta data and Npart versus b

  //____________________________________________________________________________
  // Plot E_T, dN/deta versus Npart

  TF1 *fNpart_vs_b= new TF1("fNpart_vs_b","pol6/([7]+exp((x-[8])/[9]))",0.,20.); 
  fNpart_vs_b->SetParameters(1142.49,-17.4869,-6.84553,-2.34426,0.414572,-0.0231652,0.000458723,2.98342,11.9418,1.22045)

  double Npart[Ncent];
  double eNpart[Ncent];
  double EtNormNpart2[Ncent];
  double dNchdetaNormNpart2[Ncent];

  double ET_pQCD[12]={70.,56.5,54.8,45.5,42.4,44.0,32.4,21.3,12.6,5.9,2.9,1.5};
  double errET_pQCD[12];

  for(int icen=0; icen<12; icen++)
    {
      ET_pQCD[icen]/=(npart[icen]/2.);
      errET_pQCD[icen]=ET_pQCD[icen]*0.2;
    }

  TGraphErrors *ETpQCD_vs_Npart = new TGraphErrors(12,npart,ET_pQCD,enpart,errET_pQCD);

  TF1 * fitETpQCD = new TF1("fitETpQCD","pol4",0.,1000.) ;
  ETpQCD_vs_Npart->Fit("fitETpQCD") ;
  
  for(int icen=0; icen<Ncent; icen++)
    {

      double b = 14./100.*icen;

      Npart[icen] = Npart_vs_b->Eval(b);
      eNpart[icen] = 0.;//0.1*Npart[icen];
      //cout << "Npart = " << Npart[icen] << " for centrality: " << icen << endl;

      //EtNormNpart2[icen] = etvs_b_phnx->Eval(b)/(Npart[icen]/2.);
      dy_to_deta = 1.;
      EtNormNpart2[icen] = dy_to_deta*E_T[icen]/(Npart[icen]/2.);
      EtNormNpart2[icen]+= fitETpQCD->Eval(Npart[icen]) ;
      dNchdetaNormNpart2[icen] = dndeta_vs_b_phnx->Eval(b)/(Npart[icen]/2.);
      dNchdetaNormNpart2[icen] = Nch_to_Ntot*deta_to_dy*dNchdeta[icen]/(Npart[icen]/2.);
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

  TGraphErrors *T0_vs_Npart = new TGraphErrors(Ncent,Npart,T0,eNpart,errT0);
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
      Teff_vs_Npart[is2] = new TGraphErrors(Ncent,Npart,Teff[is2],eNpart,TeffErr[is2]);
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



//   //____________________________________________________________________________
//   // Plot Teff,T0 vs Npart

//   T0_vs_Npart->Draw("C");

//   TGraphErrors *ndf_vs_T0 = new TGraphErrors(Ncent,T0,EtNorm2,errT0,errET);
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

int plot_dNchdeta_Npart_RHIC( TCanvas *c1)
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

TGraphErrors *reduced_graph_10centralities( TGraphErrors *g, TCanvas *c, double ypos )
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

  // Print centrality labels

  if (c) print_centrality_label( x, ypos, c);


  TGraphErrors *gred = new TGraphErrors(10,x,y,ex,ey);

  return gred;

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

void print_centrality_label(const double *T0_sel, double Tsel_pos, TCanvas *c)
{

  TText *t = new TText();
  t->SetTextSize(0.031);
  t->SetTextAngle(90);
  t->SetTextAlign(21);

  for(int cen = 0; cen<9; cen++)
    {
      TString txt;
      txt+= " "; 
      txt+=10*cen; 
      if (cen==0) txt = "   0";
      txt +="-"; txt+=10*(cen+1); txt += " %";
      t->SetTextColor(1);
      t->SetTextAlign(12);
      c->cd();
      t->DrawText(T0_sel[cen],Tsel_pos,txt);
      cout << "----> plotting text at: " << cen  << " " << T0_sel[cen] << endl;
      //t->DrawText(T0[int(100./14.*impParam[2*(cen-1)])],0.11,txt);
      //cout << "----> plotting text at: " << int(100./14.*impParam[2*(cen-1)]) 
      //<< " " << 100./14.*impParam[2*(cen-1)] << endl;
      //t->DrawText(T0[chiMinPiV[cen]],0.11,txt);
    }

}
