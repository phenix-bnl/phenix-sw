
void compute_ET_Nch_pQCD( const bool compute_ET = true , 
			  const char *path="/afs/rhic.bnl.gov/phenix/users/enterria/thermal_photons/jan05/" )
{
  
  char filename[500];

  double ymax = 100;
  double ymin = 1e-7;
  double ptmax = 10.;
  double ptmin = 0.;

  char title[200];
  char ytitle[200];
  if (compute_ET)
    {
      sprintf(title,"nlo_spec_AuAu200GeV_ET");
      sprintf(ytitle,"p_{T}#times dN/dp_{T}");
      ymax = 100.;
    }
  else 
    {
      sprintf(title,"nlo_spec_AuAu200GeV_Nch");
      sprintf(ytitle,"dN/dp_{T}");
    }
  TCanvas * c = new TCanvas(title,title,650,700);
  c->Range(-0.830387,-7.30357,5.12994,4.33929);
  c->SetLeftMargin(0.139319);
  c->SetRightMargin(0.0201238);
  c->SetTopMargin(0.0291411);
  c->SetBottomMargin(0.111963);
  c->SetLogy(1);
  c->SetFillColor(10);
  c->SetFillStyle(4000);


  TH2F *frame = (TH2F*)frame("bla", 10, ptmin, ptmax, 20, ymin, ymax, "p_{T} (GeV/c)",ytitle);
  frame->GetXaxis()->SetRangeUser(ptmin,ptmax);
  frame->GetXaxis()->SetTitleOffset(1.);
  frame->GetXaxis()->SetTitleSize(0.05);
  frame->GetYaxis()->SetTitleOffset(1.2);
  frame->GetYaxis()->SetTitleSize(0.05);
  frame->SetTitle("");
  frame->SetStats(0);

  //____________________________________________________________________________
  // NLO pQCD scaled by Glauber Npart or Ncoll

  //
  // Cent.Class --> b +/- eb   Ncoll +/- eNcoll   Npart +/- eNpart   
  //   (0,1%)  -->   1.0         | 1180.1         | 374.2  
  //   (0,5%)  -->   2.3 +/- 0.1 | 1065.4 +/- 105 | 351.4 +/- 2.9
  //  (5,10%)  -->   4.1 +/- 0.2 | 845.4 +/- 82.1 | 299.0 +/- 3.8
  // (10,15%)  -->   5.2 +/- 0.3 | 672.4 +/- 66.8 | 253.9 +/- 4.3
  // (15,20%)  -->   6.2 +/- 0.2 | 532.7 +/- 52.1 | 215.3 +/- 5.3
  // (20,30%)  -->   7.4 +/- 0.4 | 373.7 +/- 39.6 | 166.6 +/- 5.2
  // (30,40%)  -->   8.8 +/- 0.4 | 219.8 +/- 23.2 | 114.2 +/- 4.6
  // (40,50%)  -->   9.9 +/- 0.4 | 120.3 +/- 13.8 |  74.4 +/- 3.9 
  // (50,60%)  -->  10.9 +/- 0.4 |  61.0 +/- 10.1 |  45.5 +/- 3.4 
  // (60,70%)  -->  11.9 +/- 0.5 |  28.5 +/-  7.6 |  25.6 +/- 3.9 
  // (70,80%)  -->  12.8 +/- 0.6 |  12.3 +/-  4.2 |  13.4 +/- 3.0 
  // (80,92%)  -->  14.1 +/- 0.6 |   4.9 +/-  1.2 |   6.3 +/- 1.2 
  
  //  (0,10%)  -->   3.2 +/- 0.2 | 955.4 +/- 93.7 | 325.2 +/- 3.4 
  // (10,20%)  -->   5.7 +/- 0.3 | 602.6 +/- 59.5 | 234.6 +/- 4.8
  // (60,92%)  -->  13.0 +/- 0.5 |  14.5 +/-  4.0 |  14.5 +/- 2.5 
  // (0,100%)  -->   9.5 +/- 0.4 | 257.8 +/- 25.4 | 109.1 +/- 4.1
 
  const int nCent = 12;

  TString CentClass[nCent] = {"00-01","00-05","05-10","10-15","15-20","20-30","30-40",
			      "40-50","50-60","60-70","70-80","80-92"};
  double Ncoll[nCent] = {1200.,1065.4, 845.4, 672.4, 532.7, 373.7, 219.8, 120.3, 61.0, 28.5, 12.3, 4.9};
  double suppr[nCent] = {0.18,0.18,0.22,0.23,0.27,0.4,0.5,0.6,0.7,0.7,0.8,1.0};

  //double Npart[nCent] = {197.*2., 351.4, 299.0, 253.9, 215.3, 166.6, 114.2,  74.4, 45.5, 25.6, 13.4, 6.3};

  double sigma_pp = 42.;
  double pQCDerr = 0.20; // 20% scale uncertainty
  
  const int nBins = 17;
  double pt, sigma;

  double integrand = 0.;
  double eta_y_jacobian = 0.;
  double mt = 0.;

  int nloLine = 1;
  int nloWidth = 2;

  double fitptmin = 1.25, fitptmax = 100.;
  TF1 *hag = new TF1("hag", "[0]/(1+x/[1])^[2]", fitptmin, fitptmax);
  hag->SetParameters(10,1.0,8);

  //____________________________________________________________________________
  // NLO Pions

  sprintf(filename,"%s/pQCD_vogelsang_pp_pi0_200GeV_cteq6_sc1_kkp.dat",path);
  ifstream inpQCD(filename);
  if (!inpQCD)
    {
      cout << " <E> Can't open file: " << filename << endl;
      return 0;
    }

  double mass = 0.135;
  double ET_Nch_pi[nCent] ;

  double pQCDptPi[nCent][nBins];
  double pQCDeptPi[nCent][nBins];
  double pQCDNPiCen[nCent][nBins];
  double pQCDeNPiCen[nCent][nBins];

  TGraphErrors *pQCDPiCen[nCent];
  TH1F *hpQCDPiCen[nCent];

  for(int ipt = 0; ipt < nBins; ipt++)
    {
      inpQCD >> pt >> sigma ;
      
      sigma*=1.e-9; // pb --> mb
      sigma*=2*TMath::Pi()*pt; // (1/pt)*dsigma/dptdphi --> dsigma/dpt
      sigma/=sigma_pp; // dsigma --> dN
      
      mt = sqrt(pt*pt+mass*mass);

      for(int icen = 1; icen < nCent; icen++)
	{
	  pQCDptPi[icen][ipt]=pt;

	  if (compute_ET)
	    {
	      integrand = pt;
	      eta_y_jacobian = (mt+mass)/mt;
	    }
	  else
	    {
	      integrand = 1.;
	      eta_y_jacobian = pt/mt;
	    }

	  pQCDNPiCen[icen][ipt]=integrand*eta_y_jacobian*sigma*suppr[icen]*Ncoll[icen];
	  pQCDeNPiCen[icen][ipt]=pQCDNPiCen[icen][ipt]*pQCDerr;
	  pQCDeptPi[icen][ipt]=0.001;
	}
    }
  inpQCD.close();

  for(int icen = 1; icen < nCent; icen++)
    {
      pQCDPiCen[icen] = new TGraphErrors(nBins,pQCDptPi[icen],pQCDNPiCen[icen],pQCDeptPi[icen],pQCDeNPiCen[icen]);
      sprintf(title,"pQCDPi_Cen%s",CentClass[icen].Data());
      pQCDPiCen[icen]->SetTitle(title);
      
      pQCDPiCen[icen]->SetLineStyle(nloLine);
      pQCDPiCen[icen]->SetLineColor(4);
      pQCDPiCen[icen]->SetLineWidth(nloWidth);
      c->cd();
      //if (icen==1) 
      pQCDPiCen[icen]->Draw("l");
      pQCDPiCen[icen]->Fit(hag);
      hag->Draw("same");
      hag->SetLineColor(4);

      ET_Nch_pi[icen] = hag->Integral(fitptmin,fitptmax);

    }

  c->Update();

  //return;
  //____________________________________________________________________________
  // NLO Kaons

  sprintf(filename,"%s/pQCD_vogelsang_pp_k+-_200GeV_cteq6_sc1_kkp.dat",path);
  ifstream inpQCD(filename);
  if (!inpQCD)
    {
      cout << " <E> Can't open file: " << filename << endl;
      return 0;
    }

  mass = 0.550;
  double ET_Nch_K[nCent];

  double pQCDptK[nCent][nBins];
  double pQCDeptK[nCent][nBins];
  double pQCDNKCen[nCent][nBins];
  double pQCDeNKCen[nCent][nBins];

  TGraphErrors *pQCDKCen[nCent];
  TH1F *hpQCDKCen[nCent];

  for(int ipt = 0; ipt < nBins; ipt++)
    {
      inpQCD >> pt >> sigma ;
      
      sigma*=1.e-9; // pb --> mb
      sigma*=2*TMath::Pi()*pt; // (1/pt)*dsigma/dptdphi --> dsigma/dpt
      sigma/=sigma_pp; // dsigma --> dN
      
      mt = sqrt(pt*pt+mass*mass);

      for(int icen = 1; icen < nCent; icen++)
	{
	  pQCDptK[icen][ipt]=pt;

	  if (compute_ET)
	    {
	      integrand = pt;
	      eta_y_jacobian = (mt+mass)/mt;
	    }
	  else
	    {
	      integrand = 1.;
	      eta_y_jacobian = pt/mt;
	    }

	  pQCDNKCen[icen][ipt]=integrand*eta_y_jacobian*sigma*suppr[icen]*Ncoll[icen];
	  pQCDeNKCen[icen][ipt]=pQCDNKCen[icen][ipt]*pQCDerr;
	  pQCDeptK[icen][ipt]=0.001;
	}
    }
  inpQCD.close();

  for(int icen = 1; icen < nCent; icen++)
    {
      pQCDKCen[icen] = new TGraphErrors(nBins,pQCDptK[icen],pQCDNKCen[icen],pQCDeptK[icen],pQCDeNKCen[icen]);
      sprintf(title,"pQCDK_Cen%s",CentClass[icen].Data());
      pQCDKCen[icen]->SetTitle(title);
      
      pQCDKCen[icen]->SetLineStyle(nloLine);
      pQCDKCen[icen]->SetLineColor(2);
      pQCDKCen[icen]->SetLineWidth(nloWidth);
      c->cd();
      //if (icen==1) 
      pQCDKCen[icen]->Draw("l");
      pQCDKCen[icen]->Fit(hag);
      hag->Draw("same");
      hag->SetLineColor(2);

      ET_Nch_K[icen] = hag->Integral(fitptmin,fitptmax);

    }

  c->Update();

  //return;

  //____________________________________________________________________________
  // NLO protons

  sprintf(filename,"%s/pQCD_vogelsang_pp_proton_200GeV_cteq6_sc1_kkp.dat",path);
  ifstream inpQCD(filename);
  if (!inpQCD)
    {
      cout << " <E> Can't open file: " << filename << endl;
      return 0;
    }

  double ET_Nch_P[nCent];
  mass = 0.931;

  double pQCDptP[nCent][nBins];
  double pQCDeptP[nCent][nBins];
  double pQCDNPCen[nCent][nBins];
  double pQCDeNPCen[nCent][nBins];

  TGraphErrors *pQCDPCen[nCent];
  TH1F *hpQCDPCen[nCent];

  for(int ipt = 0; ipt < nBins; ipt++)
    {
      inpQCD >> pt >> sigma ;
      
      sigma*=1.e-9; // pb --> mb
      sigma*=2*TMath::Pi()*pt; // (1/pt)*dsigma/dptdphi --> dsigma/dpt
      sigma/=sigma_pp; // dsigma --> dN
      
      mt = sqrt(pt*pt+mass*mass);

      for(int icen = 1; icen < nCent; icen++)
	{
	  pQCDptP[icen][ipt]=pt;

	  if (compute_ET)
	    {
	      integrand = pt;
	      eta_y_jacobian = (mt+mass)/mt;
	    }
	  else
	    {
	      integrand = 1.;
	      eta_y_jacobian = pt/mt;
	    }

	  pQCDNPCen[icen][ipt]=integrand*eta_y_jacobian*sigma*suppr[icen]*Ncoll[icen];
	  pQCDeNPCen[icen][ipt]=pQCDNPCen[icen][ipt]*pQCDerr;
	  pQCDeptP[icen][ipt]=0.001;
	}
    }
  inpQCD.close();

  for(int icen = 1; icen < nCent; icen++)
    {
      pQCDPCen[icen] = new TGraphErrors(nBins,pQCDptP[icen],pQCDNPCen[icen],pQCDeptP[icen],pQCDeNPCen[icen]);
      sprintf(title,"pQCDP_Cen%s",CentClass[icen].Data());
      pQCDPCen[icen]->SetTitle(title);
      
      pQCDPCen[icen]->SetLineStyle(nloLine);
      pQCDPCen[icen]->SetLineColor(1);
      pQCDPCen[icen]->SetLineWidth(nloWidth);
      c->cd();
      pQCDPCen[icen]->Draw("l");
      pQCDPCen[icen]->Fit(hag);
      hag->Draw("same");
      hag->SetLineColor(1);

      ET_Nch_P[icen] = hag->Integral(fitptmin,fitptmax);

    }

  c->Update();

  //return;

  //____________________________________________________________________________
  // Total E_T, Nch

  double ET_Nch_tot[nCent];

  double piplus_to_pitot = 3.; // 3 species: +,-,neutral
  double Kplus_to_Ktot = 4.; // 4 species: K+,-, K0s, K0L
  double Pplus_to_Ptot = 4.; // 4 species: p,pbar,n,nbar

  for(int icen = 1; icen < nCent; icen++)
    {
      ET_Nch_tot[icen] = piplus_to_pitot*ET_Nch_pi[icen]+Kplus_to_Ktot*ET_Nch_K[icen]+Pplus_to_Ptot*ET_Nch_P[icen];

      if (compute_ET)
	{
	  cout << " Cent = " << CentClass[icen].Data() << "% -->  <E_T>[pQCD] = " << ET_Nch_tot[icen] << " GeV";
	}
      else
	{
	  cout << " Cent = " << CentClass[icen].Data() << "% -->  <N_ch>[pQCD] = " << ET_Nch_tot[icen];
	}
      cout << endl;
    }

  cout << endl;

  if (compute_ET)
    {
      cout << " double ET_pQCD[" << nCent-1 << "]={";
    }
  else
    {
      cout << " double Nch_pQCD[" << nCent-1 << "]={";
    }
  for(int icen = 1; icen < nCent; icen++) { cout << ET_Nch_tot[icen] << "," ; }
  cout << "};" << endl;

  return;
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
