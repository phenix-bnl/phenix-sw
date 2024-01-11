// Plots WA98 or WA80 R_AAs for all centralities
// pp_ref = "blatt"
// fistcent = 1,2,...,10  (= et01,et02, ..., et010)

void plot_RAA_allcent( const char *sps_data="wa98", 
		       const char *pp_ref="blatt",
		       const int firstcent = 1,
		       const int lastcent = 10 )
{

  gSystem->Load("libemcAnalyzer.so");
  emcAnalyzer e; 
  TGraphErrors *eband; 
  e.setVerbose(1);
  e.setFit(false);
  e.setConstrainedHagFit(false);
  e.keepPlot(false);

  TString sps_data_str = sps_data;

  //_____________________________________________________________________________
  // 

  char title[200];
  sprintf(title,"sps_hipt_canvas_all_data");
  TCanvas *sps_hipt_canvas = new TCanvas(title,title,780,620);
  sps_hipt_canvas->Range(-0.644,-0.90,5.06,1.10);
  sps_hipt_canvas->SetLeftMargin(0.11);
  sps_hipt_canvas->SetRightMargin(0.011);
  sps_hipt_canvas->SetTopMargin(0.05);
  sps_hipt_canvas->SetLogy();
  sps_hipt_canvas->cd();

  char legtitle[200];

  double sqrts = 0.;
  double norm = 1.;
  double sigma_NN = 0.;

  const int cent = 10;
  
  char sps_file[300]; 

  TString CCwa98[cent] = {"82-100%","66-82%","48-66% peripheral","25-48%",
			  "13-25%","7-13%","1-7% central","0-1% central","0-13% central","0-7% central"};

  TString CCwa80[cent] = {"70-100","50-70","35-50","25-35","15-25","8-15","3-8","3","8","-"};

  if (sps_data_str.Contains("wa98",TString::kIgnoreCase))
    {
      double ncollwa98[cent] = {10. ,30., 78. ,207.,408.,569.,712.,807.,651.,726.}; // paper + last one= (807+712*5.8)/6.8
      double encollwa98[cent] = {2.5, 5., 12. ,21.,41.,57.,71.,81.,65.,73.}; // paper + last one
      //double ncollwa98[cent] = { 2.9, 9.9,37.6,150.,373.,573.,756.,894.,682.,776}; // glauber MC 
      sqrts = 17.3;
      sprintf(legtitle,"Pb+Pb #rightarrow #pi^{0}+X (#sqrt{s} = %3.1f GeV) [WA98]",sqrts);
    }
  else if (sps_data_str.Contains("wa80",TString::kIgnoreCase))
    {
      // all Ncoll values are from Klaus' Glauber MC, consistent with Npart values published in WA80 paper
      double ncollwa80[cent] = {3.2,13.9,36.3,64.8,99.3,138.,167.,186.,174.,0.};
      sqrts = 19.4;
      sprintf(legtitle,"S+Au #rightarrow #pi^{0}+X (#sqrt{s} = %3.1f GeV) [WA98]",sqrts);
    }

  TLegend *leg = new TLegend(0.115772,0.811024,0.90604,0.98622,legtitle,"brNDC");
  leg->SetMargin(0.1);
  leg->SetBorderSize(4);
  leg->SetFillColor(kWhite);
  leg->SetTextSize(0.0362);


  sigma_NN = e.sigma_pp_inel(sqrts);

  TGraphErrors *sps_hipt[cent];
  TLegendEntry *entry;

  int loop = 1;

  for (int i=firstcent-1;i<lastcent;i++)
    //for (int i=firstcent;i<cent;i++)
    {

      if (sps_data_str.Contains("wa98",TString::kIgnoreCase))
	{
	  //sprintf(sps_file,"original_klaus/wa98_pi0_et0%d.dat",i+1); 
	  //sprintf(sps_file,"wa98_pi0_et0%d_rebin200MeV_1.dat",i+1); 
	  //sprintf(sps_file,"wa98_pi0_et0%d_rebin200MeV_2.dat",i+1); 
	  sprintf(sps_file,"wa98_pi0_et0%d.dat",i+1); 
          entry=leg->AddEntry(sps_hipt[i],CCwa98[i],"P");
	}
      else if (sps_data_str.Contains("wa80",TString::kIgnoreCase))
	{
	  sprintf(sps_file,"wa80_sigpi0_SAU_et0%d.dat",i+1);  
          entry=leg->AddEntry(sps_hipt[i],CCwa80[i],"P");
	}

      sps_hipt[i] = (TGraphErrors*)e.plot_spectrum_from_ascii(sps_file,eband);

      // mods. ...
      //for (int r=0;r<1;r++) sps_hipt[i]->RemovePoint(r);
      //sps_hipt[i] = (TGraphErrors*)emcAnalyzerUtils::rebin(sps_hipt[i],3);
      //emcAnalyzerUtils::scale(*sps_hipt[i],ncoll[i]);

      sps_hipt[i]->SetMaximum(1000.);
      sps_hipt[i]->SetMinimum(1e-08);
      sps_hipt[i]->SetMarkerStyle(20);
      sps_hipt[i]->SetMarkerColor(loop);
      sps_hipt[i]->SetMarkerSize(1.7);
      if (loop==1) sps_hipt[i]->Draw("AP");
      else sps_hipt[i]->Draw("P");

      loop++;
    }

  //_____________________________________________________________________________
  // p+p reference

  TF1* f_param_sps = (TF1*)e.plot_pp_sigma_parametrization("pi0",pp_ref,sqrts,1./sigma_NN);
  f_param_sps->SetRange(0.5,5.0);
  f_param_sps->SetLineColor(1);
  //sps_hipt_canvas->cd();
  f_param_sps->Draw("same");

  //_____________________________________________________________________________
  // Nuclear modifs. factors (R_AA)

  sprintf(title,"RAA_allcent_%s_%s_%d_%d",sps_data,pp_ref,firstcent,lastcent);
  //char *ytitle = "Nuclear modification factor R_{AA}";
  char *ytitle = " R_{AA} ";

  TCanvas *c10 = new TCanvas(title,title,780,620);
  c10->Range(-0.644,-0.90,5.06,1.10);
  c10->SetLeftMargin(0.11);
  c10->SetRightMargin(0.011);
  c10->SetTopMargin(0.05);
  c10->SetLogy();
  c10->cd();

  double RAA_max = 10.;
  double RAA_min = 0.2;
  double ptmax = 4.5;

  TH2F *frame = new TH2F("frame","frame",10,0.,ptmax,20,RAA_min,RAA_max);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle(ytitle);
  frame->SetXTitle("p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(0.8);
  frame->GetYaxis()->SetTitleSize(0.055);
  frame->GetYaxis()->SetLabelSize(0.055);
  frame->GetXaxis()->SetTitleOffset(0.9);
  frame->GetXaxis()->SetTitleSize(0.055);
  frame->GetXaxis()->SetLabelSize(0.055);
  frame->GetYaxis()->SetNoExponent();
  frame->GetYaxis()->SetMoreLogLabels();
  frame->Draw();
  c10->SetLogy();

  //=============================================================================

  double normerr = sqrt(0.10*0.10+0.25*0.25);  
  //TH1F *eNormBox = new TH1F("eNormBox","",6,0.35,0.55);
  TH1F *eNormBox = new TH1F("eNormBox","",5,-0.5,5.);
  for (int j=0;j<=6;j++){
    eNormBox->SetBinContent(j,1.);
    eNormBox->SetBinError(j,normerr);
  }
  eNormBox->SetFillColor(18);
  //eNormBox->SetFillStyle(3001);
  eNormBox->Draw("e3same");

  normerr = sqrt(0.15*0.15+0.25*0.25);  
  TH1F *eNormBox2 = new TH1F("eNormBox2","",6,0.1,0.3);
  for (int j=0;j<=6;j++){
    eNormBox2->SetBinContent(j,1.);
    eNormBox2->SetBinError(j,normerr);
  }
  eNormBox2->SetFillColor(18);
  //eNormBox2->SetFillStyle(3001);
  //eNormBox2->Draw("e3same");

  //=============================================================================

  TGraphErrors *vitev0 = (TGraphErrors*)e.plot_spectrum_from_ascii("R_AA_theor_vitev_sps_dndy0.txt",eband);
  TGraphErrors *vitev0a = (TGraphErrors*)vitev0->Clone();
  TGraphErrors *vitev0b = (TGraphErrors*)vitev0->Clone();
  emcAnalyzerUtils::scale(*vitev0a,0.9);
  emcAnalyzerUtils::scale(*vitev0b,1.1);
  TClonesArray*eb=(TClonesArray*)emcAnalyzerUtils::errorBand(vitev0a,vitev0b);
  TGraph *shadeband = (TGraph*)(*eb)[0]; 
  c10->cd();
  vitev0a->Draw("L");
  vitev0b->Draw("L");
  shadeband->Draw("f");
  frame->Draw("same");

  //=============================================================================

  TLegend *leg2 = new TLegend(0.114,0.80,0.67,0.98,legtitle,"brNDC");
  leg2->SetMargin(0.25);
  leg2->SetBorderSize(4);
  leg2->SetFillColor(kWhite);
  leg2->SetTextSize(0.040);

  double pT = 0.;
  double epT = 0.;
  double ey = 0.;
  double y = 0.;

  double RAA = 0.;
  double eRAA = 0.;

  double ncoll = 0.;

  TGraphErrors *RAA_sps[cent];
  loop = 1;

  bool switchorder7_8 = false;
  bool switchorder7_9 = false;

  for (int j=firstcent-1;j<lastcent;j++)
    //for (int j=firstcent;j<cent;j++)
    {

      // all this mess to change the order in which the labels appear in the plot ...
//       if (!switchorder7_8)
// 	{
// 	  if (j==7) j=8;
// 	  else if (j==9)
// 	    {
// 	      j=7;
// 	      switchorder7_8 = true;
// 	    }
// 	}
//       else
// 	{
// 	  j=9;
// 	}
      if (j==7 && !switchorder7_9)
	{
	  j=9;
	}
      if (j==9 && switchorder7_9)
	{
	  continue;
	}

      RAA_sps[j] = (TGraphErrors*)sps_hipt[j]->Clone();
      int N = sps_hipt[j]->GetN();

      for(int i=0;i<=N;i++)
	{
	  sps_hipt[j]->GetPoint(i,pT,y);
	  epT = sps_hipt[j]->GetErrorX(i);
	  ey = sps_hipt[j]->GetErrorY(i);

	   if (sps_data_str.Contains("wa98",TString::kIgnoreCase))
	     ncoll = ncollwa98[j];
	   else if (sps_data_str.Contains("wa80",TString::kIgnoreCase))
	     ncoll = ncollwa80[j];

	  RAA = y/(ncoll*f_param_sps->Eval(pT));
	  eRAA = ey/(ncoll*f_param_sps->Eval(pT));
	  
	  RAA_sps[j]->SetPoint(i,pT,RAA);
	  RAA_sps[j]->SetPointError(i,epT,eRAA);

	  printf("%4.2f  %g   %g   %g\n",pT,RAA,epT,eRAA);
	}

      cout << "<I> " << RAA_sps[j]->GetName() << " (" << CCwa98[j] 
	   << ") normalized by Ncoll=" << ncoll << endl << endl; 

      RAA_sps[j]->SetMarkerColor(loop);

      if (sps_data_str.Contains("wa98",TString::kIgnoreCase))
	{
          entry=leg->AddEntry(sps_hipt[j],CCwa98[j],"P");
	}
      else if (sps_data_str.Contains("wa80",TString::kIgnoreCase))
	{
          entry=leg->AddEntry(sps_hipt[j],CCwa80[j],"P");
	}

      // selected centralities
      if ( j==2 || j==7 || j==9 ) // 2=et03, 7=et08, 8=et09, 9=et010
	{
	  //RAA_sps[j]->SetMarkerColor(j-1);
	  RAA_sps[j]->Draw("P");

	  int col[cent]={0,0,1,0,0,0,0,13,0,1};
	  int mark[cent]={0,0,26,0,0,0,0,22,0,20};
	  double size[cent]={0,0,1.7,0,0,0,0,1.9,0,1.7};

	  RAA_sps[j]->SetMarkerColor(col[j]);
	  RAA_sps[j]->SetMarkerStyle(mark[j]);
	  RAA_sps[j]->SetMarkerSize(size[j]);

	  if (sps_data_str.Contains("wa98",TString::kIgnoreCase))
	    {
	      entry=leg2->AddEntry(RAA_sps[j],CCwa98[j],"P");
	    }
	  else if (sps_data_str.Contains("wa80",TString::kIgnoreCase))
	    {
	      entry=leg2->AddEntry(RAA_sps[j],CCwa80[j],"P");
	    }
	}

      loop++;

      if (j==9 && !switchorder7_9)
	{
	  j=6;
	  switchorder7_9 = true;
	}
    }

  //=============================================================================

  leg2->Draw();

  TLine *line = new TLine(0,1.,ptmax,1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

 
  c10->Update();

}

