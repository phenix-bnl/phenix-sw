//
// type = "data_over_fit" or "data_minus_fit_over_fit"

//=============================================================================
//

const double percentage = 100.;

TGraphErrors *comparison(TGraphErrors *data, TF1 *fit, char* type )
{
  TString type_str = type;
  double subtract = 0.;

  if ( type_str.Contains("minus",TString::kIgnoreCase) )
    {
      subtract = -1.;
    }

  data = (TGraphErrors*)emcAnalyzerUtils::add(data,fit,subtract);
  data = (TGraphErrors*)emcAnalyzerUtils::ratio(data,fit);

  emcAnalyzerUtils::scale(*data,percentage,0,2); // scale by 100. to plot in percentages

  return data;

}

//=============================================================================
//
void plot_in_double_canvas( TGraphErrors *comparison,
			    TPad *subcanvas, 
			    TH2 *frame, 
			    const double bottmarg, const double topmarg, 
			    const int pass )
{
  subcanvas->cd();
  subcanvas->SetBottomMargin(bottmarg);
  subcanvas->SetTopMargin(topmarg);
  subcanvas->SetRightMargin(0.02);
  subcanvas->SetTickx();
  TH2F* frame2 = (TH2F*)frame->Clone();
  if (pass==0)
    {
      frame2->GetYaxis()->SetTitleOffset(0.6);
      frame2->GetYaxis()->SetTitleSize(0.09);
      frame2->GetYaxis()->SetLabelSize(0.09);
      frame2->GetYaxis()->SetNdivisions(805);

      frame2->GetXaxis()->SetTitleOffset(0.9);
      frame2->GetXaxis()->SetTitleSize(0.1);
      frame2->GetXaxis()->SetLabelSize(0.1);
      frame2->Draw();
    }
  if (pass==-1)
    {
      frame2->GetYaxis()->SetTitleOffset(0.5);
      frame2->GetYaxis()->SetTitleSize(0.1);
      frame2->GetYaxis()->SetLabelSize(0.1);
      frame2->GetYaxis()->SetNdivisions(805);

      frame2->GetXaxis()->SetTitleOffset(9);
      frame2->GetXaxis()->SetTitleSize(0.);
      frame2->GetXaxis()->SetLabelSize(0.);
      frame2->Draw();

      double normerr = 0.2*percentage;
      TH1F *eNormBox = new TH1F("eNormBox","",8,-0.5,7.5);
      for (int j=0;j<=8;j++){ eNormBox->SetBinContent(j,1.); eNormBox->SetBinError(j,normerr);}
      eNormBox->SetFillColor(18);
      //eNormBox->SetFillStyle(3001);
      eNormBox->Draw("e3same");

    }
  comparison->Draw("P");
}

//=============================================================================
//
void data_vs_parametrizations( double sqrts = 19.4,
			       char* type = "data_minus_fit_over_fit" )
{

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e;
  TGraphErrors *eband; 
  e.setVerbose(0);
  e.setFit(false);
  e.setConstrainedHagFit(false);
  //e.keepPlot(false);

  TString type_str = type;

  //_____________________________________________________________________________
  // Spectra canvas

  TCanvas *spectra_canvas = new TCanvas("spectra_canvas","data_vs_params",600,600);
  spectra_canvas->SetRightMargin(0.02);
  spectra_canvas->SetTopMargin(0.013);
  spectra_canvas->SetLogy();
  spectra_canvas->cd();

  char *title  = "pi0_pp_plab200_hipt";
  char *ytitle  = "E d^{3}#sigma/dp^{3} (mb GeV^{-2}c^{3}";

  double xmin = 0.;
  double xmax = 7.;

  TH2F *frame = new TH2F("frame","frame",10,xmin,xmax,20,1e-11,100);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle(ytitle);
  frame->SetXTitle("p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.3);
  frame->GetYaxis()->SetTitleSize(0.04);
  frame->GetXaxis()->SetTitleOffset(0.9);
  frame->GetXaxis()->SetTitleSize(0.05);
  frame->Draw();

  TLegend *leg = new TLegend(0.305369,0.809783,0.880872,0.95471,NULL,"brNDC");
  leg->SetFillColor(kWhite);
  //leg->SetLineColor(1);
  //leg->SetLineStyle(1);
  //leg->SetLineWidth(1);
  //leg->SetFillStyle(0);
  //leg->SetBorderSize(0);
  leg->SetTextSize(0.034);
  TLegendEntry *entry = 0;
  leg->Draw();

  spectra_canvas->Update();

  //_____________________________________________________________________________
  // p+p parametrizations

  double ptmin = 0.5;
  double ptmax = 7.0;
  double norm = 1.;

  e.keepPlot(false);

  // WA98 parametrization for input sqrt(s)

  TF1* f_wa98 = (TF1*)e.plot_pp_sigma_parametrization("pi0","wa98",sqrts);
  TF1* f_wa98_150 = (TF1*)e.plot_pp_sigma_parametrization("pi0","wa98",16.9); // Elab = 150 GeV/c
  TF1* f_wa98_159 = (TF1*)e.plot_pp_sigma_parametrization("pi0","wa98",17.4); // Elab = 159 GeV/c
  TF1* f_wa98_175 = (TF1*)e.plot_pp_sigma_parametrization("pi0","wa98",18.2); // Elab = 175 GeV/c

  f_wa98->SetRange(ptmin,ptmax);
  f_wa98->SetLineColor(1);
  entry=leg->AddEntry(f_wa98,"p+p #rightarrow #pi^{0} fit: WA98 Collab. [EPJ C23, 225(2002)]","L");

  // Wang: pi's in p+p at sqrt(s) = 17.3, 19.4 GeV

  TF1*f_xnwang  = (TF1*)e.plot_pp_sigma_parametrization("pi0","xnwang",sqrts);
  TF1*f_xnwang_159 = (TF1*)e.plot_pp_sigma_parametrization("pi0","xnwang",17.4); // Elab = 159 GeV/c

  f_xnwang->SetRange(ptmin,ptmax);
  f_xnwang->SetLineColor(2);
  entry=leg->AddEntry(f_xnwang,"p+p  #rightarrow #pi^{0} fit: E. Wang & X.N.Wang [PRC 64, 034901(2001)]","L");

  // Blattnig's parametrization for input sqrt(s)

  TF1* f_blatt = (TF1*)e.plot_pp_sigma_parametrization("pi0","blatt",sqrts);
  TF1* f_blatt_150 = (TF1*)e.plot_pp_sigma_parametrization("pi0","blatt",16.9); // Elab = 150 GeV/c
  TF1* f_blatt_159 = (TF1*)e.plot_pp_sigma_parametrization("pi0","blatt",17.4); // Elab = 159 GeV/c
  TF1* f_blatt_175 = (TF1*)e.plot_pp_sigma_parametrization("pi0","blatt",18.2); // Elab = 175 GeV/c

  f_blatt->SetRange(ptmin,ptmax);
  f_blatt->SetLineColor(4);
  entry=leg->AddEntry(f_blatt,"p+p  #rightarrow #pi^{0} fit: R. Blattnig et al. [PRD 62, 094030(2000)]","L");

  //_____________________________________________________________________________
  // plab=200 GeV pi0 data

  e.keepPlot();
  
  const int dataSets = 10; // 7;
  char data_file[dataSets][300]; 
  char data_leg[dataSets][300]; 

  int markerStyle[dataSets]=  {20 ,22 ,25 ,22 ,30 ,24 ,27 ,24 ,26 ,29 };
  double markerSize[dataSets]={1.4,1.6,1.4,2.1,2.1,1.6,2.1,1.6,1.6,2.1};

  sprintf(data_file[0],"pi0_pp_plab150_carey.txt");
  sprintf(data_file[1],"pi0_pp_plab159_carey.txt");
  sprintf(data_file[2],"pi0_pp_plab175_carey.txt");
  sprintf(data_file[3],"pi0_pp_plab200_carey.txt");
  sprintf(data_file[4],"pi0_pp_plab200_donaldson.txt");

  sprintf(data_file[5],"pi+_pp_plab200_antrea.txt");
  sprintf(data_file[6],"pi-_pp_plab200_antrea.txt");

  sprintf(data_file[7],"pi0_pp_plab200_adams.txt");
  sprintf(data_file[8],"pi0_ppbar_plab200_adams.txt");

  sprintf(data_file[9],"pi-pi+_pp_plab200_antrea.txt");

  sprintf(data_leg[0],"Carey et al., PRD14, 1196(76):  p+p #rightarrow #pi^{0} (#sqrt{s} = 16.9 GeV)");
  sprintf(data_leg[1],"Carey et al., PRD14, 1196(76):  p+p #rightarrow #pi^{0} (#sqrt{s} = 17.4 GeV)");
  sprintf(data_leg[2],"Carey et al., PRD14, 1196(76):  p+p #rightarrow #pi^{0} (#sqrt{s} = 18.2 GeV)");
  sprintf(data_leg[3],"Carey et al., PRD14, 1196(76):  p+p #rightarrow #pi^{0} (#sqrt{s} = 19.4 GeV)");
  sprintf(data_leg[4],"Donaldson et al., PRL 36, 1110(76):  p+p #rightarrow #pi^{0} (#sqrt{s} = 19.4 GeV)");

  sprintf(data_leg[5],"Antreasyan et al., PRD19,764(79):  p+p #rightarrow #pi^{+} (#sqrt{s} = 19.4 GeV)");
  sprintf(data_leg[6],"Antreasyan et al., PRD19,764(79):  p+p #rightarrow #pi^{-} (#sqrt{s} = 19.4 GeV)");

  sprintf(data_leg[7],"E704 Coll., PRD53, 4747(96):  p+p #rightarrow #pi^{0} (#sqrt{s} = 19.4 GeV)");
  sprintf(data_leg[8],"E704 Coll., PRD53, 4747(96):  p+#bar{p} #rightarrow #pi^{0} (#sqrt{s} = 19.4 GeV)");

  sprintf(data_leg[9],"Antreasyan et al., PRD19,764(79):  p+p #rightarrow (#pi^{-}+#pi^{+})/2 (#sqrt{s} = 19.4 GeV)");

  TGraphErrors *data_hipt[dataSets];
  TGraphErrors *data_hiptScaled[dataSets];

  //_____________________________________________________________________________
  // setup for plot ratio data over fit

  char *ytitle = "data/fit";
  double ymin = 0.;
  double ymax = 3.5;
  double yref = 1.;

  if ( type_str.Contains("minus",TString::kIgnoreCase) ) 
    {
      ytitle = "(data-fit)/fit (%)";
      ymax = 2.5*percentage;
      ymin = -2.5*percentage;
      yref = 0.;
    }
  
  TH2F *frame2 = new TH2F("frame2","frame2",10,xmin,xmax,20,ymin,ymax);
  frame2->SetStats(0);
  frame2->SetTitle(type);
  frame2->SetYTitle(ytitle);
  frame2->SetXTitle("p_{T} (GeV/c)");
  frame2->GetYaxis()->SetTitleOffset(0.9);
  frame2->GetYaxis()->SetTitleSize(0.055);
  frame2->GetYaxis()->SetLabelSize(0.055);
  frame2->GetXaxis()->SetTitleOffset(0.9);
  frame2->GetXaxis()->SetTitleSize(0.055);
  frame2->GetXaxis()->SetLabelSize(0.055);

  TLegend *leg2 = new TLegend(0.305369,0.809783,0.880872,0.95471,//"p+p #rightarrow #pi+X (#sqrt{s} = 19.4 GeV)"
			      NULL,"brNDC");
  //leg2->SetLineColor(1);
  //leg2->SetLineStyle(1);
  //leg2->SetLineWidth(1);
  //leg2->SetFillStyle(1);
  leg2->SetBorderSize(3);
  leg2->SetFillColor(kWhite);
  leg2->SetTextSize(0.034);
  TLegendEntry *entry2 = 0;

  TCanvas *c_fwa98 = new TCanvas("c_fwa98","data_over_fwa98",750,500);
  c_fwa98->SetRightMargin(0.02);
  c_fwa98->SetTopMargin(0.013);
  c_fwa98->cd();
  frame2->Draw();

  double normerr = 0.2*percentage;
  TH1F *eNormBox = new TH1F("eNormBox","",8,-0.5,7.5);
  for (int j=0;j<=8;j++){ eNormBox->SetBinContent(j,1.); eNormBox->SetBinError(j,normerr);}
  eNormBox->SetFillColor(18);
  //eNormBox->SetFillStyle(3001);
  eNormBox->Draw("e3same");

  leg2->Draw("same");

  TCanvas *c_xnwang = new TCanvas("c_fxnang","data_over_fxnwang",750,500);
  c_xnwang->SetRightMargin(0.02);
  c_xnwang->SetTopMargin(0.013);
  c_xnwang->cd();
  frame2->Draw();
  leg2->Draw("same");

  TCanvas *c_blatt = new TCanvas("c_fblatt","data_over_fblatt",750,500);
  c_fblatt->SetRightMargin(0.02);
  c_fblatt->SetTopMargin(0.013);
  c_blatt->cd();
  frame2->Draw();
  leg2->Draw("same");

  normerr = 0.25*percentage;
  TH1F *eNormBox2 = new TH1F("eNormBox2","",8,-0.5,7.5);
  for (int j=0;j<=8;j++){ eNormBox2->SetBinContent(j,1.); eNormBox2->SetBinError(j,normerr);}
  eNormBox2->SetFillColor(18);
  //eNormBox2->SetFillStyle(3001);
  eNormBox2->Draw("e3same");

  //_____________________________________________________________________________
  // Double canvas for WA98 and XNWang parametrizations

  TCanvas *double_canvas = new TCanvas("double_canvas","global_data_over_params",750,750);
  double_canvas->SetRightMargin(0.02);
  double_canvas->SetTopMargin(0.02);
  TPad *double_canvas_1 = new TPad("double_canvas_1", "double_canvas_1",0.,0.40,1.,0.75);
  double_canvas->cd();
  double_canvas_1->Draw();
  TPad *double_canvas_2 = new TPad("double_canvas_2", "double_canvas_2",0.,0.,1.,0.40);
  double_canvas->cd();
  double_canvas_2->Draw();
  TPad *double_canvas_leg = new TPad("double_canvas_leg", "double_canvas_leg",0.,0.75,1.,1.);
  double_canvas->cd();
  double_canvas_leg->Draw();

  TGraphErrors *data_over_fwa98[dataSets];
  TGraphErrors *data_over_xnwang[dataSets];
  TGraphErrors *data_over_blatt[dataSets];

  //_____________________________________________________________________________
  // loop over data sets & parametrizations

  TGraphErrors *eband = 0;

  int lastSet = dataSets-1;
  int col = 0;

  for (int i=0;i<lastSet;i++)
    {

      data_hipt[i] = (TGraphErrors*)e.plot_spectrum_from_ascii(data_file[i],eband);
      emcAnalyzerUtils::setMarkerLineType(data_hipt[i], markerStyle[i], 1., markerSize[i]);

      spectra_canvas->cd();
      if (i!=1 && i!=5 && i!=6) // do not plot: a) plab=159 GeV Carey and b) individual pi+ and pi- Antreasyan measurem.
	{
	  data_hipt[i]->Draw("P");
	  entry=leg->AddEntry(data_hipt[i],data_leg[i],"P");
	  entry2=leg2->AddEntry(data_hipt[i],data_leg[i],"P");
	}

      //=============================================================================
      // WA98 parametrization

      c_fwa98->cd();

      data_hiptScaled[i] = (TGraphErrors*)data_hipt[i]->Clone();

//       // xT-scaling of data to sqrt(s)=17.3 GeV
//       TF1 *xTNorm = new TF1("xTNorm","((1-2*x/[0])/(1-2*x/[1]))^[2]",0,10);
//       double sqrts_sps = 17.3;
//       double sqrts = 19.4;
//       double F = 9.;
//       //double N = 8.;
//       xTNorm->SetParameters(sqrts,sqrts_sps,F);
//       data_hiptScaled[i] = (TGraphErrors*)emcAnalyzerUtils::ratio(data_hipt[i],xTNorm);

      if (i>2) // Elab = 200 GeV
	{
	  data_over_fwa98[i] = (TGraphErrors*)comparison(data_hiptScaled[i],f_wa98,type);
	}
      else if (i==0) // Elab = 150 GeV
	{
	  data_over_fwa98[i] = (TGraphErrors*)comparison(data_hiptScaled[i],f_wa98_150,type);
	}
      else if (i==1) // Elab = 159 GeV
	{
	  data_over_fwa98[i] = (TGraphErrors*)comparison(data_hiptScaled[i],f_wa98_159,type);
	}
      else if (i==2) // Elab = 175 GeV
	{
	  data_over_fwa98[i] = (TGraphErrors*)comparison(data_hiptScaled[i],f_wa98_175,type);
	}
      emcAnalyzerUtils::setMarkerLineType(data_over_fwa98[i], markerStyle[i], 1., markerSize[i]);

      if (i!=1 && i!=5 && i!=6) // do not plot: a) plab=159 GeV Carey and b) individual pi+ and pi- Antreasyan measurem.
	{ 
	  data_over_fwa98[i]->Draw("P");
	  int pass = (i==0) ? -1 : 1; 
	  plot_in_double_canvas(data_over_fwa98[i],double_canvas_1,frame2,0.,0.,pass);
	}
      //=============================================================================
      // X.N. Wang's

      c_xnwang->cd();

      if (i>2)// && i!=5 && i!=6) // Elab = 200 GeV but skip individual pi+ and pi- Antreasyan measurem.
	 {
	   data_over_xnwang[i] = (TGraphErrors*)comparison(data_hipt[i],f_xnwang,type);
	   emcAnalyzerUtils::setMarkerLineType(data_over_xnwang[i], markerStyle[i], 1., markerSize[i]);
	   data_over_xnwang[i]->Draw("P");

	   int pass = (i==3) ? 0 : 1; 
	   plot_in_double_canvas(data_over_xnwang[i],double_canvas_2,frame2,0.20,0.,pass);
	 }
       else if (i==1) // Elab = 159 GeV
	 {
	   //data_over_xnwang[i] = (TGraphErrors*)comparison(data_hipt[i],f_xnwang_159,type);
	   //emcAnalyzerUtils::setMarkerLineType(data_over_xnwang[i], markerStyle[i], 1., markerSize[i]);
	   //data_over_xnwang[i]->Draw("P");
	 }
       //if (i!=5 && i!=6) data_over_xnwang[i]->Draw("P"); //skip individual pi+ and pi- Antreasyan measurem.

      //=============================================================================
      // Blattnig's

      c_blatt->cd();

      if (i>2) // Elab = 200 GeV
	{
	  data_over_blatt[i] = (TGraphErrors*)comparison(data_hiptScaled[i],f_blatt,type);
	}
      else if (i==0) // Elab = 150 GeV
	{
	  data_over_blatt[i] = (TGraphErrors*)comparison(data_hiptScaled[i],f_blatt_150,type);
	}
      else if (i==1) // Elab = 159 GeV
	{
	  data_over_blatt[i] = (TGraphErrors*)comparison(data_hiptScaled[i],f_blatt_159,type);
	}
      else if (i==2) // Elab = 175 GeV
	{
	  data_over_blatt[i] = (TGraphErrors*)comparison(data_hiptScaled[i],f_blatt_175,type);
	}
      emcAnalyzerUtils::setMarkerLineType(data_over_blatt[i], markerStyle[i], 1., markerSize[i]);
      if (i!=1 && i!=5 && i!=6) // do not plot: a) plab=159 GeV Carey and b) individual pi+ and pi- Antreasyan measurem.
	{
	  data_over_blatt[i]->Draw("P"); // skip individual pi+ and pi- Antreasyan measurem.
	  //double_canvas->cd(3);
	  ////gPad->SetBottomMargin(0);
	  //gPad->SetTopMargin(0);
	  //gPad->SetRightMargin(0);
	  //if (i==0) frame2->Draw();
	  //data_over_blatt[i]->Draw("P"); // skip individual pi+ and pi- Antreasyan measurem.
	}
   }

  // last one is sum of Antreasyan pi- and pi+

  spectra_canvas->cd();
  data_hipt[lastSet] = (TGraphErrors*)emcAnalyzerUtils::add(data_hipt[5],data_hipt[6]);
  emcAnalyzerUtils::scale(*data_hipt[lastSet],1./2.);
  emcAnalyzerUtils::setMarkerLineType(data_hipt[lastSet], markerStyle[lastSet], 1., markerSize[lastSet]);
  data_hipt[lastSet]->Draw("P");
  entry=leg->AddEntry(data_hipt[lastSet],data_leg[lastSet],"P");
  entry2=leg2->AddEntry(data_hipt[lastSet],data_leg[lastSet],"P");
  leg->Draw();

  //=============================================================================
  
  f_wa98->Draw("same");
  f_xnwang->Draw("same");
  f_blatt->Draw("same");

  // ratio data/param.

  TLine *line[3];
  line[0] = new TLine(xmin,yref,xmax,yref);
  line[0]->SetLineColor(1);
  line[0]->SetLineStyle(1);
  line[0]->SetLineWidth(3);

   if ( type_str.Contains("minus",TString::kIgnoreCase) ) 
    {
      line[1] = new TLine(xmin,-1.,xmax,-1.);
      line[1]->SetLineColor(1);
      line[1]->SetLineStyle(3);
      line[1]->SetLineWidth(1);
      line[2] = new TLine(xmin,1.,xmax,1.);
      line[2]->SetLineColor(1);
      line[2]->SetLineStyle(3);
      line[2]->SetLineWidth(1);
    }

  //=============================================================================

  c_fwa98->cd();
  data_over_fwa98[lastSet] = (TGraphErrors*)comparison(data_hipt[lastSet],f_wa98,type);
  emcAnalyzerUtils::setMarkerLineType(data_over_fwa98[lastSet], markerStyle[lastSet], 1., markerSize[lastSet]);
  data_over_fwa98[lastSet]->Draw("P");
  leg2->Draw("same");
  line[0]->Draw();
  if ( type_str.Contains("minus",TString::kIgnoreCase) ) 
    {
      line[1]->Draw("same");
      line[2]->Draw("same");
    }
  c_fwa98->Update();

  double_canvas_1->cd();
  data_over_fwa98[lastSet]->Draw("P");
  line[0]->Draw();
  line[1]->Draw();
  line[2]->Draw();
  double_canvas->Update();

  //=============================================================================
   
  c_xnwang->cd();
  data_over_xnwang[lastSet] = (TGraphErrors*)comparison(data_hipt[lastSet],f_xnwang,type);
  emcAnalyzerUtils::setMarkerLineType(data_over_xnwang[lastSet], markerStyle[i], 1., markerSize[i]);
  data_over_xnwang[lastSet]->Draw("P");
  leg2->Draw("same");
  line[0]->Draw();
  if ( type_str.Contains("minus",TString::kIgnoreCase) ) 
    {
      line[1]->Draw("same");
      line[2]->Draw("same");
    }
  c_xnwang->Update();
  
  double_canvas_2->cd();
  data_over_xnwang[lastSet]->Draw("P");
  line[0]->Draw();
  line[1]->Draw();
  line[2]->Draw();
  double_canvas->Update();

  //=============================================================================
  
  c_blatt->cd();
  data_over_blatt[lastSet] = (TGraphErrors*)comparison(data_hipt[lastSet],f_blatt,type);
  emcAnalyzerUtils::setMarkerLineType(data_over_blatt[lastSet], markerStyle[i], 1., markerSize[i]);
  data_over_blatt[lastSet]->Draw("P");
  leg2->Draw("same");
  line[0]->Draw();
  if ( type_str.Contains("minus",TString::kIgnoreCase) ) 
    {
      line[1]->Draw("same");
      line[2]->Draw("same");
    }
  c_blatt->Update();

  //=============================================================================

  double_canvas_leg->cd();
  //gPad->SetTopMargin(0);
  TLegend *leg_global = (TLegend *)leg2->Clone();
  leg_global->SetMargin(0.15);
  leg_global->SetTextSize(0.12);
  leg_global->Draw("");
  
//   double_canvas->cd(3);
//   data_over_blatt[lastSet]->Draw("P");
//   line[0]->Draw();
//   line[1]->Draw();
//   line[2]->Draw();
//   double_canvas->Update();

  c_fwa98->Update();
  c_xnwang->Update();
  c_blatt->Update();
  double_canvas->Update();

}

