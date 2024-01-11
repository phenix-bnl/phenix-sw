// Plots all SPS data (WA98, WA80 and Ceres) vs.
// pp_ref = "blatt", "wa98", "wang" 

void plot_all_RAA_SPS( const char *pp_ref="blatt" )
{

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  TGraphErrors *eband; 
  e.setVerbose(1);
  e.setFit(false);
  e.setConstrainedHagFit(false);
  e.keepPlot(false);

  //_____________________________________________________________________________
  // 

  char title[200];
  sprintf(title,"sps_hipt_canvas_all_data");
  TCanvas *sps_hipt_canvas = new TCanvas(title,title,600,600);
  sps_hipt_canvas->Range(-0.54,-0.90,5.0,1.0);
  sps_hipt_canvas->SetFillColor(0);
  sps_hipt_canvas->SetBorderMode(0);
  sps_hipt_canvas->SetBorderSize(2);
  sps_hipt_canvas->SetFrameBorderMode(0);
  sps_hipt_canvas->SetFrameBorderMode(0);
  sps_hipt_canvas->SetRightMargin(0.015);
  sps_hipt_canvas->SetTopMargin(0.035);
  sps_hipt_canvas->SetLogy();
  sps_hipt_canvas->cd();

  TLegend *leg = new TLegend(0.136725,0.739872,0.771065,0.940299,NULL,"brNDC");
  leg->SetMargin(0.1);
  leg->SetBorderSize(4);
  leg->SetFillColor(kWhite);
  leg->SetTextSize(0.037);

  double sqrts = 0.;
  double ncoll = 1.;
  double norm = 1.;
  double sigma_NN = 0.;

  TF1* f_param = 0;

  char sps_file[300]; 
  char sps_leg[300]; 
  char ncoll_leg[300]; 

  //_____________________________________________________________________________
  // WA98 data

  //sprintf(sps_file,"wa98_pi0_peri.dat"); ncoll = 10.; 
  //sprintf(sps_file,"original_klaus/wa98_pi0_et0102.dat"); ncoll = 10.8;
  //sprintf(sps_file,"original_klaus/wa98_pi0_et02.dat"); ncoll = 30.; 
  //sprintf(sps_file,"wa98_pi0_cent.dat"); ncoll = 651.5.; 
  //sprintf(sps_file,"wa98_pi0_cen_com_orig.dat"); ncoll = 651.5.; 
  //sprintf(sps_file,"wa98_pi0_cen_com_orig_rebin.dat"); ncoll = 651.5.; 

  sqrts = 17.3;
  sprintf(sps_file,"wa98_pi0_et010.dat"); ncoll = 726.;
  sprintf(sps_leg,"Pb+Pb #rightarrow #pi^{0}+X  7%%  central [WA98] (#sqrt{s} = %3.1f GeV)",sqrts);
  sprintf(ncoll_leg,"N_{coll}^{Pb+Pb} = %d#pm%d",ncoll,ncoll*0.10);
  TLatex *tex1 = new TLatex(0.356935,0.53,ncoll_leg);

  TGraphErrors *wa98_hipt = (TGraphErrors*)e.plot_spectrum_from_ascii(sps_file,eband);
  wa98_hipt->SetMaximum(1000.);
  wa98_hipt->SetMinimum(1e-08);
  wa98_hipt->SetMarkerStyle(20);
  wa98_hipt->SetMarkerColor(1);
  wa98_hipt->SetMarkerSize(1.4);
  wa98_hipt->Draw("AP");

  TLegendEntry *entry=leg->AddEntry(wa98_hipt,sps_leg,"P");

  sigma_NN = e.sigma_pp_inel(sqrts);
  TF1* f_param_wa98 = (TF1*)e.plot_pp_sigma_parametrization("pi0",pp_ref,sqrts,ncoll/sigma_NN);
  f_param_wa98->SetRange(0.5,5.0);
  f_param_wa98->SetLineColor(1);
  //wa98_hipt_canvas->cd();
  f_param_wa98->Draw("same");

  cout << "<I> normalizing " << f_param_wa98->GetName() << " by Ncoll=" << ncoll << endl; 

  //_____________________________________________________________________________
  // CERES data

  sprintf(sps_file,"ceres.txt");
  //sprintf(sps_file,"ceres_piminusplus.txt");
  sqrts = 17.3;
  sprintf(sps_leg,"Pb+Au #rightarrow #pi^{#pm}+X  8%% central [CERES] (#sqrt{s} = %3.1f GeV)",sqrts);
  ncoll = 710.; // Pb+Pb ~ Pb+Au 8% most central
  sprintf(ncoll_leg,"N_{coll}^{Pb+Au} = %d#pm%d",ncoll,ncoll*0.10);
  TLatex * tex2 = new TLatex(0.356935,0.40,ncoll_leg);

  TGraphErrors *ceres_hipt = (TGraphErrors*)e.plot_spectrum_from_ascii(sps_file,eband);
  ceres_hipt->SetMaximum(1000.);
  ceres_hipt->SetMinimum(1e-08);
  ceres_hipt->SetMarkerStyle(21);
  ceres_hipt->SetMarkerColor(14);
  ceres_hipt->SetMarkerSize(1.4);
  sps_hipt_canvas->cd();
  ceres_hipt->Draw("AP");

  entry=leg->AddEntry(ceres_hipt,sps_leg,"P");

  sigma_NN = e.sigma_pp_inel(sqrts);
  TF1* f_param_ceres = (TF1*)e.plot_pp_sigma_parametrization("pi0",pp_ref,sqrts,ncoll/sigma_NN);
  f_param_ceres->SetRange(0.5,5.0);
  f_param_ceres->SetLineColor(1);
  //ceres_hipt_canvas->cd();
  f_param_ceres->Draw("same");

  cout << "<I> normalizing " << f_param_ceres->GetName() << " by Ncoll=" << ncoll << endl; 

  leg->Draw("");

  //_____________________________________________________________________________
  // WA80 S+Au data
  // all Ncoll values are from Klaus' Glauber MC, consistent with Npart values published in WA80 paper

  sprintf(sps_file,"wa80_sigpi0_SAU_cent09_0_8.dat");  ncoll = 174; // S+Au 0-8% most central 
  //sprintf(sps_file,"wa80_sigpi0_SAU_cent08_0_3.dat"); ncoll = 186; // S+Au 0-2.8% most central
  //sprintf(sps_file,"wa80_sigpi0_SAU_cent07_3_8.dat"); ncoll = 167; // S+Au 3-8% most central

  sqrts = 19.4;
  sprintf(sps_leg,"S+Au #rightarrow #pi^{0}+X 8%%  central [WA80] (#sqrt{s} = %3.1f GeV)",sqrts);
  sprintf(ncoll_leg,"N_{coll}^{Pb+Au} = %d#pm%d",ncoll,ncoll*0.10);
  TLatex * tex3 = new TLatex(0.356935,0.28,ncoll_leg);

  TGraphErrors *wa80_SAu_hipt = (TGraphErrors*)e.plot_spectrum_from_ascii(sps_file,eband);
  wa80_SAu_hipt->SetMaximum(1000.);
  wa80_SAu_hipt->SetMinimum(1e-08);
  wa80_SAu_hipt->SetMarkerStyle(28);
  wa80_SAu_hipt->SetMarkerColor(1);
  wa80_SAu_hipt->SetMarkerSize(1.6);
  wa80_SAu_hipt->Draw("AP");

  entry=leg->AddEntry(wa80_SAu_hipt,sps_leg,"P");

  sigma_NN = e.sigma_pp_inel(sqrts);
  TF1* f_param_wa80_SAu = (TF1*)e.plot_pp_sigma_parametrization("pi0",pp_ref,sqrts,ncoll/sigma_NN);
  f_param_wa80_SAu->SetRange(0.5,5.0);
  f_param_wa80_SAu->SetLineColor(1);
  //wa80_SAu_hipt_canvas->cd();
  f_param_wa80_SAu->Draw("same");

  cout << "<I> normalizing " << f_param_wa80_SAu->GetName() << " by Ncoll=" << ncoll << endl; 

  //_____________________________________________________________________________
  // WA80 S+S data

  sprintf(sps_file,"wa80_sigpi0_SS_cent0_25.dat");
  sqrts = 19.4;
  sprintf(sps_leg,"S+S #rightarrow #pi^{0}+X 25%%  central [WA80] (#sqrt{s} = %3.1f GeV)",sqrts);
  ncoll = 45; // S+S 25% most central

  TGraphErrors *wa80_SS_hipt = (TGraphErrors*)e.plot_spectrum_from_ascii(sps_file,eband);
  wa80_SS_hipt->SetMaximum(1000.);
  wa80_SS_hipt->SetMinimum(1e-08);
  wa80_SS_hipt->SetMarkerStyle(22);
  wa80_SS_hipt->SetMarkerColor(1);
  wa80_SS_hipt->SetMarkerSize(2.4);
  wa80_SS_hipt->Draw("AP");

  //entry=leg->AddEntry(wa80_SS_hipt,sps_leg,"P");

  sigma_NN = e.sigma_pp_inel(sqrts);
  TF1* f_param_wa80_SS = (TF1*)e.plot_pp_sigma_parametrization("pi0",pp_ref,sqrts,ncoll/sigma_NN);
  f_param_wa80_SS->SetRange(0.5,5.0);
  f_param_wa80_SS->SetLineColor(1);
  //wa80_SS_hipt_canvas->cd();
  f_param_wa80_SS->Draw("same");

  //_____________________________________________________________________________
  // WA80 O+Au data

  sprintf(sps_file,"wa80_sigpi0_OAu_cent0_37.txt");
  sqrts = 19.4;
  sprintf(sps_leg,"O+Au #rightarrow #pi^{0}+X 37%%  central [WA80] (#sqrt{s} = %3.1f GeV)",sqrts);
  ncoll = 62.; // O+Au 37% most central (Klaus Glauber MC)

  TGraphErrors *wa80_OAu_hipt = (TGraphErrors*)e.plot_spectrum_from_ascii(sps_file,eband);
  wa80_OAu_hipt->SetMaximum(1000.);
  wa80_OAu_hipt->SetMinimum(1e-08);
  wa80_OAu_hipt->SetMarkerStyle(22);
  wa80_OAu_hipt->SetMarkerColor(1);
  wa80_OAu_hipt->SetMarkerSize(2.4);
  wa80_OAu_hipt->Draw("AP");

  //entry=leg->AddEntry(wa80_OAu_hipt,sps_leg,"P");

  sigma_NN = e.sigma_pp_inel(sqrts);
  TF1* f_param_wa80_OAu = (TF1*)e.plot_pp_sigma_parametrization("pi0",pp_ref,sqrts,ncoll/sigma_NN);
  f_param_wa80_OAu->SetRange(0.5,5.0);
  f_param_wa80_OAu->SetLineColor(1);
  //wa80_OAu_hipt_canvas->cd();
  f_param_wa80_OAu->Draw("same");

  //_____________________________________________________________________________
  // Ncoll scaling of pp reference

  TGraphErrors *RAA_wa98 = (TGraphErrors*)wa98_hipt->Clone();
  TGraphErrors *RAA_wa80_SAu = (TGraphErrors*)wa80_SAu_hipt->Clone();
  TGraphErrors *RAA_wa80_SS = (TGraphErrors*)wa80_SS_hipt->Clone();
  TGraphErrors *RAA_ceres = (TGraphErrors*)ceres_hipt->Clone();
  TGraphErrors *RAA_wa80_OAu = (TGraphErrors*)wa80_OAu_hipt->Clone();

  double pT = 0.;
  double epT = 0.;
  double ey = 0.;
  double y = 0.;

  double RAA = 0.;
  double eRAA = 0.;

  //=============================================================================

  for(int i=0;i<=wa98_hipt->GetN();i++)
    {
      wa98_hipt->GetPoint(i,pT,y);
      epT = wa98_hipt->GetErrorX(i);
      ey = wa98_hipt->GetErrorY(i);

      RAA = y/f_param_wa98->Eval(pT);
      eRAA = ey/f_param_wa98->Eval(pT);

      RAA_wa98->SetPoint(i,pT,RAA);
      RAA_wa98->SetPointError(i,epT,eRAA);

      printf("%4.2f  %g   %g   %g\n",pT,RAA,epT,eRAA);
    }

  //=============================================================================

  for(int i=0;i<=wa80_SAu_hipt->GetN();i++)
    {
      wa80_SAu_hipt->GetPoint(i,pT,y);
      epT = wa80_SAu_hipt->GetErrorX(i);
      ey = wa80_SAu_hipt->GetErrorY(i);

      RAA = y/f_param_wa80_SAu->Eval(pT);
      eRAA = ey/f_param_wa80_SAu->Eval(pT);

      RAA_wa80_SAu->SetPoint(i,pT,RAA);
      RAA_wa80_SAu->SetPointError(i,epT,eRAA);
    }

  //=============================================================================

  for(int i=0;i<=wa80_SS_hipt->GetN();i++)
    {
      wa80_SS_hipt->GetPoint(i,pT,y);
      epT = wa80_SS_hipt->GetErrorX(i);
      ey = wa80_SS_hipt->GetErrorY(i);

      RAA = y/f_param_wa80_SS->Eval(pT);
      eRAA = ey/f_param_wa80_SS->Eval(pT);

      RAA_wa80_SS->SetPoint(i,pT,RAA);
      RAA_wa80_SS->SetPointError(i,epT,eRAA);
    }

  //=============================================================================

  for(int i=0;i<=ceres_hipt->GetN();i++)
    {
      ceres_hipt->GetPoint(i,pT,y);
      epT = ceres_hipt->GetErrorX(i);
      ey = ceres_hipt->GetErrorY(i);

      RAA = y/f_param_ceres->Eval(pT);
      eRAA = ey/f_param_ceres->Eval(pT);

      RAA_ceres->SetPoint(i,pT,RAA);
      RAA_ceres->SetPointError(i,epT,eRAA);
    }

  //=============================================================================

  for(int i=0;i<=wa80_OAu_hipt->GetN();i++)
    {
      wa80_OAu_hipt->GetPoint(i,pT,y);
      epT = wa80_OAu_hipt->GetErrorX(i);
      ey = wa80_OAu_hipt->GetErrorY(i);

      RAA = y/f_param_wa80_OAu->Eval(pT);
      eRAA = ey/f_param_wa80_OAu->Eval(pT);

      RAA_wa80_OAu->SetPoint(i,pT,RAA);
      RAA_wa80_OAu->SetPointError(i,epT,eRAA);
    }

  //=============================================================================
  // theory plots

  TGraphErrors *vitev400 = (TGraphErrors*)e.plot_spectrum_from_ascii("R_AA_theor_vitev_sps_dndy400.txt",eband);
  TGraphErrors *vitev450 = (TGraphErrors*)e.plot_spectrum_from_ascii("R_AA_theor_vitev_sps_dndy450.txt",eband);
  TGraphErrors *vitev500 = (TGraphErrors*)e.plot_spectrum_from_ascii("R_AA_theor_vitev_sps_dndy500.txt",eband);
  TGraphErrors *vitev600 = (TGraphErrors*)e.plot_spectrum_from_ascii("R_AA_theor_vitev_sps_dndy600.txt",eband);

  TClonesArray* eb=(TClonesArray*)emcAnalyzerUtils::errorBand(vitev400,vitev600);
  TGraph *shadeband = (TGraph*)(*eb)[0]; 

  //=============================================================================

  sprintf(title,"RAA_hipt_alldata_%s",pp_ref);
  //char *ytitle = "Nuclear modification factor R_{AA}";
  char *ytitle = " R_{AA} ";

  TCanvas *c10 = new TCanvas(title,title,120,50,780,620);
  c10->Range(-0.644,-0.90,5.06,1.10);
  c10->SetLeftMargin(0.11);
  c10->SetRightMargin(0.011);
  c10->SetTopMargin(0.051);

  double RAA_max = 10.;
  double RAA_min = 0.2;

  double ptmax = 4.5;

  TH2F *frame = new TH2F("frame","frame",10,0.,ptmax,20,RAA_min,RAA_max);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle(ytitle);
  frame->SetXTitle("p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(0.9);
  frame->GetYaxis()->SetTitleSize(0.055);
  frame->GetYaxis()->SetLabelSize(0.055);
  frame->GetXaxis()->SetTitleOffset(0.9);
  frame->GetXaxis()->SetTitleSize(0.055);
  frame->GetXaxis()->SetLabelSize(0.055);
  frame->GetYaxis()->SetNoExponent();
  frame->GetYaxis()->SetMoreLogLabels();
  frame->Draw();
  c10->SetLogy();

  double normerr = sqrt(0.10*0.10+0.25*0.25);  
  TH1F *eNormBox = new TH1F("eNormBox","",5,-0.5,5.);
  for (int j=0;j<=6;j++){eNormBox->SetBinContent(j,1.);eNormBox->SetBinError(j,normerr);}
  eNormBox->SetFillColor(18);
  //eNormBox->SetFillStyle(3001);
  eNormBox->Draw("e3same");

  vitev400->Draw("L");
  vitev600->Draw("L");
  shadeband->Draw("f");
  frame->Draw("same");

  RAA_wa98->Draw("P");
  RAA_wa98->Print("all");
  RAA_ceres->Draw("P");
  RAA_wa80_SAu->Draw("P");

  //RAA_wa80_SS->Draw("P");
  //RAA_wa80_OAu->Draw("P");

//   TLegend *leg2 = new TLegend(0.105369,0.809783,0.480872,0.95471,NULL,"brNDC");
//   leg2->SetMargin(0.1);
//   leg2->SetBorderSize(3);
//   leg2->SetFillStyle(0);
//   leg2->SetFillColor(kWhite);
//   leg2->SetTextSize(0.045);
//   leg2->Draw();

//   TLegendEntry *entry=leg2->AddEntry(RAA_ceres,"p+p #rightarrow #pi^{0} Blattnig parametrization","P");
//   entry=leg2->AddEntry(RAA_wa98,"p+p #rightarrow #pi^{0} WA98 parametrization","P");
//   entry=leg2->AddEntry(RAA_wa80_SAu,"p+p #rightarrow #pi^{0} Wang&Wang parametrization","P");

  leg->Draw();

  TLine *line = new TLine(0,1.,ptmax,1.);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw("same");

  tex1->SetTextSize(0.04);
  tex1->SetLineWidth(2);
  //tex1->Draw();
  tex2->SetTextSize(0.04);
  tex2->SetLineWidth(2);
  //tex2->Draw();
  tex3->SetTextSize(0.04);
  tex3->SetLineWidth(2);
  //tex3->Draw();
 
  c10->Update();

}

