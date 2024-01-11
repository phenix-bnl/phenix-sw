// sps_data = "wa98", "wa80", "ceres"
// centrality = 

void plot_RAA_WA98_3parametrizations( const char* sps_data = "wa98" ) 
  //const double centrality = 0,
  //const char* yaxis = "log" )
{

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  TGraphErrors *eband; 
  e.setVerbose(1);
  e.setFit(false);
  e.setConstrainedHagFit(false);
  e.keepPlot(false);

  char title[200];
  sprintf(title,"sps_hipt_canvas_%s",sps_data);

  TCanvas *sps_hipt_canvas = new TCanvas(title,title,600,600);
  sps_hipt_canvas->SetLogy();
  sps_hipt_canvas->cd();

  double sqrts = 0.;
  double ncoll = 1.;
  double norm = 1.;

  //_____________________________________________________________________________
  // A+A CERN-SPS data

  TString sps_data_str = sps_data;
  char sps_file[300]; 
  char sps_leg[300]; 
  char ncoll_leg[300]; 

  if (sps_data_str.Contains("wa98",TString::kIgnoreCase))
    {
      //sprintf(sps_file,"wa98_pi0_cen_com_orig_rebin.dat"); ncoll = 651.5; // Pb+Pb 7% central
      //sprintf(sps_file,"wa98_pi0_cen_com_orig.dat"); ncoll = 651.5; // Pb+Pb 7% central
      //sprintf(sps_file,"wa98_pi0_cen_com_orig.dat"); ncoll = 651.5; // Pb+Pb 7% central
      sprintf(sps_file,"wa98_pi0_et010.dat"); ncoll = 726.;
      sqrts = 17.3;
      sprintf(sps_leg,"Pb+Pb #rightarrow #pi^{0} 6.8%% central [WA98] (#sqrt{s} = %3.1f GeV)",sqrts);
      sprintf(ncoll_leg,"N_{coll}^{Pb+Pb} = %d#pm%d",ncoll,ncoll*0.10);
    }
  else if (sps_data_str.Contains("wa80",TString::kIgnoreCase))
    {
      sprintf(sps_file,"wa80_sigpi0_SAU_0_3.dat");
      sqrts = 19.4;
      ncoll = 110; // S+Au 3% most central
      sprintf(sps_leg,"S+Au #rightarrow #pi^{0} 3.0%% central [WA80] (#sqrt{s} = %3.1f GeV)",sqrts);
      sprintf(ncoll_leg,"N_{coll}^{S+Au} = %d#pm%d",ncoll,ncoll*0.10);
    }
  else if (sps_data_str.Contains("ceres",TString::kIgnoreCase))
    {
      sprintf(sps_file,"ceres.txt");
      //sprintf(sps_file,"ceres_piminusplus.txt");
      sqrts = 17.3;
      ncoll = 730.; // Pb+Pb ~ Pb+Au 8% most central
      sprintf(sps_leg,"Pb+Au #rightarrow #pi^{#pm} 8.0%% central [CERES] (#sqrt{s} = %3.1f GeV)",sqrts);
      sprintf(ncoll_leg,"N_{coll}^{S+Au} = %d#pm%d",ncoll,ncoll*0.10);
    }

  TGraphErrors *SPS_hipt = (TGraphErrors*)e.plot_spectrum_from_ascii(sps_file,eband);
  // for WA80 we add the 2 most central classes: 0-2.8% + 2.8-7.7% = 0-7.7% 
  if (sps_data_str.Contains("wa80",TString::kIgnoreCase))
    {
      //sprintf(sps_file,"wa80_sigpi0_SAU_3_8.dat");
      //TGraphErrors *wa80_2 = (TGraphErrors*)e.plot_spectrum_from_ascii(sps_file,eband);
      //SPS_hipt = (TGraphErrors*)emcAnalyzerUtils::add(SPS_hipt,wa80_2);
    }

  SPS_hipt->SetMaximum(1000.);
  SPS_hipt->SetMinimum(1e-08);

  sps_hipt_canvas->cd();
  SPS_hipt->Draw("AP");

  TLegend *leg = new TLegend(0.11958,0.761317,0.617608,0.940329,sps_leg,"brNDC");
//   leg->SetLineColor(1);
//   leg->SetLineStyle(1);
//   leg->SetLineWidth(1);
//   leg->SetFillStyle(0);
  leg->SetBorderSize(4);
  leg->SetFillColor(kWhite);
  leg->SetTextSize(0.045);
  TLegendEntry *entry=leg->AddEntry(SPS_hipt,sps_leg,"P");

  //_____________________________________________________________________________
  // p+p SPS reference

  double sigma_NN = e.sigma_pp_inel(sqrts);

  // WA98 parametrization at sqrt(s) = 17.3 GeV

  TF1* f_wa98 = (TF1*)e.plot_pp_sigma_parametrization("pi0","wa98",sqrts,ncoll/sigma_NN);
  f_wa98->SetRange(0.5,5.0);
  f_wa98->SetLineColor(1);
  sps_hipt_canvas->cd();
  f_wa98->Draw("same");
  entry=leg->AddEntry(f_wa98,"WA98 p+p #rightarrow #pi^{0} parametrization #times N_{coll}(0-12.7%)","L");

  // Wang: pi's in p+p at sqrt(s) = 17.3, 19.4 GeV

  TF1*f_xnwang = (TF1*)e.plot_pp_sigma_parametrization("pi0","xnwang",sqrts,ncoll/sigma_NN);
  f_xnwang->SetRange(0.5,5.0);
  f_xnwang->SetLineColor(2);
  sps_hipt_canvas->cd();
  f_xnwang->Draw("same");

  entry=leg->AddEntry(f_xnwang,"X.N.Wang p+p  #rightarrow #pi^{0} param. #times N_{coll}(0-12.7%)","L");

  // Blattnig's parametrization

  TF1* f_blatt = (TF1*)e.plot_pp_sigma_parametrization("pi0","blatt",sqrts,ncoll/sigma_NN);
  f_blatt->SetRange(0.5,5.0);
  f_blatt->SetLineColor(4);
  sps_hipt_canvas->cd();
  f_blatt->Draw("same");

  entry=leg->AddEntry(f_blatt,"Blattnig p+p  #rightarrow #pi^{0} param. #times N_{coll}(0-12.7%)","L");

  // E704 measurement

//   TGraphErrors* adams = (TGraphErrors*)e.plot_spectrum_from_ascii("pi0_pp_plab200_adams.txt",eband);
//   emcAnalyzerUtils::scale(*adams,ncoll);
//   adams->SetLineColor(4);
//   sps_hipt_canvas->cd();
//   adams->Draw("P");

//   entry=leg->AddEntry(adams,"E704 p+p  #rightarrow #pi^{0} param. #times N_{coll}(0-12.7%)","L");

  leg->Draw("");

  //_____________________________________________________________________________
  // Ncoll scaling of pp reference

  TGraphErrors *RAA_SPS_wa98f = (TGraphErrors*)SPS_hipt->Clone();
  TGraphErrors *RAA_SPS_blatt = (TGraphErrors*)SPS_hipt->Clone();
  TGraphErrors *RAA_SPS_wang  = (TGraphErrors*)SPS_hipt->Clone();
  TGraphErrors *RAA_SPS_adams = (TGraphErrors*)SPS_hipt->Clone();

  double pT = 0.;
  double epT = 0.;
  double ey = 0.;
  double y = 0.;

  double RAA = 0.;
  double eRAA = 0.;

  for(int i=0;i<=SPS_hipt->GetN();i++)
    {
      SPS_hipt->GetPoint(i,pT,y);
      epT = SPS_hipt->GetErrorX(i);
      ey = SPS_hipt->GetErrorY(i);

      // WA98 ref.

      RAA = y/f_wa98->Eval(pT);
      eRAA = ey/f_wa98->Eval(pT);

      RAA_SPS_wa98f->SetPoint(i,pT,RAA);
      RAA_SPS_wa98f->SetPointError(i,epT,eRAA);

      // X.N.Wang ref.

      RAA = y/f_xnwang->Eval(pT);
      eRAA = ey/f_xnwang->Eval(pT);

      RAA_SPS_wang->SetPoint(i,pT,RAA);
      RAA_SPS_wang->SetPointError(i,epT,eRAA);

      // Blattnig's ref.

      RAA = y/f_blatt->Eval(pT);
      eRAA = ey/f_blatt->Eval(pT);

      RAA_SPS_blatt->SetPoint(i,pT,RAA);
      RAA_SPS_blatt->SetPointError(i,epT,eRAA);
    }

  sprintf(title,"RAA_SPS_hipt_%s",sps_data);
  //char *ytitle = "R_{AA}^{#pi^{0}} CERN-SPS";
  //char *ytitle = "Nuclear modif. factor R_{AA}";
  char *ytitle = " R_{AA}";

  TCanvas *c10 = new TCanvas(title,title,18,125,765,534);
  c10->Range(-0.548148,-0.910305,5.08889,1.1036);
  c10->SetRightMargin(0.0157687);
  c10->SetTopMargin(0.0514403);
  c10->SetLogy();

  //TString yaxis_str = yaxis;

  double RAA_max = 10.;
  double RAA_min = 0.2;
  double yline = 1.;
  double ylinebox = 1.;

//   if (yaxis_str.Contains("log",TString::kIgnoreCase))
//     {
//       c10->SetLogy();
//       RAA_max = 10.;
//       RAA_min = 0.2;
//       //yline = log(1.);
//     }

  double ptmax = 4.5;

  TH2F *frame = new TH2F("frame","frame",9,0.,ptmax,20,RAA_min,RAA_max);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle(ytitle);
  frame->SetXTitle("p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(0.7);
  frame->GetYaxis()->SetTitleSize(0.065);
  frame->GetYaxis()->SetLabelSize(0.055);
  frame->GetXaxis()->SetTitleOffset(0.9);
  frame->GetXaxis()->SetTitleSize(0.055);
  frame->GetXaxis()->SetLabelSize(0.055);
  frame->GetYaxis()->SetNoExponent();
  frame->GetYaxis()->SetMoreLogLabels();
  c10->cd();
  frame->Draw();

  double normerr = 0.10;
  
  TH1F *eNormBox = new TH1F("eNormBox","",6,-1.,5.);
  for (int j=0;j<=6;j++){eNormBox->SetBinContent(j,1.);eNormBox->SetBinError(j,normerr);}
  eNormBox->SetFillColor(18);
  //eNormBox->SetFillStyle(3001);
  eNormBox->Draw("e3same");

  normerr = 0.25;
  TH1F *eNormBox2 = new TH1F("eNormBox2","",2,0.1,0.25);
  for (int j=0;j<=2;j++){eNormBox2->SetBinContent(j,1.);eNormBox2->SetBinError(j,normerr);}
  eNormBox2->SetFillColor(13);
  eNormBox2->Draw("e3same");

  normerr = 0.20;
  TH1F *eNormBox3 = new TH1F("eNormBox3","",2,0.28,0.43);
  for (int j=0;j<=2;j++){eNormBox3->SetBinContent(j,1.);eNormBox3->SetBinError(j,normerr);}
  eNormBox3->SetFillColor(20);
  eNormBox3->Draw("e3same");

  frame->Draw("same");

  RAA_SPS_wa98f->SetMarkerStyle(24);//(21);
  RAA_SPS_wa98f->SetMarkerColor(1); //(4);
  RAA_SPS_wa98f->SetMarkerSize(1.7);
  RAA_SPS_wa98f->Draw("P");

  RAA_SPS_wang->SetMarkerStyle(28);//(29);
  RAA_SPS_wang->SetMarkerColor(1); //(6);
  RAA_SPS_wang->SetMarkerSize(2.1);
  RAA_SPS_wang->Draw("P");

  RAA_SPS_blatt->SetMarkerStyle(20);
  RAA_SPS_blatt->SetMarkerColor(1); //(2);
  RAA_SPS_blatt->SetMarkerSize(1.7);
  RAA_SPS_blatt->Draw("P");

   TLegend *leg2 = new TLegend(0.122208,0.728395,0.729304,0.960905,sps_leg,"brNDC");
   //TLegend *leg2 = new TLegend(0.105369,0.809783,0.480872,0.95471,sps_leg,"brNDC");
//   leg2->SetLineColor(1);
//   leg2->SetLineStyle(1);
//   leg2->SetLineWidth(1);
  leg2->SetMargin(0.1);
  leg2->SetBorderSize(3);
  leg2->SetFillStyle(0);
  leg2->SetFillColor(kWhite);
  leg2->SetTextSize(0.045);
  leg2->Draw();

  TLegendEntry *entry=leg2->AddEntry(RAA_SPS_wa98f,"p+p #rightarrow #pi^{0} WA98 parametrization","P");
  entry=leg2->AddEntry(RAA_SPS_wang,"p+p #rightarrow #pi^{0} Wang parametrization","P");
  entry=leg2->AddEntry(RAA_SPS_blatt,"p+p #rightarrow #pi^{0} Blattnig parametrization","P");

  TLatex * tex2 = new TLatex(0.20,2.0,ncoll_leg);
  tex2->SetTextSize(0.045);
  tex2->SetLineWidth(2);
  tex2->Draw();

//   double normerr = 0.10;
//   double ymin = 1.-normerr;
//   double ymax = 1.+normerr;

//   if (yaxis_str.Contains("log",TString::kIgnoreCase))
//     {
//       ymin = log(ymin);
//       ymax = log(ymax);
//     }

//   TBox *eNormBox = new TBox(0.2,ymin,0.4,ymax);
//   eNormBox->Draw();
//   eNormBox->SetFillColor(1);
//   eNormBox->SetFillStyle(3001);

  TLine *line = new TLine(0,yline,ptmax,yline);
  line->SetLineColor(1);
  line->SetLineStyle(4);
  line->SetLineWidth(3);
  line->Draw("same");

  c10->Update();

}

