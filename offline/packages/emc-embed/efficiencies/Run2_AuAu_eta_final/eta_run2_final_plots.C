//*
//************************************************************
// Macros for final Au+Au eta (PbSc) plots
//        
// Copyright (C) PHENIX collaboration, 2004
//
// Author: David D'ENTERRIA - NEVIS LABS, Oct. 2004
//
//************************************************************
//*

#include "plot_minv_eta.h"

//_____________________________________________________________________________
//

void plot_all_AuAu_spectra_plain()
{

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  e.setEfficParticle("eta");
  e.setFit(false);
  TGraphErrors *eband;

  double ptmax = 11.5;
  int ptbins = (int)ptmax*2;
  double ymax = 10.;
  double ymin = 5e-09;  

  char label[100];
  TCanvas *c10 = new TCanvas("eta_all_spectra_AuAu_200GeV_phenix", 
			     "eta_all_spectra_AuAu_200GeV_phenix",147,22,674,754);
  //TCanvas *c10 = (TCanvas*)e.canvas("eta_all_spectra_AuAu_200GeV_phenix", 674,736);
  c10->Range(-2.10391,-9.48933,11.5817,1.26629);
  c10->SetFillColor(0);
  c10->SetBorderMode(0);
  c10->SetBorderSize(0);
  c10->SetLogy();
  c10->SetLeftMargin(0.153731);
  c10->SetRightMargin(0.00597015);
  c10->SetTopMargin(0.0141643);
  c10->SetBottomMargin(0.110482);
  c10->SetLogy();
  
  TH2F *myframe = (TH2F*)e.frame("eta_all_AuAu_200GeV_spectra", ptbins, 0., ptmax, 100, ymin, ymax);
  myframe->Draw();
  myframe->GetYaxis()->SetTitleOffset(1.4);
  c10->Update();

  //TLegend *legend = new TLegend(0.645, 0.61, 0.96, 0.96, label, "brNDC");
  TLegend *legend = new TLegend(0.412791,0.769337,0.921512,0.966851, "", "brNDC");
  legend->SetMargin(0.1);
  legend->SetTextSize(0.038);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->Draw();
  c10->Update();
 
  TGraphErrors *eta_spec[4] ;

  double iscale[4] = {20., 1., 1/5., 1/10. };
  int cent1, cent2;

  int col[4]={1,2,94,38};
  int mark[4]={20,20,21,22};
  float size[4]={1.8,1.8,1.6,2.0};
  int j = 0;

  for (int i=19; i<=22; i++)
    {
      if (i==22) i = 17;
      emcAnalyzerUtils::getCentralityClassLimits(i, cent1, cent2);
      eta_spec[j] = (TGraphErrors*)e.plot_spectrum_from_ascii(cent1, cent2, "eta");

      if (eta_spec[j])
	{
	  emcAnalyzerUtils::scale(*eta_spec[j], iscale[j]);
	  c10->cd();
	  eta_spec[j]->Draw("P");
	  eta_spec[j]->SetMaximum(ymax);
	  eta_spec[j]->SetMinimum(ymin);

	  if (cent2 == 100) cent2 = 92;
 	  if (iscale[j]==1) sprintf(label, " Au+Au #rightarrow #eta+X [%i-%i%%]",cent1,cent2, iscale[j]);
 	  else if (iscale[j]>1) sprintf(label, " Au+Au #rightarrow #eta+X [%i-%i%%]  #times %i",cent1,cent2, iscale[j]);
 	  else sprintf(label, " Au+Au #rightarrow #eta+X [%i-%i%%]  #times 1/%i",cent1,cent2, (int)(1/iscale[j]));
 	  legend->AddEntry(eta_spec[j], label, "P");
	  //sprintf(label, "[%i-%i%%]",cent1,cent2);
	  //sprintf(label, "#times1/%i",(int)1/iscale[j]);
	  //if (iscale[j] == 20) sprintf(label, "#times20");

	  emcAnalyzerUtils::setMarkerLineType(eta_spec[j], mark[j], col[j], size[j]);
	  j+=1;
	}
      if (i==17) break;
    }


  //TLatex *tex1 = new TLatex(0.612789,2.77729,"Au+Au #rightarrow #eta+X");
  //tex1->SetTextSize(0.045);
  //tex1->Draw();
  
  c10->Update();

}

//_____________________________________________________________________________
//

void plot_all_AuAu_spectra()
{

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  e.setEfficParticle("eta");
  e.setFit(false);
  TGraphErrors *eband;

  double ptmax = 11.5;
  int ptbins = (int)ptmax*2;
  double ymax = 13.;
  double ymin = 5e-09;  

  char label[100];
  TCanvas *c10 = new TCanvas("eta_all_spectra_AuAu_200GeV_phenix", 
			     "eta_all_spectra_AuAu_200GeV_phenix",147,22,674,754);
  //TCanvas *c10 = (TCanvas*)e.canvas("eta_all_spectra_AuAu_200GeV_phenix", 674,736);
  c10->Range(-2.10391,-9.48933,11.5817,1.26629);
  c10->SetFillColor(0);
  c10->SetBorderMode(0);
  c10->SetBorderSize(0);
  c10->SetLogy();
  c10->SetLeftMargin(0.153731);
  c10->SetRightMargin(0.00597015);
  c10->SetTopMargin(0.0141643);
  c10->SetBottomMargin(0.110482);
  c10->SetLogy();
  
  TH2F *myframe = (TH2F*)e.frame("eta_all_AuAu_200GeV_spectra", ptbins, 0., ptmax, 100, ymin, ymax);
  myframe->Draw();
  myframe->GetYaxis()->SetTitleOffset(1.4);
  c10->Update();

  TLegend *legend = new TLegend(0.645, 0.61, 0.96, 0.96, label, "brNDC");
  legend->SetMargin(0.1);
  legend->SetTextSize(0.042);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  //legend->Draw();
  c10->Update();

  double xycoords[4][2]  = {{0.36,0.33},{0.36,0.031},{0.36,0.0032},{0.36,7.8e-05}};
  double xycoords2[4][2] = {{0.63,0.125},{100.,100.},{0.59,0.00106},{0.63,2.96e-05}};
  TLatex *tex[4];
  TLatex *tex2[4];
 
  TGraphErrors *eta_spec[4] ;

  double iscale[4] = {20., 1., 1/5., 1/10. };
  int cent1, cent2;

  int col[4]={1,2,94,38};
  int mark[4]={20,20,21,22};
  float size[4]={1.8,1.8,1.6,2.0};
  int j = 0;

  for (int i=19; i<=22; i++)
    {
      if (i==22) i = 17;
      emcAnalyzerUtils::getCentralityClassLimits(i, cent1, cent2);
      eta_spec[j] = (TGraphErrors*)e.plot_spectrum_from_ascii(cent1, cent2, "eta");

      if (eta_spec[j])
	{
	  emcAnalyzerUtils::scale(*eta_spec[j], iscale[j]);
	  c10->cd();
	  eta_spec[j]->Draw("P");
	  eta_spec[j]->SetMaximum(ymax);
	  eta_spec[j]->SetMinimum(ymin);

// 	  if (iscale>1) sprintf(label, " #eta [%i-%i%%]  #times %i",cent1,cent2, iscale);
// 	  else sprintf(label, " #eta [%i-%i%%]  #times 1/%i",cent1,cent2, 1/iscale);
// 	  legend->AddEntry(eta_spec[j], label, "P");
	  if (cent2 == 100) cent2 = 92;
	  sprintf(label, "[%i-%i%%]",cent1,cent2);
	  tex[j] = new TLatex(xycoords[j][0],xycoords[j][1],label);
	  tex[j]->SetTextSize(0.03);
	  c10->cd();
	  tex[j]->Draw();

	  sprintf(label, "#times1/%i",1/iscale[j]);
	  if (iscale[j] == 20) sprintf(label, "#times20");
	  tex2[j] = new TLatex(xycoords2[j][0],xycoords2[j][1],label);
	  tex2[j]->SetTextSize(0.03);
	  c10->cd();
	  tex2[j]->Draw();

	  emcAnalyzerUtils::setMarkerLineType(eta_spec[j], mark[j], col[j], size[j]);
	  j+=1;
	}
      if (i==17) break;
    }


  TLatex *tex1 = new TLatex(0.612789,2.77729,"Au+Au #rightarrow #eta+X");
  tex1->SetTextSize(0.045);
  tex1->Draw();
  
  // M_inv. plot
  TPad *minv_pad = new TPad("minv_pad", "minv_pad",0.491045,0.584986,0.989552,0.984419);
  minv_pad->Draw();
  minv_pad->cd();
  minv_pad->Range(-0.161062,-433.018,1.02124,1214.54);
  minv_pad->SetLeftMargin(0.305389);
  minv_pad->SetRightMargin(0.0179641);
  minv_pad->SetTopMargin(0.0248227);
  minv_pad->SetBottomMargin(0.202128);
  minv_pad->SetFillColor(4000);
  minv_pad->SetBorderMode(0);
  minv_pad->SetBorderSize(0);
  minv_pad->SetFrameBorderMode(0);
  minv_pad->Modified();
  minv_pad->cd();
  plot_minv_eta(minv_pad);
  minv_pad->Modified();
  minv_pad->Update();

  c10->Update();

}

//_____________________________________________________________________________
//

void final_pp_eta_spectra_plots()
{

  gSystem->Load("libemcAnalyzer");
  emcAnalyzer e; TGraphErrors *eband;
  //e.setFit(0);

  TGraphErrors *p=(TGraphErrors*)e.plot_spectrum_from_ascii("eta_pp_emcal_200GeV_preliminary_Oct03.txt",eband);
  TGraphErrors *n=(TGraphErrors*)e.plot_spectrum_from_ascii("eta_pp_emcal_200GeV.txt",eband);
  n->GetYaxis()->SetTitle("Ed^{3}#sigma/d^{3}p (mb GeV^{-2}c^{3})");
  n->GetYaxis()->SetTitleOffset(1.5);
  p->Draw("P");
  p->SetMarkerColor(4);

  char label[200];
  sprintf(label,"p+p #rightarrow #eta+X");
  TLegend *legend = new TLegend(0.5,0.731579,0.959732,0.961404,NULL,"brNDC");
  legend->SetMargin(0.1);
  legend->SetTextSize(0.042);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->AddEntry(p,"Preliminary DNP'04","P");
  legend->AddEntry(n, "Final","P");
  legend->Draw();

  // pp new/preliminary

  TGraphErrors *ratio=(TGraphErrors*)
    e.plot_ratio_spectra("eta_pp_emcal_200GeV.txt","eta_pp_emcal_200GeV_preliminary_Oct03.txt",
			 "p_{T} (GeV/c)","ratio Final/Preliminary","eta_pp_ratio_final_prelim");
  TF1*fit=(TF1*)p->GetListOfFunctions()->FindObject("constrhagedornascii");
  TGraphErrors *ratiofits=(TGraphErrors*)emcAnalyzerUtils::ratio(n,fit);
  ratiofits->Draw("P");

  // pp eta/pi0

  TGraphErrors *ratio_eta_pi0=e.plot_spectrum_from_ascii("phenix_pp_200GeV_eta_pi0_ratio.txt",eband);
  ratio_eta_pi0->GetYaxis()->SetTitle("#eta/#pi^{0}");
  ratio_eta_pi0->GetYaxis()->SetTitleOffset(0.9);
  ratio_eta_pi0->GetYaxis()->SetTitleSize(0.05); 
  ratio_eta_pi0->GetXaxis()->SetTitleSize(0.05); 
  
  TLine *line = new TLine(0,1,11.5,1);
  //TLine *line = new TLine(xmin,1,xmax,1);
  line->SetLineColor(9);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();
 
  // dAu eta/pi0

  TGraphErrors *dAu_pi0 = e.plot_spectrum_from_ascii("pi0_emcal_dAu_0_88.txt",eband);
  TGraphErrors *dAu_eta = e.plot_spectrum_from_ascii("eta_dAu_0_88_emcal_200GeV.txt",eband);
  TGraphErrors *dAu_etapi0 = (TGraphErrors*)
    e.plot_ratio_spectra("eta_dAu_0_88_emcal_200GeV.txt","pi0_emcal_dAu_0_88.txt",
			 "p_{T} (GeV/c)","#eta/#pi^{0}","phenix_dAu_200GeV_eta_pi0_ratio");
  TF1*fit=(TF1*)dAu_pi0->GetListOfFunctions()->FindObject("constrhagedornascii");
  TGraphErrors *ratiofits=(TGraphErrors*)emcAnalyzerUtils::ratio(dAu_eta,fit);
  ratiofits->Draw("P");
  
  TLine *line = new TLine(0,1,11.5,1);
  //TLine *line = new TLine(xmin,1,xmax,1);
  line->SetLineColor(9);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();

}

//_____________________________________________________________________________
//

TGraphErrors *eta_pi0_ratio_dAu()
{
   TGraphErrors *gre = new TGraphErrors(13);
   gre->SetPoint(0,2.25,0.411289);
   gre->SetPointError(0,0,0.0541871);
   gre->SetPoint(1,2.75,0.462179);
   gre->SetPointError(1,0,0.0618837);
   gre->SetPointError(1,0,0.0618837);
   gre->SetPoint(2,3.25,0.375004);
   gre->SetPointError(2,0,0.0572528);
   gre->SetPoint(3,3.75,0.460277);
   gre->SetPointError(3,0,0.0681954);
   gre->SetPoint(4,4.25,0.468038);
   gre->SetPointError(4,0,0.0652761);
   gre->SetPoint(5,4.75,0.473942);
   gre->SetPointError(5,0,0.0637391);
   gre->SetPoint(9,7.5,0.632026);
   gre->SetPointError(9,0,0.121281);
   gre->SetPoint(10,8.5,0.672622);
   gre->SetPointError(10,0,0.150339);
   gre->SetPoint(11,9.5,0.806631);
   gre->SetPointError(11,0,0.180614);
   gre->SetPoint(12,11,0.598101);
   gre->SetPointError(12,0,0.142872);
   gre->SetPoint(6,5.25,0.415692);
   gre->SetPointError(6,0,0.049217);
   gre->SetPoint(7,5.75,0.457285);
   gre->SetPointError(7,0,0.0565527);
   gre->SetPoint(8,6.5,0.50508);
   gre->SetPointError(8,0,0.0645475);

   return gre;

}


//_____________________________________________________________________________
//

void comparison_eta_spectra()
{

  //_____________________________________________________________________________
  // Those below are the fully corrected eta spectra from May 2003 
  // (Saskia fast MC)
  
  // 0-20%:
  // 2.5 0.0331684 0.0154323
  // 3.5 0.00110986 0.000364858
  // 4.5 0.000217865 6.92224e-05
  // 5.5 4.40058e-05 1.40835e-05
  // 6.5 6.3386e-06 2.84162e-06
  // 7.5 4.57007e-06 1.33397e-06
  
  // 20-60%:
  // 2.5 0.00974967 0.00379689
  // 3.5 0.000523004 8.63736e-05
  // 4.5 9.85748e-05 1.54914e-05
  // 5.5 1.74018e-05 4.07759e-06
  // 6.5 5.30734e-06 1.30249e-06
  // 7.5 1.1827e-06 4.34126e-07
  
  // 60-92%:
  // 2.5 0.000788346 0.000285904
  // 3.5 6.62863e-05 1.17056e-05
  // 4.5 1.43029e-05 2.63236e-06
  // 5.5 2.37609e-06 9.39851e-07
  // 6.5 3.0389e-07 3.71875e-07
  
  const int ptbins = 6;
  double pt[ptbins]  = {2.5,3.5,4.5,5.5,6.5,7.5};
  double ept[ptbins] = {0.05,0.05,0.05,0.05,0.05,0.05};
  
  double Eta_fastMC_00_20[ptbins]  = {0.0331684,0.00110986,0.000217865,4.40058e-05,6.3386e-06,4.57007e-06};
  double Eta_fastMC_err_00_20[ptbins]  = {0.0154323,0.000364858,6.92224e-05,1.40835e-05,2.84162e-06,1.33397e-06};
  double Eta_fastMC_20_60[ptbins]  = {0.00974967,0.000523004,9.85748e-05,1.74018e-05,5.30734e-06,1.1827e-06};
  double Eta_fastMC_err_20_60[ptbins]  = {0.00379689,8.63736e-05,1.54914e-05,4.07759e-06,1.30249e-06,4.34126e-07};
  double Eta_fastMC_60_92[ptbins]  = {0.000788346,6.62863e-05,1.43029e-05,2.37609e-06,3.0389e-07};
  double Eta_fastMC_err_60_92[ptbins]  = { 0.000285904,1.17056e-05,2.63236e-06,9.39851e-07,3.71875e-07};
  
  TGraphErrors *eta_fastMC_00_20 = new TGraphErrors(ptbins,pt,Eta_fastMC_00_20,ept,Eta_fastMC_err_00_20);
  TGraphErrors *eta_fastMC_20_60 = new TGraphErrors(ptbins,pt,Eta_fastMC_20_60,ept,Eta_fastMC_err_20_60);
  TGraphErrors *eta_fastMC_60_92 = new TGraphErrors(ptbins-1,pt,Eta_fastMC_60_92,ept,Eta_fastMC_err_60_92);
  
  //_____________________________________________________________________________
  // Those below are the fully corrected eta spectra from July 2003 
  // (D.d'E. first embedding 7% smearing, 5x5 dead&warn)

  // 0-20%:
  //    pT    epT        Npi0       eNpi0    eNpi0_stat    eNpi0_sys    eNpi0_ccuncorr    eNpi0_pTcorr.
  //   2.50   0.05      2.788e-02    9.447e-03    3.297e-03    8.853e-03    9.280e-03    1.394e-03  
  //   3.50   0.05      9.119e-04    4.077e-04    2.856e-04    2.910e-04    4.025e-04    4.560e-05  
  //   4.50   0.05      1.824e-04    7.557e-05    4.776e-05    5.857e-05    7.417e-05    9.121e-06  
  //   5.50   0.05      3.781e-05    1.505e-05    8.788e-06    1.222e-05    1.468e-05    1.890e-06  
  //   6.50   0.05      5.519e-06    3.017e-06    2.424e-06    1.797e-06    2.971e-06    2.760e-07  
  //   7.50   0.05      3.928e-06    1.717e-06    1.136e-06    1.288e-06    1.668e-06    1.964e-07  
  
  // 20-60%:
  //    pT    epT        Npi0       eNpi0    eNpi0_stat    eNpi0_sys    eNpi0_ccuncorr    eNpi0_pTcorr.
  //   2.50   0.05      7.532e-03    2.476e-03    6.418e-04    2.391e-03    2.429e-03    3.766e-04  
  //   3.50   0.05      4.090e-04    1.433e-04    5.909e-05    1.305e-04    1.403e-04    2.045e-05  
  //   4.50   0.05      7.921e-05    2.777e-05    1.115e-05    2.543e-05    2.705e-05    3.961e-06  
  //   5.50   0.05      1.434e-05    5.306e-06    2.581e-06    4.635e-06    5.154e-06    7.170e-07  
  //   6.50   0.05      4.454e-06    1.721e-06    9.272e-07    1.450e-06    1.667e-06    2.227e-07  
  //   7.50   0.05      1.002e-06    4.826e-07    3.537e-07    3.284e-07    4.714e-07    5.008e-08   
  
  // 60-92%:
  //   pT    epT        Npi0       eNpi0    eNpi0_stat    eNpi0_sys    eNpi0_ccuncorr    eNpi0_pTcorr.
  //  2.50   0.05      5.998e-04    2.049e-04    7.569e-05    1.904e-04    2.014e-04    2.999e-05  
  //  3.50   0.05      5.183e-05    1.873e-05    8.779e-06    1.654e-05    1.836e-05    2.592e-06  
  //  4.50   0.05      1.170e-05    4.253e-06    1.992e-06    3.758e-06    4.151e-06    5.852e-07  
  //  5.50   0.05      2.021e-06    9.982e-07    7.548e-07    6.531e-07    9.823e-07    1.010e-07  
  //  6.50   0.05      2.636e-07    3.328e-07    3.215e-07    8.582e-08    3.318e-07    1.318e-08  
  
  // 0-92%:
  //   pT    epT        Npi0       eNpi0    eNpi0_stat    eNpi0_sys    eNpi0_ccuncorr    eNpi0_pTcorr.
  //  2.50   0.05      9.485e-03    3.072e-03    6.053e-04    3.012e-03    3.012e-03    4.742e-04  
  //  3.50   0.05      3.929e-04    1.389e-04    5.987e-05    1.254e-04    1.361e-04    1.965e-05  
  //  4.50   0.05      6.895e-05    2.395e-05    9.144e-06    2.214e-05    2.332e-05    3.448e-06  
  //  5.50   0.05      1.516e-05    5.291e-06    1.996e-06    4.900e-06    5.121e-06    7.579e-07  
  //  6.50   0.05      3.264e-06    1.239e-06    6.376e-07    1.063e-06    1.199e-06    1.632e-07  
  //  7.50   0.05      1.251e-06    4.957e-07    2.782e-07    4.102e-07    4.784e-07    6.256e-08  

  double Eta_embedd1_00_20[ptbins]  = {2.788e-02,9.119e-04,1.824e-04,3.781e-05,5.519e-06,3.928e-06};
  double Eta_embedd1_err_00_20[ptbins]  = {9.447e-03,4.077e-04,7.557e-05,1.505e-05,3.017e-06,1.717e-06};
  double Eta_embedd1_20_60[ptbins]  = {7.532e-03,4.090e-04,7.921e-05,1.434e-05,4.454e-06,1.002e-06};
  double Eta_embedd1_err_20_60[ptbins]  = {2.476e-03,1.433e-04,2.777e-05,5.306e-06,1.721e-06,4.826e-07};
  double Eta_embedd1_60_92[ptbins]  = {5.998e-04,5.183e-05,1.170e-05,2.021e-06,2.636e-07};
  double Eta_embedd1_err_60_92[ptbins]  = {2.049e-04,1.873e-05,4.253e-06,9.982e-07,3.328e-07};
  double Eta_embedd1_00_92[ptbins]  = {9.485e-03,3.929e-04,6.895e-05,1.516e-05,3.264e-06,1.251e-06};
  double Eta_embedd1_err_00_92[ptbins]  = {3.072e-03,1.389e-04,2.395e-05,5.291e-06,1.239e-06,4.957e-07};
  
  TGraphErrors *eta_embedd1_00_20 = new TGraphErrors(ptbins,pt,Eta_embedd1_00_20,ept,Eta_embedd1_err_00_20);
  TGraphErrors *eta_embedd1_20_60 = new TGraphErrors(ptbins,pt,Eta_embedd1_20_60,ept,Eta_embedd1_err_20_60);
  TGraphErrors *eta_embedd1_60_92 = new TGraphErrors(ptbins-1,pt,Eta_embedd1_60_92,ept,Eta_embedd1_err_60_92);
  TGraphErrors *eta_embedd1_00_92 = new TGraphErrors(ptbins-1,pt,Eta_embedd1_00_92,ept,Eta_embedd1_err_00_92);

  //_____________________________________________________________________________
  // Preliminary eta spectra Oct. 2003 
  // (5% smearing, 3x3 dead&warn)

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  e.setEfficParticle("eta");
  e.setFit(false);
  TGraphErrors *eband;

  e.keepPlot(false);
  TGraphErrors *eta_prelim_oct03_00_20 = (TGraphErrors*)
    e.plot_spectrum_from_ascii("eta_prelim_0_20_3x3tof1chisq1.txt",eband);
  TGraphErrors *eta_prelim_oct03_20_60 = (TGraphErrors*)
    e.plot_spectrum_from_ascii("eta_prelim_20_60_3x3tof1chisq1.txt",eband);
  TGraphErrors *eta_prelim_oct03_60_92 = (TGraphErrors*)
    e.plot_spectrum_from_ascii("eta_prelim_60_92_3x3tof1chisq1.txt",eband);
  TGraphErrors *eta_prelim_oct03_00_92 = (TGraphErrors*)
    e.plot_spectrum_from_ascii("eta_prelim_0_100_3x3tof1chisq1.txt",eband);

  //_____________________________________________________________________________
  // Final fully corrected yields

  e.keepPlot();
  e.setConstrainedHagFit(false);
  e.setFit(true);

  TLegend *leg = new TLegend(0.305369,0.809783,0.880872,0.95471,NULL,"brNDC");
  leg->SetTextSize(0.034);
  leg->SetFillColor(kWhite);

  char* cut="tof2chisq2"; 
  char* correction="full";

  /*
   To get the final central spectrum run:

   TGraphErrors *eta0_20_final1 = (TGraphErrors *)e.plot_spectrum(0,20,cut,correction,1);   // binshift = 1
   TGraphErrors *eta0_20_final2 = (TGraphErrors *)e.plot_spectrum(0,20,cut,correction,0.5); // binshift = 0.5
   
   Then dump eta0_20_final1 and eta0_20_final2 into "eta_pbsc_0_20_tof2chisq2.txt"

  */

  TGraphErrors *eta0_20_final = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_0_20_tof2chisq2.txt",eband);
  emcAnalyzerUtils::setMarkerLineType(eta0_20_final, 20, 2, 2);

  emcAnalyzerUtils::setMarkerLineType(eta_fastMC_00_20, 28, 1, 2);
  emcAnalyzerUtils::setMarkerLineType(eta_prelim_oct03_00_20, 21, 9, 1.8);
  //emcAnalyzerUtils::setMarkerLineType(eta_embedd1_00_20, 29, 9, 2.2);
  eta_fastMC_00_20->Draw("P");
  eta_prelim_oct03_00_20->Draw("P");
  //eta_embedd1_00_20->Draw("P");

  leg->AddEntry(eta0_20_final, "Final #eta", "P");
  leg->AddEntry(eta_prelim_oct03_00_20, "Prelim. #eta Oct.'03 (not abs. scale)", "P");
  leg->AddEntry(eta_fastMC_00_20, "First #eta May'03 (fast MC)", "P");
  //leg->AddEntry(eta_embedd1_00_20, "eta July 2003 (1st embedding)", "P");
  leg->Draw();

  /*
    To get the final semicentral spectrum run:
    
    TGraphErrors *eta20_60_final1 = (TGraphErrors *)e.plot_spectrum(20,60,cut,correction,1); // binshift = 1
    TGraphErrors *eta20_60_final2 = (TGraphErrors *)e.plot_spectrum(20,60,cut,correction,0.5); // binshift = 0.5
    
    Then dump eta20_60_final1 and eta20_60_final2 into "eta_pbsc_20_60_tof2chisq2.txt"

  */

  TGraphErrors *eta20_60_final = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_20_60_tof2chisq2.txt",eband);
  emcAnalyzerUtils::setMarkerLineType(eta20_60_final, 20, 2, 2);

  emcAnalyzerUtils::setMarkerLineType(eta_fastMC_20_60, 28, 1, 2);
  emcAnalyzerUtils::setMarkerLineType(eta_prelim_oct03_20_60, 29, 9, 2.2);
  //emcAnalyzerUtils::setMarkerLineType(eta_embedd1_20_60, 29, 9, 2.2);
  eta_fastMC_20_60->Draw("P");
  eta_prelim_oct03_20_60->Draw("P");
  //eta_embedd1_20_60->Draw("P");
  TLegend *leg2 = leg->Clone();
  leg2->Draw();

  /*
    To get the final peripheral spectrum run:
    
    TGraphErrors *eta60_92_final1 = (TGraphErrors *)e.plot_spectrum(60,92,cut,correction,1);  // binshift = 1
    TGraphErrors *eta60_92_final2 = (TGraphErrors *)e.plot_spectrum(60,92,cut,correction,0.5);  // binshift = 0.5
    
    Then dump eta60_92_final1 and eta60_92_final2 into "eta_pbsc_60_92_tof2chisq2.txt"

  */

  TGraphErrors *eta60_92_final = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_60_92_tof2chisq2.txt",eband);
  emcAnalyzerUtils::setMarkerLineType(eta60_92_final, 20, 2, 2);

  emcAnalyzerUtils::setMarkerLineType(eta_fastMC_60_92, 28, 1, 2);
  emcAnalyzerUtils::setMarkerLineType(eta_prelim_oct03_60_92, 29, 9, 2.2);
  //emcAnalyzerUtils::setMarkerLineType(eta_embedd1_60_92, 29, 9, 2.2);
  eta_fastMC_60_92->Draw("P");
  eta_prelim_oct03_60_92->Draw("P");
  //eta_embedd1_60_92->Draw("P");
  TLegend *leg3 = leg->Clone();
  leg3->Draw();

  /*
   To get the final min.bias spectrum run:

   TGraphErrors *eta0_92_final2 = (TGraphErrors *)e.plot_spectrum(0,100,cut,correction,0.5); // binshift = 0.5
   TGraphErrors *eta0_92_final1 = (TGraphErrors *)e.plot_spectrum(0,100,cut,correction,1); // binshift = 1
   
   Then dump eta0_92_final1 and eta0_92_final2 into "eta_pbsc_0_92_tof2chisq2.txt"

  */

  TGraphErrors *eta0_92_final = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_0_100_tof2chisq2.txt",eband);
  emcAnalyzerUtils::setMarkerLineType(eta0_92_final, 20, 2, 2);

  emcAnalyzerUtils::setMarkerLineType(eta_prelim_oct03_00_92, 29, 9, 2.2);
  //emcAnalyzerUtils::setMarkerLineType(eta_embedd1_00_92, 29, 9, 2.2);
  eta_prelim_oct03_00_92->Draw("P");
  //eta_embedd1_00_92->Draw("P");
  TLegend *leg4 = leg->Clone();
  leg4->Draw();

  /*
    .!mv fully_corr_cent0_20_tof2chisq2.gif ~/afsphnx/fully_corr_cent0_20_tof2chisq2_comparison_previous.gif
    .!mv fully_corr_cent20_60_tof2chisq2.gif ~/afsphnx/fully_corr_cent20_60_tof2chisq2_comparison_previous.gif
    .!mv fully_corr_cent60_92_tof2chisq2.gif ~/afsphnx/fully_corr_cent60_92_tof2chisq2_comparison_previous.gif
    .!mv fully_corr_cent0_100_tof2chisq2.gif ~/afsphnx/fully_corr_cent0_92_tof2chisq2_comparison_previous.gif


  */

}

//_____________________________________________________________________________
//

void dump_spectra()
{

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e;  
  TGraphErrors *eband;
  e.setEfficParticle("eta");
  e.setVerbose(0);

  /*

  e.plot_spectrum(0,100,"tof2chisq2","full"); >> fully_corrected_0_100_tof2chisq2.txt
  e.plot_spectrum(60,92,"tof2chisq2","full"); >> fully_corrected_60_92_tof2chisq2.txt
  e.plot_spectrum(20,60,"tof2chisq2","full"); >> fully_corrected_20_60_tof2chisq2.txt
  e.plot_spectrum(0,20,"tof2chisq2","full"); >> fully_corrected_0_20_tof2chisq2.txt  
  
  e.setDumpLaTeX();
  e.setVerbose(0);
  e.plot_spectrum_from_ascii("eta_pbsc_0_100_tof2chisq2.txt",eband); > fully_corrected_0_100_tof2chisq2.tex
  e.plot_spectrum_from_ascii("eta_pbsc_60_92_tof2chisq2.txt",eband); > fully_corrected_60_92_tof2chisq2.tex
  e.plot_spectrum_from_ascii("eta_pbsc_20_60_tof2chisq2.txt",eband); > fully_corrected_20_60_tof2chisq2.tex
  e.plot_spectrum_from_ascii("eta_pbsc_0_20_tof2chisq2.txt",eband); > fully_corrected_0_20_tof2chisq2.tex

  */
}

//_____________________________________________________________________________
//

void plot_error_boxes( TGraphErrors *g, TCanvas *c, int col )
{

  TGraphErrors *errg= (TGraphErrors*)g->Clone();
  for (int i = 0; i < errg->GetN();i++) { errg->SetPointError(i,0.2,0.);}
  emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(errg,"pT","+",1);

  TClonesArray* errboxes = (TClonesArray*)emcAnalyzerUtils::errorBoxes( errg, 0.);
  TBox *box = 0;
  for (int j = 0; j < errboxes->GetEntries();j++)
    {
      box = ((TBox*)(*errboxes)[j]);
      box->SetFillStyle(3001);
      box->SetFillColor(col);
      c->cd();
      box->Draw();
    }

  return;
}

//_____________________________________________________________________________
//

void plot_Rcp_eta()
{

  char *title="eta_Rcp_AuAu_200GeV_phenix";
  int xbins=100; double xmin=0.; double xmax=10.;
  int ybins=100; double ymin=0.; double ymax=1.3;
  char *xtitle="p_{T} (GeV/#font[72]{c})";
  char *ytitle="R_{cp} eta";
  
  TH2F *myframe = new TH2F(title,title,xbins,xmin,xmax,ybins,ymin,ymax);
  myframe->SetStats(0);
  myframe->SetTitle(title);
  myframe->SetXTitle(xtitle);
  myframe->SetYTitle(ytitle);
  
  myframe->GetXaxis()->SetTitleSize(0.055);
  myframe->GetXaxis()->SetTitleOffset(0.9);
  myframe->GetXaxis()->SetLabelSize(0.05);
  
  myframe->GetYaxis()->SetTitleSize(0.055);
  myframe->GetYaxis()->SetTitleOffset(1.);
  myframe->GetYaxis()->SetLabelSize(0.05);
  myframe->Draw();
  
  TLine *line = new TLine(2,1,7.5,1);
  //TLine *line = new TLine(xmin,1,xmax,1);
  line->SetLineColor(9);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();
  
  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  e.setEfficParticle("eta");

  TGraphErrors *cent = (TGraphErrors *)e.plot_spectrum(0,20,"tof2chisq2","full");
  TGraphErrors *periph = (TGraphErrors *)e.plot_spectrum(60,92,"tof2chisq2","full");
  emcAnalyzerUtils::scale(*cent,1./779.);
  emcAnalyzerUtils::scale(*periph,1./14.5);
  c1.cd();
  TGraphErrors* rcp= (TGraphErrors*)emcAnalyzerUtils::ratio(cent,periph);
  rcp.Draw("P");
  
  /*

    .!mv c1.eps ~/afsphnx/Rcp_eta.eps
    .!mv c1.gif ~/afsphnx/Rcp_eta.gif

  */

  e.setEfficParticle("pi0");
  TGraphErrors *pi0cent = (TGraphErrors *)e.plot_spectrum(0,10,"tof2chisq2","full");
  TGraphErrors *pi0per = (TGraphErrors *)e.plot_spectrum(60,92,"tof2chisq2","full");
  emcAnalyzerUtils::scale(*pi0cent,1./955.);
  emcAnalyzerUtils::scale(*pi0per,1./14.5);
  c1.cd();
  TGraphErrors *Rcp_pi0 = (TGraphErrors*)emcAnalyzerUtils::ratio(pi0cent,pi0per);
  Rcp_pi0.Draw("P");

  /*

  .!mv c1.eps ~/afsphnx/Rcp_eta_pi0.eps
  .!mv c1.gif ~/afsphnx/Rcp_eta_pi0.gif

  */
}


//_____________________________________________________________________________
//

void plot_RAA_eta( bool dump = false )
{

  char *title="eta_RAA_200GeV_phenix";
  int xbins=100; double xmin=0.; double xmax=11.;
  int ybins=100; double ymin=0.; double ymax=1.5;
  char *xtitle="p_{T} (GeV/#font[72]{c})";
  char *ytitle="R_{AA}";
  
  TCanvas *c10 = new TCanvas(title, "c10",30,62,650,518);
  c10->Range(-1.48381,-0.2,11.2968,1.54074);
  c10->SetFillColor(0);
  c10->SetBorderMode(0);
  c10->SetBorderSize(2);
  c10->SetLeftMargin(0.116099);
  c10->SetRightMargin(0.0232198);
  c10->SetTopMargin(0.0234043);
  c10->SetBottomMargin(0.114894);
  c10->SetFrameBorderMode(0);
   
  TH2F *myframe = new TH2F(title,title,xbins,xmin,xmax,ybins,ymin,ymax);
  myframe->SetStats(0);
  myframe->SetTitle(title);
  myframe->SetXTitle(xtitle);
  myframe->SetYTitle(ytitle);
  
  myframe->GetXaxis()->SetTitleSize(0.055);
  myframe->GetXaxis()->SetTitleOffset(0.9);
  myframe->GetXaxis()->SetLabelSize(0.055);
  
  myframe->GetYaxis()->SetTitleSize(0.055);
  myframe->GetYaxis()->SetTitleOffset(1.);
  myframe->GetYaxis()->SetLabelSize(0.055);
  myframe->Draw();
  
  //TLine *line = new TLine(2,1,7.5,1);
  TLine *line = new TLine(xmin,1,xmax,1);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();
  
  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  TGraphErrors *eband;
  e.setEfficParticle("eta");
  e.setFit(false);

  //_____________________________________________________________________________
  // p+p eta

  TGraphErrors *pp_eta = (TGraphErrors*)e.plot_spectrum_from_ascii("eta_pp_emcal_200GeV.txt",eband); 
  //double sigma = 42.2;
  //emcAnalyzerUtils::scale(*pp_eta,sigma);

  // p+p eta fit

  double ptmin = 1., ptmax = 14.;
  TF1 *pp_eta_fit = new TF1("pp_eta_fit", "[0]/(exp([2]*x*x+[3]*x)+x/[1])^[4]", ptmin,ptmax);
  pp_eta_fit->SetParameters(2.5,10.,0.,0.,20.);
  pp_eta_fit->SetParLimits(0,2.3,2.5);
  //pp_eta->Fit("pp_eta_fit","Q","",ptmin,ptmax);

  //TF1 *pp_eta_fit = (TF1*)pp->GetListOfFunctions()->FindObject("constrhagedornascii");
  //TF1 *pp_eta_fit = (TF1*)pp->GetListOfFunctions()->FindObject("hagedornascii");

  e.keepPlot(false);

  //_____________________________________________________________________________
  // Au+Au eta

  TGraphErrors *cent = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_0_20_tof2chisq2.txt",eband);
  TGraphErrors *semicent = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_20_60_tof2chisq2.txt",eband);
  TGraphErrors *periph = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_60_92_tof2chisq2.txt",eband);
  TGraphErrors *mb = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_0_100_tof2chisq2.txt",eband);

  double TAA_cent   = emcAnalyzerUtils::getTAB(0,20);
  double TAA_semicent = emcAnalyzerUtils::getTAB(20,60);
  double TAA_periph = emcAnalyzerUtils::getTAB(60,92);
  double TAA_mb = emcAnalyzerUtils::getTAB(0,100);

  double eTAA_cent   = emcAnalyzerUtils::getTAB(0,20,"error")/TAA_cent;
  double eTAA_semicent = emcAnalyzerUtils::getTAB(20,60,"error")/TAA_semicent;
  double eTAA_periph = emcAnalyzerUtils::getTAB(60,92,"error")/TAA_periph;
  double eTAA_mb = emcAnalyzerUtils::getTAB(0,100,"error")/TAA_mb;

  emcAnalyzerUtils::scale(*cent,1./(TAA_cent));
  emcAnalyzerUtils::scale(*semicent,1./(TAA_semicent));
  emcAnalyzerUtils::scale(*periph,1./(TAA_periph));
  emcAnalyzerUtils::scale(*mb,1./(TAA_mb));

  double run3_pp_enorm = 0.097;
  TBox *eNormppBox = new TBox(10.4,1.-run3_pp_enorm,10.7,1+run3_pp_enorm);
  eNormppBox->SetFillColor(14);
  c10->cd();
  eNormppBox->Draw("same");

  TH1F *eNormBox[3];
  TH1F *eNormBox2[3];
  TLegend *leg2[3];

  for (int j=0;j<3;j++)
  {
    char tit[100];
    sprintf(tit,"eNormBox%d",j);
    eNormBox[j]= new TH1F(tit,"",2,0.2+j/2.,0.8+j/2.);
    eNormBox2[j]= new TH1F(tit,"",2,0.1,0.9); //(tit,"",2,0.5,1.1);

    double normerr = eTAA_cent; 
    int col = 2; 
    if (j==1) { normerr = eTAA_semicent; col=94; }
    if (j==2) { normerr = eTAA_periph; col=38; }
    //if (j==3) normerr = sqrt(run3_pp_enorm*run3_pp_enorm+eTAA_mb*eTAA_mb);  

    for (int i=0;i<=2;i++)
      {
	eNormBox[j]->SetBinContent(i,1.);
	eNormBox[j]->SetBinError(i,normerr);
	eNormBox2[j]->SetBinContent(i,1.);
	eNormBox2[j]->SetBinError(i,normerr);
      }

    eNormBox[j]->SetFillColor(col);//16+j);
    eNormBox2[j]->SetFillColor(col);//16+j);
    //eNormBox[j]->SetFillStyle(3001);
    c10->cd();
    eNormBox[j]->Draw("e3same");

    // for the individual eta vs pi0 R_AA plots ...

    leg2[j] = new TLegend(0.305369,0.809783,0.880872,0.95471,NULL,"brNDC");
    leg2[j]->SetTextSize(0.038);
    leg2[j]->SetFillColor(kWhite);
  }

  e.keepPlot();

  e.setVerbose(0);

  //_____________________________________________________________________________
  // 0-20% central

  c10->cd();

  // Bin-to-fit ratio
  //TGraphErrors* RAAcent = (TGraphErrors*)emcAnalyzerUtils::ratio(cent,pp_eta_fit);

  // Bin-to-bin ratio
  TGraphErrors* RAAcent = (TGraphErrors*)emcAnalyzerUtils::ratio(cent,pp_eta);
  emcAnalyzerUtils::setMarkerLineType(RAAcent, 20, 2, 2);

  //double common_err = -1.*sqrt(0.05*0.05+0.092*0.092); // 5% acceptance + 9.2% BBC (moved to box)
  double common_err = -0.092; // 9.2% BBC (moved to box)
  emcAnalyzerUtils::AddOrSubstractRelatError(*RAAcent,common_err,1);
  emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(RAAcent,"pT","-",1);  // acceptance+energy-scale errors cancel

  emcAnalyzerUtils::displaceXvalues(*RAAcent,0.05);
  //plot_error_boxes(RAAcent,c10,2);
  RAAcent->Draw("P");
  TGraphErrors* RAAcent2 = (TGraphErrors*)RAAcent->Clone();
  emcAnalyzerUtils::setMarkerLineType(RAAcent2, 24, 1, 2);
  RAAcent2->Draw("P");

  if (dump)
    {
       //emcAnalyzerUtils::dumpGraph(RAAcent,"x-ex-y-ey");

      const int N = 9;
      double errNorm[N];
      
      for (int i = 0; i < N; i++)
	{
	  double x, y;
	  RAAcent->GetPoint(i, x, y);
	  double ex = RAAcent->GetErrorX(i);
	  double ey = RAAcent->GetErrorY(i);
	  double erelat = ey/y*100.;
	  errNorm[i] = eTAA_cent*y;
	  printf("  %.2f    %.2f    %.3f    %.3f    %.1f    %.3f\n", x, ex, y, ey, erelat, errNorm[i]);
	}

      e.dumpLaTeX(RAAcent,errNorm);

    }
  
  //_____________________________________________________________________________
  // 20-60% semicentral

  c10->cd();
  //TGraphErrors* RAAsemicent= (TGraphErrors*)emcAnalyzerUtils::ratio(semicent,pp_eta_fit);
  TGraphErrors* RAAsemicent= (TGraphErrors*)emcAnalyzerUtils::ratio(semicent,pp_eta);
  emcAnalyzerUtils::setMarkerLineType(RAAsemicent, 21, 94, 1.7);

  common_err = -0.092; // 9.2% BBC (moved to box)
  emcAnalyzerUtils::AddOrSubstractRelatError(*RAAsemicent,common_err,1);
  emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(RAAsemicent,"pT","-",1);  // acceptance+energy-scale errors cancel

  //plot_error_boxes(RAAsemicent,c10,4);

  RAAsemicent->Draw("P");
  TGraphErrors* RAAsemicent2 = (TGraphErrors*)RAAsemicent->Clone();
  emcAnalyzerUtils::setMarkerLineType(RAAsemicent2, 25, 1, 1.7);
  RAAsemicent2->Draw("P");

  if (dump)
    {
      //emcAnalyzerUtils::dumpGraph(RAAsemicent,"x-ex-y-ey");
      const int N = 9;
      double errNorm[N];
      
      for (int i = 0; i < N; i++)
	{
	  double x, y;
	  RAAsemicent->GetPoint(i, x, y);
	  double ex = RAAsemicent->GetErrorX(i);
	  double ey = RAAsemicent->GetErrorY(i);
	  double erelat = ey/y*100.;
	  errNorm[i] = eTAA_semicent*y;
	  printf("  %.2f    %.2f    %.3f    %.3f    %.1f    %.3f\n", x, ex, y, ey, erelat, errNorm[i]);
	}

      e.dumpLaTeX(RAAsemicent,errNorm);  
    }

  //_____________________________________________________________________________
  // 60-92% peripheral

  c10->cd();
  //TGraphErrors* RAAperiph= (TGraphErrors*)emcAnalyzerUtils::ratio(periph,pp_eta_fit);
  TGraphErrors* RAAperiph= (TGraphErrors*)emcAnalyzerUtils::ratio(periph,pp_eta);
  emcAnalyzerUtils::setMarkerLineType(RAAperiph, 22, 38, 2);

  common_err = -0.092; // 9.2% BBC (moved to box)
  emcAnalyzerUtils::AddOrSubstractRelatError(*RAAperiph,common_err,1);
  emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(RAAperiph,"pT","-",1);  // acceptance+energy-scale errors cancel

  emcAnalyzerUtils::displaceXvalues(*RAAperiph,-0.05);
  //plot_error_boxes(RAAperiph,c10,7);

  RAAperiph->Draw("P");
  TGraphErrors* RAAperiph2 = (TGraphErrors*)RAAperiph->Clone();
  emcAnalyzerUtils::setMarkerLineType(RAAperiph2, 26, 4, 2);
  RAAperiph2->Draw("P");

  if (dump)
    {
      //emcAnalyzerUtils::dumpGraph(RAAperiph,"x-ex-y-ey");

      const int N = 5;
      double errNorm[N];
      
      for (int i = 0; i < N; i++)
	{
	  double x, y;
	  RAAperiph->GetPoint(i, x, y);
	  double ex = RAAperiph->GetErrorX(i);
	  double ey = RAAperiph->GetErrorY(i);
	  double erelat = ey/y*100.;
	  errNorm[i] = eTAA_periph*y;
	  printf("  %.2f    %.2f    %.3f    %.3f    %.1f    %.3f\n", x, ex, y, ey, erelat, errNorm[i]);
	}

      e.dumpLaTeX(RAAperiph,errNorm);  
    }

  RAAcent->Draw("P");

  //_____________________________________________________________________________
  // 0-92% min.bias 

  c10->cd();
  //TGraphErrors* RAAmb= (TGraphErrors*)emcAnalyzerUtils::ratio(mb,pp_eta_fit);
  TGraphErrors* RAAmb= (TGraphErrors*)emcAnalyzerUtils::ratio(mb,pp_eta);
  emcAnalyzerUtils::setMarkerLineType(RAAmb, 20, 6, 2);

  common_err = -0.092; // 9.2% BBC (moved to box)
  emcAnalyzerUtils::AddOrSubstractRelatError(*RAAmb,common_err,1);
  emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(RAAmb,"pT","-",1);  // acceptance+energy-scale errors cancel

  //plot_error_boxes(RAAmb,c10);
  //RAAmb->Draw("P");

  //_____________________________________________________________________________
  //

  TLegend *leg = new TLegend(0.544892,0.768443,0.959752,0.97541,"Au+Au#rightarrow #eta+#font[72]{X}","brNDC");
  leg->SetTextSize(0.041);
  leg->SetFillColor(kWhite);
  leg->AddEntry(RAAcent, "0-20% central", "P");
  leg->AddEntry(RAAsemicent, "20-60% semicentral", "P");
  leg->AddEntry(RAAperiph,"60-92% peripheral", "P");
  //leg->AddEntry(RAAmb,"0-92% ", "P");
  leg->Draw();

  return;

  //_____________________________________________________________________________
  //
  // Compare R_AA pi0 to eta
  // 

  e.setEfficParticle("pi0");
  e.setFit(false);
  e.keepPlot();

  //TGraphErrors *pppi0 = (TGraphErrors *)e.plot_spectrum_from_ascii("pi0_pp_emcal_200GeV_run2_cross_published.txt",eband);
  //TF1 *hag_pp = (TF1*)pppi0->GetListOfFunctions()->FindObject("constrhagedornascii");

  TGraphErrors *vitev=e.plot_spectrum_from_ascii("R_AA_theor_vitev_rhic_pi0_dNdy1100.txt",eband);
  //vitev->Draw("L");
  vitev->SetLineWidth(5);
  vitev->SetLineColor(6);

  TGraphErrors* RAApi0cent= (TGraphErrors*)e.plot_RAA(0,20,"emcalrun3",false);
  //TGraphErrors* RAApi0cent= (TGraphErrors*)e.plot_RAA(0,10,"emcalrun3",false);
  RAApi0cent->Draw("P");

  emcAnalyzerUtils::setMarkerLineType(RAApi0cent, 24, 2, 2);
  RAAcent->Draw("P");
  eNormBox2[0]->Draw("e3same");
  leg2[0]->AddEntry(RAApi0cent, "Au+Au #pi^{0} (0-20% central)", "P");
  leg2[0]->AddEntry(RAAcent, "Au+Au #eta (0-20\% central)", "P");
  leg2[0]->Draw();

  if (dump)
    {
      TGraphErrors* RAApi0centfit= (TGraphErrors*)e.plot_RAA(0,20,"emcalrun3",false,true,true);
      e.dumpLaTeX(RAApi0centfit);
    }

  TGraphErrors* RAApi0semicent= (TGraphErrors*)e.plot_RAA(20,60,"emcalrun3",false);
  emcAnalyzerUtils::setMarkerLineType(RAApi0semicent, 24, 4, 2);
  RAAsemicent->Draw("P");
  eNormBox2[1]->Draw("e3same");
  leg2[1]->AddEntry(RAApi0semicent, "Au+Au #pi^{0} (20-60% semicentral)", "P");
  leg2[1]->AddEntry(RAAsemicent, "Au+Au #eta (20-60\% semicentral)", "P");
  leg2[1]->Draw();

  if (dump)
    {
      TGraphErrors* RAApi0semicentfit= (TGraphErrors*)e.plot_RAA(0,20,"emcalrun3",false,true,true);
      e.dumpLaTeX(RAApi0semicentfit);
    }

  TGraphErrors* RAApi0periph= (TGraphErrors*)e.plot_RAA(60,92,"emcalrun3",false);
  emcAnalyzerUtils::setMarkerLineType(RAApi0periph, 24, 7, 2);
  RAAperiph->Draw("P");
  eNormBox2[2]->Draw("e3same");
  leg2[2]->AddEntry(RAApi0periph, "Au+Au #pi^{0} (60-92% peripheral)", "P");
  leg2[2]->AddEntry(RAAperiph, "Au+Au #eta (60-92\% peripheral)", "P");
  leg2[2]->Draw();

  if (dump)
    {
      TGraphErrors* RAApi0periphfit= (TGraphErrors*)e.plot_RAA(0,20,"emcalrun3",false,true,true);
      e.dumpLaTeX(RAApi0periphfit);
    }

  TGraphErrors* RAApi0mb= (TGraphErrors*)e.plot_RAA(0,100,"emcalrun3",false);
  RAAmb->Draw("P");

  /*

    .!mv c1.eps ~/afsphnx/RAA_eta.eps
    .!mv c1.gif ~/afsphnx/RAA_eta.gif

  */

}


//_____________________________________________________________________________
//

void plot_eta_pi0_ratio( bool readAscii = true, int verbose = 1)
{

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  TGraphErrors *eband;
  e.setFit(false);
  e.setVerbose(verbose);
  e.keepPlot(0);

  double sigma = 42.2;

  gStyle->SetOptFit(0);

  char *title="eta_pi0_ratio_AuAu_200GeV_phenix";
  int xbins=100; double xmin=0.; double xmax=12.;
  int ybins=100; double ymin=0.; double ymax=1.3;
  char *xtitle="p_{T} (GeV/#font[72]{c})";
  char *ytitle="#eta/#pi^{0}";
  
  TCanvas *c1 = new TCanvas(title,title,20,52,720,590);
  c1->Range(-1.54839,-0.182185,12.3441,1.33088);
  c1->SetLeftMargin(0.111455);
  c1->SetRightMargin(0.0247678);
  c1->SetTopMargin(0.0204082);
  c1->SetBottomMargin(0.120408);

  TH2F *myframe = new TH2F(title,title,xbins,xmin,xmax,ybins,ymin,ymax);
  myframe->SetStats(0);
  myframe->SetTitle(title);
  myframe->SetXTitle(xtitle);
  myframe->SetYTitle(ytitle);
  
  myframe->GetXaxis()->SetTitleSize(0.055);
  myframe->GetXaxis()->SetTitleOffset(0.9);
  myframe->GetXaxis()->SetLabelSize(0.05);
  
  myframe->GetYaxis()->SetTitleSize(0.055);
  myframe->GetYaxis()->SetTitleOffset(1.);
  myframe->GetYaxis()->SetLabelSize(0.05);
  myframe->Draw();
  
  TLine *line = new TLine(xmin,1,xmax,1);
  //TLine *line = new TLine(xmin,1,xmax,1);
  line->SetLineColor(1);
  line->SetLineStyle(1);
  line->SetLineWidth(2);
  line->Draw();

  TF1 *etapi0_pythia200GeV=new TF1("etapi0_pythia200GeV","[0]*(1-exp([1]+[2]*x+[3]*x*x))",0.,12.);
  etapi0_pythia200GeV->SetParameters(0.500,0.0895,-1.016,-0.0044);
  etapi0_pythia200GeV->SetLineStyle(2);
  etapi0_pythia200GeV->Draw("same");

  TLine *line2 = new TLine(2.,0.45,xmax,0.45);
  line2->SetLineColor(9);
  line2->SetLineStyle(2);
  line2->SetLineWidth(3);
  //line2->Draw();

  //_____________________________________________________________________________
  // eta's

  e.setEfficParticle("eta");
  e.keepPlot(false);

  TGraphErrors *mbeta = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_0_100_tof2chisq2.txt",eband);
  TGraphErrors *centeta = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_0_20_tof2chisq2.txt",eband);
  TGraphErrors *semicenteta = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_20_60_tof2chisq2.txt",eband);
  TGraphErrors *peripheta = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_60_92_tof2chisq2.txt",eband);
  //TGraphErrors *pp_eta = (TGraphErrors*)e.plot_spectrum_from_ascii("eta_pp_emcal_200GeV.txt",eband);
  //emcAnalyzerUtils::scale(*pp_eta,sigma);

  //_____________________________________________________________________________
  // pi0's

  e.setEfficParticle("pi0");

  double ptmin = 1., ptmax = 12.;
  TF1 *fpi0fit = new TF1("fpi0fit", "[0]/(exp([2]*x*x+[3]*x)+x/[1])^[4]", ptmin,ptmax);

  TF1 *pol0 = new TF1("pol0", "pol0", ptmin,ptmax);

  //_________________
  // min.bias

  if (!readAscii)
    {

      e.keepPlot(true);

      TGraphErrors *mbpi0 = (TGraphErrors *)e.plot_spectrum_from_ascii("phenix_pi0_final_emcal_Run2_AuAu_0_100.txt",eband);
      
      TGraphErrors *eta_pi0_mb = (TGraphErrors*)emcAnalyzerUtils::ratio(mbeta,mbpi0);
      
      fpi0fit->SetParameters(200.,9.,0.,0.,33.);
      mbpi0->Fit("fpi0fit","Q","",ptmin,ptmax);
      TGraphErrors *eta_pi0_mb = (TGraphErrors*)emcAnalyzerUtils::ratio(mbeta,fpi0fit);
      
      emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(eta_pi0_mb,"pT","-",1);  // acc.+en.scale error cancels
      emcAnalyzerUtils::dumpGraph(eta_pi0_mb,"x-y-ey");
    }
  else
    {
      eta_pi0_mb = (TGraphErrors*)e.plot_spectrum_from_ascii("phenix_AuAu_0_100_200GeV_eta_pi0_ratio.txt",eband);
    }

  eta_pi0_mb->Fit("pol0","Q0");
  cout << "<I> eta/pi0 (MB)= " << pol0->GetParameter(0) << " +/- " << pol0->GetParError(0) 
       << " chi2/ndf = " << pol0->GetChisquare()/pol0->GetNDF() << endl;

  //_________________
  // central

  if (!readAscii)
    {
      TGraphErrors *centpi0 = (TGraphErrors *)e.plot_spectrum_from_ascii("phenix_pi0_final_emcal_Run2_AuAu_0_20.txt",eband);

      TGraphErrors *eta_pi0_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(centeta,centpi0);
      
      fpi0fit->SetParameters(1000.,1.,0.,0.,10.);
      centpi0->Fit("fpi0fit","Q","",ptmin,10.);
      TGraphErrors *eta_pi0_cent = (TGraphErrors*)emcAnalyzerUtils::ratio(centeta,fpi0fit);
      
      emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(eta_pi0_cent,"pT","-",1);  // acc.+en.scale error cancels
      emcAnalyzerUtils::dumpGraph(eta_pi0_cent,"x-y-ey");
    }
  else
    {
      eta_pi0_cent = (TGraphErrors*)e.plot_spectrum_from_ascii("phenix_AuAu_0_20_200GeV_eta_pi0_ratio.txt",eband);
    }

  eta_pi0_cent->Fit("pol0","Q0");
  cout << "<I> eta/pi0 (cent) = " << pol0->GetParameter(0) << " +/- " << pol0->GetParError(0) 
       << " chi2/ndf = " << pol0->GetChisquare()/pol0->GetNDF() << endl;

  //_________________
  // semi-central

  if (!readAscii)
    {
      TGraphErrors *semicentpi0 = (TGraphErrors *)e.plot_spectrum_from_ascii("phenix_pi0_final_emcal_Run2_AuAu_20_60.txt",eband);
      
      TGraphErrors *eta_pi0_semicent = (TGraphErrors*)emcAnalyzerUtils::ratio(semicenteta,semicentpi0);
      
      fpi0fit->SetParameters(200.,8.,0.,0.,31.);
      semicentpi0->Fit("fpi0fit","Q","",ptmin,10.);
      TGraphErrors *eta_pi0_semicent = (TGraphErrors*)emcAnalyzerUtils::ratio(semicenteta,fpi0fit);
      
      emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(eta_pi0_semicent,"pT","-",1);  // acc.+en.scale error cancels
      emcAnalyzerUtils::dumpGraph(eta_pi0_semicent,"x-y-ey");
    }
  else
    {
      eta_pi0_semicent = (TGraphErrors*)e.plot_spectrum_from_ascii("phenix_AuAu_20_60_200GeV_eta_pi0_ratio.txt",eband);
    }

  eta_pi0_semicent->Fit("pol0","Q0");
  cout << "<I> eta/pi0 (semicent) = " << pol0->GetParameter(0) << " +/- " << pol0->GetParError(0) 
       << " chi2/ndf = " << pol0->GetChisquare()/pol0->GetNDF() << endl;

  //_________________
  // periph

  if (!readAscii)
    {
      TGraphErrors *periphpi0 = (TGraphErrors *)e.plot_spectrum_from_ascii("phenix_pi0_final_emcal_Run2_AuAu_60_92.txt",eband);

      TGraphErrors *eta_pi0_periph = (TGraphErrors*)emcAnalyzerUtils::ratio(peripheta,periphpi0);
      
      periphpi0->Fit("fpi0fit","Q","",ptmin,7.);
      TGraphErrors *eta_pi0_periph = (TGraphErrors*)emcAnalyzerUtils::ratio(peripheta,fpi0fit);
      
      emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(eta_pi0_periph,"pT","-",1);  // acc.+en.scale error cancels
      //emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(eta_pi0_periph,"RAA","-",1);  // acceptance error cancels
      //emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(eta_pi0_periph,"pT","-",1);  // acceptance error cancels
      emcAnalyzerUtils::dumpGraph(eta_pi0_periph,"x-y-ey");
    }
  else
    {
     eta_pi0_periph = (TGraphErrors*)e.plot_spectrum_from_ascii("phenix_AuAu_60_92_200GeV_eta_pi0_ratio.txt",eband);
    }

  eta_pi0_periph->Fit("pol0","Q0");
  cout << "<I> eta/pi0 (periph) = " << pol0->GetParameter(0) << " +/- " << pol0->GetParError(0) 
       << " chi2/ndf = " << pol0->GetChisquare()/pol0->GetNDF() << endl;


  //_____________________________________________________________________________
  // p+p eta/pi0

  TGraphErrors *eta_pi0_pp = (TGraphErrors*)e.plot_spectrum_from_ascii("phenix_pp_200GeV_eta_pi0_ratio.txt",eband);

  //   TGraphErrors *pp_pi0 = (TGraphErrors*)e.plot_spectrum_from_ascii("pi0_pp_emcal_200GeV_run3.txt",eband);
  //   fpi0fit->SetParameters(300.,1.,0.,0.,10.);
  //   pp_pi0->Fit("fpi0fit","Q","",ptmin,ptmax);
  //   TGraphErrors *eta_pi0_pp = (TGraphErrors*)emcAnalyzerUtils::ratio(pp_eta,fpi0fit);

  eta_pi0_pp->Fit("pol0","Q0","",2.5,14.);
  cout << "<I> eta/pi0 (pp)= " << pol0->GetParameter(0) << " +/- " << pol0->GetParError(0) 
       << " chi2/ndf = " << pol0->GetChisquare()/pol0->GetNDF() << endl;

  //_____________________________________________________________________________
  // d+Au eta/pi0

  TGraphErrors *eta_pi0_dAu = (TGraphErrors*)e.plot_spectrum_from_ascii("phenix_dAu_200GeV_eta2pi0_ratio.txt",eband);

  //TGraphErrors *eta_pi0_dAu = (TGraphErrors*)eta_pi0_ratio_dAu();

  eta_pi0_dAu->Fit("pol0","Q0","",2.5,9.);
  cout << endl << "<I> eta/pi0 (dAu)= " << pol0->GetParameter(0) << " +/- " << pol0->GetParError(0) 
       << " chi2/ndf" << pol0->GetChisquare()/pol0->GetNDF() << " = " << endl << endl;

  //_____________________________________________________________________________
  // Plot ratios

  emcAnalyzerUtils::setMarkerLineType(eta_pi0_mb, 20, 1, 1.8);
  emcAnalyzerUtils::setMarkerLineType(eta_pi0_cent, 20, 2, 1.8);
  emcAnalyzerUtils::setMarkerLineType(eta_pi0_semicent, 21, 94, 1.6);
  emcAnalyzerUtils::setMarkerLineType(eta_pi0_periph, 22, 38, 2.0);
  emcAnalyzerUtils::setMarkerLineType(eta_pi0_pp, 28, 1, 1.6);
  emcAnalyzerUtils::setMarkerLineType(eta_pi0_dAu, 27, 1, 1.9);

//   eta_pi0_pp->Fit(etapi0_pythia200GeV);
//   eta_pi0_cent->Fit(etapi0_pythia200GeV);
//   eta_pi0_semicent->Fit(etapi0_pythia200GeV);
//   eta_pi0_periph->Fit(etapi0_pythia200GeV);

  c1->cd();
  eta_pi0_pp->Draw("P");
  emcAnalyzerUtils::displaceXvalues(*eta_pi0_dAu,-0.25);
  eta_pi0_dAu->Draw("P");

  //eta_pi0_mb->Draw("P");
  c1->cd();
  emcAnalyzerUtils::displaceXvalues(*eta_pi0_cent,+0.1);
  eta_pi0_cent->Draw("P");
  c1->cd();
  emcAnalyzerUtils::displaceXvalues(*eta_pi0_semicent,+0.1);
  eta_pi0_semicent->Draw("P");
  c1->cd();
  emcAnalyzerUtils::displaceXvalues(*eta_pi0_periph,-0.1);
  eta_pi0_periph->Draw("P");

  //_____________________________________________________________________________
  // Labels

  TLegend *le = new TLegend(0.311146,0.718367,0.882353,0.963265,"#eta/#pi^{0} ratio at #sqrt{s_{NN}} = 200 GeV:","brNDC");
  le->SetTextSize(0.040);
  le->SetFillColor(kWhite);
  le->AddEntry(eta_pi0_cent, "  Au+Au cent., semi-cent., periph", "P");
  //le->AddEntry(eta_pi0_semicent, "#eta/#pi^{0} Au+Au semi-central", "P");
  //le->AddEntry(eta_pi0_periph, "Au+Au peripheral", "P");
  //le->AddEntry(eta_pi0_mb, "#eta/#pi^{0} Au+Au min.bias", "P");

//   le->AddEntry(eta_pi0_dAu, "#eta/#pi^{0} d+Au min.bias", "P");
//   le->AddEntry(eta_pi0_pp, "#eta/#pi^{0} p+p ", "P");
//   le->AddEntry(etapi0_pythia200GeV, "#eta/#pi^{0} PYTHIA v6.1 (p+p)", "L");

  le->AddEntry(eta_pi0_dAu, "d+Au min.bias", "P");
  le->AddEntry(eta_pi0_pp,  "p+p", "P");
  le->AddEntry(etapi0_pythia200GeV, "#eta/#pi^{0} PYTHIA v6.1 (p+p)", "L");
  le->Draw();

  double x = 4.2367, y = 1.165;
  TMarker *marker_cent2 = new TMarker(x,y,21);
  marker_cent2->SetMarkerColor(94);
  marker_cent2->SetMarkerSize(1.6);
  marker_cent2->Draw();

  x = 4.64114;
  y =1.16876;
  TMarker *marker_periph = new TMarker(x,y,22);
  marker_periph->SetMarkerColor(38);
  marker_periph->SetMarkerSize(1.8);
  marker_periph->Draw();


  /**
     .!mv c1.eps ~/afsphnx/R_eta_pi0.eps
     .!mv c1.gif ~/afsphnx/R_eta_pi0.gif
  **/

}

//_____________________________________________________________________________
//

void plot_RAA_gamma_pi0_eta_AuAu_200GeV( bool logy = true, bool plot_eta = true )
{

  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e; 
  TGraphErrors *eband;
  e.setEfficParticle("pi0");
  e.setFit(false);
  //e.keepPlot(false);

  char *title="RAA_gamma_pi0_eta_AuAu_200GeV_phenix";
  int xbins=100; double xmin=0.; double xmax=16.;
  int ybins=100; double ymin=0.; double ymax=1.4;

  //if (logy){ ymax = 6.5; ymin = 0.06; }
  if (logy){ ymax = 19.5; ymin = 0.06; }

  char *xtitle="p_{T} (GeV/#font[72]{c})";
  char *ytitle="R_{AA}";
  
  TCanvas *c1 = new TCanvas(title,title,68,115,777,569);
  c1->Range(-1.62238,-1.59863,16.4476,1.50817);
  c1->SetLeftMargin(0.0897833);
  c1->SetRightMargin(0.0247678);
  c1->SetTopMargin(0.0702128);
  c1->SetBottomMargin(0.121277);
  if (logy) c1->SetLogy();

  TH2F *myframe = (TH2F*)e.frame(title, xbins, xmin, xmax, ybins, ymin, ymax, xtitle, ytitle);
  myframe->Draw();
  myframe->GetYaxis()->SetTitleOffset(0.8);
  c1->Update();
  
  TLine *line = new TLine(xmin,1,xmax,1);
  line->SetLineColor(1);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();
  
  //_____________________________________________________________________________
  // pi0

  // we use the fit to the p+p reference spectrum
  TGraphErrors *RAA_pi0cent= (TGraphErrors*)e.plot_RAA(0,10,"emcalrun3",false,true,true); 
  emcAnalyzerUtils::setMarkerLineType(RAA_pi0cent, 22, 94, 2.1);
  TGraphErrors* RAA_pi0cent2 = (TGraphErrors*)RAA_pi0cent->Clone();
  emcAnalyzerUtils::setMarkerLineType(RAA_pi0cent2, 26, 1, 2.1);

  //_____________________________________________________________________________
  // eta

  e.setEfficParticle("eta");
  e.setFit(false);

  TGraphErrors *pp_eta = (TGraphErrors*)e.plot_spectrum_from_ascii("eta_pp_emcal_200GeV.txt",eband); 
  //double sigma = 42.2;
  //emcAnalyzerUtils::scale(*pp_eta,sigma);

  double ptmin = 1., ptmax = 14.;
  TF1 *pp_eta_fit = new TF1("pp_eta_fit", "[0]/(exp([2]*x*x+[3]*x)+x/[1])^[4]", ptmin,ptmax);
  pp_eta_fit->SetParameters(100.,1.,0.,0.,10.);
  pp_eta->Fit("pp_eta_fit","Q","",ptmin,ptmax);

  TGraphErrors *cent = (TGraphErrors *)e.plot_spectrum_from_ascii("eta_pbsc_0_20_tof2chisq2.txt",eband);
  double TAA_cent = emcAnalyzerUtils::getTAB(0,20);
  double eTAA_cent = emcAnalyzerUtils::getTAB(0,20,"error")/TAA_cent;
  emcAnalyzerUtils::scale(*cent,1./(TAA_cent));

  //_____________________________________________________________________________
  // errors

  char tit[100];
  sprintf(tit,"eNormppBox");

  TH1F *eNormppBox = new TH1F(tit,"",2,14.,16.);
  double run3_pp_enorm = 0.097;
  for (int i=0;i<=2;i++)
    {
      eNormppBox->SetBinContent(i,1.);
      eNormppBox->SetBinError(i,run3_pp_enorm);
    }
  eNormppBox->SetFillColor(14);

  sprintf(tit,"eNormBox");
  TH1F *eNormBox = new TH1F(tit,"",2,0.2,1.5);
  
  double normerr = eTAA_cent; 
  for (int i=0;i<=2;i++)
    {
      eNormBox->SetBinContent(i,1.);
      eNormBox->SetBinError(i,normerr);
    }
  eNormBox->SetFillColor(2);//16+j);

  c1->cd();
  eNormppBox->Draw("e3same");
  eNormBox->Draw("e3same");
  
  //_____________________________________________________________________________
  // eta

  c1->cd();
  TGraphErrors* RAA_etacent = (TGraphErrors*)emcAnalyzerUtils::ratio(cent,pp_eta);
  //TGraphErrors* RAA_etacent = (TGraphErrors*)emcAnalyzerUtils::ratio(cent,pp_eta_fit);
  emcAnalyzerUtils::setMarkerLineType(RAA_etacent, 20, 2, 1.8);

  double common_err = -0.092; // 9.2% BBC (moved to box)
  emcAnalyzerUtils::AddOrSubstractRelatError(*RAA_etacent,common_err,1);
  emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(RAA_etacent,"pT","-",1);  // acceptance+energy-scale errors cancel

  emcAnalyzerUtils::displaceXvalues(*RAA_etacent,0.05);
  c1->cd();
  if (logy==0) plot_error_boxes(RAA_etacent,c1,2,logy);
  if (plot_eta) RAA_etacent->Draw("P");

  TGraphErrors* RAA_etacent2 = (TGraphErrors*)RAA_etacent->Clone();
  emcAnalyzerUtils::setMarkerLineType(RAA_etacent2, 24, 1, 1.6);
  
  TLegend *leg = new TLegend(0.346211,0.727459,0.94948,0.987705,"PHENIX Au+Au (central collisions):","brNDC");
  leg->SetMargin(0.2);
  leg->SetTextSize(0.0389);
  leg->SetFillColor(kWhite);
  leg->Draw();

  //_____________________________________________________________________________
  // photons

  TGraphErrors *pp_gamma = (TGraphErrors*)e.plot_spectrum_from_ascii("pQCD_vogelsang_pp_gamma_200GeV_cteq6_sc1.dat",eband); 
  emcAnalyzerUtils::AddOrSubstractRelatError(*pp_gamma,0.2);
  double ptmin = 1., ptmax = 14.;
  TF1 *pp_gamma_fit = new TF1("pp_gamma_fit", "[0]/(exp([2]*x*x+[3]*x)+x/[1])^[4]", ptmin,ptmax);
  pp_gamma_fit->SetParameters(1.,1.,0.,0.,10.);
  pp_gamma->Fit("pp_gamma_fit","Q","",ptmin,ptmax);

  TGraphErrors *gamma_cent = (TGraphErrors *)e.plot_spectrum_from_ascii("direct_gamma_0_10_chisq2.txt",eband);
  TAA_cent = emcAnalyzerUtils::getTAB(0,10);
  eTAA_cent = emcAnalyzerUtils::getTAB(0,10,"error")/TAA_cent;
  emcAnalyzerUtils::scale(*gamma_cent,1./(TAA_cent));

  c1->cd();
  TGraphErrors* RAA_gammacent= (TGraphErrors*)emcAnalyzerUtils::ratio(gamma_cent,pp_gamma_fit);
  emcAnalyzerUtils::setMarkerLineType(RAA_gammacent, 21, 51, 1.6);
  //emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(RAA_gammacent,"RAA","-",1);  // fidudead error cancels
  //emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(RAA_gammacent,"pT","-",1);  // acceptance error cancels
  //emcAnalyzerUtils::displaceXvalues(*RAA_gammacent,0.05);

  emcAnalyzerUtils::dumpGraph(RAA_gammacent,"x-ex-y-ey");

  c1->cd();
  //plot_error_boxes(RAA_gammacent,c1,2,logy);
  plot_upper_limits(*RAA_gammacent,c1);
  plot_RAAgamma_uncertainty(RAA_gammacent,c1);
  RAA_gammacent->Draw("P");

  emcAnalyzerUtils::dumpGraph(RAA_gammacent,"x-ex-y-ey");
  e.dumpLaTeX(RAA_gammacent);//,errNorm);

  //_____________________________________________________________________________
  // vitev

  e.keepPlot(false);

  TGraphErrors *vitev=e.plot_spectrum_from_ascii("R_AA_theor_vitev_rhic_pi0_dNdy1100.txt",eband);
  vitev->SetLineWidth(5);
  vitev->SetLineColor(92);

//   //_____________________________________________________________________________
//   // STAR

//   TGraphErrors *RAA_star = (TGraphErrors*)e.plot_spectrum_from_ascii("RAA_chhad_star_0_5.txt",eband);
//   emcAnalyzerUtils::setMarkerLineType(RAA_star, 29, 6, 2.2);
//   RAA_star->Draw("P");

  //_____________________________________________________________________________
  // plot all

  c1->cd();
  vitev->Draw("L");
  RAA_pi0cent->Draw("P");
  RAA_pi0cent2->Draw("P");

  if (plot_eta)
    {
      RAA_etacent->Draw("P");
      RAA_etacent2->Draw("P");
    }

  leg->AddEntry(RAA_gammacent, "Direct #gamma", "P");// (0-10% central)", "P");
  //leg->AddEntry(RAA_star, "Inclusive h^{#pm} (STAR)", "P");// (0-5% central)", "P");
  leg->AddEntry(RAA_pi0cent, "#pi^{0}", "P");// (0-10% central)", "P");
  if (plot_eta) leg->AddEntry(RAA_etacent, "#eta", "P");// (0-20\% central)", "P");
  leg->AddEntry(vitev, "GLV parton energy loss (dN^{g}/dy = 1100)", "L");
  leg->Draw();

//   TF1 *totOverpQCD = new TF1("totOverpQCD","pol8",1.25,9.25);
//   totOverpQCD->SetParameters(-14.7833,63.8814,-62.6471,29.3141,-7.78982,1.24259,-0.118019,0.00615821,-0.000135981);
//   totOverpQCD->Draw("same");

  double x = 5.707, y = 11.5485;
  TMarker *marker_pi02 = new TMarker(x,y,26);
  marker_pi02->SetMarkerColor(1);
  marker_pi02->SetMarkerSize(1.8);
  marker_pi02->Draw();

  x = 5.7178;
  y = 8.0518;
  TMarker *marker_eta2 = new TMarker(x,y,24);
  marker_eta2->SetMarkerColor(1);
  marker_eta2->SetMarkerSize(1.8);
  if (plot_eta) marker_eta2->Draw();

}


//_____________________________________________________________________________
//

void plot_error_boxes( TGraphErrors *g, TCanvas *c, int col, bool logy )
{

  TGraphErrors *errg= (TGraphErrors*)g->Clone();
  for (int i = 0; i < errg->GetN();i++) {
    //if (logy) errg->SetPoint(i,errg->GetX()[i],log(errg->GetY()[i]));
    errg->SetPointError(i,0.2,0.); // only x-error
  }
  emcAnalyzerCorrFactors::AddOrSubstractRelatErrors(errg,"pT","+",1); // y-error

  TClonesArray* errboxes = (TClonesArray*)emcAnalyzerUtils::errorBoxes( errg, 0.);
  TBox *box = 0;
  for (int j = 0; j < errboxes->GetEntries();j++)
    {
      box = ((TBox*)(*errboxes)[j]);
      box->SetFillStyle(3001);
      box->SetFillColor(col);
      c->cd();
      box->Draw();
    }

  return;
}

//_____________________________________________________________________________
//

void plot_RAAgamma_uncertainty( TGraphErrors *g, TCanvas *c )
{

//   TF1 *up = new TF1("up","pol4",4.0,13.5);
//   up->SetParameters(4.79905,-1.09595,0.150847,-0.0109309,0.000318571);
//   up->SetLineColor(51);
//   up->SetLineWidth(2);
//   up->SetLineStyle(4);
//   c->cd();
//   up->Draw("same");

//   TF1 *dwn = new TF1("dwn","[0]*([1]+[2]*x+[3]*x*x+[4]*x*x*x+[5]*x*x*x*x)",4.,13.5);
//   dwn->SetParameters(0.55,4.79905,-1.09595,0.150847,-0.0109309,0.000318571);
//   //dwn->SetParameters(6.54283,-2.54433,0.423924,-0.0313491,0.000853894);
//   dwn->SetLineColor(51);
//   dwn->SetLineWidth(2);
//   dwn->SetLineStyle(4);
//   c->cd();
//   dwn->Draw("same");

//  return;

  int N = g->GetN();
  int N0 = 6;  // skip 6 first (upper limits) points
  const int N2 = N-N0;

  TGraph *eg_up = 0;
  TGraph *eg_dwn = 0;

  double x[N2],y_up[N2],y_dwn[N2];
  double y;

  double ey = 0.2; // 20% NLO uncertainty

  int j =0;

  for (int i=N0;i<N;i++) 
    {
      g->GetPoint(i,x[j],y);
      y_up[j]=y*(1+ey);
      y_dwn[j]=y*(1-ey);
      j++;
    }
 
  eg_up  = new TGraph(N2,x,y_up);
  eg_dwn = new TGraph(N2,x,y_dwn) ;

  c->cd();
  eg_dwn->SetLineColor(51);
  eg_dwn->SetLineWidth(4);
  eg_dwn->SetLineStyle(4);
  eg_dwn->Draw("L");

  eg_up->SetLineColor(51);
  eg_up->SetLineWidth(4);
  eg_up->SetLineStyle(4);
  eg_up->Draw("L");

  return;

//   TGraphErrors *gup =(TGraphErrors*)g->Clone();
//   emcAnalyzerUtils::scale(*gup,1.2);

//   TGraphErrors *gdwn =(TGraphErrors*)g->Clone();
//   emcAnalyzerUtils::scale(*gdwn,0.8);
//   c->cd();
//   gup->Draw("L");
//   c->cd();
//   gdwn->Draw("L");

}

//_____________________________________________________________________________
//

void plot_upper_limits( TGraphErrors &g, TCanvas *c )
{

  const int N = 6; // N low pT points (upper limits)
  double x[N],y[N],ex[N],ey[N];

  TArrow *arrow = 0;
  TLine *line = 0;

  for (int i=0;i<N;i++) 
    {
      g.GetPoint(i,x[i],y[i]);
      ex[i] = g.GetErrorX(i);
      ey[i] = g.GetErrorY(i);

      g.SetPointError(i,ex[i],0.); // delete y-errors from original points

      double xshift = 0.;
      double ylenght = 0.2;

      if (i<2)
	{
	  ylenght = 0.15;
	}
      // 2 points shifted in x to improve visibility
      else if (i>=2 && i<=3)
	{
	  xshift = -0.05;
	  ylenght = 0.1;
	  g.SetPoint(i,x[i]+xshift,y[i]); 
	}
      else
	{
	  ylenght = 0.28;
	}

      // Not true ... this is only correct for min.bias
//       if (i==2 || i==3)
// 	{
// 	  // 	  g.SetPoint(i,0.,0.); // points 2 and 3 are true upper limits: (0+1.28*sigma) rather than just 0+sigma.
// 	  // 	  arrow = new TArrow(x[i],y[i]+1.28*ey[i],x[i],(y[i]+1.28*ey[i])*0.35,0.02,">");
// 	  // 	  line  = new TLine(x[i]-ex[i],y[i]+1.28*ey[i],x[i]+ex[i],y[i]+1.28*ey[i]);
// 	  arrow = new TArrow(x[i],y[i]+ey[i],x[i],(y[i]+ey[i])*0.1,0.02,">");//*0.5,0.02,">");
// 	  line  = new TLine(x[i]-ex[i],y[i]+ey[i],x[i]+ex[i],y[i]+ey[i]);
// 	}

      arrow = new TArrow(x[i]+xshift,y[i]+ey[i],x[i]+xshift,(y[i]+ey[i])*ylenght,0.02,">");
      line  = new TLine(x[i]-ex[i]+xshift,y[i]+ey[i],x[i]+ex[i]+xshift,y[i]+ey[i]);

      c->cd();
      arrow->Draw();
      line->Draw();
    }

  return;
}

//_____________________________________________________________________________
//

void acceptance_comparison()
{
  
  TCanvas *c1 = new TCanvas("eta_acceptance_comparison","eta_acceptance_comparison",650,500);

  // ================================================================================
  // Acceptance_eta_PbSc_Run2_y1.root
  // ================================================================================
  // ([0]+[1]*x+[2]*x*x)*(1.0-exp([3]-[4]*x))
  // [0] = 0.0809679 +/- 0.00493571  Relative Error = 6.09588%
  // [1] = 0.00202661 +/- 0.000781838  Relative Error = 38.5786%
  // [2] = -4.42198e-05 +/- 2.91904e-05  Relative Error = 66.012%
  // [3] = 1.00312 +/- 0.0689294  Relative Error = 6.87148%
  // [4] = 0.693112 +/- 0.0470475  Relative Error = 6.78786%
  
  double p0 = 0.0774647;// +/- 0.00143458  Relative Error = 1.85192%
  double p1 = 0.00255461;// +/- 0.000231586  Relative Error = 9.06541%
  double p2 = -6.14987e-05;// +/- 8.75568e-06  Relative Error = 14.2372%
  double p3 = 1.06228;// +/- 0.0222332  Relative Error = 2.09297%
  double p4 = 4.92930e-01;//   1.24854e-02  -0.00000e+00  -3.03880e-04

  p0 = 7.74647e-02;//   1.43458e-03  -0.00000e+00   2.08978e-03
  p1 = 2.55461e-03;//   2.31586e-04   0.00000e+00  -2.11542e-02
  p2 = -6.14987e-05 ;//  8.75568e-06  -0.00000e+00  -9.20429e-01
  p3 = 1.06228e+00;//   2.22332e-02   0.00000e+00  -3.77249e-04
  p4 = 7.39681e-01;//   1.53423e-02   0.00000e+00   5.17661e-04

//   double p0 = 0.0809679;    double ep0 = 0.00493571;
//   double p1 = 0.00202661;   double ep1 = 0.000781838;
//   double p2 = -4.42198e-05; double ep2 = 2.91904e-05; 
//   double p3 = 1.00312;      double ep3 = 0.0689294;
//   double p4 = 0.693112;     double ep4 = 0.0470475;
  
  double ptmin = 1., ptmax = 20.;
//   TF1 *acc_david = new TF1("acc_david","([0]+[1]*x+[2]*x*x)*(1.0-exp([3]-[4]*x))",ptmin,ptmax);
//   acc_david->SetParameters(p0,p1,p2,p3,p4);
//   acc_david->Draw();

//    1  p0          -9.09877e-03   4.15343e-04   4.40518e-10  -6.40356e-04
//    2  p1           9.59023e-03   2.31645e-04  -2.03133e-10   5.47678e-03
//    3  p2          -2.18892e-04   1.00763e-05   1.07886e-11   1.84526e-01
//    4  p3           1.14065e+00   1.73169e-02   2.13608e-08  -2.61187e-05
//    5  p4          -2.18412e-01   5.64770e-03   1.32130e-10  -3.79053e-05

  ptmin = 2., ptmax = 20.;
  TF1 *acc_david = new TF1("acc_david","([0]+[1]*x+[2]*x*x)*(1.0+exp([2]+[3]*x))",ptmin,ptmax);
  p0 =  -9.09877e-03;//   4.15343e-04   4.40518e-10  -6.40356e-04
  p1 =   9.59023e-03;//   2.31645e-04  -2.03133e-10   5.47678e-03
  p2 =  -2.18892e-04;//   1.00763e-05   1.07886e-11   1.84526e-01
  p3 =   1.14065e+00;//   1.73169e-02   2.13608e-08  -2.61187e-05
  p4 =  -2.18412e-01;//   5.64770e-03   1.32130e-10  -3.79053e-05
  acc_david->SetParameters(p0,p1,p2,p3,p4);
  acc_david->Draw();

  // ================================================================================
  // Eta acceptance computed differently
  // ================================================================================
  // /afs/run2_eta/run2_eta_acceptance/Acceptance_eta_PbSc_Run2.root
  // ================================================================================
  // Fit to 4-degree polinomial
  //
  //   1  p0 =  -3.03784e-02   6.03324e-04   2.88304e-07   4.62104e-06
  //   2  p1 =   3.71808e-02   5.75195e-04   4.34431e-08   7.21397e-06
  //   3  p2 =  -4.97931e-03   1.58637e-04   4.12875e-09  -5.68597e-05
  //   4  p3 =   3.18322e-04   1.64939e-05   3.52046e-10  -1.26391e-03
  //   5  p4 =  -7.72499e-06   5.72931e-07   2.86494e-11   3.72020e-03

  double p0b = -3.03784e-02  ; double ep0b = 6.03324e-04; 
  double p1b =  3.71808e-02  ; double ep1b = 5.75195e-04;
  double p2b = -4.97931e-03  ; double ep2b = 1.58637e-04;
  double p3b =  3.18322e-04  ; double ep3b = 1.64939e-05;
  double p4b = -7.72499e-06  ; double ep4b = 5.72931e-07;

  TF1 *acc_david2 = new TF1("acc_david2","pol4",ptmin,ptmax);
  acc_david2->SetParameters(p0b,p1b,p2b,p3b,p4b);
  acc_david2->SetLineColor(6);
  acc_david2->Draw("same");

  // ================================================================================
  // Baldo's acceptance:
  // ================================================================================
  // ([0]+[1]*x)*(1.0-exp([2]-[3]*x)) with the parameters:
  //    1  p0 =   1.97466e-01   2.79783e-01
  //    2  p1 =   3.09929e-03   2.11112e-02
  //    3  p2 =   4.34005e-01   1.33475e+00
  //    4  p3 =   4.25437e-01   9.96158e-01
 
  double BRgg = 0.3943;
  p0 = 1.97466e-01;//   2.79783e-01
  p1 = 3.09929e-03;//   2.11112e-02
  p2 = 4.34005e-01;//   1.33475e+00
  p3 = 4.25437e-01;//   9.96158e-01

  TF1 *acc_baldo = new TF1("acc_baldo","[4]*([0]+[1]*x)*(1.0-exp([2]-[3]*x))",ptmin,ptmax);
  acc_baldo->SetParameters(p0,p1,p2,p3,BRgg);
  acc_baldo->SetLineColor(2);
  acc_baldo->Draw("same");

//   // ================================================================================
//   // RATIO
//   // ================================================================================

//   TCanvas *c2 = new TCanvas("ratio_accs","accs",650,500);

//   TF1 *acc_ratio_david_baldo = new TF1("acc_ratio_david_baldo",
// 				       "([0]+[1]*x+[2]*x*x)*(1.0-exp([3]-[4]*x))/(([5]+[6]*x)*(1.0-exp([7]-[8]*x)))",
// 				       ptmin,ptmax);
//   acc_ratio_david_baldo->SetParameters(p0b,p1b,p2b,p3b,p4b,p0,p1,p2,p3);
//   acc_ratio_david_baldo->SetLineColor(2);
//   acc_ratio_david_baldo->Draw("");

  // ================================================================================
  // Saskia
  // ================================================================================

  c1->cd();

  BRgg = 0.3943*2.; //  wrongly divided by 2
  p0 = 1.30577e-01;  //   4.80289e-01   2.13358e-05  -3.23162e-04
  p1 = -4.55964e-03; //    7.82534e-02   3.10411e-06  -1.94892e-03
  p2 = 3.23040e-04;  //    6.13715e-03   3.90128e-07  -1.39319e-02
  p3 = 3.73802e-01;  //    3.50604e+00   3.07151e-04   1.48944e-05
  p4 = 3.46993e-01;  //    2.06887e+00   1.02973e-04  -6.08479e-05

  TF1 *acc_saskia = new TF1("acc_saskia", "[5]*([0]+[1]*x+[2]*x*x)*(1.0-exp([3]-[4]*x))",ptmin,10.);
  acc_saskia->SetParameters(p0,p1,p2,p3,p4,BRgg);
  acc_saskia->SetLineColor(4);
  acc_saskia->Draw("same");
  

  // ================================================================================
  // Hiro
  //   NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
  //    1  p0 =  -2.79514e-02   5.76287e-03  -0.00000e+00  -8.25937e-05
  //    2  p1 =   4.00110e-02   3.42327e-03   0.00000e+00  -1.07258e-03
  //    3  p2 =  -4.77807e-03   7.13420e-04  -0.00000e+00  -1.37143e-02
  //    4  p3 =   2.88524e-04   6.05686e-05   0.00000e+00  -1.83483e-01
  //    5  p4 =  -6.81524e-06   1.79568e-06  -0.00000e+00  -2.38829e+00
  // ================================================================================

  c1->cd();

  //BRgg = 0.3943;
  p0 = -2.79514e-02;
  p1 = 4.00110e-02;
  p2 = -4.77807e-03;
  p3 = 2.88524e-04;
  p4 = -6.81524e-06;

  TF1 *acc_hiro = new TF1("acc_hiro", "pol4",ptmin,15.);
  acc_hiro->SetParameters(p0,p1,p2,p3,p4);
  acc_hiro->SetLineColor(8);
  acc_hiro->Draw("same");


}


/*

gSystem->Load("libemcAnalyzer"); emcAnalyzer e; TGraphErrors *eband;
e.setEfficParticle("eta");

TGraphErrors *cent_eta = (TGraphErrors *)e.plot_spectrum(0,20,"tof2chisq2")
TGraphErrors *semicent_eta = (TGraphErrors *)e.plot_spectrum(20,60,"tof2chisq2")
TGraphErrors *periph_eta = (TGraphErrors *)e.plot_spectrum(60,92,"tof2chisq2")
TGraphErrors *mb_eta = (TGraphErrors *)e.plot_spectrum(0,100,"tof2chisq2")

emcAnalyzerUtils::scale(*cent_eta,0.2);
emcAnalyzerUtils::scale(*semicent_eta,0.4);
emcAnalyzerUtils::scale(*periph_eta,0.32);
emcAnalyzerUtils::scale(*mb_eta,0.92);

TGraphErrors *sum_eta=emcAnalyzerUtils::add(cent_eta,semicent_eta);
TGraphErrors *sum_eta=emcAnalyzerUtils::add(sum_eta,periph_eta);
TGraphErrors *r_eta=emcAnalyzerUtils::ratio(sum_eta,mb_eta);

*/
