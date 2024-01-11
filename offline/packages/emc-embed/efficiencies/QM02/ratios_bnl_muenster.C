//  .x ratios_bnl_muenster.C(1.952e+5,2.55e+5,11.75,11.87,"PbSc","PbSc","minbias");
//  .x ratios_bnl_muenster.C(1.952e+5,2.31e+5,11.75,11.77,"PbSc","PbGl","minbias");

//  .x ratios_bnl_muenster.C(1.036e+4,1.54e+4,11.25,11.68,"PbSc","PbGl","periph")
//  .x ratios_bnl_muenster.C(1.036e+4,   8136,11.25,11.21,"PbSc","PbSc","periph");

//  .x ratios_bnl_muenster.C(1.450e+6,1.300e+6,12.36,12.22,"PbSc","PbGl","central")
//  .x ratios_bnl_muenster.C(1.450e+6,1.306e+6,12.36,12.26,"PbSc","PbSc","central")

ratios_bnl_muenster( Double_t Abnl, Double_t Amuenster, 
		     Double_t n_bnl, Double_t n_muenster, 
		     const char* bnl = "pbsc", const char* muenster = "pbgl",
		     const char* type = "central")
{ 
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Double_t ratios_AHag = Abnl/Amuenster;
  cout << "Ratios of Normalizations: " << ratios_AHag << endl;
  Double_t diff_nHag = n_bnl-n_muenster;
  cout << "Difference of exponents : " << diff_nHag << endl;

  char name[200];
  sprintf(name,"%s_%s_bnlsubatech_over_%s_%s_muenster",bnl,type,muenster,type);
  TCanvas *c1 = new TCanvas(name,name,600,600);
  TF1 *ratios = new TF1("ratios","[0]*(x/(x+1.72))**[1]",1.,10.);
  ratios->SetParNames("A Hag","p_o", "n");
  ratios->SetParameters(ratios_AHag,diff_nHag);

  ratios->SetMinimum(0.7);
  ratios->SetMaximum(1.3);
  ratios->SetLineColor(2);
  ratios->SetLineWidth(3.);
  ratios->Draw("");
  ratios->GetHistogram()->SetXTitle("#pi^{0} p_{T} GeV/c");
  ratios->GetHistogram()->SetYTitle("Ratio yields BNL-SUBATECH / MUENSTER");
  ratios->GetHistogram()->GetXaxis()->SetTitleSize(0.05);
  ratios->GetHistogram()->GetYaxis()->SetTitleSize(0.043);
  ratios->GetHistogram()->GetYaxis()->SetTitleOffset(1.2);

  //TString TLabel = "Calculations : I. Sarcevic et al. - QF = " + QF + " p_{T}";

  char *label = "Hagedorn (power-law) fit:";
  char bnlsub_label1[200];
  sprintf(bnlsub_label1,"%s %s (BNL-SUBATECH): ",bnl,type);
  char bnlsub_label2[200];
  sprintf(bnlsub_label2,"A = %3.1f, p0 = 1.72, n = %3.1f",Abnl,n_bnl);
  char muenster_label1[200];
  sprintf(muenster_label1,"%s %s (MUENSTER): ",muenster,type);
  char muenster_label2[200];
  sprintf(muenster_label2,"A = %3.1f, p0 = 1.72, n = %3.1f",Amuenster,n_muenster);

  TPaveText *ratiosPaveText = new TPaveText(2.8,0.867074,9.3318,0.979733,"");
  ratiosPaveText->SetFillStyle(0);
  ratiosPaveText->SetBorderSize(0);
  ratiosPaveText->SetTextSize(0.042);
  ratiosPaveText->AddText(label);
  ratiosPaveText->AddText(bnlsub_label1);
  ratiosPaveText->AddText(bnlsub_label2);
  ratiosPaveText->AddText(muenster_label1);
  ratiosPaveText->AddText(muenster_label2);
  
  ratiosPaveText->Draw();

  c1->Update();

// Reading /afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/yieldfinal_t0a0_cent70_80.txt ...
// Fit result:
// AHag =1.036e+04
// p0   =1.72
// n    =11.25
// Chi2/Ndf  =0.12 
//  Reading /afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/pi0_ms_pbsc_70-80.dat ...
// Fit result:
// AHag =8136
// p0   =1.72
// n    =11.21
// Chi2/Ndf  =0.6394

//  Reading /afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/yieldfinal_t0a0_cent0_10.txt ....
// Fit result:
// AHag =1.449e+06
// p0   =1.72
// n    =12.36
// Chi2/Ndf  =0.1123
 
//  Reading /afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/pi0_ms_pbsc_00-10.dat ... 
// Fit result:
// AHag =1.306e+06
// p0   =1.72
// n    =12.26
// Chi2/Ndf  =0.8384
        
}
