#include <iostream>
#include <iomanip>
#include <fstream>
#include "TMath.h"
#include "TFile.h"
#include "TF1.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TGraphErrors.h"

static const Float_t ADDITIONAL_DEADWARN_EFF_LOSS = 0.60/0.75; // -15% additional eff. loss for deadwarn cut wrongly computed
static const Int_t CentClasses = 20;

static Float_t p0[CentClasses];
static Float_t ep0[CentClasses];
static Float_t p1[CentClasses];
static Float_t ep1[CentClasses];

void centrality_distribution(TFile* f)
{
  TCanvas* c1 = new TCanvas("centrality_distribution");

  f->cd("general/C0");
  
//   c1->SetOptStat(0);
//   c1->SetOptTitle(0);
  c1->Divide(1,2);

  c1->cd(1);
  TH1* hCentrality = (TH1*)gDirectory->Get("hCentrality");
  hCentrality->GetXaxis()->SetRange(1,93);
  hCentrality->SetXTitle("Centrality (%)");
  hCentrality->SetYTitle("Number of events");
  hCentrality->Draw("histe");

  c1->cd(2);
  TH1* hCentClass = (TH1*)gDirectory->Get("hCentClass");
  hCentClass->GetXaxis()->SetRange(1,10);
  hCentClass->SetXTitle("Centrality Class");
  hCentClass->SetYTitle("Number of events");
  hCentClass->Draw("histe");  
}

void peak_sigma(TFile* f, const char* cutname)
{
  new TCanvas("peak_sigma","peak_sigma");

  TH1 *hRecoParticleMinvSigma = 0;

  f->cd(cutname);
  gDirectory->cd("C10");
  hRecoParticleMinvSigma = (TH1*)gDirectory->Get("hRecoParticleMinvSigma");
  hRecoParticleMinvSigma->SetMarkerColor(1);  
  hRecoParticleMinvSigma->SetMaximum(0.025);
  hRecoParticleMinvSigma->SetMinimum(0);
  hRecoParticleMinvSigma->SetXTitle("#pi^{0} p_{T} GeV/c");
  hRecoParticleMinvSigma->SetYTitle("#pi^{0} peak sigma (GeV/c^2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.3);
  hRecoParticleMinvSigma->Draw("P");

  f->cd(cutname);
  gDirectory->cd("C0");
  hRecoParticleMinvSigma = (TH1*)gDirectory->Get("hRecoParticleMinvSigma");
  hRecoParticleMinvSigma->SetMarkerColor(2);
  hRecoParticleMinvSigma->Draw("PSAME");

  f->cd(cutname);
  gDirectory->cd("C7");
  hRecoParticleMinvSigma = (TH1*)gDirectory->Get("hRecoParticleMinvSigma");  
  hRecoParticleMinvSigma->SetMarkerColor(4);
  hRecoParticleMinvSigma->Draw("PSAME");
} 

void peak_position(TFile* f, const char* cutname)
{ 
  new TCanvas("peak_position","peak_position");
  TH1 *hRecoParticleMinvPeak = 0;

  f->cd(cutname);
  gDirectory->cd("C10");
  hRecoParticleMinvPeak = (TH1*)gDirectory->Get("hRecoParticleMinvPeak");
  hRecoParticleMinvPeak->SetMarkerColor(1);  
  hRecoParticleMinvPeak->SetMaximum(0.150);
  hRecoParticleMinvPeak->SetMinimum(0.130);
  hRecoParticleMinvPeak->SetXTitle("#pi^{0} p_{T} GeV/c");
  hRecoParticleMinvPeak->SetYTitle("#pi^{0} peak position (GeV/c^2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.3);  
  hRecoParticleMinvPeak->Draw("P");

  f->cd(cutname);
  hRecoParticleMinvPeak = (TH1*)gDirectory->Get("hRecoParticleMinvPeak");
  gDirectory->cd("C0");  
  hRecoParticleMinvPeak->SetMarkerColor(2);
  hRecoParticleMinvPeak->Draw("PSAME");
  
  f->cd(cutname);
  hRecoParticleMinvPeak = (TH1*)gDirectory->Get("hRecoParticleMinvPeak");
  gDirectory->cd("C7");
  hRecoParticleMinvPeak->SetMarkerColor(4);
  hRecoParticleMinvPeak->Draw("PSAME");
}


void effic_tof(TFile* f, const char* centrality)
{
  TCanvas* ceffic_tof = new TCanvas("effic_tof","effic_tof");
  ceffic_tof->cd();

  TH1* htof1 = 0;
  TH1* htof2 = 0;
  TH1* hEfficiency_fit = 0;
 
  f->cd("NoCut");
  gDirectory->cd(centrality);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->SetMarkerColor(1);
  hEfficiency_fit->SetMaximum(1);
  hEfficiency_fit->SetXTitle("p_{T} (GeV/c)");
  hEfficiency_fit->SetYTitle("Efficiency");
  hEfficiency_fit->Draw("p");
  
  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2Cut");
  gDirectory->cd(centrality);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");  
  hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
  //hEfficiency_counts->Draw();
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("samep");
  
  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  gDirectory->cd(centrality);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);

  htof1 = (TH1*)hEfficiency_fit->Clone();
  htof1->SetName("htof1");

  //hEfficiency_counts->Draw("samep");
  hEfficiency_fit->SetMarkerColor(4);
  hEfficiency_fit->Draw("samep");
  
  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut");
  gDirectory->cd(centrality);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);

  htof2 = (TH1*)hEfficiency_fit->Clone();
  htof2->SetName("htof2");
  
  //hEfficiency_counts->Draw("samep");
  hEfficiency_fit->SetMarkerColor(8);
  hEfficiency_fit->Draw("samep");

  new TCanvas("tof1_div_tof2","tof1_div_tof2");

  TH1* hdiv = (TH1*)htof1->Clone();
  hdiv->SetName("hdiv");

  hdiv->SetMinimum(0.5);
  hdiv->SetMaximum(2.0);
  hdiv->Divide(htof2);

  hdiv->Draw("p");
}


void centrality_dependent_effic(TFile* f, const char* cutname)
{
  new TCanvas("centrality_dependent_effic",
	      "centrality_dependent_effic");
  TH1* hEfficiency_fit = 0;

  //  TF1 * fit = new TF1("fit","([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))",1.,7.);

  Bool_t apply_correction = false;
  TString temp_string = cutname;
  if (temp_string.Contains("Dead",TString::kExact))  apply_correction = true ;

  f->cd(cutname);
  gDirectory->cd("C10");
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  if (apply_correction) hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
  hEfficiency_fit->SetMarkerColor(1);
  hEfficiency_fit->SetMaximum(1);
  hEfficiency_fit->SetXTitle("#pi^{0} p_{T} GeV/c");
  hEfficiency_fit->SetYTitle("Efficiency");  
  hEfficiency_fit->Draw("P");
  
  f->cd(cutname);
  gDirectory->cd("C0");
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  if (apply_correction) hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  
  f->cd(cutname);
  gDirectory->cd("C7");
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  if (apply_correction) hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
  hEfficiency_fit->SetMarkerColor(4);
  hEfficiency_fit->Draw("PSAME");
}

void cut_by_cut_effic(TFile* f, const char* CentClass)
{

  char name[30];
  sprintf(name,"cut_by_cut_%s_effic",CentClass);
  new TCanvas(name,name);

  TH1* hEfficiency_fit = 0;

  //  TF1 * fit = new TF1("fit","([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))",1.,7.);  
  TF1 *linfit = new TF1("linfit","pol0",1.2,7.5);  

  f->cd("NoCut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->SetMarkerColor(1);
  hEfficiency_fit->SetMaximum(1.4);
  hEfficiency_fit->SetXTitle("#pi^{0} p_{T} GeV/c");
  hEfficiency_fit->SetYTitle("Efficiency");
  hEfficiency_fit->Draw("P");
  hEfficiency_fit->Fit("linfit","QR","",2.,7.5);
  //hEfficiency_fit->Fit("pol0","QR","",2.,7.5);
  Float_t base_effic = linfit->GetParameter(0);
  cout << " Average efficiency for cut NoCut: " << base_effic << endl;
 
  f->cd("EnergyCut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  hEfficiency_fit->Fit("linfit","QR","",2.,7.5);
  //hEfficiency_fit->Fit("pol0","QR","",2.,7.5);
  Float_t effic_energy = linfit->GetParameter(0)/base_effic;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cut EnergyCut (with respect to NoCut): " 
       << effic_energy << " (-" << (1-effic_energy)*100 << "%)" << endl;

  f->cd("FiduCut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  hEfficiency_fit->Fit("linfit","QR","",2.,7.5);
  //hEfficiency_fit->Fit("pol0","QR","",2.,7.5);
  Float_t effic_fidu = linfit->GetParameter(0)/base_effic;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cut FiduCut (with respect to NoCut): " 
       << effic_fidu << " (-" << (1-effic_fidu)*100 << "%)" << endl;

  f->cd("NoW3Cut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  hEfficiency_fit->Fit("linfit","QR","",2.,7.5);
  // hEfficiency_fit->Fit("pol0","QR","",2.,7.5);
  Float_t effic_noW3 = linfit->GetParameter(0)/base_effic;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cut NoW3Cut (with respect to NoCut): " 
       << effic_noW3 << " (-" << (1-effic_noW3)*100 << "%)" << endl;

  f->cd("DeadWarnCut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  hEfficiency_fit->Fit("linfit","QR","",2.,7.5);
  //hEfficiency_fit->Fit("pol0","QR","",2.,7.5);
  Float_t effic_deadwarn = linfit->GetParameter(0)/base_effic;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cut DeadWarnCut (with respect to NoCut): " 
       << effic_deadwarn << " (-" << (1-effic_deadwarn)*100 << "%)" << endl;

  f->cd("Asym1Cut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  hEfficiency_fit->Fit("linfit","QR","",2.,7.5);
  //hEfficiency_fit->Fit("pol0","QR","",2.,7.5);
  Float_t effic_asym1 = linfit->GetParameter(0)/base_effic ;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cut Asym1Cut (with respect to NoCut): " 
       << effic_asym1 << " (-" << (1-effic_asym1)*100 << "%)" << endl;

  f->cd("Chi2Cut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  hEfficiency_fit->Fit("linfit","QR","",2.,7.5);
  //hEfficiency_fit->Fit("pol0","QR","",2.,7.5);
  Float_t effic_chi2 = linfit->GetParameter(0)/base_effic;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cut Chi2Cut (with respect to NoCut): " 
       << effic_chi2 << " (-" << (1-effic_chi2)*100 << "%)" << endl;

  f->cd("ToF1Cut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  hEfficiency_fit->Fit("linfit","QR","",2.,7.5);
  //hEfficiency_fit->Fit("pol0","QR","",2.,7.5);
  Float_t effic_tof1 = linfit->GetParameter(0)/base_effic;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cut ToF1Cut (with respect to NoCut): " 
       << effic_tof1 << " (-" << (1-effic_tof1)*100 << "%)" << endl;

  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for all cuts combined: " 
       << effic_chi2*effic_tof1*effic_asym1*effic_deadwarn*effic_noW3*effic_fidu*effic_energy << endl ;

  f->cd("FiduNoW3DeadWarnEnergyCut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  hEfficiency_fit->Fit("linfit","QR","",2.,7.5);
  //hEfficiency_fit->Fit("pol0","R","",1.,7.5);
  Float_t effic_fidudeadNoW3energy = linfit->GetParameter(0)/base_effic;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cut FiduNoW3DeadWarnEnergyCut *combined* (with respect to NoCut): " 
       << effic_fidudeadNoW3energy << " (-" << (1-effic_fidudeadNoW3energy)*100 << "%)" << endl; 
  Float_t effic_fidudeadNoW3energy_factorized = effic_fidu*effic_deadwarn*effic_noW3*effic_energy ;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cuts FiduNoW3DeadWarnEnergyCut *factorized* (with respect to NoCut): " 
       << effic_fidudeadNoW3energy_factorized << " (-" << (1-effic_fidudeadNoW3energy_factorized)*100 << "%)" << endl << endl;

  Float_t effic_fidudead = effic_fidu*effic_deadwarn ;
  cout << " Average efficiency (pT=2-7 GeV/c, 0-10% cent) for cuts FiduDeadWarn *factorized* (with respect to NoCut): " 
       << effic_fidudead << " (-" << (1-effic_fidudead)*100 << "%)" << endl;
 
//   f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2Cut");
//   gDirectory->cd(CentClass);
  //  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  //hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
//   hEfficiency_fit->SetMarkerColor(8);
//   hEfficiency_fit->Draw("PSAME");
//   hEfficiency_fit->Fit("pol1","R","",1.,7.5);

//   f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
//   gDirectory->cd(CentClass);
  //  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  //hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
//   hEfficiency_fit->SetMarkerColor(4);
//   hEfficiency_fit->Draw("PSAME");
//   hEfficiency_fit->Fit("pol1","R","",1.,7.5);

}

//_____________________________________________________________________________
//void  format_output_effiency(const Float_t p0,const Float_t p0,const Float_t p1,const Float_t ep1);
void  format_output_efficiencies()
{
  for (Int_t i=0; i<=CentClasses/2; i++)
    {
      if (i==0) cout << " Float_t p0[CentClasses] = {" ;
      cout << p0[i] << "," <<  p0[i] << "," << endl;
      if (i==10) cout << p0[i] << "," <<  p0[i] << "};"<< endl ;
    }
  for (Int_t i=0; i<=CentClasses/2; i++)
    {
      if (i==0) cout << " Float_t p0_error[CentClasses] = {" ;
      cout << ep0[i] << "," <<  ep0[i] << "," << endl; 
      if (i==10) cout << ep0[i] << "," <<  ep0[i] << "};"<< endl ;
    }
  for (Int_t i=0; i<=CentClasses/2; i++)
    {
      if (i==0) cout << " Float_t p1[CentClasses] = {" ;
      cout << p1[i] << "," <<  p1[i] << "," << endl;
      if (i==10) cout << p1[i] << "," << p1[i] << "};"<< endl ;
    }
  for (Int_t i=0; i<=CentClasses/2; i++)
    {
      if (i==0) cout << " Float_t p1_error[CentClasses] = {" ;
      cout << ep1[i] << "," <<  ep1[i] << "," << endl;
      if (i==10) cout << ep1[i] << "," <<  ep1[i] << "};"<< endl ;
    }

}

//_____________________________________________________________________________
void efficiency_final(TFile* f, const Int_t centrality)
{
  char CentClass[3];
  sprintf(CentClass,"C%d",centrality);

  char name[30];
  sprintf(name,"effic_fit_%s",CentClass);
  char title[30];
  sprintf(title,"Efficiency for Centrality %s",CentClass);
  new TCanvas(name,name);
  TH1 *hEfficiency_fit = 0;
  TH1 *hEfficiency_counts = 0;
  TH1 *effic_combined = 0;

  //  TF1 * fit = new TF1("fit","([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))",1.,7.);
  TF1 *polfit = new TF1("polfit","pol1",1.2,7.5);  

  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  gDirectory->cd(CentClass);
  hEfficiency_fit = (TH1*)gDirectory->Get("hEfficiency_fit");
  hEfficiency_fit->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->SetMaximum(1.);
  hEfficiency_fit->SetXTitle("#pi^{0} p_{T} GeV/c");
  hEfficiency_fit->SetYTitle(title);  
  hEfficiency_fit->Draw("P");
  hEfficiency_fit->Sumw2();

  hEfficiency_counts = (TH1*)gDirectory->Get("hEfficiency_counts");
  hEfficiency_counts->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
  hEfficiency_counts->SetMarkerColor(4);
  hEfficiency_counts->Sumw2();
  hEfficiency_counts->Draw("Psame");

  effic_combined = (TH1*)(hEfficiency_counts->Clone());
  effic_combined->SetName("effic_combined");
  effic_combined->Add(hEfficiency_fit);
  effic_combined->Scale(0.5);
  effic_combined->SetMaximum(1.);
  effic_combined->SetAxisRange(0,7.2,"X");
  effic_combined->SetXTitle("p_{T} (GeV/c)");
  effic_combined->SetYTitle(title);

  Float_t new_combined_eff_error = 0;
  Int_t Neff_bins = effic_combined->GetNbinsX();

  for (Int_t eff_bin = 1; eff_bin <= Neff_bins; eff_bin++)
    {
      new_combined_eff_error = TMath::Abs(hEfficiency_fit->GetBinContent(eff_bin)-hEfficiency_counts->GetBinContent(eff_bin));
      new_combined_eff_error /= 2.;
      effic_combined->SetBinError(eff_bin,new_combined_eff_error);
    }
  effic_combined->SetMarkerColor(8);
  effic_combined->Fit("polfit","QR","",1.2,7.0);
  p0[centrality] = polfit->GetParameter(0);
  ep0[centrality] = polfit->GetParError(0);
  p1[centrality] = polfit->GetParameter(1);
  ep1[centrality] = polfit->GetParError(1);
  //hEfficiency_combined->Fit("polfit","R","",1.,7.5);
  hEfficiency_fit->SetMarkerSize(0.5);
  hEfficiency_counts->SetMarkerSize(0.5);
  hEfficiency_fit->Draw("Psame");
  hEfficiency_counts->Draw("Psame");

  gStyle->SetOptStat(111);
  gStyle->SetOptFit(1);

}

void efficiency_counts(TFile* f, const char* CentClass)
{

  char name[30];
  sprintf(name,"effic_counts_%s",CentClass);
  new TCanvas(name,name);

  TH1 *hEfficiency_counts = 0;
  //  TF1 * fit = new TF1("fit","([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))",1.,7.);
  TF1 *polfit = new TF1("polfit","pol1",1.2,7.5);  

  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  gDirectory->cd(CentClass);
  hEfficiency_counts = (TH1*)gDirectory->Get("hEfficiency_counts");
  hEfficiency_counts->Scale(ADDITIONAL_DEADWARN_EFF_LOSS);
  hEfficiency_counts->SetMarkerColor(2);
  hEfficiency_counts->SetMaximum(1.);
  hEfficiency_counts->SetXTitle("#pi^{0} p_{T} GeV/c");
  hEfficiency_counts->SetYTitle("Efficiency");  
  hEfficiency_counts->Draw("P");
  hEfficiency_counts->Fit("polfit","QR","",1.,7.5);
  //hEfficiency_counts->Fit("pol1","R","",1.,7.5);
  gStyle->SetOptStat(111);
  gStyle->SetOptFit(1);

}

void dplots(const char* filename)
{ 
  gROOT->SetStyle("Plain") ;
  gStyle->SetPalette(1) ; 
  gStyle->SetOptTitle(0);
  gStyle->SetOptStat(0);
  gStyle->SetMarkerStyle(20);
  gROOT->ForceStyle();

  TFile* f = new TFile(filename);

  //centrality_dependent_effic(f,"FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  //effic_tof(f,"C10");
  //peak_position(f,"FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  peak_sigma(f,"FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  //cut_by_cut_effic(f,"C10");
  //cut_by_cut_effic(f,"C0");
  //cut_by_cut_effic(f,"C7");

//   for (Int_t i=0; i<11; ++i)
//     {
//       if (i==9) continue ; // CentClass==9 is empty
//       efficiency_final(f,i);
//       //efficiency_counts(f,centrality);
//       cout << "BE SURE OF RUNNING THIS FUNCTION ALONE !! OTHERWISE SCALE(ADDITIONAL_..) IS APPLIED SEVERAL TIMES !!" << endl ;
//     }
//   format_output_efficiencies();
}

// 0-10%:
// p0   3.21237e-01   6.83773e-03   7.20952e-06   1.23195e-09
// p1   1.27153e-02   1.44185e-03   1.52025e-06  -2.33693e-08

// 10-20%:
// p0   3.35306e-01   6.22022e-03   6.47307e-06  -1.37211e-09
// p1   1.25462e-02   1.37585e-03   1.43178e-06  -1.24067e-08

// 20-30%:
// p0   3.49344e-01   5.88363e-03   8.22565e-06   1.29572e-08
// p1   1.34109e-02   1.25117e-03   1.74921e-06   6.09312e-08

// 30-40%:
// p0   3.63564e-01   6.01372e-03   8.56254e-06   8.29827e-08
// p1   1.41243e-02   1.27293e-03   1.81245e-06   5.88053e-07

// 40-50%:
// p0   3.78570e-01   5.74787e-03   1.09560e-05  -3.24271e-09
// p1   1.28730e-02   1.20568e-03   2.29815e-06  -6.18360e-08

// 50-60%:
// p0   3.90351e-01   5.74893e-03   1.27181e-05  -5.58685e-09
// p1   1.21340e-02   1.20466e-03   2.66501e-06   7.99857e-08

// 60-70%:
// p0   3.99867e-01   5.72576e-03   1.19291e-05   1.19128e-09
// p1   1.17519e-02   1.19043e-03   2.48015e-06   4.29737e-08

// 70-80%:
// p0   3.98510e-01   5.64870e-03   1.20772e-05  -8.82503e-09
// p1   9.45437e-03   1.15841e-03   2.47674e-06   1.43443e-08

// 80-92%:
// p0   3.96293e-01   5.21520e-03   9.72051e-06   1.09646e-08
// p1   1.36524e-02   1.09392e-03   2.03893e-06  -6.96976e-08

// min. bias:
// p0   3.76377e-01   2.25104e-03   9.63202e-06   1.18030e-08
// p1   1.16184e-02   4.69144e-04   2.00743e-06   7.07912e-08
