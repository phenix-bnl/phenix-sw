
void peak_sigma(TFile* f, const char* cutname)
{
  new TCanvas("peak_sigma","peak_sigma");

  f->cd(cutname);
  gDirectory->cd("C10");

  hRecoParticleMinvSigma->SetMarkerColor(1);
  
  hRecoParticleMinvSigma->SetMaximum(0.025);
  hRecoParticleMinvSigma->SetMinimum(0);
  //hRecoParticleMinvSigma->SetXTitle("#p^{0} p#_{T} GeV/c");
  hRecoParticleMinvSigma->SetXTitle("Pi-zero pT GeV/c");
  hRecoParticleMinvSigma->SetYTitle("Pi-zero peak sigma (GeV/c^2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.3);
  
  hRecoParticleMinvSigma->Draw("P");

  f->cd(cutname);
  gDirectory->cd("C0");

  hRecoParticleMinvSigma->SetMarkerColor(2);
  hRecoParticleMinvSigma->Draw("PSAME");

  f->cd(cutname);
  gDirectory->cd("C7");
  
  hRecoParticleMinvSigma->SetMarkerColor(4);
  hRecoParticleMinvSigma->Draw("PSAME");
} 

void peak_position(TFile* f, const char* cutname)
{ 
  new TCanvas("peak_position","peak_position");

  
  f->cd(cutname);
  gDirectory->cd("C10");
  hRecoParticleMinvPeak->SetMarkerColor(1);
  
  hRecoParticleMinvPeak->SetMaximum(0.150);
  hRecoParticleMinvPeak->SetMinimum(0.130);
  //hRecoParticleMinvPeak->SetXTitle("#p^{0} p#_{T} GeV/c");
  hRecoParticleMinvPeak->SetXTitle("Pi-zero pT GeV/c");
  hRecoParticleMinvPeak->SetYTitle("Pi-zero peak position (GeV/c^2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.3);
  
  hRecoParticleMinvPeak->Draw("P");

  f->cd(cutname);
  gDirectory->cd("C0");  
  hRecoParticleMinvPeak->SetMarkerColor(2);
  hRecoParticleMinvPeak->Draw("PSAME");
  
  f->cd(cutname);
  gDirectory->cd("C7");
  hRecoParticleMinvPeak->SetMarkerColor(4);
  hRecoParticleMinvPeak->Draw("PSAME");
}


void effic_tof(TFile* f, const char* centrality)
{
  TCanvas* ceffic_tof = new TCanvas("effic_tof","effic_tof");

  TH1* htof1;
  TH1* htof2;

  f->cd("NoCut");
  gDirectory->cd(centrality);

  hEfficiency_fit->SetMarkerColor(1);
  hEfficiency_fit->SetMaximum(1);
  hEfficiency_fit->SetXTitle("pT (GeV/c)");
  hEfficiency_fit->SetYTitle("Efficiency");
  hEfficiency_fit->Draw("p");
  
  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2Cut");
  gDirectory->cd(centrality);
  
  //hEfficiency_counts->Draw();
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("samep");
  
  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  gDirectory->cd(centrality);

  htof1 = (TH1*)hEfficiency_fit->Clone();
  htof1->SetName("htof1");

  //hEfficiency_counts->Draw("samep");
  hEfficiency_fit->SetMarkerColor(4);
  hEfficiency_fit->Draw("samep");
  
  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut");
  gDirectory->cd(centrality);

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

  f->cd(cutname);
  gDirectory->cd("C10");
  hEfficiency_fit->SetMarkerColor(1);

  hEfficiency_fit->SetMaximum(1);
  //hEfficiency_fit->SetXTitle("#p^{0} p#_{T} GeV/c");
  hEfficiency_fit->SetXTitle("Pi-zero pT GeV/c");
  hEfficiency_fit->SetYTitle("Efficiency");
  
  //  TF1 * fit = new TF1("fit","([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))",1.,7.);
  
  hEfficiency_fit->Draw("P");
  
  f->cd(cutname);
  gDirectory->cd("C0");
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  
  f->cd(cutname);
  gDirectory->cd("C7");
  hEfficiency_fit->SetMarkerColor(4);
  hEfficiency_fit->Draw("PSAME");
}

void cut_by_cut_effic(TFile* f, const char* CentClass)
{

  char name[30];
  sprintf(name,"cut_by_cut_%s_effic",CentClass);
  new TCanvas(name,name);

  f->cd("NoCut");
  gDirectory->cd(CentClass);
  hEfficiency_fit->SetMarkerColor(1);

  hEfficiency_fit->SetMaximum(1.4);
  //hEfficiency_fit->SetXTitle("#p^{0} p#_{T} GeV/c");
  hEfficiency_fit->SetXTitle("Pi-zero pT GeV/c");
  hEfficiency_fit->SetYTitle("Efficiency");
  
  //  TF1 * fit = new TF1("fit","([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))",1.,7.);
  
  hEfficiency_fit->Draw("P");
  
  f->cd("FiduNoW3DeadWarnEnergyCut");
  gDirectory->cd(CentClass);
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->Draw("PSAME");
  
  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2Cut");
  gDirectory->cd(CentClass);
  hEfficiency_fit->SetMarkerColor(8);
  hEfficiency_fit->Draw("PSAME");

  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  gDirectory->cd(CentClass);
  hEfficiency_fit->SetMarkerColor(4);
  hEfficiency_fit->Draw("PSAME");

}

void efficiency_fit(TFile* f, const char* CentClass)
{

  char name[30];
  sprintf(name,"effic_fit_%s",CentClass);
  new TCanvas(name,name);

  f->cd("FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  gDirectory->cd(CentClass);
  hEfficiency_fit->SetMarkerColor(2);
  hEfficiency_fit->SetMaximum(1.);
  //hEfficiency_fit->SetXTitle("#p^{0} p#_{T} GeV/c");
  hEfficiency_fit->SetXTitle("Pi-zero pT GeV/c");
  hEfficiency_fit->SetYTitle("Efficiency");
  
  //  TF1 * fit = new TF1("fit","([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))",1.,7.);
  
  hEfficiency_fit->Draw("P");
  hEfficiency_fit->Fit("pol1","R","",1.,7.5);
  gStyle->SetOptStat(111);
  gStyle->SetOptFit(1);

}

void plots(const char* filename)
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
  //peak_sigma(f,"FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut");
  //cut_by_cut_effic(f,"C10");
  cut_by_cut_effic(f,"C1");
  //cut_by_cut_effic(f,"C7");

//   char centrality[3];
//   for (Int_t i=0; i<11; ++i)
//     {
//       sprintf(centrality,"C%i",i);
//       efficiency_fit(f,centrality);
//     }
}

0-10%:
p0   3.21237e-01   6.83773e-03   7.20952e-06   1.23195e-09
p1   1.27153e-02   1.44185e-03   1.52025e-06  -2.33693e-08

10-20%:
p0   3.35306e-01   6.22022e-03   6.47307e-06  -1.37211e-09
p1   1.25462e-02   1.37585e-03   1.43178e-06  -1.24067e-08

20-30%:
p0   3.49344e-01   5.88363e-03   8.22565e-06   1.29572e-08
p1   1.34109e-02   1.25117e-03   1.74921e-06   6.09312e-08

30-40%:
p0   3.63564e-01   6.01372e-03   8.56254e-06   8.29827e-08
p1   1.41243e-02   1.27293e-03   1.81245e-06   5.88053e-07

40-50%:
p0   3.78570e-01   5.74787e-03   1.09560e-05  -3.24271e-09
p1   1.28730e-02   1.20568e-03   2.29815e-06  -6.18360e-08

50-60%:
p0   3.90351e-01   5.74893e-03   1.27181e-05  -5.58685e-09
p1   1.21340e-02   1.20466e-03   2.66501e-06   7.99857e-08

60-70%:
p0   3.99867e-01   5.72576e-03   1.19291e-05   1.19128e-09
p1   1.17519e-02   1.19043e-03   2.48015e-06   4.29737e-08

70-80%:
p0   3.98510e-01   5.64870e-03   1.20772e-05  -8.82503e-09
p1   9.45437e-03   1.15841e-03   2.47674e-06   1.43443e-08

80-92%:
p0   3.96293e-01   5.21520e-03   9.72051e-06   1.09646e-08
p1   1.36524e-02   1.09392e-03   2.03893e-06  -6.96976e-08

min. bias:
p0   3.76377e-01   2.25104e-03   9.63202e-06   1.18030e-08
p1   1.16184e-02   4.69144e-04   2.00743e-06   7.07912e-08
