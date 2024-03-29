{ 
 gStyle->SetOptTitle(0);
  gStyle->SetOptStat(0);
gStyle->SetMarkerStyle(20);
gROOT->ForceStyle();

TCanvas* c1 = new TCanvas("centrality_dependent_effic_nocut","centrality_dependent_effic_nocut");

TFile* f = new TFile("effic_test.root");

f->cd("NoCut/C10");
hEfficiency_fit->SetMarkerColor(1);

hEfficiency_fit->SetMaximum(1);
//hEfficiency_fit->SetXTitle("#p^{0} p#_{T} GeV/c");
hEfficiency_fit->SetXTitle("Pi-zero pT GeV/c");
hEfficiency_fit->SetYTitle("Efficiency");

TF1 * fit = new TF1("fit","([0]+[1]*x)*(1.0-exp(-(x-[2])/[3]))",1.,7.);

hEfficiency_fit->Draw("P");

f->cd("NoCut/C0");
hEfficiency_fit->SetMarkerColor(2);
hEfficiency_fit->Draw("PSAME");

f->cd("NoCut/C7");
hEfficiency_fit->SetMarkerColor(4);
hEfficiency_fit->Draw("PSAME");



}
