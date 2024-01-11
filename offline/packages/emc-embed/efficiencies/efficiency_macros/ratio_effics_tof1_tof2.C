{ 

gROOT->SetStyle("Plain");
gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);
gStyle->SetMarkerStyle(20);
gROOT->ForceStyle();

TCanvas* c1 = new TCanvas("effic_tof","effic_tof");

TFile* f = new TFile("~/afsphnx/efficiency.root");

f->cd("NoCut/C10");

hEfficiency_fit->SetMarkerColor(1);
hEfficiency_fit->SetMaximum(1);
hEfficiency_fit->SetXTitle("pT (GeV/c)");
hEfficiency_fit->SetYTitle("Efficiency");
hEfficiency_fit->Draw("p");

f->cd("FiduDeadWarnAsym1Chi2Energy/C0");

//hEfficiency_counts->Draw();
hEfficiency_fit->SetMarkerColor(2);
hEfficiency_fit->Draw("samep");

f->cd("FiduDeadWarnAsym1ToF1Chi2Energy/C0");

//hEfficiency_counts->Draw("samep");
hEfficiency_fit->SetMarkerColor(4);
hEfficiency_fit->Draw("samep");

TH1F *ratio = (TH1F*)hEfficiency_fit->Clone();

f->cd("FiduDeadWarnAsym1ToF2Chi2Energy/C0");

//hEfficiency_counts->Draw("samep");
hEfficiency_fit->SetMarkerColor(8);
hEfficiency_fit->Draw("samep");

ratio->Divide(hEfficiency_fit);
ratio->SetXTitle("pT (GeV/c)");
ratio->SetYTitle("Ratio Effics: All_Cuts_TOF1 / All_Cuts_TOF2");

}
