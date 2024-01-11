{ 
gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);
gStyle->SetMarkerStyle(20);
gROOT->ForceStyle();

TCanvas* c1 = new TCanvas("peak_position","peak_position");

TFile* f = new TFile("effic_test.root");

f->cd("FiduDeadWarnAsym1ToF1Chi2Energy/C10");
hRecoParticleMinvPeak->SetMarkerColor(1);

hRecoParticleMinvPeak->SetMaximum(0.150);
hRecoParticleMinvPeak->SetMinimum(0.130);
//hRecoParticleMinvPeak->SetXTitle("#p^{0} p#_{T} GeV/c");
hRecoParticleMinvPeak->SetXTitle("Pi-zero pT GeV/c");
hRecoParticleMinvPeak->SetYTitle("Pi-zero peak position (GeV/c^2)");
hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.3);

hRecoParticleMinvPeak->Draw("P");

f->cd("FiduDeadWarnAsym1ToF1Chi2Energy/C0");
hRecoParticleMinvPeak->SetMarkerColor(2);
hRecoParticleMinvPeak->Draw("PSAME");

f->cd("FiduDeadWarnAsym1ToF1Chi2Energy/C7");
hRecoParticleMinvPeak->SetMarkerColor(4);
hRecoParticleMinvPeak->Draw("PSAME");



}
