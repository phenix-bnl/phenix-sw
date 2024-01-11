{ 
  gStyle->SetOptTitle(0);
  gStyle->SetOptStat(0);

gStyle->SetMarkerStyle(20);
gROOT->ForceStyle();

TCanvas* c1 = new TCanvas("peak_sigma","peak_sigma");

TFile* f = new TFile("effic_test.root");

gTOOT->cd("FiduDeadWarnAsym1ToF1Chi2Energy/C10");
hRecoParticleMinvSigma->SetMarkerColor(1);

hRecoParticleMinvSigma->SetMaximum(0.025);
hRecoParticleMinvSigma->SetMinimum(0);
//hRecoParticleMinvSigma->SetXTitle("#p^{0} p#_{T} GeV/c");
hRecoParticleMinvSigma->SetXTitle("Pi-zero pT GeV/c");
hRecoParticleMinvSigma->SetYTitle("Pi-zero peak sigma (GeV/c^2)");
hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.3);

hRecoParticleMinvSigma->Draw("P");

f->cd("FiduDeadWarnAsym1ToF1Chi2Energy/C0");
hRecoParticleMinvSigma->SetMarkerColor(2);
hRecoParticleMinvSigma->Draw("PSAME");

f->cd("FiduDeadWarnAsym1ToF1Chi2Energy/C7");
hRecoParticleMinvSigma->SetMarkerColor(4);
hRecoParticleMinvSigma->Draw("PSAME");



}
