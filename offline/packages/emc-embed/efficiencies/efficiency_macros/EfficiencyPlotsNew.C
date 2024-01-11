#include <TFile.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TF1.h>
#include <iostream.h>

void EfficiencyPlots(char InFile[60]="ReconstructionEfficiencyFile.root")
{

  gROOT->SetStyle("Plain");
  //char InFile[60]="ReconstructionEfficiencyFile_fake.root";  
  char InFile[60]="effic_merged_0_314_runs.root"
  TFile* in = new TFile(InFile,"READ");

  //_____________________________________________________________________________
  // No cuts here

  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("#pi^{0} p_{T}(GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("#pi^{0} peak position (GeV/c^{2})");
  hRecoParticleMinvPeak->GetXaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.4);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");
  
  hRecoParticleMinvSigma->Draw("");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("#pi^{0} p_{T}(GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("#pi^{0} peak width (GeV/c^{2})");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.4);
  hRecoParticleMinvSigma->GetXaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);

  //_____________________________________________________________________________
  //_____________________________________________________________________________
  // check histos

  TCanvas check("check","check",600,500);

  hCentClass->Draw("");

  hMeasyz_w->Draw("");
  hMeasyz_w->GetXaxis()->SetTitle("z (cm)");
  hMeasyz_w->GetYaxis()->SetTitle("y (cm)");
  hMeasyz_w->SetMarkerColor(4);
  hMeasyz_w_out->SetMarkerColor(1);
  hMeasyz_w_out->Draw("same");
  //hMeasyz_e->Draw("");

  hMeasyz_w_out->Draw("");
  hMeasyz_w_out->GetXaxis()->SetTitle("z (cm)");
  hMeasyz_w_out->GetYaxis()->SetTitle("y (cm)");
  hMeasyz_w_out->SetMarkerColor(1);

  hfiducial_local_yz0_geaclustertrack->SetMarkerColor(2);
  hfiducial_local_yz0_geaclustertrack->Draw("same");

  check->SetLogy();
  hMeasx->Draw("hist");
  hMeasy->Draw("hist");
  hMeasz->Draw("hist");

  hArm->Draw("hist");
  hSector->Draw("hist");

  //_____________________________________________________________________________
  
  hSimulSecondaryParticlePt->Draw("");
  hSimulSecondaryParticlePt->SetMarkerStyle(21);
  hSimulSecondaryParticlePt->SetMarkerSize(1.3);
  hSimulSecondaryParticlePt->SetMarkerColor(2);
  hSimulSecondaryParticlePt->GetXaxis()->SetTitle("pT (GeV/c)");
  TF1 *powlaw = new TF1("powlaw","[0]/pow(x,7)",1.5,4);
  hSimulSecondaryParticlePt->Fit("powlaw","","R",1.5,4);

  hRecoSecondaryParticlePt->Draw("same");
  hRecoSecondaryParticlePt->SetMarkerStyle(8);
  hRecoSecondaryParticlePt->SetMarkerSize(1.3);
  hRecoSecondaryParticlePt->SetMarkerColor(3);
  hRecoSecondaryParticlePt->Fit("powlaw","","R",1.5,4);  

  hMease->Draw("");
  hEcore->Draw("");

  hRecoParticleEnergyAsym->Draw("");
  hRecoParticleEnergyAsymPi0->Draw("");

  hRecalibrationFactors->Draw("");

  //_____________________________________________________________________________
  
  hTof->Draw("eh");
  hTof->GetXaxis()->SetTitle("PbSc TOF (ns)");

  hProbPhot->Draw("eh");
  hProbPhot->GetXaxis()->SetTitle("PbSc Prob. Photon");

  hChi2->Draw("eh");
  hChi2->GetXaxis()->SetTitle("PbSc Chi2");

  hTotalClusterMul->Draw("eh");
  hEmbedClusterMul->Draw("ehsame");

  //_____________________________________________________________________________
  
  //FiduAsymChi2TOFCutCent.cd();
  FiduAsymTOFCutCent.cd();
  Cent.cd();

  TCanvas c10("c10","Minv",900,700);
  gStyle->SetOptStat(11111111);
  c10.Divide(3,3,0,0);

  c10.cd(1);
  hMinv_1.SetMinimum(1e-09);
  hMinv_1->GetXaxis()->SetRange(0,100);
  hMinv_1.Draw();
  gauss_const_backgd_1.Draw("same");
  c10.cd(2);
  hMinv_2.SetMinimum(1e-09);
  hMinv_2->GetXaxis()->SetRange(0,100);
  hMinv_2.Draw();
  gauss_const_backgd_2.Draw("same");
  c10.cd(3);
  hMinv_3.SetMinimum(1e-09);
  hMinv_3->GetXaxis()->SetRange(0,100);
  hMinv_3.Draw();
  gauss_const_backgd_3.Draw("same");
  c10.cd(4);
  hMinv_4.SetMinimum(1e-09);
  hMinv_4->GetXaxis()->SetRange(0,100);
  hMinv_4.Draw();
  gauss_const_backgd_4.Draw("same");
  c10.cd(5);
  hMinv_5.SetMinimum(1e-09);
  hMinv_5->GetXaxis()->SetRange(0,100);
  hMinv_5.Draw();
  gauss_const_backgd_5.Draw("same");
  c10.cd(6);
  hMinv_6.SetMinimum(1e-09);
  hMinv_6->GetXaxis()->SetRange(0,100);
  hMinv_6.Draw();
  gauss_const_backgd_6.Draw("same");
  c10.cd(7);
  hMinv_7.SetMinimum(1e-09);
  hMinv_7->GetXaxis()->SetRange(0,100);
  hMinv_7.Draw();
  gauss_const_backgd_7.Draw("same");
  c10.cd(8);
  hMinv_8.SetMinimum(1e-09);
  hMinv_8->GetXaxis()->SetRange(0,100);
  hMinv_8.Draw();
  gauss_const_backgd_8.Draw("same");
  c10.cd(9);
  hMinv_9.SetMinimum(1e-09);
  hMinv_9->GetXaxis()->SetRange(0,100);
  hMinv_9.Draw();
  gauss_const_backgd_9.Draw("same");

  //_____________________________________________________________________________
  
  FiduAsymChi2TOFCutPeriph.cd();
  //FiduAsymTOFCutPeriph.cd();

  TCanvas c11("c11","Minv",900,700);
  gStyle->SetOptStat(11111111);
  c11.Divide(3,3,0,0);

  c11.cd(1);
  hMinv_1.SetMinimum(1e-09);
  hMinv_1->GetXaxis()->SetRange(0,100);
  hMinv_1.Draw();
  gauss_const_backgd_1.Draw("same");
  c11.cd(2);
  hMinv_2.SetMinimum(1e-09);
  hMinv_2->GetXaxis()->SetRange(0,100);
  hMinv_2.Draw();
  gauss_const_backgd_2.Draw("same");
  c11.cd(3);
  hMinv_3.SetMinimum(1e-09);
  hMinv_3->GetXaxis()->SetRange(0,100);
  hMinv_3.Draw();
  gauss_const_backgd_3.Draw("same");
  c11.cd(4);
  hMinv_4.SetMinimum(1e-09);
  hMinv_4->GetXaxis()->SetRange(0,100);
  hMinv_4.Draw();
  gauss_const_backgd_4.Draw("same");
  c11.cd(5);
  hMinv_5.SetMinimum(1e-09);
  hMinv_5->GetXaxis()->SetRange(0,100);
  hMinv_5.Draw();
  gauss_const_backgd_5.Draw("same");
  c11.cd(6);
  hMinv_6.SetMinimum(1e-09);
  hMinv_6->GetXaxis()->SetRange(0,100);
  hMinv_6.Draw();
  gauss_const_backgd_6.Draw("same");
  c11.cd(7);
  hMinv_7.SetMinimum(1e-09);
  hMinv_7->GetXaxis()->SetRange(0,100);
  hMinv_7.Draw();
  gauss_const_backgd_7.Draw("same");
  c11.cd(8);
  hMinv_8.SetMinimum(1e-09);
  hMinv_8->GetXaxis()->SetRange(0,100);
  hMinv_8.Draw();
  gauss_const_backgd_8.Draw("same");
  c11.cd(9);
  hMinv_9.SetMinimum(1e-09);
  hMinv_9->GetXaxis()->SetRange(0,100);
  hMinv_9.Draw();
  gauss_const_backgd_9.Draw("same");
  in.cd();

  //_____________________________________________________________________________
  // Inclusive Efficiencies

  TCanvas *incl = new TCanvas("incl","inclusive",650,500);

  NoCuts.cd();
  hEfficiency_integfit->SetMinimum(0);
  hEfficiency_integfit->SetMaximum(1.1);
  hEfficiency_integfit->GetXaxis()->SetTitle("#pi^{0} p_{T}(GeV/c)");
  //hEfficiency_integfit->GetYaxis()->SetTitle("Efficiency (N_{meas. Cut X}^{#pi^{0}}(p_{T})/(N_{true}^{#pi^{0}})(p_{T})");
  hEfficiency_integfit->GetYaxis()->SetTitle("Efficiency (N_{meas. Cut X}^{#pi^{0}} / (N_{true}^{#pi^{0}})");
  hEfficiency_integfit->Draw("");
  TF1 *straight = new TF1("straight","1",0.,5.);
  straight->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(3);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduCut.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(3);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymCut.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(3);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymTruePhotonCut.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hEfficiency_integfit->GetYaxis()->SetTitle("Efficiency");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(1);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2Cut.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(4);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymTOFCut.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(6);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2TOFCut.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(2);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();
  incl.Update();

//   // without asymetry cut ...
//   .cd();
//   hEfficiency_integfit->Draw("same");
//   hEfficiency_integfit->SetMarkerStyle(21);
//   hEfficiency_integfit->SetMarkerSize(1.4);
//   hEfficiency_integfit->SetMarkerColor(2);
//   in.cd();

//   .cd();
//   hEfficiency_integfit->Draw("same");
//   hEfficiency_integfit->SetMarkerStyle(21);
//   hEfficiency_integfit->SetMarkerSize(1.4);
//   hEfficiency_integfit->SetMarkerColor(9);

  //_____________________________________________________________________________
  // Central Efficiencies

  TCanvas *central = new TCanvas("central","central",650,500);

  Cent.cd();
  hEfficiency_integfit->SetMinimum(0);
  hEfficiency_integfit->SetMaximum(1.1);
  hEfficiency_integfit->GetXaxis()->SetTitle("#pi^{0} p_{T}(GeV/c)");
  hEfficiency_integfit->GetYaxis()->SetTitle("Efficiency (No Cuts)");
  hEfficiency_integfit->Draw("");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(3);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  TF1 *straight = new TF1("straight","1",0.,5.);
  straight->Draw("same");

  FiduCutCent.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(3);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymCutCent.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(3);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymTruePhotonCutCent.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hEfficiency_integfit->GetYaxis()->SetTitle("Efficiency");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(1);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2CutCent.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(4);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2TOFCutCent.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(2);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerColor(1);
  hEfficiency_integcounts->SetMarkerSize(1.);
  in.cd();

  FiduAsymTOFCutCent.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(4);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  //hEfficiency_integcounts->SetError(0);
  hEfficiency_integcounts->SetMarkerColor(1);
  hEfficiency_integcounts->SetMarkerSize(1.);
  in.cd();

  // bis ...

  TCanvas *central2 = new TCanvas("central2","central",650,500);

  Cent.cd();
  hEfficiency_integcounts->Draw("");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(3);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  TF1 *straight = new TF1("straight","1",0.,5.);
  straight->Draw("same");

  FiduCutCent.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(3);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  FiduAsymCutCent.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(3);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  FiduAsymTruePhotonCutCent.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hEfficiency_integcounts->GetYaxis()->SetTitle("Efficiency");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(1);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2CutCent.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(4);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2TOFCutCent.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(2);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  in.cd();

  FiduAsymTOFCutCent.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(4);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  in.cd();

//   // without asymetry cut ...
//   .cd();
//   hEfficiency_integfit->Draw("same");
//   hEfficiency_integfit->SetMarkerStyle(21);
//   hEfficiency_integfit->SetMarkerSize(1.4);
//   hEfficiency_integfit->SetMarkerColor(2);
//   in.cd();

//   .cd();
//   hEfficiency_integfit->Draw("same");
//   hEfficiency_integfit->SetMarkerStyle(21);
//   hEfficiency_integfit->SetMarkerSize(1.4);
//   hEfficiency_integfit->SetMarkerColor(9);

  //_____________________________________________________________________________
  // Peripheral Efficiencies

  TCanvas *periph0 = new TCanvas("periph0","peripheral",650,500);

  Periph.cd();
  hEfficiency_integfit->SetMinimum(0);
  hEfficiency_integfit->SetMaximum(1.1);
  hEfficiency_integfit->GetXaxis()->SetTitle("#pi^{0} p_{T}(GeV/c)");
  hEfficiency_integfit->GetXaxis()->SetTitleOffset(1.2);
  hEfficiency_integfit->GetYaxis()->SetTitle("Efficiency (No Cuts)");
  hEfficiency_integfit->Draw("");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(3);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  TF1 *straight = new TF1("straight","1",0.,5.);
  straight->Draw("same");

  FiduCutPeriph.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(3);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymCutPeriph.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(3);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymTruePhotonCutPeriph.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hEfficiency_integfit->GetYaxis()->SetTitle("Efficiency");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(1);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2CutPeriph.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(4);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2TOFCutPeriph.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(2);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  FiduAsymTOFCutPeriph.cd();
  hEfficiency_integfit->Draw("same");
  hEfficiency_integfit->SetMarkerStyle(21);
  hEfficiency_integfit->SetMarkerSize(1.4);
  hEfficiency_integfit->SetMarkerColor(4);
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(8);
  hEfficiency_integcounts->SetMarkerSize(1.);
  hEfficiency_integcounts->SetMarkerColor(1);
  in.cd();

  // bis ...

  TCanvas *periphral2 = new TCanvas("periphral2","periphral",650,500);

  Periph.cd();
  hEfficiency_integcounts->Draw("");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(3);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  TF1 *straight = new TF1("straight","1",0.,5.);
  straight->Draw("same");

  FiduCutPeriph.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(3);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  FiduAsymCutPeriph.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(3);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  FiduAsymTruePhotonCutPeriph.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hEfficiency_integcounts->GetYaxis()->SetTitle("Efficiency");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(1);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2CutPeriph.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(4);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  in.cd();

  FiduAsymChi2TOFCutPeriph.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(2);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  in.cd();

  FiduAsymTOFCutPeriph.cd();
  hEfficiency_integcounts->Draw("same");
  hEfficiency_integcounts->SetMarkerStyle(21);
  hEfficiency_integcounts->SetMarkerSize(1.4);
  hEfficiency_integcounts->SetMarkerColor(4);
  hEfficiency_fixedwidth->Draw("same");
  hEfficiency_fixedwidth->SetMarkerStyle(8);
  hEfficiency_fixedwidth->SetMarkerColor(1);
  hEfficiency_fixedwidth->SetMarkerSize(1.);
  in.cd();

   // without asymetry cut ...
//   .cd();
//   hEfficiency_integfit->Draw("same");
//   hEfficiency_integfit->SetMarkerStyle(21);
//   hEfficiency_integfit->SetMarkerSize(1.4);
//   hEfficiency_integfit->SetMarkerColor(2);
//   in.cd();

//   .cd();
//   hEfficiency_integfit->Draw("same");
//   hEfficiency_integfit->SetMarkerStyle(21);
//   hEfficiency_integfit->SetMarkerSize(1.4);
//   hEfficiency_integfit->SetMarkerColor(9);

  //_____________________________________________________________________________
  
  //_____________________________________________________________________________
  // Inclusive Inv. mass peak and width

  TCanvas minv_peak;

  NoCuts.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymCut.cd();
  hRecoParticleMinvPeak->Draw("same");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymTruePhotonCut.cd();
  hRecoParticleMinvPeak->Draw("same");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymChi2Cut.cd();
  hRecoParticleMinvPeak->Draw("same");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymChi2TOFCut.cd();
  hRecoParticleMinvPeak->Draw("same");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  //_____________________________________________________________________________
  // Central Inv. mass peak and width

  TCanvas minv_peak_cent;

  Cent.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymCutCent.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymTruePhotonCutCent.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymChi2CutCent.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymChi2TOFCutCent.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  //_____________________________________________________________________________
  // Peripheral Inv. mass peak and width

  Periph.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  FiduAsymCutPeriph.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymTruePhotonCutPeriph.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymChi2CutPeriph.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("same");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();

  FiduAsymChi2TOFCutPeriph.cd();
  hRecoParticleMinvPeak->Draw("");
  hRecoParticleMinvPeak->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvPeak->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvPeak->Fit("pol1","","R",1.,4.);
  hRecoParticleMinvPeak->SetMarkerSize(1.4);
  hRecoParticleMinvPeak->SetMarkerStyle(20);
  hRecoParticleMinvPeak->SetMarkerColor(2);
  TF1 *straight = new TF1("straight","0.1349",1,4);
  straight->Draw("same");

  hRecoParticleMinvSigma->Draw("");
  hRecoParticleMinvSigma->GetXaxis()->SetTitle("pi0 pT (GeV/c)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitle("mass (GeV/c2)");
  hRecoParticleMinvSigma->GetYaxis()->SetTitleOffset(1.2);
  hRecoParticleMinvSigma->SetMarkerSize(1.4);
  hRecoParticleMinvSigma->SetMarkerStyle(20);
  hRecoParticleMinvSigma->SetMarkerColor(2);
  in.cd();


}
