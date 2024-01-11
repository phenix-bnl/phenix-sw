/*
  Pad Chamber Level2 evaluation macro
  Author: Charles F. Maguire
  Creation Date: July 3, 2001

*/

void pltPcLevel2(Int_t iDraw=1, Int_t maxEvents=9999999, const char *fname="first.root",
		   const char *gname="second.root", const char *hname="third.root",
		   const char *iname="fourth.root") {
 
 const float DEGRAD = 57.295779513;
 
 double fitf(double *x, double *par)
   {
     double arg = 0.0;
     if (par[1]) arg = (x[0]-par[2])/par[1];
     
     double fitval = par[0]*exp(-0.5*arg*arg);
     return fitval;
   }

 TCanvas *c1 = 0;
 c1 = new TCanvas("c1", "PC Level2", 200, 10, 700, 500);
 c1->SetFillColor(kWhite);
 
 gStyle->SetOptStat(1110);
 gStyle->SetFuncColor(kBlue);
 gStyle->SetOptFit();

 if(iDraw==1) {
   c1->Divide(2,3);
   c1->cd(1);

   Float_t rScale = 4.0;
   TH1F *h1PC1WPhiErr = new TH1F("h1PC1WPhiErr", "PC1 West Phi Error", 50, -1., 1.);
   h1PC1WPhiErr->SetLineColor(kGreen);
   h1PC1WPhiErr->SetFillStyle(1001);
   h1PC1WPhiErr->SetMaximum(20.0*rScale);
   h1PC1WPhiErr->SetXTitle("Error in PC1 West Phi Angle (Cluster - GEANT, deg)");
   h1PC1WPhiErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("PHI-PHIGEA>>h1PC1WPhiErr","IPC==1&&PHI<90.0");

   TText *t0 = new TText(-0.8, 70, "June 20, 2001, PC1 West Arm Checks");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   TText *t1 = new TText(-0.8, 63, "Reconstruction of 1500 single pi- events");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.045);
   t1->Draw();

   c1->cd(2);
   TH1F *h1PC1WThetErr = new TH1F("h1PC1WThetErr", "PC1 West Theta Error", 50, -1., 1.);
   h1PC1WThetErr->SetLineColor(kGreen);
   h1PC1WThetErr->SetFillStyle(1001);
   h1PC1WThetErr->SetMaximum(30.0*rScale);
   h1PC1WThetErr->SetXTitle("Error in PC1 West Theta Angle (Cluster - GEANT, deg)");
   h1PC1WThetErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("THETA-THETAGEA>>h1PC1WThetErr","IPC==1&&PHI<90.0");

   c1->cd(3);
   TH1F *h1PC2WPhiErr = new TH1F("h1PC2WPhiErr", "PC2 West Phi Error", 50, -1., 1.);
   h1PC2WPhiErr->SetLineColor(kGreen);
   h1PC2WPhiErr->SetFillStyle(1001);
   h1PC2WPhiErr->SetMaximum(20.0*rScale);
   h1PC2WPhiErr->SetXTitle("Error in PC2 West Phi Angle (Cluster - GEANT, deg)");
   h1PC2WPhiErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("PHI-PHIGEA>>h1PC2WPhiErr","IPC==2&&PHI<90.0");

   c1->cd(4);
   TH1F *h1PC2WThetErr = new TH1F("h1PC2WThetErr", "PC2 West Theta Error", 50, -1., 1.);
   h1PC2WThetErr->SetLineColor(kGreen);
   h1PC2WThetErr->SetFillStyle(1001);
   h1PC2WThetErr->SetMaximum(30.0*rScale);
   h1PC2WThetErr->SetXTitle("Error in PC2 West Theta Angle (Cluster - GEANT, deg)");
   h1PC2WThetErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("THETA-THETAGEA>>h1PC2WThetErr","IPC==2&&PHI<90.0");

   c1->cd(5);
   TH1F *h1PC3WPhiErr = new TH1F("h1PC3WPhiErr", "PC3 West Phi Error", 50, -1., 1.);
   h1PC3WPhiErr->SetLineColor(kGreen);
   h1PC3WPhiErr->SetFillStyle(1001);
   h1PC3WPhiErr->SetMaximum(20.0*rScale);
   h1PC3WPhiErr->SetXTitle("Error in PC3 West Phi Angle (Cluster - GEANT, deg)");
   h1PC3WPhiErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("PHI-PHIGEA>>h1PC3WPhiErr","IPC==3&&PHI<90.0");

   c1->cd(6);
   TH1F *h1PC3WThetErr = new TH1F("h1PC3WThetErr", "PC3 West Theta Error", 50, -1., 1.);
   h1PC3WThetErr->SetLineColor(kGreen);
   h1PC3WThetErr->SetFillStyle(1001);
   h1PC3WThetErr->SetMaximum(30.0*rScale);
   h1PC3WThetErr->SetXTitle("Error in PC3 West Theta Angle (Cluster - GEANT, deg)");
   h1PC3WThetErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("THETA-THETAGEA>>h1PC3WThetErr","IPC==3&&PHI<90.0");

 }  // iDraw = 1, West Arm errors

 if(iDraw==2) {
   c1->Divide(2,2);
   c1->cd(1);

   Float_t rScale = 4.0;
   TH1F *h1PC1EPhiErr = new TH1F("h1PC1EPhiErr", "PC1 East Phi Error", 50, -1., 1.);
   h1PC1EPhiErr->SetLineColor(kGreen);
   h1PC1EPhiErr->SetFillStyle(1001);
   h1PC1EPhiErr->SetMaximum(20.0*rScale);
   h1PC1EPhiErr->SetXTitle("Error in PC1 East Phi Angle (Cluster - GEANT, deg)");
   h1PC1EPhiErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("PHI-PHIGEA>>h1PC1EPhiErr","IPC==1&&PHI>90.0");

   TText *t0 = new TText(-0.8, 70, "June 20, 2001, PC1 East Arm Checks");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   TText *t1 = new TText(-0.8, 63, "Reconstruction of 1500 single pi- events");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.045);
   t1->Draw();

   c1->cd(2);
   TH1F *h1PC1EThetErr = new TH1F("h1PC1EThetErr", "PC1 East Theta Error", 50, -1., 1.);
   h1PC1EThetErr->SetLineColor(kGreen);
   h1PC1EThetErr->SetFillStyle(1001);
   h1PC1EThetErr->SetMaximum(30.0*rScale);
   h1PC1EThetErr->SetXTitle("Error in PC1 East Theta Angle (Cluster - GEANT, deg)");
   h1PC1EThetErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("THETA-THETAGEA>>h1PC1EThetErr","IPC==1&&PHI>90.0");

   c1->cd(3);
   TH1F *h1PC3EPhiErr = new TH1F("h1PC3EPhiErr", "PC3 East Phi Error", 50, -1., 1.);
   h1PC3EPhiErr->SetLineColor(kGreen);
   h1PC3EPhiErr->SetFillStyle(1001);
   h1PC3EPhiErr->SetMaximum(20.0*rScale);
   h1PC3EPhiErr->SetXTitle("Error in PC3 East Phi Angle (Cluster - GEANT, deg)");
   h1PC3EPhiErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("PHI-PHIGEA>>h1PC3EPhiErr","IPC==3&&PHI>90.0");

   c1->cd(4);
   TH1F *h1PC3EThetErr = new TH1F("h1PC3EThetErr", "PC3 East Theta Error", 50, -1., 1.);
   h1PC3EThetErr->SetLineColor(kGreen);
   h1PC3EThetErr->SetFillStyle(1001);
   h1PC3EThetErr->SetMaximum(30.0*rScale);
   h1PC3EThetErr->SetXTitle("Error in PC3 East Theta Angle (Cluster - GEANT, deg)");
   h1PC3EThetErr->SetYTitle("Counts per 0.04 deg bin");

   PCCluster->Draw("THETA-THETAGEA>>h1PC3EThetErr","IPC==3&&PHI>90.0");

 }  // iDraw = 2, East Arm errors

 if(iDraw==3) {
   c1->Divide(2,2);
   c1->cd(1);

   TH2F *h2PC1PC3DTh = new TH2F("h2PC1PC3Dth", "Delta Theta PC3 - PC1",
				25, 0., 20., 25, -0.5, 0.5);
   h2PC1PC3Dth->SetMarkerColor(kGreen);
   h2PC1PC3Dth->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3Dth->SetYTitle("Delta Theta PC3-PC1");
   h2PC1PC3Dth->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3Dth->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("THETA3-THETA1:PTOT>>h2PC1PC3Dth","THETA3>0.0&&PHI1<90.0");

   TText *t0 = new TText(1.0, 0.3, "June 20, West Arm Angle Correlations");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t1 = new TText(1.0, 0.2, "PC3 - PC1 Theta Correlation");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.045);
   t1->Draw();
   c1->cd(2);

   TH2F *h2PC1PC2DTh = new TH2F("h2PC1PC2Dth", "Delta Theta PC2 - PC1",
				25, 0., 20., 25, -0.5, 0.5);
   h2PC1PC2Dth->SetMarkerColor(kGreen);
   h2PC1PC2Dth->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC2Dth->SetYTitle("Delta Theta PC3-PC2");
   h2PC1PC2Dth->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2Dth->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2Dth->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2Dth->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("THETA2+0.074-THETA1:PTOT>>h2PC1PC2Dth","THETA2>0.0&&PHI1<90.0");

   TText *t2 = new TText(1.0, 0.2, "PC2 - PC1 Theta Correlation");
   t2.SetTextColor(kBlue);
   t2.SetTextSize(0.045);
   t2->Draw();
   c1->cd(3);

   TH2F *h2PC1PC3DPh = new TH2F("h2PC1PC3DPh", "Delta Phi PC3 - PC1",
				25, 0., 20., 25, -3.0, 2.0);
   h2PC1PC3DPh->SetMarkerColor(kGreen);
   h2PC1PC3DPh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3DPh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC3DPh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3DPh->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("PHI3-PHI1:PTOT>>h2PC1PC3DPh","PHI3>-90.0&&PHI1<90.0");

   TText *t3 = new TText(1.0, 1.0, "PC3 - PC1 Phi Correlation");
   t3.SetTextColor(kBlue);
   t3.SetTextSize(0.045);
   t3->Draw();
   c1->cd(4);

   TH2F *h2PC1PC2DPh = new TH2F("h2PC1PC2DPh", "Delta Phi PC2 - PC1",
				25, 0., 20., 25, -3.0, 2.0);
   h2PC1PC2DPh->SetMarkerColor(kGreen);
   h2PC1PC2DPh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC2DPh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC2DPh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2DPh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2DPh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2DPh->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("PHI2+0.055-PHI1:PTOT>>h2PC1PC2DPh","PHI2>-90.0&&PHI1<90.0");

   TText *t4 = new TText(1.0, 1.0, "PC2 - PC1 Phi Correlation");
   t4.SetTextColor(kBlue);
   t4.SetTextSize(0.045);
   t4->Draw();

 }  // iDraw = 3, West Arm correlated angles

 if(iDraw==4) {
   c1->Divide(1,2);
   c1->cd(1);

   TH2F *h2PC1PC3DTh = new TH2F("h2PC1PC3Dth", "Delta Theta PC3 - PC1",
				25, 0., 20., 25, -0.5, 0.5);
   h2PC1PC3Dth->SetMarkerColor(kGreen);
   h2PC1PC3Dth->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3Dth->SetYTitle("Delta Theta PC3-PC1");
   h2PC1PC3Dth->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3Dth->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("THETA3-THETA1:PTOT>>h2PC1PC3Dth","THETA3>0.0&&PHI1>90.0");

   TText *t0 = new TText(1.0, 0.3, "June 20, East Arm Angle Correlations");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t1 = new TText(1.0, 0.2, "PC3 - PC1 Theta Correlation");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.045);
   t1->Draw();
   c1->cd(2);

   TH2F *h2PC1PC3DPh = new TH2F("h2PC1PC3DPh", "Delta Phi PC3 - PC1",
				25, 0., 20., 25, -3.0, 2.0);
   h2PC1PC3DPh->SetMarkerColor(kGreen);
   h2PC1PC3DPh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3DPh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC3DPh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3DPh->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("PHI3-PHI1:PTOT>>h2PC1PC3DPh","PHI3>90.0&&PHI1>90.0");

   TText *t3 = new TText(1.0, 1.0, "PC3 - PC1 Phi Correlation");
   t3.SetTextColor(kBlue);
   t3.SetTextSize(0.045);
   t3->Draw();

 }  // iDraw = 4, East Arm correlated angles

 if(iDraw==5) {
   c1->Divide(1,4);
   c1->cd(1);

   Float_t rScale = 1.0;
   TH1F *h1WestPtot = new TH1F("h1WestPtot", "West Momentum", 50, 0., 20.);
   h1WestPtot->SetLineColor(kBlack);
   h1WestPtot->SetFillStyle(1001);
   h1WestPtot->SetMaximum(10.0*rScale);
   h1WestPtot->SetXTitle("Momentum (GeV/c)");
   h1WestPtot->SetYTitle("Counts per 0.4 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtot","THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");

   TText *t0 = new TText(6., 8., "June 20, Momentum for Particles in West Arm");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   c1->cd(2);
   TH1F *h1WestPtotCut31 = new TH1F("h1WestPtotCut31", "West Momentum, PC3-PC1 Cut", 50, 0., 20.);
   h1WestPtotCut31->SetLineColor(kBlack);
   h1WestPtotCut31->SetFillStyle(1001);
   h1WestPtotCut31->SetMaximum(10.0*rScale);
   h1WestPtotCut31->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut31->SetYTitle("Counts per 0.4 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtotCut31","abs(PHI3-PHI1)<0.5&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");
   TText *t0 = new TText(6., 8., "Momentum for Particles in West Arm after PC3-PC1 cut");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   c1->cd(3);
   TH1F *h1WestPtotCut21 = new TH1F("h1WestPtotCut21", "West Momentum, PC2-PC1 Cut", 50, 0., 20.);
   h1WestPtotCut21->SetLineColor(kBlack);
   h1WestPtotCut21->SetFillStyle(1001);
   h1WestPtotCut21->SetMaximum(10.0*rScale);
   h1WestPtotCut21->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut21->SetYTitle("Counts per 0.4 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtotCut21","abs(PHI2+0.055-PHI1)<0.4&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");
   TText *t0 = new TText(6., 8., "Momentum for Particles in West Arm after PC2-PC1 cut");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   c1->cd(4);
   TH1F *h1WestPtotCut321 = new TH1F("h1WestPtotCut321", "West Momentum, Combined Cuts", 50, 0., 20.);
   h1WestPtotCut321->SetLineColor(kBlack);
   h1WestPtotCut321->SetFillStyle(1001);
   h1WestPtotCut321->SetMaximum(10.0*rScale);
   h1WestPtotCut321->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut321->SetYTitle("Counts per 0.4 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtotCut321","abs(PHI2+0.055-PHI1)<0.4&&abs(PHI3-PHI1)<0.5&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");
   TText *t0 = new TText(6., 8., "Momentum for Particles in West Arm after Combined cut");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

 } // iDraw = 5,  West Arm momentum projection


 if(iDraw==6) {
   c1->Divide(1,2);
   c1->cd(1);

   Float_t rScale = 1.0;
   TH1F *h1EastPtot = new TH1F("h1EastPtot", "East Momentum", 50, 0., 20.);
   h1EastPtot->SetLineColor(kBlack);
   h1EastPtot->SetFillStyle(1001);
   h1EastPtot->SetMaximum(10.0*rScale);
   h1EastPtot->SetXTitle("Momentum (GeV/c)");
   h1EastPtot->SetYTitle("Counts per 0.4 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1EastPtot","THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1");

   TText *t0 = new TText(6., 8., "June 20, Momentum for Particles in East Arm");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   c1->cd(2);
   TH1F *h1EastPtotCut31 = new TH1F("h1EastPtotCut31", "East Momentum, PC3-PC1 Cut", 50, 0., 20.);
   h1EastPtotCut31->SetLineColor(kBlack);
   h1EastPtotCut31->SetFillStyle(1001);
   h1EastPtotCut31->SetMaximum(10.0*rScale);
   h1EastPtotCut31->SetXTitle("Momentum (GeV/c)");
   h1EastPtotCut31->SetYTitle("Counts per 0.4 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1EastPtotCut31","abs(PHI3-PHI1)<0.5&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1");
   TText *t0 = new TText(6., 8., "Momentum for Particles in East Arm after PC3-PC1 cut");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

 } // iDraw = 6,  East Arm momentum projection


 if(iDraw==7) {   // West Arm angle correlations Pi- HIJING
   c1->Divide(2,2);
   c1->cd(1);

   TH2F *h2PC1PC3DTh = new TH2F("h2PC1PC3Dth", "Delta Theta PC3 - PC1",
				25, 0., 10., 25, -0.5, 0.5);
   h2PC1PC3Dth->SetMarkerColor(kGreen);
   h2PC1PC3Dth->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3Dth->SetYTitle("Delta Theta PC3-PC1");
   h2PC1PC3Dth->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3Dth->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("THETA3-THETA1:PTOT>>h2PC1PC3Dth","THETA3>0.0&&PHI1<90.0");

   TText *t0 = new TText(1.0, 0.3, "June 20, West Arm Angle Correlations");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t1 = new TText(1.0, 0.2, "PC3 - PC1 Theta Correlation");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.045);
   t1->Draw();
   c1->cd(2);

   TH2F *h2PC1PC2DTh = new TH2F("h2PC1PC2Dth", "Delta Theta PC2 - PC1",
				25, 0., 10., 25, -0.5, 0.5);
   h2PC1PC2Dth->SetMarkerColor(kGreen);
   h2PC1PC2Dth->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC2Dth->SetYTitle("Delta Theta PC3-PC2");
   h2PC1PC2Dth->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2Dth->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2Dth->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2Dth->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("THETA2+0.074-THETA1:PTOT>>h2PC1PC2Dth","THETA2>0.0&&PHI1<90.0");

   TText *t2 = new TText(1.0, 0.2, "PC2 - PC1 Theta Correlation");
   t2.SetTextColor(kBlue);
   t2.SetTextSize(0.045);
   t2->Draw();
   c1->cd(3);

   TH2F *h2PC1PC3DPh = new TH2F("h2PC1PC3DPh", "Delta Phi PC3 - PC1",
				25, 0., 10., 25, -3.0, 2.0);
   h2PC1PC3DPh->SetMarkerColor(kGreen);
   h2PC1PC3DPh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3DPh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC3DPh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3DPh->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("PHI3-PHI1:PTOT>>h2PC1PC3DPh","PHI3>-90.0&&PHI1<90.0");

   TText *t3 = new TText(1.0, 1.0, "PC3 - PC1 Phi Correlation");
   t3.SetTextColor(kBlue);
   t3.SetTextSize(0.045);
   t3->Draw();
   c1->cd(4);

   TH2F *h2PC1PC2DPh = new TH2F("h2PC1PC2DPh", "Delta Phi PC2 - PC1",
				25, 0., 10., 25, -3.0, 2.0);
   h2PC1PC2DPh->SetMarkerColor(kGreen);
   h2PC1PC2DPh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC2DPh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC2DPh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2DPh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2DPh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2DPh->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("PHI2+0.055-PHI1:PTOT>>h2PC1PC2DPh","PHI2>-90.0&&PHI1<90.0");

   TText *t4 = new TText(1.0, 1.0, "PC2 - PC1 Phi Correlation");
   t4.SetTextColor(kBlue);
   t4.SetTextSize(0.045);
   t4->Draw();

 }  // iDraw = 7, West Arm correlated angles, Pi- HIJING


 if(iDraw==8) {   // West Arm angle correlations Pi+ HIJING
   c1->Divide(2,2);
   c1->cd(1);

   TH2F *h2PC1PC3DTh = new TH2F("h2PC1PC3Dth", "Delta Theta PC3 - PC1",
				25, 0., 10., 25, -0.5, 0.5);
   h2PC1PC3Dth->SetMarkerColor(kGreen);
   h2PC1PC3Dth->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3Dth->SetYTitle("Delta Theta PC3-PC1");
   h2PC1PC3Dth->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3Dth->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("THETA3-THETA1:PTOT>>h2PC1PC3Dth","THETA3>0.0&&PHI1<90.0");

   TText *t0 = new TText(1.0, 0.3, "June 20, West Arm Angle Correlations");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t1 = new TText(1.0, 0.2, "PC3 - PC1 Theta Correlation");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.045);
   t1->Draw();
   c1->cd(2);

   TH2F *h2PC1PC2DTh = new TH2F("h2PC1PC2Dth", "Delta Theta PC2 - PC1",
				25, 0., 10., 25, -0.5, 0.5);
   h2PC1PC2Dth->SetMarkerColor(kGreen);
   h2PC1PC2Dth->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC2Dth->SetYTitle("Delta Theta PC3-PC2");
   h2PC1PC2Dth->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2Dth->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2Dth->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2Dth->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("THETA2+0.074-THETA1:PTOT>>h2PC1PC2Dth","THETA2>0.0&&PHI1<90.0");

   TText *t2 = new TText(1.0, 0.2, "PC2 - PC1 Theta Correlation");
   t2.SetTextColor(kBlue);
   t2.SetTextSize(0.045);
   t2->Draw();
   c1->cd(3);

   TH2F *h2PC1PC3DPh = new TH2F("h2PC1PC3DPh", "Delta Phi PC3 - PC1",
				25, 0., 10., 25, -2.0, 3.0);
   h2PC1PC3DPh->SetMarkerColor(kGreen);
   h2PC1PC3DPh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3DPh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC3DPh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3DPh->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("PHI3-PHI1:PTOT>>h2PC1PC3DPh","PHI3>-90.0&&PHI1<90.0");

   TText *t3 = new TText(+1.0, -1.0, "PC3 - PC1 Phi Correlation");
   t3.SetTextColor(kBlue);
   t3.SetTextSize(0.045);
   t3->Draw();
   c1->cd(4);

   TH2F *h2PC1PC2DPh = new TH2F("h2PC1PC2DPh", "Delta Phi PC2 - PC1",
				25, 0., 10., 25, -2.0, 3.0);
   h2PC1PC2DPh->SetMarkerColor(kGreen);
   h2PC1PC2DPh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC2DPh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC2DPh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2DPh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2DPh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2DPh->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("PHI2+0.055-PHI1:PTOT>>h2PC1PC2DPh","PHI2>-90.0&&PHI1<90.0");

   TText *t4 = new TText(+1.0, -1.0, "PC2 - PC1 Phi Correlation");
   t4.SetTextColor(kBlue);
   t4.SetTextSize(0.045);
   t4->Draw();

 }  // iDraw = 8, West Arm correlated angles, Pi+ HIJING

 if(iDraw==9) {  // Delta Theta Pi- and Pi+ HIJING

   gROOT->Reset();

   TFile *f1 =  new TFile(fname);   // Pi-
   if (!f1) {
     cerr << "\n Unable to find file " << fname << endl;
     return;
   }

   TFile *f2 =  new TFile(gname);   // Pi+
   if (!f2) {
     cerr << "\n Unable to find file " << gname << endl;
     return;
   }

   TTree *PCLevel21 = (TTree*)f1.Get("PCLevel2");
   if(!PCLevel21) {
     cerr << "\n Unable to find PCLevel2 in first file" << endl;
     return;
   }

   TTree *PCLevel22 = (TTree*)f2.Get("PCLevel2");
   if(!PCLevel22) {
     cerr << "\n Unable to find PCLevel2 in second file" << endl;
     return;
   }

   c1->Divide(2,2);
   c1->cd(1);

   TH1F *h1PC1PC3DthNeg = new TH1F("h1PC1PC3DthNeg", "Pi- West PC3-PC1 Theta", 100, -2., 2.);
   h1PC1PC3DthNeg->SetLineColor(kGreen);
   h1PC1PC3DthNeg->SetFillStyle(1001);
   h1PC1PC3DthNeg->SetMaximum(200.0);
   h1PC1PC3DthNeg->SetXTitle("THETA3 - THETA1 (degrees)");
   h1PC1PC3DthNeg->SetYTitle("Counts per 0.04 deg bin");

   PCLevel21->Project("h1PC1PC3DthNeg","THETA3-THETA1","THETA3>0");

   TF1 *func1gausDth13Neg = new TF1("fitDth13Neg", fitf, -0.3, +0.3, 3);
   func1gausDth13Neg->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausDth13Neg->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausDth13Neg->SetParameters(30, 0.010, 0.0);  // initial guesses for fit parameters

   h1PC1PC3DthNeg->Fit("fitDth13Neg","r");

   TH1F *h1PC1PC3DthNegCut = new TH1F("h1PC1PC3DthNegCut", "Pi- West PC3-PC1 Theta", 100, -2., 2.);
   h1PC1PC3DthNegCut->SetLineColor(kGreen);
   h1PC1PC3DthNegCut->SetFillStyle(1001);
   h1PC1PC3DthNegCut->SetMaximum(200.0);
   h1PC1PC3DthNegCut->SetXTitle("THETA3 - THETA1 (degrees)");
   h1PC1PC3DthNegCut->SetYTitle("Counts per 0.04 deg bin");
   PCLevel21->Project("h1PC1PC3DthNegCut","THETA3-THETA1","abs(THETA3-THETA1)<0.15&&THETA3>0");
   cout << "\n PC1PC3 Neg in cut = " << h1PC1PC3DthNegCut->GetSum() << endl;

   c1->cd(2);

   TH1F *h1PC1PC2DthNeg = new TH1F("h1PC1PC2DthNeg", "Pi- West PC2-PC1 Theta", 100, -2., 2.);
   h1PC1PC2DthNeg->SetLineColor(kGreen);
   h1PC1PC2DthNeg->SetFillStyle(1001);
   h1PC1PC2DthNeg->SetMaximum(200.0);
   h1PC1PC2DthNeg->SetXTitle("THETA2 - THETA1 (degrees)");
   h1PC1PC2DthNeg->SetYTitle("Counts per 0.04 deg bin");

   PCLevel21->Project("h1PC1PC2DthNeg","THETA2-THETA1","THETA2>0");

   TF1 *func1gausDth12Neg = new TF1("fitDth12Neg", fitf, -0.3, +0.3, 3);
   func1gausDth12Neg->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausDth12Neg->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausDth12Neg->SetParameters(30, 0.010, 0.0);  // initial guesses for fit parameters

   h1PC1PC2DthNeg->Fit("fitDth12Neg","r");

   TH1F *h1PC1PC2DthNegCut = new TH1F("h1PC1PC2DthNegCut", "Pi- West PC2-PC1 Theta", 100, -2., 2.);
   h1PC1PC2DthNegCut->SetLineColor(kGreen);
   h1PC1PC2DthNegCut->SetFillStyle(1001);
   h1PC1PC2DthNegCut->SetMaximum(200.0);
   h1PC1PC2DthNegCut->SetXTitle("THETA2 - THETA1 (degrees)");
   h1PC1PC2DthNegCut->SetYTitle("Counts per 0.04 deg bin");
   PCLevel21->Project("h1PC1PC2DthNegCut","THETA2-THETA1","abs(THETA2+0.072-THETA1)<0.15&&THETA2>0");
   cout << "\n PC1PC2 Neg in cut = " << h1PC1PC2DthNegCut->GetSum() << endl;

   c1->cd(3);

   TH1F *h1PC1PC3DthPos = new TH1F("h1PC1PC3DthPos", "Pi+ West PC3-PC1 Theta", 100, -2., 2.);
   h1PC1PC3DthPos->SetLineColor(kGreen);
   h1PC1PC3DthPos->SetFillStyle(1001);
   h1PC1PC3DthPos->SetMaximum(200.0);
   h1PC1PC3DthPos->SetXTitle("THETA3 - THETA1 (degrees)");
   h1PC1PC3DthPos->SetYTitle("Counts per 0.04 deg bin");

   PCLevel22->Project("h1PC1PC3DthPos","THETA3-THETA1","THETA2>0");

   TF1 *func1gausDth13Pos = new TF1("fitDth13Pos", fitf, -0.3, +0.3, 3);
   func1gausDth13Pos->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausDth13Pos->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausDth13Pos->SetParameters(30, 0.010, 0.0);  // initial guesses for fit parameters

   h1PC1PC3DthPos->Fit("fitDth13Pos","r");

   TH1F *h1PC1PC3DthPosCut = new TH1F("h1PC1PC3DthPosCut", "Pi+ West PC3-PC1 Theta", 100, -2., 2.);
   h1PC1PC3DthPosCut->SetLineColor(kGreen);
   h1PC1PC3DthPosCut->SetFillStyle(1001);
   h1PC1PC3DthPosCut->SetMaximum(200.0);
   h1PC1PC3DthPosCut->SetXTitle("THETA3 - THETA1 (degrees)");
   h1PC1PC3DthPosCut->SetYTitle("Counts per 0.04 deg bin");
   PCLevel22->Project("h1PC1PC3DthPosCut","THETA3-THETA1","abs(THETA3-THETA1)<0.15&&THETA3>0");
   cout << "\n PC1PC3 Pos in cut = " << h1PC1PC3DthPosCut->GetSum() << endl;

   c1->cd(4);

   TH1F *h1PC1PC2DthPos = new TH1F("h1PC1PC2DthPos", "Pi+ West PC2-PC1 Theta", 100, -2., 2.);
   h1PC1PC2DthPos->SetLineColor(kGreen);
   h1PC1PC2DthPos->SetFillStyle(1001);
   h1PC1PC2DthPos->SetMaximum(200.0);
   h1PC1PC2DthPos->SetXTitle("THETA2 - THETA1 (degrees)");
   h1PC1PC2DthPos->SetYTitle("Counts per 0.04 deg bin");

   PCLevel22->Project("h1PC1PC2DthPos","THETA2-THETA1","THETA2>0");

   TF1 *func1gausDth12Pos = new TF1("fitDth12Pos", fitf, -0.3, +0.3, 3);
   func1gausDth12Pos->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausDth12Pos->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausDth12Pos->SetParameters(30, 0.010, 0.0);  // initial guesses for fit parameters

   h1PC1PC2DthPos->Fit("fitDth12Pos","r");

   TH1F *h1PC1PC2DthPosCut = new TH1F("h1PC1PC2DthPosCut", "Pi+ West PC2-PC1 Theta", 100, -2., 2.);
   h1PC1PC2DthPosCut->SetLineColor(kGreen);
   h1PC1PC2DthPosCut->SetFillStyle(1001);
   h1PC1PC2DthPosCut->SetMaximum(200.0);
   h1PC1PC2DthPosCut->SetXTitle("THETA2 - THETA1 (degrees)");
   h1PC1PC2DthPosCut->SetYTitle("Counts per 0.04 deg bin");
   PCLevel22->Project("h1PC1PC2DthPosCut","THETA2-THETA1","abs(THETA2+0.072-THETA1)<0.15&&THETA2>0");
   cout << "\n PC1PC2 Pos in cut = " << h1PC1PC2DthPosCut->GetSum() << endl;

 } // iDraw = 9,  Delta Theta Pions for HIJING seed

 if(iDraw==10) {  // 2-D Delta Theta Pi- and Pi+ HIJING

   gROOT->Reset();

   TFile *f1 =  new TFile(fname);   // Pi-
   if (!f1) {
     cerr << "\n Unable to find file " << fname << endl;
     return;
   }

   TFile *f2 =  new TFile(gname);   // Pi+
   if (!f2) {
     cerr << "\n Unable to find file " << gname << endl;
     return;
   }

   TTree *PCLevel21 = (TTree*)f1.Get("PCLevel2");
   if(!PCLevel21) {
     cerr << "\n Unable to find PCLevel2 in first file" << endl;
     return;
   }

   TTree *PCLevel22 = (TTree*)f2.Get("PCLevel2");
   if(!PCLevel22) {
     cerr << "\n Unable to find PCLevel2 in second file" << endl;
     return;
   }

   c1->Divide(1,2);
   c1->cd(1);

   TH2F *h2PC1PC2PC3DthNeg = new TH2F("h2PC1PC2PC3DthNeg", "Pi- Theta Correl", 100, -1., 1.,
				      100, -1., 1.);
   h2PC1PC2PC3DthNeg->SetMarkerColor(kGreen);
   h2PC1PC2PC3DthNeg->SetXTitle("Delta Theta PC2-PC1 (deg)");
   h2PC1PC2PC3DthNeg->SetYTitle("Delta Theta PC3-PC1 (deg)");
   h2PC1PC2PC3DthNeg->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2PC3DthNeg->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2PC3DthNeg->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2PC3DthNeg->GetYaxis()->SetTitleSize(0.045);

   PCLevel21->Project("h2PC1PC2PC3DthNeg","THETA3-THETA1:THETA2-THETA1","THETA2>0&&THETA3>0");
   h2PC1PC2PC3DthNeg->Draw();

   c1->cd(2);

   TH2F *h2PC1PC2PC3DthPos = new TH2F("h2PC1PC2PC3DthPos", "Pi- Theta Correl", 100, -1., 1.,
				      100, -1., 1.);
   h2PC1PC2PC3DthPos->SetMarkerColor(kGreen);
   h2PC1PC2PC3DthPos->SetXTitle("Delta Theta PC2-PC1 (deg)");
   h2PC1PC2PC3DthPos->SetYTitle("Delta Theta PC3-PC1 (deg)");
   h2PC1PC2PC3DthPos->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2PC3DthPos->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2PC3DthPos->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2PC3DthPos->GetYaxis()->SetTitleSize(0.045);

   PCLevel22->Project("h2PC1PC2PC3DthPos","THETA3-THETA1:THETA2-THETA1","THETA2>0&&THETA3>0");
   h2PC1PC2PC3DthPos->Draw();
 
 } // iDraw = 10,  2-D Delta Theta Pions for HIJING seed

 if(iDraw==11) {    //  Pi- singles, for HIJING
   c1->Divide(1,3);
   c1->cd(1);

   TH1F *h1WestPtot = new TH1F("h1WestPtot", "West Momentum Pi-, No cuts", 50, 0., 10.);
   h1WestPtot->SetLineColor(kBlue);
   h1WestPtot->SetLineWidth(2);
   h1WestPtot->SetFillStyle(1001);
   h1WestPtot->SetMaximum(30);
   h1WestPtot->SetXTitle("Momentum (GeV/c)");
   h1WestPtot->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot","PTOT","GPC123==111&&THETA2>0.0&&THETA3>0.0");
   h1WestPtot->Draw();

   TText *t0 = new TText(1.5, 25., "Pi- particles in stand-alone events (no cuts)");
   t0.SetTextColor(kBlue);
   t0.SetTextSize(0.060);
   t0->Draw();

   TH1F *h1WestPtot5 = new TH1F("h1WestPtot5", "West Arm, 5 GeV cut", 50, 0., 10.);
   h1WestPtot5->SetLineColor(kGreen);
   h1WestPtot5->SetFillColor(kGreen);
   h1WestPtot5->SetFillStyle(1001);
   h1WestPtot5->SetMaximum(30);
   h1WestPtot5->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5","PTOT","PTOT>5.0&&GPC123==111&&THETA2>0.0&&THETA3>0.0");
   h1WestPtot5->Draw("same");

   cout << "\n Sum above 5 GeV/c = " <<  h1WestPtot5->GetSum() << endl;

   TText *t0a = new TText(1.5, 4., "Yield above 5 GeV/c = 337");
   t0a.SetTextColor(kGreen);
   t0a.SetTextSize(0.060);
   t0a->Draw();

   c1->cd(2);

   TH1F *h1WestPtotCutThSum = new TH1F("h1WestPtotCutThSum", "West Arm Pi-, Theta Cuts", 50, 0., 10.);
   h1WestPtotCutThSum->SetLineColor(kBlue);
   h1WestPtotCutThSum->SetLineWidth(2);
   h1WestPtotCutThSum->SetFillStyle(1001);
   h1WestPtotCutThSum->SetMaximum(30);
   h1WestPtotCutThSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSum","PTOT","GPC123==111&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.15&&abs(THETA2+0.072-THETA1)<0.15&&abs(THETA3-THETA2-0.072)<0.15");
   h1WestPtotCutThSum->Draw();

   TText *t1 = new TText(1.5, 25., "Pi- particles in stand-alone events (Theta cuts)");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.060);
   t1->Draw();

   TH1F *h1WestPtot5CutThSum = new TH1F("h1WestPtot5CutThSum", "West Arm Pi-, 5 GeV/c", 50, 0., 10.);
   h1WestPtot5CutThSum->SetLineColor(kGreen);
   h1WestPtot5CutThSum->SetFillColor(kGreen);
   h1WestPtot5CutThSum->SetFillStyle(1001);
   h1WestPtot5CutThSum->SetMaximum(30);
   h1WestPtot5CutThSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5CutThSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5CutThSum","PTOT","GPC123==111&&PTOT>5.0&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.15&&abs(THETA2+0.072-THETA1)<0.15&&abs(THETA3-THETA2-0.072)<0.15");
   h1WestPtot5CutThSum->Draw("same");

   TText *t1a = new TText(1.5, 4., "Yield above 5 GeV/c = 331");
   t1a.SetTextColor(kGreen);
   t1a.SetTextSize(0.060);
   t1a->Draw();

   c1->cd(3);

   TH1F *h1WestPtotCutThSumCutPhSum = new TH1F("h1WestPtotCutThSumCutPhSum", "West Arm Pi-, All cuts", 50, 0., 10.);
   h1WestPtotCutThSumCutPhSum->SetLineColor(kBlue);
   h1WestPtotCutThSumCutPhSum->SetLineWidth(2);
   h1WestPtotCutThSumCutPhSum->SetFillStyle(1001);
   h1WestPtotCutThSumCutPhSum->SetMaximum(30);
   h1WestPtotCutThSumCutPhSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSumCutPhSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSumCutPhSum","PTOT","GPC123==111&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.15&&abs(THETA2+0.072-THETA1)<0.15&&abs(THETA3-THETA2-0.072)<0.15&&abs(PHI3-PHI1)<0.45&&abs(PHI2+0.05-PHI1)<0.40&&abs(PHI2+0.055-PHI1)<0.43&&abs(PHI3-PHI1)");
   h1WestPtotCutThSumCutPhSum->Draw();

   TH1F *h1WestPtot5CutThSumCutPhSum = new TH1F("h1WestPtot5CutThSumCutPhSum", "West Arm Pi-", 50, 0., 10.);
   h1WestPtot5CutThSumCutPhSum->SetLineColor(kGreen);
   h1WestPtot5CutThSumCutPhSum->SetFillColor(kGreen);
   h1WestPtot5CutThSumCutPhSum->SetFillStyle(1001);
   h1WestPtot5CutThSumCutPhSum->SetMaximum(30);
   h1WestPtot5CutThSumCutPhSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5CutThSumCutPhSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5CutThSumCutPhSum","PTOT","GPC123==111&&PTOT>5.0&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.15&&abs(THETA2+0.072-THETA1)<0.15&&abs(THETA3-THETA2-0.072)<0.15&&abs(PHI3-PHI1)<0.45&&abs(PHI2+0.05-PHI1)<0.40&&abs(PHI2+0.055-PHI1)<0.43&&abs(PHI3-PHI1)");
   h1WestPtot5CutThSumCutPhSum->Draw("same");

   cout << "\n Phi cut sum above 5 GeV/c = " <<  h1WestPtot5CutThSumCutPhSum->GetSum() << endl;

   TText *t2 = new TText(1.5, 25., "Pi- particles in stand-alone events ( Theta and Phi cuts)");
   t2.SetTextColor(kBlue);
   t2.SetTextSize(0.060);
   t2->Draw();

   TText *t2a = new TText(1.5, 10., "Yield above 5 GeV/c = 294");
   t2a.SetTextColor(kGreen);
   t2a.SetTextSize(0.060);
   t2a->Draw();

   TText *t2b = new TText(1.0, 6., "Singles efficiency above 5 GeV/c = 0.87");
   t2b.SetTextColor(kGreen);
   t2b.SetTextSize(0.060);
   t2b->Draw();
 
 } // iDraw = 11,  Pi- West Arm momentum projection for singles efficiency

 if(iDraw==13) {
   c1->Divide(2,2);
   c1->cd(1);

   TH2F *h2PC1PC3DTh = new TH2F("h2PC1PC3Dth", "Delta Theta PC3 - PC1",
				25, 0., 20., 25, -0.5, 0.5);
   h2PC1PC3Dth->SetMarkerColor(kGreen);
   h2PC1PC3Dth->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3Dth->SetYTitle("Delta Theta PC3-PC1");
   h2PC1PC3Dth->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3Dth->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3Dth->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("THETA3-THETA1:PTOT>>h2PC1PC3Dth","IDPARNT1==0&&THETA3>0.0&&PHI1<90.0");

   TText *t0 = new TText(1.0, 0.3, "June 20, West Arm Angle Correlations");
   t0.SetTextColor(kBlue);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t1 = new TText(1.0, 0.2, "PC3 - PC1 Theta Correlation");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.045);
   t1->Draw();


   TH2F *h2PC1PC3DthHigh = new TH2F("h2PC1PC3DthHigh", "Delta Theta PC3 - PC1",
				25, 0., 20., 25, -0.5, 0.5);

   h2PC1PC3DthHigh->SetMarkerColor(kRed);
   h2PC1PC3DthHigh->SetMarkerStyle(20);
   h2PC1PC3DthHigh->SetMarkerSize(0.45);

   h2PC1PC3DthHigh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3DthHigh->SetYTitle("Delta Theta PC3-PC1");
   h2PC1PC3DthHigh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3DthHigh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3DthHigh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3DthHigh->GetYaxis()->SetTitleSize(0.045);

   PCLevel2->Project("h2PC1PC3DthHigh","THETA3-THETA1:PTOT","IDPARNT1==0&&PTOT>2.5&&PHI3>-90.0&&PHI1<90.0");

   h2PC1PC3DphHigh->Draw("same");

   c1->cd(2);

   TH2F *h2PC1PC2DTh = new TH2F("h2PC1PC2Dth", "Delta Theta PC2 - PC1",
				25, 0., 20., 25, -0.5, 0.5);
   h2PC1PC2Dth->SetMarkerColor(kGreen);
   h2PC1PC2Dth->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC2Dth->SetYTitle("Delta Theta PC3-PC2");
   h2PC1PC2Dth->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2Dth->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2Dth->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2Dth->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("THETA2+0.074-THETA1:PTOT>>h2PC1PC2Dth","IDPARNT1==0&&THETA2>0.0&&PHI1<90.0");

   TText *t2 = new TText(1.0, 0.2, "PC2 - PC1 Theta Correlation");
   t2.SetTextColor(kBlue);
   t2.SetTextSize(0.045);
   t2->Draw();
   c1->cd(3);

   TH2F *h2PC1PC3DPh = new TH2F("h2PC1PC3DPh", "Delta Phi PC3 - PC1",
				25, 0., 20., 25, -3.0, 2.0);
   h2PC1PC3DPh->SetMarkerColor(kGreen);
   h2PC1PC3DPh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3DPh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC3DPh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3DPh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3DPh->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("PHI3-PHI1:PTOT>>h2PC1PC3DPh","IDPARNT1==0&&PHI3>-90.0&&PHI1<90.0");

   TText *t3 = new TText(1.0, 1.0, "PC3 - PC1 Phi Correlation");
   t3.SetTextColor(kBlue);
   t3.SetTextSize(0.045);
   t3->Draw();

   TH2F *h2PC1PC3DphHigh = new TH2F("h2PC1PC3DphHigh", "Delta Phi PC3 - PC1",
				25, 0., 20., 25, -3.0, 2.0);

   h2PC1PC3DphHigh->SetMarkerColor(kRed);
   h2PC1PC3DphHigh->SetMarkerStyle(20);
   h2PC1PC3DphHigh->SetMarkerSize(0.45);

   h2PC1PC3DphHigh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC3DphHigh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC3DphHigh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC3DphHigh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC3DphHigh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC3DphHigh->GetYaxis()->SetTitleSize(0.045);

   PCLevel2->Project("h2PC1PC3DphHigh","PHI3-PHI1:PTOT","IDPARNT1==0&&PTOT>2.5&&PHI3>-90.0&&PHI1<90.0");

   h2PC1PC3DphHigh->Draw("same");

   c1->cd(4);

   TH2F *h2PC1PC2DPh = new TH2F("h2PC1PC2DPh", "Delta Phi PC2 - PC1",
				25, 0., 20., 25, -3.0, 2.0);
   h2PC1PC2DPh->SetMarkerColor(kGreen);
   h2PC1PC2DPh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC2DPh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC2DPh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2DPh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2DPh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2DPh->GetYaxis()->SetTitleSize(0.045);
   PCLevel2->Draw("PHI2+0.055-PHI1:PTOT>>h2PC1PC2DPh","IDPARNT1==0&&PHI2>-90.0&&PHI1<90.0");

   TText *t4 = new TText(1.0, 1.0, "PC2 - PC1 Phi Correlation");
   t4.SetTextColor(kBlue);
   t4.SetTextSize(0.045);
   t4->Draw();

   TH2F *h2PC1PC2DphHigh = new TH2F("h2PC1PC2DphHigh", "Delta Phi PC3 - PC1",
				25, 0., 20., 25, -3.0, 2.0);

   h2PC1PC2DphHigh->SetMarkerColor(kRed);
   h2PC1PC2DphHigh->SetMarkerStyle(20);
   h2PC1PC2DphHigh->SetMarkerSize(0.45);

   h2PC1PC2DphHigh->SetXTitle("Particle Momentum (GeV/c)");
   h2PC1PC2DphHigh->SetYTitle("Delta Phi PC3-PC1");
   h2PC1PC2DphHigh->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2DphHigh->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2DphHigh->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2DphHigh->GetYaxis()->SetTitleSize(0.045);

   PCLevel2->Project("h2PC1PC2DphHigh","PHI2-PHI1:PTOT","IDPARNT1==0&&PTOT>2.5&&PHI3>-90.0&&PHI1<90.0");

   h2PC1PC2DphHigh->Draw("same");

 }  // iDraw = 13, HIJING West Arm correlated angles


 if(iDraw==15) {    //  HIJING West Arm momentum projection
   c1->Divide(1,4);
   c1->cd(1);

   Float_t rScale = 1.0;
   TH1F *h1WestPtot = new TH1F("h1WestPtot", "West Momentum, THETA cuts", 50, 0., 10.);
   h1WestPtot->SetLineColor(kBlack);
   h1WestPtot->SetFillStyle(1001);
   h1WestPtot->SetMaximum(200);
   h1WestPtot->SetXTitle("Momentum (GeV/c)");
   h1WestPtot->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtot","NFILE1==0&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");

   TText *t0 = new TText(3., 100., "June 20, Momentum for Particles in West Arm 867 HIJING min bias events");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   TText *t0a = new TText(3., 70., "PC1/PC2/PC3 tracklets after THETA3-THETA1 and THETA2-THETA1 cuts");
   t0a.SetTextColor(kRed);
   t0a.SetTextSize(0.055);
   t0a->Draw();

   c1->cd(2);
   TH1F *h1WestPtotCut31 = new TH1F("h1WestPtotCut31", "West Momentum, PC3-PC1 Cut", 50, 0., 10.);
   h1WestPtotCut31->SetLineColor(kBlack);
   h1WestPtotCut31->SetFillStyle(1001);
   h1WestPtotCut31->SetMaximum(30);
   h1WestPtotCut31->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut31->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtotCut31","NFILE1==0&&abs(PHI3-PHI1)<0.5&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");
   TText *t0 = new TText(3., 15., "Momentum for Particles in West Arm after PC3-PC1 cut");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   c1->cd(3);
   TH1F *h1WestPtotCut21 = new TH1F("h1WestPtotCut21", "West Momentum, PC2-PC1 Cut", 50, 0., 10.);
   h1WestPtotCut21->SetLineColor(kBlack);
   h1WestPtotCut21->SetFillStyle(1001);
   h1WestPtotCut21->SetMaximum(30);
   h1WestPtotCut21->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut21->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtotCut21","NFILE1==0&&abs(PHI2+0.055-PHI1)<0.4&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");
   TText *t0 = new TText(3., 15., "Momentum for Particles in West Arm after PC2-PC1 cut");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   c1->cd(4);
   TH1F *h1WestPtotCut321 = new TH1F("h1WestPtotCut321", "West Momentum, Combined Cuts", 50, 0., 10.);
   h1WestPtotCut321->SetLineColor(kBlack);
   h1WestPtotCut321->SetFillStyle(1001);
   h1WestPtotCut321->SetMaximum(30);
   h1WestPtotCut321->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut321->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtotCut321","NFILE1==0&&abs(PHI2+0.055-PHI1)<0.4&&abs(PHI3-PHI1)<0.5&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");

   TText *t4 = new TText(3., 15., "Momentum for Particles in West Arm after Combined cut");
   t4.SetTextColor(kRed);
   t4.SetTextSize(0.055);
   t4->Draw();

   TH1F *h1WestPtotCut321Sign = new TH1F("h1WestPtotCut321Sign", "West Momentum, Combined Cuts with Sign", 50, 0., 10.);
   h1WestPtotCut321Sign->SetLineColor(kGreen);
   h1WestPtotCut321Sign->SetFillColor(kGreen);
   h1WestPtotCut321Sign->SetFillStyle(1001);
   h1WestPtotCut321Sign->SetMaximum(30);
   h1WestPtotCut321Sign->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut321Sign->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCut321Sign","PTOT","NFILE1==0&&abs(PHI2+0.055-PHI3)<0.7*abs(PHI3+PHI2+0.055-2.0*PHI1)&&abs(PHI2+0.055-PHI1)<0.4&&abs(PHI3-PHI1)<0.5&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");

   h1WestPtotCut321Sign->Draw("same");
   cout << "\ Sign cut sum = " <<  h1WestPtotCut321Sign->GetSum() << endl;

   TText *t4a = new TText(3., 10., "After charge sign consistency cut there are only 11 events");
   t4a.SetTextColor(kGreen);
   t4a.SetTextSize(0.055);
   t4a->Draw();

   TText *t4b = new TText(3., 7., "Rejection factor = 867/11 = 79");
   t4b.SetTextColor(kGreen);
   t4b.SetTextSize(0.055);
   t4b->Draw();

 } // iDraw = 15,  HIJING West Arm momentum projection

 if(iDraw==16) {    //  Pi- seed in HIJING
   c1->Divide(1,4);
   c1->cd(1);

   TH1F *h1WestPtot = new TH1F("h1WestPtot", "West Momentum, THETA cuts", 50, 0., 10.);
   h1WestPtot->SetLineColor(kBlack);
   h1WestPtot->SetFillStyle(1001);
   h1WestPtot->SetMaximum(30);
   h1WestPtot->SetXTitle("Momentum (GeV/c)");
   h1WestPtot->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtot","NFILE1==1&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<10.1&&abs(THETA2+0.074-THETA1)<10.1");

   TText *t0 = new TText(1.5, 25., "Pi- particles seeded into HIJING events (no cuts)");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   TH1F *h1WestPtot5 = new TH1F("h1WestPtot5", "West Arm Pi-", 50, 0., 10.);
   h1WestPtot5->SetLineColor(kBlack);
   h1WestPtot5->SetFillColor(kBlue);
   h1WestPtot5->SetFillStyle(1001);
   h1WestPtot5->SetMaximum(30);
   h1WestPtot5->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5","PTOT","NFILE1==1&&PTOT>5.0&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<10.1&&abs(THETA2+0.074-THETA1)<10.1");

   cout << "\n 5 GeV/c cut yield " <<  h1WestPtot5->GetSum() << endl;

   h1WestPtot5->Draw("same");

   TText *t0b = new TText(1.5, 22., "383 particles above 5 GeV/c");
   t0b.SetTextColor(kBlue);
   t0b.SetTextSize(0.055);
   t0b->Draw();

   c1->cd(2);
   TH1F *h1WestPtotCut31 = new TH1F("h1WestPtotCut31", "West Momentum, PC3-PC1 Cut", 50, 0., 10.);
   h1WestPtotCut31->SetLineColor(kBlack);
   h1WestPtotCut31->SetFillStyle(1001);
   h1WestPtotCut31->SetMaximum(30);
   h1WestPtotCut31->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut31->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtotCut31","NFILE1==1&&abs(PHI3-PHI1)<0.5&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.15&&abs(THETA2+0.074-THETA1)<0.15");
   TText *t0 = new TText(1.5, 20., "Momentum for Particles in West Arm after PC3-PC1 cut");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   c1->cd(3);
   TH1F *h1WestPtotCut21 = new TH1F("h1WestPtotCut21", "West Momentum, PC2-PC1 Cut", 50, 0., 10.);
   h1WestPtotCut21->SetLineColor(kBlack);
   h1WestPtotCut21->SetFillStyle(1001);
   h1WestPtotCut21->SetMaximum(30);
   h1WestPtotCut21->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut21->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtotCut21","NFILE1==1&&abs(PHI2+0.055-PHI1)<0.4&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");
   TText *t0 = new TText(1.5, 20., "Momentum for Particles in West Arm after PC2-PC1 cut");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.055);
   t0->Draw();

   c1->cd(4);
   TH1F *h1WestPtotCut321 = new TH1F("h1WestPtotCut321", "West Momentum, Combined Cuts", 50, 0., 10.);
   h1WestPtotCut321->SetLineColor(kBlack);
   h1WestPtotCut321->SetFillStyle(1001);
   h1WestPtotCut321->SetMaximum(30);
   h1WestPtotCut321->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut321->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Draw("PTOT>>h1WestPtotCut321","NFILE1==1&&abs(PHI2+0.055-PHI1)<0.4&&abs(PHI3-PHI1)<0.5&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");

   TText *t4 = new TText(1.5, 20., "Momentum for Particles in West Arm after Combined cut");
   t4.SetTextColor(kRed);
   t4.SetTextSize(0.055);
   t4->Draw();

   TH1F *h1WestPtotCut321Sign = new TH1F("h1WestPtotCut321Sign", "West Momentum, Combined Cuts with Sign", 50, 0., 10.);
   h1WestPtotCut321Sign->SetLineColor(kGreen);
   h1WestPtotCut321Sign->SetFillColor(kGreen);
   h1WestPtotCut321Sign->SetFillStyle(1001);
   h1WestPtotCut321Sign->SetMaximum(30);
   h1WestPtotCut321Sign->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCut321Sign->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCut321Sign","PTOT","NFILE1==1&&PTOT>5.&&abs(PHI2+0.055-PHI3)<0.7*abs(PHI3+PHI2+0.055-2.0*PHI1)&&abs(PHI2+0.055-PHI1)<0.4&&abs(PHI3-PHI1)<0.5&&THETA2>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.1&&abs(THETA2+0.074-THETA1)<0.1");

   h1WestPtotCut321Sign->Draw("same");
   cout << "\ Sign cut sum = " <<  h1WestPtotCut321Sign->GetSum() << endl;

   TText *t4a = new TText(1.5, 15., "After charge sign consistency cut there are 215 particles above 5 GeV/c");
   t4a.SetTextColor(kGreen);
   t4a.SetTextSize(0.055);
   t4a->Draw();

 } // iDraw = 16,  Pi- West Arm momentum projection


 if(iDraw==19) {  // Delta Theta Pi- and Pi+ seeded in HIJING

   c1->Divide(2,2);
   c1->cd(1);

   TH1F *h1PC1PC3DthNeg = new TH1F("h1PC1PC3DthNeg", "Pi- West PC3-PC1 Theta", 100, -2., 2.);
   h1PC1PC3DthNeg->SetLineColor(kGreen);
   h1PC1PC3DthNeg->SetFillStyle(1001);
   h1PC1PC3DthNeg->SetMaximum(200.0);
   h1PC1PC3DthNeg->SetXTitle("THETA3 - THETA1 (degrees)");
   h1PC1PC3DthNeg->SetYTitle("Counts per 0.04 deg bin");

   PCLevel2->Project("h1PC1PC3DthNeg","THETA3-THETA1","NFILE1==1&&THETA3>0");

   TF1 *func1gausDth13Neg = new TF1("fitDth13Neg", fitf, -0.3, +0.3, 3);
   func1gausDth13Neg->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausDth13Neg->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausDth13Neg->SetParameters(30, 0.010, 0.0);  // initial guesses for fit parameters

   h1PC1PC3DthNeg->Fit("fitDth13Neg","r");

   TH1F *h1PC1PC3DthNegCut = new TH1F("h1PC1PC3DthNegCut", "Pi- West PC3-PC1 Theta", 100, -2., 2.);
   h1PC1PC3DthNegCut->SetLineColor(kGreen);
   h1PC1PC3DthNegCut->SetFillStyle(1001);
   h1PC1PC3DthNegCut->SetMaximum(200.0);
   h1PC1PC3DthNegCut->SetXTitle("THETA3 - THETA1 (degrees)");
   h1PC1PC3DthNegCut->SetYTitle("Counts per 0.04 deg bin");
   PCLevel2->Project("h1PC1PC3DthNegCut","THETA3-THETA1","NFILE1==1&&abs(THETA3-THETA1)<0.15&&THETA3>0");
   cout << "\n PC1PC3 Neg in cut = " << h1PC1PC3DthNegCut->GetSum() << endl;

   c1->cd(2);

   TH1F *h1PC1PC2DthNeg = new TH1F("h1PC1PC2DthNeg", "Pi- West PC2-PC1 Theta", 100, -2., 2.);
   h1PC1PC2DthNeg->SetLineColor(kGreen);
   h1PC1PC2DthNeg->SetFillStyle(1001);
   h1PC1PC2DthNeg->SetMaximum(200.0);
   h1PC1PC2DthNeg->SetXTitle("THETA2 - THETA1 (degrees)");
   h1PC1PC2DthNeg->SetYTitle("Counts per 0.04 deg bin");

   PCLevel2->Project("h1PC1PC2DthNeg","THETA2-THETA1","NFILE1==1&&THETA2>0");

   TF1 *func1gausDth12Neg = new TF1("fitDth12Neg", fitf, -0.3, +0.3, 3);
   func1gausDth12Neg->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausDth12Neg->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausDth12Neg->SetParameters(30, 0.010, 0.0);  // initial guesses for fit parameters

   h1PC1PC2DthNeg->Fit("fitDth12Neg","r");

   TH1F *h1PC1PC2DthNegCut = new TH1F("h1PC1PC2DthNegCut", "Pi- West PC2-PC1 Theta", 100, -2., 2.);
   h1PC1PC2DthNegCut->SetLineColor(kGreen);
   h1PC1PC2DthNegCut->SetFillStyle(1001);
   h1PC1PC2DthNegCut->SetMaximum(200.0);
   h1PC1PC2DthNegCut->SetXTitle("THETA2 - THETA1 (degrees)");
   h1PC1PC2DthNegCut->SetYTitle("Counts per 0.04 deg bin");
   PCLevel2->Project("h1PC1PC2DthNegCut","THETA2-THETA1","NFILE1==1&&abs(THETA2+0.072-THETA1)<0.15&&THETA2>0");
   cout << "\n PC1PC2 Neg in cut = " << h1PC1PC2DthNegCut->GetSum() << endl;

   c1->cd(3);

   TH1F *h1PC1PC3DthPos = new TH1F("h1PC1PC3DthPos", "Pi+ West PC3-PC1 Theta", 100, -2., 2.);
   h1PC1PC3DthPos->SetLineColor(kGreen);
   h1PC1PC3DthPos->SetFillStyle(1001);
   h1PC1PC3DthPos->SetMaximum(200.0);
   h1PC1PC3DthPos->SetXTitle("THETA3 - THETA1 (degrees)");
   h1PC1PC3DthPos->SetYTitle("Counts per 0.04 deg bin");

   PCLevel2->Project("h1PC1PC3DthPos","THETA3-THETA1","NFILE1==2&&THETA2>0");

   TF1 *func1gausDth13Pos = new TF1("fitDth13Pos", fitf, -0.3, +0.3, 3);
   func1gausDth13Pos->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausDth13Pos->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausDth13Pos->SetParameters(30, 0.010, 0.0);  // initial guesses for fit parameters

   h1PC1PC3DthPos->Fit("fitDth13Pos","r");

   TH1F *h1PC1PC3DthPosCut = new TH1F("h1PC1PC3DthPosCut", "Pi+ West PC3-PC1 Theta", 100, -2., 2.);
   h1PC1PC3DthPosCut->SetLineColor(kGreen);
   h1PC1PC3DthPosCut->SetFillStyle(1001);
   h1PC1PC3DthPosCut->SetMaximum(200.0);
   h1PC1PC3DthPosCut->SetXTitle("THETA3 - THETA1 (degrees)");
   h1PC1PC3DthPosCut->SetYTitle("Counts per 0.04 deg bin");
   PCLevel2->Project("h1PC1PC3DthPosCut","THETA3-THETA1","NFILE1==2&&abs(THETA3-THETA1)<0.15&&THETA3>0");
   cout << "\n PC1PC3 Pos in cut = " << h1PC1PC3DthPosCut->GetSum() << endl;

   c1->cd(4);

   TH1F *h1PC1PC2DthPos = new TH1F("h1PC1PC2DthPos", "Pi+ West PC2-PC1 Theta", 100, -2., 2.);
   h1PC1PC2DthPos->SetLineColor(kGreen);
   h1PC1PC2DthPos->SetFillStyle(1001);
   h1PC1PC2DthPos->SetMaximum(200.0);
   h1PC1PC2DthPos->SetXTitle("THETA2 - THETA1 (degrees)");
   h1PC1PC2DthPos->SetYTitle("Counts per 0.04 deg bin");

   PCLevel2->Project("h1PC1PC2DthPos","THETA2-THETA1","NFILE1==2&&THETA2>0");

   TF1 *func1gausDth12Pos = new TF1("fitDth12Pos", fitf, -0.3, +0.3, 3);
   func1gausDth12Pos->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausDth12Pos->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausDth12Pos->SetParameters(30, 0.010, 0.0);  // initial guesses for fit parameters

   h1PC1PC2DthPos->Fit("fitDth12Pos","r");

   TH1F *h1PC1PC2DthPosCut = new TH1F("h1PC1PC2DthPosCut", "Pi+ West PC2-PC1 Theta", 100, -2., 2.);
   h1PC1PC2DthPosCut->SetLineColor(kGreen);
   h1PC1PC2DthPosCut->SetFillStyle(1001);
   h1PC1PC2DthPosCut->SetMaximum(200.0);
   h1PC1PC2DthPosCut->SetXTitle("THETA2 - THETA1 (degrees)");
   h1PC1PC2DthPosCut->SetYTitle("Counts per 0.04 deg bin");
   PCLevel2->Project("h1PC1PC2DthPosCut","THETA2-THETA1","NFILE1==2&&abs(THETA2+0.072-THETA1)<0.15&&THETA2>0");
   cout << "\n PC1PC2 Pos in cut = " << h1PC1PC2DthPosCut->GetSum() << endl;

 } // iDraw = 19,  Delta Theta Pions seeded in HIJING

 if(iDraw==20) {  // 2-D Delta Theta Pi- and Pi+ as HIJING seed

   c1->Divide(1,2);
   c1->cd(1);

   TH2F *h2PC1PC2PC3DthNeg = new TH2F("h2PC1PC2PC3DthNeg", "Pi- Theta Correl", 100, -1., 1.,
				      100, -1., 1.);
   h2PC1PC2PC3DthNeg->SetMarkerColor(kGreen);
   h2PC1PC2PC3DthNeg->SetXTitle("Delta Theta PC2-PC1 (deg)");
   h2PC1PC2PC3DthNeg->SetYTitle("Delta Theta PC3-PC1 (deg)");
   h2PC1PC2PC3DthNeg->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2PC3DthNeg->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2PC3DthNeg->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2PC3DthNeg->GetYaxis()->SetTitleSize(0.045);

   PCLevel2->Project("h2PC1PC2PC3DthNeg","THETA3-THETA1:THETA2-THETA1","NFILE1==1&&THETA2>0&&THETA3>0");
   h2PC1PC2PC3DthNeg->Draw();

   c1->cd(2);

   TH2F *h2PC1PC2PC3DthPos = new TH2F("h2PC1PC2PC3DthPos", "Pi- Theta Correl", 100, -1., 1.,
				      100, -1., 1.);
   h2PC1PC2PC3DthPos->SetMarkerColor(kGreen);
   h2PC1PC2PC3DthPos->SetXTitle("Delta Theta PC2-PC1 (deg)");
   h2PC1PC2PC3DthPos->SetYTitle("Delta Theta PC3-PC1 (deg)");
   h2PC1PC2PC3DthPos->GetXaxis()->SetLabelSize(0.04);
   h2PC1PC2PC3DthPos->GetYaxis()->SetLabelSize(0.04);
   h2PC1PC2PC3DthPos->GetXaxis()->SetTitleSize(0.045);
   h2PC1PC2PC3DthPos->GetYaxis()->SetTitleSize(0.045);

   PCLevel2->Project("h2PC1PC2PC3DthPos","THETA3-THETA1:THETA2-THETA1","NFILE1==2&&THETA2>0&&THETA3>0");
   h2PC1PC2PC3DthPos->Draw();
 
 } // iDraw = 20,  2-D Delta Theta Pions as HIJING seed


 if(iDraw==31) {    //  HIJING rejection power
   c1->Divide(1,3);
   c1->cd(1);

   TH1F *h1WestPtot = new TH1F("h1WestPtot", "West Momentum, No cuts", 50, 0., 10.);
   h1WestPtot->SetLineColor(kBlue);
   h1WestPtot->SetLineWidth(2);
   h1WestPtot->SetFillStyle(1001);
   h1WestPtot->SetMaximum(15000);
   h1WestPtot->SetXTitle("Momentum (GeV/c)");
   h1WestPtot->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot","PTOT","NFILE1==0&&THETA2>0.0&&THETA3>0.0");
   h1WestPtot->Draw();

   TText *t0 = new TText(1.5, 10000., "867 HIJING min bias events (no cuts)");
   t0.SetTextColor(kBlue);
   t0.SetTextSize(0.060);
   t0->Draw();

   c1->cd(2);

   TH1F *h1WestPtotCutThSum = new TH1F("h1WestPtotCutThSum", "West Arm, Theta Cuts", 50, 0., 10.);
   h1WestPtotCutThSum->SetLineColor(kBlue);
   h1WestPtotCutThSum->SetLineWidth(2);
   h1WestPtotCutThSum->SetFillStyle(1001);
   h1WestPtotCutThSum->SetMaximum(500);
   h1WestPtotCutThSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSum","PTOT","NFILE1==0&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2+0.072-THETA1)<0.120&&abs(THETA3-THETA2-0.072)<0.120");
   h1WestPtotCutThSum->Draw();

   TText *t1 = new TText(1.5, 350., "867 HIJING min bias events (Theta cuts)");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.060);
   t1->Draw();

   c1->cd(3);

   TH1F *h1WestPtotCutThSumCutPhSum = new TH1F("h1WestPtotCutThSumCutPhSum", "West Arm, All cuts", 50, 0., 10.);
   h1WestPtotCutThSumCutPhSum->SetLineColor(kBlue);
   h1WestPtotCutThSumCutPhSum->SetLineWidth(2);
   h1WestPtotCutThSumCutPhSum->SetFillStyle(1001);
   h1WestPtotCutThSumCutPhSum->SetMaximum(30);
   h1WestPtotCutThSumCutPhSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSumCutPhSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSumCutPhSum","PTOT","NFILE1==0&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2+0.072-THETA1)<0.120&&abs(THETA3-THETA2-0.072)<0.120&&abs(PHI3-PHI1)<0.45&&abs(PHI2+0.05-PHI1)<0.40&&abs(PHI2+0.055-PHI1)<0.43&&abs(PHI3-PHI1)");
   h1WestPtotCutThSumCutPhSum->Draw();

   TText *t2 = new TText(1.5, 20., "867 min bias HIJING events (Theta and Phi cuts)");
   t2.SetTextColor(kBlue);
   t2.SetTextSize(0.060);
   t2->Draw();

   TText *t2a = new TText(1.5, 15., "Rejection Power = 867/21 = 41");
   t2a.SetTextColor(kBlue);
   t2a.SetTextSize(0.060);
   t2a->Draw();

 
 } // iDraw = 31,  HIJING rejection power

 if(iDraw==32) {    //  HIJING rejection power
   c1->Divide(1,3);
   c1->cd(1);

   TH1F *h1WestPtot = new TH1F("h1WestPtot", "West Momentum, No cuts", 50, 0., 10.);
   h1WestPtot->SetLineColor(kBlue);
   h1WestPtot->SetLineWidth(2);
   h1WestPtot->SetFillStyle(1001);
   h1WestPtot->SetMaximum(15000);
   h1WestPtot->SetXTitle("Momentum (GeV/c)");
   h1WestPtot->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot","PTOT","NFILE1==0&&THETA2>0.0&&THETA3>0.0");
   h1WestPtot->Draw();

   TText *t0 = new TText(1.5, 10000., "867 HIJING min bias events (no cuts)");
   t0.SetTextColor(kBlue);
   t0.SetTextSize(0.060);
   t0->Draw();

   c1->cd(2);

   TH1F *h1WestPtotCutThSum = new TH1F("h1WestPtotCutThSum", "West Arm, Theta Cuts", 50, 0., 10.);
   h1WestPtotCutThSum->SetLineColor(kBlue);
   h1WestPtotCutThSum->SetLineWidth(2);
   h1WestPtotCutThSum->SetFillStyle(1001);
   h1WestPtotCutThSum->SetMaximum(500);
   h1WestPtotCutThSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSum","PTOT","NFILE1==0&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2-THETA1)<0.120&&abs(THETA3-THETA2)<0.120");
   h1WestPtotCutThSum->Draw();

   TText *t1 = new TText(1.5, 350., "867 HIJING min bias events (Theta cuts)");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.060);
   t1->Draw();

   c1->cd(3);

   TH1F *h1WestPtotCutThSumCutPhSum = new TH1F("h1WestPtotCutThSumCutPhSum", "West Arm, All cuts", 50, 0., 10.);
   h1WestPtotCutThSumCutPhSum->SetLineColor(kBlue);
   h1WestPtotCutThSumCutPhSum->SetLineWidth(2);
   h1WestPtotCutThSumCutPhSum->SetFillStyle(1001);
   h1WestPtotCutThSumCutPhSum->SetMaximum(30);
   h1WestPtotCutThSumCutPhSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSumCutPhSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSumCutPhSum","PTOT","NFILE1==0&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2-THETA1)<0.120&&abs(THETA3-THETA2)<0.120&&abs(PHI3-PHI1)<0.45&&abs(PHI2-PHI1)<0.40&&abs(PHI2-PHI1)<0.43&&abs(PHI3-PHI1)");
   h1WestPtotCutThSumCutPhSum->Draw();

   TText *t2 = new TText(1.5, 20., "867 min bias HIJING events (Theta and Phi cuts)");
   t2.SetTextColor(kBlue);
   t2.SetTextSize(0.060);
   t2->Draw();

   TText *t2a = new TText(1.5, 15., "Rejection Power = 867/21 = 41");
   t2a.SetTextColor(kBlue);
   t2a.SetTextSize(0.060);
   t2a->Draw();

 
 } // iDraw = 32,  HIJING rejection power without angle corrections

 if(iDraw==41) {    //  Pi- in HIJING
   c1->Divide(1,3);
   c1->cd(1);

   TH1F *h1WestPtot = new TH1F("h1WestPtot", "West Momentum Pi-, No cuts", 50, 0., 10.);
   h1WestPtot->SetLineColor(kBlue);
   h1WestPtot->SetLineWidth(2);
   h1WestPtot->SetFillStyle(1001);
   h1WestPtot->SetMaximum(30);
   h1WestPtot->SetXTitle("Momentum (GeV/c)");
   h1WestPtot->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot","PTOT","GPC123==111&&NFILE1==1&&THETA2>0.0&&THETA3>0.0");
   h1WestPtot->Draw();

   TText *t0 = new TText(1.5, 25., "Pi- particles embedded in HIJING events (no cuts)");
   t0.SetTextColor(kBlue);
   t0.SetTextSize(0.060);
   t0->Draw();

   TH1F *h1WestPtot5 = new TH1F("h1WestPtot5", "West Arm, 5 GeV cut", 50, 0., 10.);
   h1WestPtot5->SetLineColor(kGreen);
   h1WestPtot5->SetFillColor(kGreen);
   h1WestPtot5->SetFillStyle(1001);
   h1WestPtot5->SetMaximum(30);
   h1WestPtot5->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5","PTOT","PTOT>5.0&&GPC123==111&&NFILE1==1&&THETA2>0.0&&THETA3>0.0");
   h1WestPtot5->Draw("same");

   TText *t0a = new TText(1.5, 4., "Yield above 5 GeV/c = 337");
   t0a.SetTextColor(kGreen);
   t0a.SetTextSize(0.060);
   t0a->Draw();

   c1->cd(2);

   TH1F *h1WestPtotCutThSum = new TH1F("h1WestPtotCutThSum", "West Arm Pi-, Theta Cuts", 50, 0., 10.);
   h1WestPtotCutThSum->SetLineColor(kBlue);
   h1WestPtotCutThSum->SetLineWidth(2);
   h1WestPtotCutThSum->SetFillStyle(1001);
   h1WestPtotCutThSum->SetMaximum(30);
   h1WestPtotCutThSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSum","PTOT","GPC123==111&&NFILE1==1&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.12&&abs(THETA2+0.072-THETA1)<0.12&&abs(THETA3-THETA2-0.072)<0.12");
   h1WestPtotCutThSum->Draw();

   TText *t1 = new TText(1.5, 25., "Pi- particles embedded in HIJING events (Theta cuts)");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.060);
   t1->Draw();

   TH1F *h1WestPtot5CutThSum = new TH1F("h1WestPtot5CutThSum", "West Arm Pi-, 5 GeV/c", 50, 0., 10.);
   h1WestPtot5CutThSum->SetLineColor(kGreen);
   h1WestPtot5CutThSum->SetFillColor(kGreen);
   h1WestPtot5CutThSum->SetFillStyle(1001);
   h1WestPtot5CutThSum->SetMaximum(30);
   h1WestPtot5CutThSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5CutThSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5CutThSum","PTOT","GPC123==111&&NFILE1==1&&PTOT>5.0&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2+0.072-THETA1)<0.120&&abs(THETA3-THETA2-0.072)<0.120");
   h1WestPtot5CutThSum->Draw("same");

   TText *t1a = new TText(1.5, 4., "Yield above 5 GeV/c = 331");
   t1a.SetTextColor(kGreen);
   t1a.SetTextSize(0.060);
   //t1a->Draw();

   c1->cd(3);

   TH1F *h1WestPtotCutThSumCutPhSum = new TH1F("h1WestPtotCutThSumCutPhSum", "West Arm Pi-, All cuts", 50, 0., 10.);
   h1WestPtotCutThSumCutPhSum->SetLineColor(kBlue);
   h1WestPtotCutThSumCutPhSum->SetLineWidth(2);
   h1WestPtotCutThSumCutPhSum->SetFillStyle(1001);
   h1WestPtotCutThSumCutPhSum->SetMaximum(30);
   h1WestPtotCutThSumCutPhSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSumCutPhSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSumCutPhSum","PTOT","GPC123==111&&NFILE1==1&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2+0.072-THETA1)<0.120&&abs(THETA3-THETA2-0.072)<0.120&&abs(PHI3-PHI1)<0.45&&abs(PHI2+0.05-PHI1)<0.40&&abs(PHI2+0.055-PHI1)<0.43&&abs(PHI3-PHI1)");
   h1WestPtotCutThSumCutPhSum->Draw();

   TH1F *h1WestPtot5CutThSumCutPhSum = new TH1F("h1WestPtot5CutThSumCutPhSum", "West Arm Pi-", 50, 0., 10.);
   h1WestPtot5CutThSumCutPhSum->SetLineColor(kGreen);
   h1WestPtot5CutThSumCutPhSum->SetFillColor(kGreen);
   h1WestPtot5CutThSumCutPhSum->SetFillStyle(1001);
   h1WestPtot5CutThSumCutPhSum->SetMaximum(30);
   h1WestPtot5CutThSumCutPhSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5CutThSumCutPhSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5CutThSumCutPhSum","PTOT","GPC123==111&&NFILE1==1&&PTOT>5.0&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2+0.072-THETA1)<0.120&&abs(THETA3-THETA2-0.072)<0.120&&abs(PHI3-PHI1)<0.45&&abs(PHI2+0.05-PHI1)<0.40&&abs(PHI2+0.055-PHI1)<0.43&&abs(PHI3-PHI1)");
   h1WestPtot5CutThSumCutPhSum->Draw("same");

   cout << "\n Phi cut sum above 5 GeV/c = " <<  h1WestPtot5CutThSumCutPhSum->GetSum() << endl;

   TText *t2 = new TText(1.5, 25., "Pi- particles embedded in HIJING events (Theta and Phi cuts)");
   t2.SetTextColor(kBlue);
   t2.SetTextSize(0.060);
   t2->Draw();

   TText *t2a = new TText(1.5, 10., "Yield above 5 GeV/c = 232");
   t2a.SetTextColor(kGreen);
   t2a.SetTextSize(0.060);
   t2a->Draw();

   TText *t2b = new TText(1.0, 6., "Embedded efficiency above 5 GeV/c = 0.66");
   t2b.SetTextColor(kGreen);
   t2b.SetTextSize(0.060);
   t2b->Draw();
 
 } // iDraw = 41,  Pi- West Arm momentum projection for HIJING efficiency

 if(iDraw==42) {    //  Pi- in HIJING
   c1->Divide(1,3);
   c1->cd(1);

   TH1F *h1WestPtot = new TH1F("h1WestPtot", "West Momentum Pi-, No cuts", 50, 0., 10.);
   h1WestPtot->SetLineColor(kBlue);
   h1WestPtot->SetLineWidth(2);
   h1WestPtot->SetFillStyle(1001);
   h1WestPtot->SetMaximum(30);
   h1WestPtot->SetXTitle("Momentum (GeV/c)");
   h1WestPtot->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot","PTOT","GPC123==111&&NFILE1==1&&THETA2>0.0&&THETA3>0.0");
   h1WestPtot->Draw();

   TText *t0 = new TText(1.5, 25., "Pi- particles embedded in HIJING events (no cuts)");
   t0.SetTextColor(kBlue);
   t0.SetTextSize(0.060);
   t0->Draw();

   TH1F *h1WestPtot5 = new TH1F("h1WestPtot5", "West Arm, 5 GeV cut", 50, 0., 10.);
   h1WestPtot5->SetLineColor(kGreen);
   h1WestPtot5->SetFillColor(kGreen);
   h1WestPtot5->SetFillStyle(1001);
   h1WestPtot5->SetMaximum(30);
   h1WestPtot5->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5","PTOT","PTOT>5.0&&GPC123==111&&NFILE1==1&&THETA2>0.0&&THETA3>0.0");
   h1WestPtot5->Draw("same");

   TText *t0a = new TText(1.5, 4., "Yield above 5 GeV/c = 337");
   t0a.SetTextColor(kGreen);
   t0a.SetTextSize(0.060);
   t0a->Draw();

   c1->cd(2);

   TH1F *h1WestPtotCutThSum = new TH1F("h1WestPtotCutThSum", "West Arm Pi-, Theta Cuts", 50, 0., 10.);
   h1WestPtotCutThSum->SetLineColor(kBlue);
   h1WestPtotCutThSum->SetLineWidth(2);
   h1WestPtotCutThSum->SetFillStyle(1001);
   h1WestPtotCutThSum->SetMaximum(30);
   h1WestPtotCutThSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSum","PTOT","GPC123==111&&NFILE1==1&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.12&&abs(THETA2-THETA1)<0.12&&abs(THETA3-THETA2)<0.12");
   h1WestPtotCutThSum->Draw();

   TText *t1 = new TText(1.5, 25., "Pi- particles embedded in HIJING events (Theta cuts)");
   t1.SetTextColor(kBlue);
   t1.SetTextSize(0.060);
   t1->Draw();

   TH1F *h1WestPtot5CutThSum = new TH1F("h1WestPtot5CutThSum", "West Arm Pi-, 5 GeV/c", 50, 0., 10.);
   h1WestPtot5CutThSum->SetLineColor(kGreen);
   h1WestPtot5CutThSum->SetFillColor(kGreen);
   h1WestPtot5CutThSum->SetFillStyle(1001);
   h1WestPtot5CutThSum->SetMaximum(30);
   h1WestPtot5CutThSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5CutThSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5CutThSum","PTOT","GPC123==111&&NFILE1==1&&PTOT>5.0&&THETA2>0.0&&THETA3>0.0&&PHI1<90.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2-THETA1)<0.120&&abs(THETA3-THETA2)<0.120");
   h1WestPtot5CutThSum->Draw("same");

   TText *t1a = new TText(1.5, 4., "Yield above 5 GeV/c = 331");
   t1a.SetTextColor(kGreen);
   t1a.SetTextSize(0.060);
   //t1a->Draw();

   c1->cd(3);

   TH1F *h1WestPtotCutThSumCutPhSum = new TH1F("h1WestPtotCutThSumCutPhSum", "West Arm Pi-, All cuts", 50, 0., 10.);
   h1WestPtotCutThSumCutPhSum->SetLineColor(kBlue);
   h1WestPtotCutThSumCutPhSum->SetLineWidth(2);
   h1WestPtotCutThSumCutPhSum->SetFillStyle(1001);
   h1WestPtotCutThSumCutPhSum->SetMaximum(30);
   h1WestPtotCutThSumCutPhSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtotCutThSumCutPhSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtotCutThSumCutPhSum","PTOT","GPC123==111&&NFILE1==1&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2-THETA1)<0.120&&abs(THETA3-THETA2)<0.120&&abs(PHI3-PHI1)<0.45&&abs(PHI2-PHI1)<0.40&&abs(PHI2-PHI1)<0.43&&abs(PHI3-PHI1)");
   h1WestPtotCutThSumCutPhSum->Draw();

   TH1F *h1WestPtot5CutThSumCutPhSum = new TH1F("h1WestPtot5CutThSumCutPhSum", "West Arm Pi-", 50, 0., 10.);
   h1WestPtot5CutThSumCutPhSum->SetLineColor(kGreen);
   h1WestPtot5CutThSumCutPhSum->SetFillColor(kGreen);
   h1WestPtot5CutThSumCutPhSum->SetFillStyle(1001);
   h1WestPtot5CutThSumCutPhSum->SetMaximum(30);
   h1WestPtot5CutThSumCutPhSum->SetXTitle("Momentum (GeV/c)");
   h1WestPtot5CutThSumCutPhSum->SetYTitle("Counts per 0.2 GeV/c bin");

   PCLevel2->Project("h1WestPtot5CutThSumCutPhSum","PTOT","GPC123==111&&NFILE1==1&&PTOT>5.0&&THETA2>0.0&&THETA3>0.0&&abs(THETA3-THETA1)<0.120&&abs(THETA2-THETA1)<0.120&&abs(THETA3-THETA2)<0.120&&abs(PHI3-PHI1)<0.45&&abs(PHI2-PHI1)<0.40&&abs(PHI2-PHI1)<0.43&&abs(PHI3-PHI1)");
   h1WestPtot5CutThSumCutPhSum->Draw("same");

   cout << "\n Phi cut sum above 5 GeV/c = " <<  h1WestPtot5CutThSumCutPhSum->GetSum() << endl;

   TText *t2 = new TText(1.5, 25., "Pi- particles embedded in HIJING events (Theta and Phi cuts)");
   t2.SetTextColor(kBlue);
   t2.SetTextSize(0.060);
   t2->Draw();

   TText *t2a = new TText(1.5, 10., "Yield above 5 GeV/c = 294");
   t2a.SetTextColor(kGreen);
   t2a.SetTextSize(0.060);
   t2a->Draw();

   TText *t2b = new TText(1.0, 6., "Embedded efficiency above 5 GeV/c = 0.66");
   t2b.SetTextColor(kGreen);
   t2b.SetTextSize(0.060);
   t2b->Draw();
 
 } // iDraw = 42,  Pi- West Arm momentum projection for HIJING efficiency (without angle corrections)

 if(iDraw==50) {

   c1->cd(1);

   Float_t rScale = 4.0;
   TH1F *h1PC2PC3Dph = new TH1F("h1PC2PC3Dph", "PC3 - PC2 Delta Phi", 100, -10., 10.);
   h1PC2PC3Dph->SetLineColor(kBlue);
   h1PC2PC3Dph->SetFillStyle(1001);
   //h1PC2PC3Dph->SetMaximum(20.0*rScale);
   h1PC2PC3Dph->SetXTitle("Difference in Azimuth PC3 - PC2 (deg)");
   h1PC2PC3Dph->SetYTitle("Counts per 0.2 deg");

   PCCluster->Draw("PHI3-PHI2>>h1PC2PC3Dph","EVENT==5&&PHI1<90.0");

   TText *t0 = new TText(-9, 25, "July 19, PC2 WORKS !");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.053);
   t0->Draw();

   TText *t1 = new TText(-9, 20, "PRDF 22826, EVENT 13");
   t1.SetTextColor(kRed);
   t1.SetTextSize(0.050);
   t1->Draw();

 }  // iDraw = 50, PC3 - PC2 angular correlation in azimuth

 if(iDraw==60) {

   c1->Divide(1,3);
   c1->cd(1);

   TH2F *h2DchPC1East = new TH2F("h2DchPC1East", "East Dch PC1 Correlation",
				125, 0., 400., 125, 0., 400.);
   h2DchPC1East->SetMarkerColor(kBlue);
   h2DchPC1East->SetXTitle("PC1 East Cluster Counter");
   h2DchPC1East->SetYTitle("Dch East Track Count");
   h2DchPC1East->GetXaxis()->SetLabelSize(0.04);
   h2DchPC1East->GetYaxis()->SetLabelSize(0.04);
   h2DchPC1East->GetXaxis()->SetTitleSize(0.045);
   h2DchPC1East->GetYaxis()->SetTitleSize(0.045);
   EvtSum->Draw("DCHEAST:PC1EAST>>h2DchPC1East","PC1EAST>0");

   TText *t0 = new TText(30., 300., "Run 23433 DCH/PC1 East Correlation");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t0a = new TText(30., 250., "With Run2 Objy Calibs for Dch and PCs");
   t0a.SetTextColor(kRed);
   t0a.SetTextSize(0.050);
   t0a->Draw();

   c1->cd(2);

   TH2F *h2DchPC1West = new TH2F("h2DchPC1West", "West Dch PC1 Correlation",
				125, 0., 400., 125, 0., 400.);
   h2DchPC1West->SetMarkerColor(kBlue);
   h2DchPC1West->SetXTitle("PC1 West Cluster Counter");
   h2DchPC1West->SetYTitle("Dch West Track Count");
   h2DchPC1West->GetXaxis()->SetLabelSize(0.04);
   h2DchPC1West->GetYaxis()->SetLabelSize(0.04);
   h2DchPC1West->GetXaxis()->SetTitleSize(0.045);
   h2DchPC1West->GetYaxis()->SetTitleSize(0.045);
   EvtSum->Draw("DCHWEST:PC1WEST>>h2DchPC1West","PC1WEST>0");

   TText *t0 = new TText(30., 300., "Run 23433 DCH/PC1 West Correlation");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   c1->cd(3);

   TH2F *h2DchPC1EW = new TH2F("h2DchPC1EW", "East+West Dch PC1 Correlation",
				200, 0., 800., 200, 0., 800.);
   h2DchPC1EW->SetMarkerColor(kBlue);
   h2DchPC1EW->SetXTitle("PC1 E+W Cluster Counter");
   h2DchPC1EW->SetYTitle("Dch E+W Track Count");
   h2DchPC1EW->GetXaxis()->SetLabelSize(0.04);
   h2DchPC1EW->GetYaxis()->SetLabelSize(0.04);
   h2DchPC1EW->GetXaxis()->SetTitleSize(0.045);
   h2DchPC1EW->GetYaxis()->SetTitleSize(0.045);
   EvtSum->Draw("DCHEAST+DCHWEST:PC1EAST+PC1WEST>>h2DchPC1EW","PC1EAST>0&&PC1WEST>0");

   TText *t0 = new TText(60., 600., "Run 23433 DCH/PC1 E+W Correlation");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

 } // iDraw = 60, Dch and PC1 comparisons, run 23433


 if(iDraw==61) {

   c1->Divide(1,3);
   c1->cd(1);

   TH2F *h2DchPC1East = new TH2F("h2DchPC1East", "East Dch PC1 Correlation",
				125, 0., 400., 125, 0., 400.);
   h2DchPC1East->SetMarkerColor(kBlue);
   h2DchPC1East->SetXTitle("PC1 East Cluster Counter");
   h2DchPC1East->SetYTitle("Dch East Track Count");
   h2DchPC1East->GetXaxis()->SetLabelSize(0.04);
   h2DchPC1East->GetYaxis()->SetLabelSize(0.04);
   h2DchPC1East->GetXaxis()->SetTitleSize(0.045);
   h2DchPC1East->GetYaxis()->SetTitleSize(0.045);
   EvtSum->Draw("DCHEAST:PC1EAST>>h2DchPC1East","PC1EAST>0");

   TText *t0 = new TText(30., 300., "Run 11305 DCH/PC1 East Correlation");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t0a = new TText(30., 250., "V05 Pass, July 21");
   t0a.SetTextColor(kRed);
   t0a.SetTextSize(0.050);
   t0a->Draw();

   c1->cd(2);

   TH2F *h2DchPC1West = new TH2F("h2DchPC1West", "West Dch PC1 Correlation",
				125, 0., 400., 125, 0., 400.);
   h2DchPC1West->SetMarkerColor(kBlue);
   h2DchPC1West->SetXTitle("PC1 West Cluster Counter");
   h2DchPC1West->SetYTitle("Dch West Track Count");
   h2DchPC1West->GetXaxis()->SetLabelSize(0.04);
   h2DchPC1West->GetYaxis()->SetLabelSize(0.04);
   h2DchPC1West->GetXaxis()->SetTitleSize(0.045);
   h2DchPC1West->GetYaxis()->SetTitleSize(0.045);
   EvtSum->Draw("DCHWEST:PC1WEST>>h2DchPC1West","PC1WEST>0");

   TText *t0 = new TText(30., 300., "Run 11305 DCH/PC1 West Correlation");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   c1->cd(3);

   TH2F *h2DchPC1EW = new TH2F("h2DchPC1EW", "East+West Dch PC1 Correlation",
				200, 0., 800., 200, 0., 800.);
   h2DchPC1EW->SetMarkerColor(kBlue);
   h2DchPC1EW->SetXTitle("PC1 E+W Cluster Counter");
   h2DchPC1EW->SetYTitle("Dch E+W Track Count");
   h2DchPC1EW->GetXaxis()->SetLabelSize(0.04);
   h2DchPC1EW->GetYaxis()->SetLabelSize(0.04);
   h2DchPC1EW->GetXaxis()->SetTitleSize(0.045);
   h2DchPC1EW->GetYaxis()->SetTitleSize(0.045);
   EvtSum->Draw("DCHEAST+DCHWEST:PC1EAST+PC1WEST>>h2DchPC1EW","PC1EAST>0&&PC1WEST>0");

   TText *t0 = new TText(60., 600., "Run 11305 DCH/PC1 E+W Correlation");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

 } // iDraw = 61, Dch and PC1 comparisons, run 11305


 if(iDraw==62) {

   c1->cd(1);

   TH1F *h1PC1PC3Z0East = new TH1F("h1PC1PC3Z0East", "Z0 from PC1/PC3 East",
				100, -50., +50.);
   h1PC1PC3Z0East->SetMarkerColor(kBlue);
   h1PC1PC3Z0East->SetMaximum(12.);
   h1PC1PC3Z0East->SetXTitle("Projected Z0 (cm)");
   h1PC1PC3Z0East->SetYTitle("Projections per 1.0 cm bin");
   h1PC1PC3Z0East->GetXaxis()->SetLabelSize(0.04);
   h1PC1PC3Z0East->GetYaxis()->SetLabelSize(0.04);
   h1PC1PC3Z0East->GetXaxis()->SetTitleSize(0.045);
   h1PC1PC3Z0East->GetYaxis()->SetTitleSize(0.045);
   PCZ0->Project("h1PC1PC3Z0East","Z0PC","PHIIN>90.0&&abs(PHIIN-PHIOUT)<0.20&&EVENT==50&&IPCIN==1");

   TF1 *func1gausPC1PC3Z0 = new TF1("fitPC1PC3Z0", fitf, 6.0, 16.0, 3);
   func1gausPC1PC3Z0->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausPC1PC3Z0->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausPC1PC3Z0->SetParameters(10, 0.80, 12.0);  // initial guesses for fit parameters

   h1PC1PC3Z0East->Fit("fitPC1PC3Z0","r");

   TText *t0 = new TText(-40., 10., "Run 23433, Event 50");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t0a = new TText(-40., 8., "Z0 determined from East Arm PC1/PC3");
   t0a.SetTextColor(kRed);
   t0a.SetTextSize(0.035);
   t0a->Draw();

 } // iDraw = 62, Z0 for Event 50, Run 23433


 if(iDraw==63) {

   c1->cd(1);

   TH1F *h1PC1PC3Z0West = new TH1F("h1PC1PC3Z0West", "Z0 from PC1/PC3 West",
				100, -50., +50.);
   h1PC1PC3Z0West->SetMarkerColor(kBlue);
   //   h1PC1PC3Z0West->SetMaximum(12.);
   h1PC1PC3Z0West->SetXTitle("Projected Z0 (cm)");
   h1PC1PC3Z0West->SetYTitle("Projections per 1.0 cm bin");
   h1PC1PC3Z0West->GetXaxis()->SetLabelSize(0.04);
   h1PC1PC3Z0West->GetYaxis()->SetLabelSize(0.04);
   h1PC1PC3Z0West->GetXaxis()->SetTitleSize(0.045);
   h1PC1PC3Z0West->GetYaxis()->SetTitleSize(0.045);
   PCZ0->Project("h1PC1PC3Z0West","Z0PC","abs(Z0PC)<50.0&&PHIIN<90.0&&abs(PHIIN-PHIOUT)<0.20&&EVENT==50&&IPCIN==1");

   TF1 *func1gausPC1PC3Z0 = new TF1("fitPC1PC3Z0", fitf, 6.0, 16.0, 3);
   func1gausPC1PC3Z0->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausPC1PC3Z0->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausPC1PC3Z0->SetParameters(10, 0.80, 12.0);  // initial guesses for fit parameters

   h1PC1PC3Z0West->Fit("fitPC1PC3Z0","r");

   TText *t0 = new TText(-40., 10., "Run 23433, Event 50");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t0a = new TText(-40., 8., "Z0 determined from West Arm PC1/PC3");
   t0a.SetTextColor(kRed);
   t0a.SetTextSize(0.035);
   t0a->Draw();
 }

 if(iDraw==64) {

   c1->cd(1);

   TH1F *h1PC2PC3Z0West = new TH1F("h1PC2PC3Z0West", "Z0 from PC2/PC3 West",
				50, -50., +50.);
   h1PC2PC3Z0West->SetMarkerColor(kBlue);
   //   h1PC2PC3Z0West->SetMaximum(12.);
   h1PC2PC3Z0West->SetXTitle("Projected Z0 (cm)");
   h1PC2PC3Z0West->SetYTitle("Projections per 2.0 cm bin");
   h1PC2PC3Z0West->GetXaxis()->SetLabelSize(0.04);
   h1PC2PC3Z0West->GetYaxis()->SetLabelSize(0.04);
   h1PC2PC3Z0West->GetXaxis()->SetTitleSize(0.045);
   h1PC2PC3Z0West->GetYaxis()->SetTitleSize(0.045);
   PCZ0->Project("h1PC2PC3Z0West","Z0PC","abs(Z0PC)<50.0&&PHIIN<90.0&&abs(PHIIN-PHIOUT)<0.20&&EVENT==50&&IPCIN==2");

   TF1 *func1gausPC2PC3Z0 = new TF1("fitPC2PC3Z0", fitf, 6.0, 16.0, 3);
   func1gausPC2PC3Z0->SetNpx(1200);  // how many points to use in drawing the fit function
   func1gausPC2PC3Z0->SetParNames("Constant","Sigma","Centroid");  // names of fit parameters
   func1gausPC2PC3Z0->SetParameters(10, 0.80, 12.0);  // initial guesses for fit parameters

   h1PC2PC3Z0West->Fit("fitPC2PC3Z0","r");

   TText *t0 = new TText(-40., 10., "Run 23433, Event 50");
   t0.SetTextColor(kRed);
   t0.SetTextSize(0.050);
   t0->Draw();

   TText *t0a = new TText(-40., 8., "Z0 determined from West Arm PC2/PC3");
   t0a.SetTextColor(kRed);
   t0a.SetTextSize(0.035);
   t0a->Draw();
 }

 return;
}



