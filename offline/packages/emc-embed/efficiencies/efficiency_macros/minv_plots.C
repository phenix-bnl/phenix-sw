{ 

//  gStyle->SetOptTitle(0);
  gStyle->SetOptStat(0);
  gROOT->ForceStyle();

TFile* f = new TFile("effic_test.root");

//f->cd("FiduDeadWarnAsym1ToF1Chi2Energy/C10");
f->cd("FiduDeadWarnAsym1ToF1Chi2Energy/C0");

TCanvas c10("minv_plots","minv_plots",900,700);
gStyle->SetOptStat(11111111);
c10.Divide(3,3,0,0);

  c10.cd(1);
  hMinv_1.SetMinimum(1e-09);
  hMinv_1->GetXaxis()->SetRange(0,100);
  hMinv_1.Draw();

  c10.cd(2);
  hMinv_3.SetMinimum(1e-09);
  hMinv_3->GetXaxis()->SetRange(0,100);
  hMinv_3.Draw();

  c10.cd(3);
  hMinv_5.SetMinimum(1e-09);
  hMinv_5->GetXaxis()->SetRange(0,100);
  hMinv_5.Draw();

  c10.cd(4);
  hMinv_7.SetMinimum(1e-09);
  hMinv_7->GetXaxis()->SetRange(0,100);
  hMinv_7.Draw();

  c10.cd(5);
  hMinv_9.SetMinimum(1e-09);
  hMinv_9->GetXaxis()->SetRange(0,100);
  hMinv_9.Draw();

  c10.cd(6);
  hMinv_11.SetMinimum(1e-09);
  hMinv_11->GetXaxis()->SetRange(0,100);
  hMinv_11.Draw();

  c10.cd(7);
  hMinv_13.SetMinimum(1e-09);
  hMinv_13->GetXaxis()->SetRange(0,100);
  hMinv_13.Draw();

  c10.cd(8);
  hMinv_15.SetMinimum(1e-09);
  hMinv_15->GetXaxis()->SetRange(0,100);
  hMinv_15.Draw();

  c10.cd(9);
  hMinv_17.SetMinimum(1e-09);
  hMinv_17->GetXaxis()->SetRange(0,100);
  hMinv_17.Draw();
}

