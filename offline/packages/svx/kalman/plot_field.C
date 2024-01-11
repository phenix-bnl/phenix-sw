void plot_field()
{
  const char* filename = "Sim3D++.root";
  gSystem->Load("libsvxkalman.so");

  TH1F* hbz1 = new TH1F("hbz1", "", 1000, 0, 300);
  hbz1->SetStats(0);
  hbz1->SetXTitle("r (cm)");
  hbz1->SetYTitle("B_{z} (gauss)");

  TH1F* hbz2 = new TH1F("hbz2", "", 1000, 0, 300);
  hbz2->SetStats(0);
  hbz2->SetXTitle("r (cm)");
  hbz2->SetYTitle("B_{z} (gauss)");
  hbz2->SetLineColor(2);

  PHField3DWrapper* field = new PHField3DWrapper(filename);

  for(int i=0; i<1000; i++) {
    double r = hbz1->GetBinCenter(i+1);
    double phi = 0.;
    double theta = TMath::Pi()/4;

    double x = r*cos(phi)*sin(theta);
    double y = r*sin(phi)*sin(theta);
    double z = r*cos(theta);

    double bz = field->GetBz(x, y, z);
    hbz1->SetBinContent(i+1, bz);

    //bz = field->GetBz(z, r, phi);
    //hbz2->SetBinContent(i+1, bz);
  }

  hbz1->Draw();
  hbz2->Draw("same");

}
