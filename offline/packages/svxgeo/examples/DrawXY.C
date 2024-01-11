
void DrawXY()
{
  SvxTGeo *geo = new SvxTGeo;
  geo->ReadParFile("parfiles/svxPISA.par");

  // Make an empty 100x100x100 cm^3 space in the experiment hall.
  geo->MakeTopVolume(200/2, 200/2, 200/2);

  // Place VTX sensors in the volume.
  geo->AddSensors();

  // Get handles for further manipulation
  TGeoManager *mgr = geo->GeoManager();
  TGeoVolume *top = mgr->GetTopVolume();

  // Done building model.
  // Close geometry to check for problems (overlapping boundaries)
  mgr->CloseGeometry();

  TCanvas *c = new TCanvas("cxy", "VTX in transverse plane", 900, 900);
  TH1F *hf = c->DrawFrame(-20, -20, 20, 20, "VTX");
  hf->SetXTitle("East                 x [cm]                 West");
  hf->GetXaxis()->CenterTitle();
  hf->SetYTitle("y [cm]");
  hf->GetYaxis()->CenterTitle();

  TLatex label;
  label.SetTextSize(0.02);
  double xyz[3] = {0};
  for (int i=0; i<geo->GetNLayers(); i++)
    for (int j=0; j<geo->GetNLadders(i); j++)
    {
      TPolyLine *s = geo->LadderOutlineXY(i,j);
      s->SetLineColor(kGray+2);
      s->SetLineWidth(2);
      s->Draw();
      geo->GetSensorXYZ(i, j, 0, xyz);
      int horz = xyz[0] > 0 ? 1 : 3;
      int vert = xyz[1] > 0 ? 1 : 3;
      label.SetTextAlign(10*horz + vert);
      label.DrawLatex(xyz[0], xyz[1], Form(" %d ", j));
    }

  cxy->Print("vtx-xy.pdf");

  return;
}
