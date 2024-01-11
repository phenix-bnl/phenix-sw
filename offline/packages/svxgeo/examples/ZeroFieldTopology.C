typedef vector<SvxGeoTrack> geoTracks;

void ZeroFieldTopology()
{
  // Build model.
  SvxTGeo *geo = new SvxTGeo;
  geo->ReadParFile("parfiles/svxPISA.par");
  geo->MakeTopVolume(100, 100, 100);
  geo->AddSensors();
  TGeoManager *mgr = geo->GeoManager();
  TGeoVolume *top = mgr->GetTopVolume();
  mgr->CloseGeometry();

  // Draw x-y plane.
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
      s->SetLineColor(kGray);
      s->SetLineWidth(2);
      s->Draw();

      if (false) // Ladder index labels
      {
        geo->GetSensorXYZ(i, j, 0, xyz);
        int horz = xyz[0] > 0 ? 1 : 3;
        int vert = xyz[1] > 0 ? 1 : 3;
        label.SetTextAlign(10*horz + vert);
        label.DrawLatex(xyz[0], xyz[1], Form(" %d ", j));
      }
    }

  // Get hit data into track vector.
  geoTracks tracks;
  TFile *inFile = new TFile("vtx-fake.0.root", "read");
  TNtuple *t = (TNtuple *)inFile->Get("ht1");
  assert(t);
  GetTracksFromTree(t, geo, tracks);

  // Draw
  unsigned int ntdraw = t->GetEntries() > 1000 ? 1000 : t->GetEntries();
  for (unsigned int i=0; i<ntdraw; i++)
  {
    SvxGeoTrack track = tracks[i];
    DrawTrackRoute(geo, track);
  }
  c->Print("zft.pdf");

  return;
}

void
DrawTrackRoute(SvxTGeo *geo, SvxGeoTrack &t)
{
  int n = t.nhits;
  double xyz[3] = {0};
  double xhits[7] = {0}, yhits[7] = {0};

  bool B2L3 = false;
  bool smalldz = false;

  for (int j=0; j<n; j++)
  {
    SvxGeoHit hit = t.hits[j];
    geo->GetSensorXYZ(hit.layer, hit.ladder, 0, xyz);
    xhits[j] = xyz[0];
    yhits[j] = xyz[1];
    if (hit.layer == 2 && hit.ladder == 3)
    {
      B2L3 = true;
      if (hit.dz > -0.02)
        smalldz = true;
    }
  }

//  if (B2L3 && smalldz)
  {
    TColor *bluet = gROOT->GetColor(kBlue+1);
    bluet->SetAlpha(0.02);
    int kBlueT = bluet->GetNumber();

    TPolyLine p(n);
    p.SetLineWidth(3);
    p.SetLineColor(kBlueT);
    p.DrawPolyLine(n, xhits, yhits);
  }

  return;
}

void
GetTracksFromTree(TNtuple *t, SvxTGeo *geo, geoTracks &tracks)
{
  // This function reads hit ntuple variables of the form
  // "layer:ladder:sensor:xs:ys:zs:x:y:z:xsigma:zsigma:dz:ds:trkid"
  // into the tracks vector.

  long nentries = t->GetEntries();
  int previd = -1;

  Printf("Reading tracks from NTuple (%d entries)...", (int)nentries);
  for (int i=0; i<nentries; i++)
  {
    t->GetEntry(i);
    int id = t->GetLeaf("trkid")->GetValue();
    if (id != previd) // New track
    {
      SvxGeoTrack t;
      // Track info is not stored in tree. If it were, it would go here.
      tracks.push_back(t);
    }

    SvxGeoHit hit;
    hit.layer  = t->GetLeaf("layer")->GetValue();
    hit.ladder = t->GetLeaf("ladder")->GetValue();
    hit.sensor = t->GetLeaf("sensor")->GetValue();
    hit.xs     = t->GetLeaf("xs")->GetValue();
    hit.ys     = t->GetLeaf("ys")->GetValue();
    hit.zs     = t->GetLeaf("zs")->GetValue();
    hit.x      = t->GetLeaf("x")->GetValue();
    hit.y      = t->GetLeaf("y")->GetValue();
    hit.z      = t->GetLeaf("z")->GetValue();
    hit.xsigma = t->GetLeaf("xsigma")->GetValue();
    hit.zsigma = t->GetLeaf("zsigma")->GetValue();
    hit.dz     = t->GetLeaf("dz")->GetValue();
    hit.ds     = t->GetLeaf("ds")->GetValue();
    hit.trkid  = t->GetLeaf("trkid")->GetValue();
    hit.node   = geo->SensorNode(hit.layer, hit.ladder, hit.sensor);
    tracks.back().nhits++;
    tracks.back().hits.push_back(hit);
    previd = id;
  }

  Printf("%d tracks imported.", (int)tracks.size());
  return;
}


