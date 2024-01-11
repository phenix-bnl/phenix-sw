// SVX tracking modeler including deadmap information.
//
// 1. Build a full model of the VTX active area from PISA info.
// 2. Flag dead pixel chips and dead (or partially dead) stripixel half-modules.
// 3. Generate some random tracks pointing out from (0,0,0).
// 4. Step them through the model and find hits using volume boundary checking.
// 5. Draw a fake event display, highlighting hits in good or dead areas.
//
// A. Adare, May 2013 (Updated Aug 2013)

void SvxTrackingModeler(int run = 347602,
			bool ideal = true,
			bool export = false,
			bool txtmap = false,
			bool dotracking = true)
{
  gSystem->Load("libsvx.so");

  svxDetectorGeo *geo = new svxDetectorGeo();
  geo->set_Verbose(100);

  //This is for simulation (with ideal geometry ) setup
  if (ideal) {
    geo->setOffsetVtxToCnt(0,0,0);
    geo->setOffsetEastToWest(0,0,0);
    geo->setOffsetCntEastToWest(0,0,0);
    geo->Read_svxPISApar("svxPISA.par.ideal");
  }

  else {
    geo->Fetch_coordinateOffset(run);
    RunToTime   *rt      = RunToTime::instance();
    PHTimeStamp *Tsearch = rt->getBeginTime(run);
    geo->Fetch_svxPISApar(Tsearch);
  }

  // Construct VTX model from PISA parameters.
  SvxComponentGeom *cg = new SvxComponentGeom(geo);
  cg->SetVerbosity(1);    // Print every hit if > 0

  if (txtmap) { // Assign deadmap from text files instead of DB

    // Pixel hot/dead map
    SvxPixelHotDeadMap* pixelMap = new SvxPixelHotDeadMap;
    int run1, run2;
    pixelMap->readChipMapsFromFile(run1, run2,
				   "chipref.txt", "chipdiff_347602.txt");
    pixelMap->readPixelMapsFromFile(run1, run2,
				    "pixref.txt", "pixdiff_347602.txt");
    pixelMap->setTileMap();

    // Strip hot/dead map
    SvxDeadMap* stripMap = new SvxDeadMap;
    stripMap->readReadoutsFromDatabase(run);
    stripMap->readFromDatabase(run);

    // svxAddress
    svxAddress* address = new svxAddress;

    // Get timestamp from run number - used by svxAddress
    RunToTime *rt = RunToTime::instance();
    PHTimeStamp* tStamp = rt->getBeginTime(run);
    tStamp->print(); cout << endl;
    address->set_Verbose(0);
    address->set_usedatabase(1);
    address->setFetchTime(tStamp);
    address->Initialize();

    cg->AssignMap(pixelMap, stripMap, address);
  }
  else {
    cg->AssignMap(run); // Read deadmap from DB and instantiate svxAddress
  }

  TGeoManager* mgr = cg->GetGeoManager();
  TGeoVolume* top = mgr->GetTopVolume();
  Printf("%d sensors, %d total nodes.\n\n",
	 top->GetNdaughters(), mgr->GetNNodes());

  TCanvas* c = new TCanvas("c", "svx model with deadmap", 1400, 1000);
  c->SetFillColor(kBlack);
  top->Draw("");

  if (export) {
    mgr->Export(Form("svxmodel_run%d.root", run));
  }

  if (dotracking) {    // Generate tracks

    int nTracks = 50;
    TRandom3 ran3;
    for (int i=0; i<nTracks; i++) {

      double mom   = ran3.Uniform(0.0, 1.0);
      double phi   = ran3.Uniform(0,TMath::TwoPi());
      double theta = ran3.Uniform(TMath::PiOver4(), 3*TMath::PiOver4());
      double charge = (i%2)? 1 : -1;
      double magField = 0.90;

      SvxGeoTrack strk =
	cg->FindHitsFromVertex(0,0,0, mom, phi, theta, charge, magField);

      // Visualize connected hits with TGeoTracks
      TGeoTrack* trk = new TGeoTrack(i,211);
      trk->SetLineColor(kGreen);
      int ip = 0;
      trk->AddPoint(0,0,0,ip++);  // Track starting point

      // Add hits to track
      for (int ihit=0; ihit < strk.nhits; ihit++) {
	SvxGeoHit hit = strk.GetHit(ihit);
	hit.node->GetVolume()->SetVisibility(true);
	trk->AddPoint(hit.x,hit.y,hit.z,ip++);
      }

      trk->Draw();
    }
  }
  return;
}
