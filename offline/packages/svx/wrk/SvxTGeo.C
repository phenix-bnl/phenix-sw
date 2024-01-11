// SvxTGeo.C: Macro to examine SVX geometry and test GetNearestSensor() function.
// A. Adare, April 2013

void SvxTGeo()
{
  bool pdf = false; // Print many pdf files
  int pdfCounter = 0;
  gSystem->Load("libsvx.so");
  svxDetectorGeo *geo = new svxDetectorGeo();
  geo->set_Verbose(2);
  geo->Read_svxPISApar();

  gSystem->Load("libGeom");

  TGeoManager* gm = new TGeoManager("world", "world geometry");

  // Materals: name, A, Z, rho (g/cm3)
  TGeoMaterial* matVac = new TGeoMaterial("Vacuum material",0,0,0);
  TGeoMaterial* matSi  = new TGeoMaterial("Silicon", 28.085, 14, 2.329);
  TGeoMaterial* matAl  = new TGeoMaterial("Aluminum", 26.98,  13, 2.7);

  // Media
  TGeoMedium* vacuum = new TGeoMedium("Vacuum", 1, matVac);
  TGeoMedium* Si  = new TGeoMedium("Si", 2, matSi);
  TGeoMedium* Al  = new TGeoMedium("Al", 3, matAl);

  // Outermost "top" volume in the hierarchy
  TGeoVolume* top = gm->MakeBox("Top",vacuum,25.,25.,25.); // cm
  top->SetLineColor(kBlue);
  gm->SetTopVolume(top);
  gm->SetTopVisible(false);

  int layerColor[4] = {kWhite, kYellow, kRed, kCyan};

  // Create TGeoVolume objects, with positions and orientations
  for (int ily=0; ily<4; ily++) {
    
    int nLadder = geo->get_nBarLadder(ily);
    int nSensor = geo->get_nBarSensor(ily);
    float xHalfWidth = 0, yHalfWidth = 0, zHalfWidth = 0;
    
    for (int ildr=0; ildr<nLadder; ildr++) { 
      for (int isen=0; isen<nSensor; isen++) {
	SvxSensor *sens_p = geo->GetSensorPtr(ily, ildr, isen);

	// Sensor orientation
	double cosphi     = sens_p->get_rotMatrix(0, 0); 
	double sinphi     = sens_p->get_rotMatrix(0, 1); 
	double phicos     = TMath::ACos(cosphi)/3.14*180.;
	double phisin     = TMath::ASin(sinphi)/3.14*180.;
	double phiangle   = TMath::ATan2(sinphi, cosphi); // (x,y)==(cosphi, sinphi)
	double phideg     = phiangle/TMath::Pi()*180.;
	double costheta   = sens_p->get_rotMatrix(2, 2); 
	double sintheta   = sens_p->get_rotMatrix(1, 2); 
	double thetaangle = TMath::ATan2(sintheta, costheta); // (x,y)==(costheta, sintheta)
	double thetadeg   = thetaangle/TMath::Pi()*180.;
	double psideg     = 0;
	TGeoRotation *rot = new TGeoRotation("rot", phideg, thetadeg, psideg);
	
	TGeoCombiTrans* placement = 
	  new TGeoCombiTrans(sens_p->get_transVector(0),
			     sens_p->get_transVector(1),
			     sens_p->get_transVector(2),
			     rot);
	
	// Sensor dimensions.
	// For pixel sensors, z-width requires summing over sections.
	// For stripixels, there is only one section.
	xHalfWidth = sens_p->get_xhalfWidth();
	yHalfWidth = (ily < 2) ? 0.02 : 0.0625; // From offline wiki
	zHalfWidth = 0;
	int nsc = sens_p->get_nSection();
	int readout = 0; // This remains zero
	for (int isc=0; isc<nsc; isc++)
	  zHalfWidth += sens_p->get_zhalfWidth(isc,readout);
	
	// Print some checks
	Printf("layer,ladder,sensor %d %d %d: x/2, y/2, z/2 = (%.3f,%.3f,%.3f)",
	       ily,ildr,isen, xHalfWidth,yHalfWidth,zHalfWidth);
	int nsc = sens_p->get_nSection();
	int nrd = sens_p->get_nReadout();
	for (int isc=0; isc<nsc; isc++)
	  for (int ird=0; ird<nrd; ird++)
	    Printf("   isc %d ird %d: z/2 = %f",
		   isc, ird, sens_p->get_zhalfWidth(isc,ird));

	TString sensorName = Form("sensor_%d_%d_%d", ily, ildr, isen);
	TGeoVolume *sensor = gm->MakeBox(sensorName.Data(), 
					 Si, xHalfWidth, yHalfWidth, zHalfWidth);
	
	// Position volumes within the top volume
	sensor->SetLineColor(layerColor[ily]);
	sensor->SetFillColor(layerColor[ily]);
	top->AddNode(sensor, 1, placement);
      }
    }
  }
  Printf("Done.");

  gm->CloseGeometry();
  gm->SetVisLevel(4);

  // Zero-field "cosmic" test with 1 track
  TCanvas* c = new TCanvas("c", "svxDetGeo model", 1400, 1000);    
  c->SetFillColor(kBlack);
  top->Draw("");
  float latitude = 10, longitude = 2, psi = 270; // degrees
  int irep=0;
  TView* view = c->GetView();
  view->SetView(longitude, latitude, psi, irep);
  view->ZoomIn();
  view->ZoomIn();
  gPad->Modified();
  gPad->Update();

  TGeoTrack* track1 = new TGeoTrack(1,211);
  track1->SetLineColor(kGreen);
  track1->SetMarkerColor(kGreen);
  track1->SetMarkerStyle(kFullCircle);
  track1->SetMarkerSize(1.0);
  track1->SetLineWidth(2);

  // Position and velocity components
  // n is the direction (unit velocity vector)
  double dt = 0.5;
  double x=20,y=20,z=20;
  double dx=-1*dt,dy=-1*dt,dz=-1*dt;
  double norm = 1./TMath::Sqrt(dx*dx + dy*dy + dz*dz);
  double nx = norm*dx, ny = norm*dy, nz = norm*dz;

  for (int i=0; i<80; i++) {

    // Save the previous radius, nearest layer, and their separation
    double rPrev  = TMath::Sqrt(x*x + y*y);
    int nlPrev = geo->GetNearestLayer(rPrev);
    double drPrev = rPrev - geo->get_svxRadius(nlPrev);

    // Update position
    x += dx; y += dy; z += dz;
    track1->AddPoint(x,y,z,i);
    double r = TMath::Sqrt(x*x + y*y);
    int nearestLayer = geo->GetNearestLayer(r);
    double dr = r - geo->get_svxRadius(nearestLayer);

    // Check if a nominal layer radius has been traversed
    bool didCrossLayer = (dr==0) || 
      (dr>0 && drPrev<0 && nearestLayer==nlPrev) || 
      (dr<0 && drPrev>0 && nearestLayer==nlPrev);

    TString crossed = didCrossLayer ? "<-- crossed!" : "";
    Printf("i= %d, r = %.2f (nearest layer = %d, dr = %.2f cm) %s",
	   i, r, nearestLayer, dr, crossed.Data());

    track1->Draw();
    gSystem->Sleep(50);
    if (pdf) {
      c->SaveAs(Form("pdfs/%03d.pdf",pdfCounter++));
    }

    // Highlight sensors 
    if (didCrossLayer) {
      SvxSensor *ssvx = geo->GetNearestSensor(x,y,z);
      TGeoNode* nearestNode = gm->FindNode(ssvx->get_transVector(0),
					   ssvx->get_transVector(1),
					   ssvx->get_transVector(2));
      TGeoVolume* vol = nearestNode->GetVolume();
      vol->SetLineColor(kGreen);
      vol->SetLineWidth(3);
    }
  }

  // Spin the thing around once
  for (int i=1; i<=90; i++) {
    latitude += 4;
    view->SetView(longitude, latitude, psi, irep);
    gPad->Modified();
    gPad->Update();
    if (pdf)
      c->SaveAs(Form("pdfs/%03d.pdf",pdfCounter++));
  }

  gm->Export("svxmodel.root");

  return;
}
