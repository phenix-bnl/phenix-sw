void writeActivePlane() {

// Load shared libraries
//  gROOT->Macro("loadlibraries.C");

// Create geometry object
   TecGeometryObject* TGO = new TecGeometryObject();
   cout << TGO->getName() << " created." << endl;
   TGO->setDebugLevel(2);

// Define validity range
   PHTimeStamp* Tbeg = new PHTimeStamp(2004,3,21,0,0,0);
//   PHTimeStamp* Tbeg = new PHTimeStamp(2000,1,1,0,0,0);
// East Arm was moved in on August 1st 2000
//   PHTimeStamp* Tend = new PHTimeStamp(2000,8,1,0,0,0);
//   PHTimeStamp* Tbeg = new PHTimeStamp(2000,8,1,0,0,0);
   PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
   Tbeg->print(); cout << endl; Tend->print(); cout << endl;

// Define location (calibration bank name)
//   char* location = "geom.tec.geant00";
//   char* location = "geom.tec.run00";
//   TGO->setCalibName(location);

// Write description
//   TGO->setDescription("TEC Geant Geometry (non-retracted arms)");
//   TGO->setDescription("TEC Survey Geometry with Retracted Arms");
//   TGO->setDescription("TEC Survey Geometry (non-retracted arms)");

// Get geant geometry from file
//   PHBoolean status1 = TGO->FetchFromFile();
//   PHBoolean status1 = TGO->FetchFromPHNXPAR();
// Get survey geometry from file
//   PHBoolean status1 = TGO->FetchFromFile("tecgeom_database_run00_00.dat");
//   PHBoolean status1 = TGO->FetchFromFile("tecgeom_database_run00_00.dat");

// Shift the whole arm in by 44cm for survey geometry
// We will use retracted arms survey results shifted by 44cm until we have
// survey results for non-retracted geometry
//   PHBoolean statuss = TGO->Shift(44.0, 0.0, 0.0);

// Write info to the database
//  if(status1) PHBoolean status2 = TGO->Update(Tbeg, Tend);
   TGO->setTimeStamp(PHTimeStamp(2004,1,20,0,0,0));
   PHBoolean status1 = TGO->Fetch();
   for(int i=0; i<12; i++) { TGO->setActivePlane(i,0); }
   for(int i=12; i<24; i++) { TGO->setActivePlane(i,1); }
   for(int i=24; i<36; i++) { TGO->setActivePlane(i,0); }
   for(int i=36; i<48; i++) { TGO->setActivePlane(i,0); }
   //TGO->setActivePlane(29,0); // E2N, plane 2
   TGO->setActiveSectorSide(0,0);
   TGO->setActiveSectorSide(1,0);
   TGO->setActiveSectorSide(2,1);
   TGO->setActiveSectorSide(3,1);
   TGO->setActiveSectorSide(4,0);
   TGO->setActiveSectorSide(5,0);
   TGO->setActiveSectorSide(6,0);
   TGO->setActiveSectorSide(7,0);

   for(int i=0; i<48; i++) { cout << TGO->isActivePlane(i); }
   cout << endl;
   for(int i=0; i<8; i++) { cout << TGO->isActiveSectorSide(i); }
   cout << endl;

   PHBoolean status = TGO->UpdateActivePlanes(Tbeg, Tend);

}

