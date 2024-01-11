void writeGeom() {

// Load shared libraries
  gROOT->Macro("loadlibraries.C");

// Create geometry object
   TecGeometryObject* TGO = new TecGeometryObject();
   cout << TGO->getName() << " created." << endl;
   TGO->setDebugLevel(2);

// Define validity range
   PHTimeStamp* Tbeg = new PHTimeStamp(2000,1,1,0,0,0);
// East Arm was moved in on August 1st 2000
//   PHTimeStamp* Tend = new PHTimeStamp(2000,8,1,0,0,0);
//   PHTimeStamp* Tbeg = new PHTimeStamp(2000,8,1,0,0,0);
   PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
   Tbeg->print(); cout << endl; Tend->print(); cout << endl;

// Define location (calibration bank name)
   char* location = "geom.tec.geant00";
//   char* location = "geom.tec.run00";
   TGO->setCalibName(location);

// Write description
   TGO->setDescription("TEC Geant Geometry (non-retracted arms)");
//   TGO->setDescription("TEC Survey Geometry with Retracted Arms");
//   TGO->setDescription("TEC Survey Geometry (non-retracted arms)");

// Get geant geometry from file
   PHBoolean status1 = TGO->FetchFromFile();
//   PHBoolean status1 = TGO->FetchFromPHNXPAR();
// Get survey geometry from file
//   PHBoolean status1 = TGO->FetchFromFile("tecgeom_database_run00_00.dat");

// Shift the whole arm in by 44cm for survey geometry
// We will use retracted arms survey results shifted by 44cm until we have
// survey results for non-retracted geometry
//   PHBoolean statuss = TGO->Shift(44.0, 0.0, 0.0);

// Write info to the database
  if(status1) PHBoolean status2 = TGO->Update(Tbeg, Tend);

}

