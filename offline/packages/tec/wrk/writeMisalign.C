void writeMisalign() {

// Load shared libraries
//  gROOT->Macro("loadlibraries.C");

// Create geometry object
   TecGeometryObject* TGO = new TecGeometryObject();
   cout << TGO->getName() << " created." << endl;
   TGO->setDebugLevel(5);

//   TGO->setTimeStamp(PHTimeStamp(2003,2,20,0,0,0));
//   TGO->UseSimulationDatabase();
//   PHBoolean status1 = TGO->Fetch();
   //
   PHTimeStamp* Tbeg = new PHTimeStamp(2003,1,1,0,0,0);
   PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
   cout << "Validity range: " << endl;
   Tbeg->print(); cout << endl; Tend->print(); cout << endl;

// Write description
//   TGO->setDescription("TEC Geant Geometry (non-retracted arms)");

// Read misalignment ad write info to the database
  PHBoolean status = TGO->UpdateMisalignment(Tbeg, Tend);
  if(status) {
    cout << "Successfull commit." << endl;
  }
  else { cout << "Failed to commit !!!" << endl; }


}

