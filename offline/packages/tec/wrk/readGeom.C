void readGeom() {

// Load shared libraries
//  gROOT->Macro("loadlibraries.C");

// Create geometry object
   TecGeometryObject* TGO = new TecGeometryObject();
   cout << TGO->getName() << " created." << endl;
   TGO->setDebugLevel(5);

// Set search time
//   TGO->setTimeStamp(PHTimeStamp(2000,8,20,0,0,0));
//   TGO->setTimeStamp(PHTimeStamp(2003,2,20,0,0,0));
   TGO->setTimeStamp(PHTimeStamp(2004,1,20,0,0,0));

//   TGO->UseSimulationDatabase();

// Read info from the database
   PHBoolean status = TGO->Fetch();

   PHTimeStamp Ttmp1 = TGO->getStartValidity();
   Ttmp1.print(); cout << endl;
   PHTimeStamp Ttmp2 = TGO->getEndValidity();
   Ttmp2.print(); cout << endl;

}

