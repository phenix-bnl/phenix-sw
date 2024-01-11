void readMap() {

// Load shared libraries
//  gROOT->Macro("loadlibraries.C");
gSystem->Load("libtec");

// Create address object
   TecAddressObject* TAO = new TecAddressObject();
   cout << TAO->getName() << " created." << endl;
   TAO->setDebugLevel(5);

// Set search time
//   TAO->setTimeStamp(PHTimeStamp(2002,8,10,0,0,0));
   TAO->setTimeStamp(PHTimeStamp(2004,12,10,0,0,0));

// Set location (calibration bank name) <--- don't need this any more
//   char* location = "map.tec.geant0";
//   char* location = "map.tec.run00";
//   TAO->setCalibName(location);

// Read info from the database
   PHBoolean status = TAO->Fetch();

   PHTimeStamp Ttmp1 = TAO->getStartValidity();
   Ttmp1.print(); cout << endl;
   PHTimeStamp Ttmp2 = TAO->getEndValidity();
   Ttmp2.print(); cout << endl;

}

