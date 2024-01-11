void readCalA2() {

// Load shared libraries
//  gROOT->Macro("loadlibraries.C");
   gSystem->Load("libpreco.so");

// Create calibration object
   TecCalibrationObject* TCO = new TecCalibrationObject();
   cout << TCO->getName() << " created." << endl;
   TCO->setDebugLevel(5);

// Set search time
//   TCO->setTimeStamp(PHTimeStamp(2000,5,20,0,0,0));
   TCO->SetRunNumber(69502);

// Read info from the database
   PHBoolean status = TCO->FetchAbsGain();

}

