void readCalT() {

// Load shared libraries
  gROOT->Macro("loadlibraries.C");

// Create calibration object
   TecCalibrationObject* TCO = new TecCalibrationObject();
   cout << TCO->getName() << " created." << endl;
   TCO->setDebugLevel(5);

// Set search time
   TCO->setTimeStamp(PHTimeStamp(2000,5,20,0,0,0));

// Set location (calibration bank name)
   char* location = "calib.tec.tecgain_geant0";
//   char* location = "calib.tec.tecgain_run00";
   TCO->setCalibName(location);

// Read info from the database
   PHBoolean status = TCO->FetchTimingGain();

}

