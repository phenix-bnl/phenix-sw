void writeCalT() {

// Load shared libraries
  gROOT->Macro("loadlibraries.C");

// Create calibration object
   TecCalibrationObject* TCO = new TecCalibrationObject();
   cout << TCO->getName() << " created." << endl;
   TCO->setDebugLevel(2);

// Define validity range
   PHTimeStamp* Tbeg = new PHTimeStamp(2000,1,1,0,0,0);
   PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
//   Tbeg->print(); cout << endl; Tend->print(); cout << endl;

// Define location (calibration bank name)
   char* location = "calib.tec.tecgain_geant0";
//   char* location = "calib.tec.tecgain_run00";
   TCO->setCalibName(location);
   TCO->setDescription("TEC Timing Calibration Constants");

// Set geant calibration constants to 1.0
   PHBoolean status1 = TCO->FetchFromFile();
// Get calibration constants from a file
//   PHBoolean status1 = TCO->FetchTimingGainFromFile("tectimecalib_database.txt");

// Write info to the database
   PHBoolean status2 = TCO->UpdateTimeCalib(Tbeg, Tend);

}

