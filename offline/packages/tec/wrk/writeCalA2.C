void writeCalA2() {

// Load shared libraries
//  gROOT->Macro("loadlibraries.C");

// Create calibration object
   TecCalibrationObject* TCO = new TecCalibrationObject();
   cout << TCO->getName() << " created." << endl;
   TCO->setDebugLevel(2);

// Define validity range
//   PHTimeStamp* Tbeg = new PHTimeStamp(2000,1,1,0,0,0);
//   PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
//   Tbeg->print(); cout << endl; Tend->print(); cout << endl;

//   TCO->setDescription("TEC Absolute Chamber-by-Chamber Calibration Constants");

// Set geant calibration constants to 1.0
//   PHBoolean status1 = TCO->FetchFromFile();
// Get calibration constants from a file
//   PHBoolean status1 = TCO->FetchAbsGainFromFile("tecabsgain_database.txt");
//   PHBoolean status1 = TCO->FetchAbsGainFromFile("gains71488.txt");
   PHBoolean status1 = TCO->FetchAbsGainFromFile("gain69502.txt");

//   for(int i=0; i<48; i++) {
//     cout << TCO->getAbsoluteGain(i) << endl;
//   }
// Write info to the database
//  PHBoolean status2 = TCO->UpdateAbsGain(Tbeg, Tend);
//  PHBoolean status2 = TCO->UpdateAbsGain(71488);
   PHBoolean status2 = TCO->UpdateAbsGain(69502);

}

