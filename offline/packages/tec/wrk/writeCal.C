void writeCal() {

cout << endl;
cout << "WARNING: This macro will set Geant calibration constants to 1." << endl;
cout << endl;

// Load shared libraries
  gROOT->Macro("loadlibraries.C");

// Create calibration object
   TecCalibrationObject* TCO = new TecCalibrationObject();
   cout << TCO->getName() << " created." << endl;
   TCO->setDebugLevel(2);

// Define validity range
   PHTimeStamp* Tbeg = new PHTimeStamp(2000,1,1,0,0,0);
   PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
   Tbeg->print(); cout << endl; Tend->print(); cout << endl;

// Define location (calibration bank name)
//   char* location = "calib.tec.relgaingeant0";
//   TGO->setCalibName(location);

// Set geant calibration constants to 1.0
   PHBoolean status1 = TCO->FetchFromFile();

// Write relative gains to the database
   PHBoolean status2 = TCO->UpdateRelGain(Tbeg, Tend);

// Write absolute gains to the database
   PHBoolean status3 = TCO->UpdateRelGain(Tbeg, Tend);

// Write timing calibration to the database
   PHBoolean status4 = TCO->UpdateRelGain(Tbeg, Tend);

}

