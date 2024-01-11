void writeHotDead() {

// Load shared libraries
//  gROOT->Macro("loadlibraries.C");

// Create calibration object
   TecCalibrationObject* TCO = new TecCalibrationObject();
   cout << TCO->getName() << " created." << endl;
   TCO->setDebugLevel(2);

// Define validity range
//   PHTimeStamp* Tbeg = new PHTimeStamp(2000,1,1,0,0,0);
//   PHTimeStamp* Tbeg = new PHTimeStamp(2003,1,1,0,0,0);
   PHTimeStamp* Tbeg = new PHTimeStamp(2004,1,1,0,0,0);
   PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
   Tbeg->print(); cout << endl; Tend->print(); cout << endl;

// Define location (calibration bank name)
//   char* location = "calib.tec.tecgain_geant0";
//   char* location = "calib.tec.tecgain_run00";
//   TCO->setCalibName(location);
//   TCO->setDescription("TEC Absolute Chamber-by-Chamber Calibration Constants");
   TCO->setDescription("List of TEC Hot/Dead Channels");

// Set geant calibration constants to 1.0
//   PHBoolean status1 = TCO->FetchFromFile();
// Get calibration constants from a file
//   PHBoolean status1 = TCO->FetchAbsGainFromFile("tecabsgain_database.txt");
//   PHBoolean status1 = TCO->FetchHotDeadFromFile("hotdead_run3.txt");
   PHBoolean status1 = TCO->FetchHotDeadFromFile("hotdead_run4.txt");

// Write info to the database
   if(status1) {
     PHBoolean status2 = TCO->UpdateHotDead(Tbeg, Tend);
     if(status2) {cout << "Successful commit." << endl;}
       else {cout << "Commit ERROR !!!" << endl;}
   }
   else {cout << "Commit ERROR !!!" << endl;}
}

