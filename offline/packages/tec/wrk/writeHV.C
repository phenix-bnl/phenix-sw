void writeHV(const int myrunnumber) {

// Load shared libraries
  gROOT->Macro("loadlibraries.C");

  char gfile[80];

// Create hv object
   TecHVObject* TCO = new TecHVObject();
   cout << TCO->getName() << " created." << endl;
   TCO->setDebugLevel(2);

// Set run number

   TCO->setRunNumber(myrunnumber);         // find run start time

// Fetch HV values from a file
   
   //   sprintf(gfile,"techvAnDw_database_run%d.txt",myrunnumber);
   sprintf(gfile,"techvAnDw_databasetest.txt");
   cout << gfile << endl;

   PHBoolean status1 = TCO->FetchHVValFromFile(gfile);

// Write info to the database
   PHBoolean status2 = TCO->UpdateHVVal(myrunnumber);

}
