void writeHV_bytime(Int_t year=2003,
		    Int_t month=1,
		    Int_t day=1,
		    Int_t hour=0,
		    Int_t minute=0,
		    Int_t second=0) {

// Load shared libraries
  gROOT->Macro("loadlibraries.C");

  char gfile[80];

// Create hv object
   TecHVObject* TCO = new TecHVObject();
   cout << TCO->getName() << " created." << endl;
   TCO->setDebugLevel(2);

// Set time

   PHTimeStamp Tstart(year,month,day,hour,minute,second);
   PHTimeStamp Tend(year,month,day,hour,minute,second);
   Tstart -= 30;
   Tend += 30;
   cout << Tstart << endl;
   cout << Tend << endl;

// Fetch HV values from a file
   
   //   sprintf(gfile,"techvAnDw_database_run%d.txt",myrunnumber);
   sprintf(gfile,"techvAnDw_databasetest.txt");
   cout << gfile << endl;

   PHBoolean status1 = TCO->FetchHVValFromFile(gfile);

// Write info to the database
   PHBoolean status2 = TCO->UpdateHVVal(&Tstart, &Tend);

}
