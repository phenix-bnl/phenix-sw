void writeMap() {

// Load shared libraries (done in rootlogon.C)
//  gROOT->Macro("loadlibraries.C");
gSystem->Load("libtec");

// Create address object
   TecAddressObject* TAO = new TecAddressObject();
   cout << TAO->getName() << " created." << endl;

// Define validity range
//   PHTimeStamp* Tbeg = new PHTimeStamp(2000,1,1,0,0,0);
// This is begin of validity for the second real data map 
// (file tecmap_database_run00b.txt)
// You can use the new (00b) map with the old data
//   PHTimeStamp* Tbeg = new PHTimeStamp(2000,7,10,0,0,0);
// This is beginning of validity range for run3
//   PHTimeStamp* Tbeg = new PHTimeStamp(2003,1,1,0,0,0);
// Map was modified on January 30, 2003
//   PHTimeStamp* Tbeg = new PHTimeStamp(2003,1,30,0,0,0);
// Another modification on Feb 26, 2003
   PHTimeStamp* Tbeg = new PHTimeStamp(2004,10,1,0,0,0);
   PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
   Tbeg->print(); cout << endl; Tend->print(); cout << endl;

// Set description
//   TAO->setDescription("TEC Hardware/Software Map for the Simulation");
   TAO->setDescription("Run5 Tec Hardware/Software Map for the Data");

// Define input file name
//   char* infilename = "tecmap_run3-after-feb26.txt";
//   char* infilename = "tecmap_run3-after-jan30.txt";
//   char* infilename = "tecmap_run3.txt";
//   char* infilename = "tecmap_database_east0.txt";
   char* infilename = "tecmap_database_run5.dat";
   TAO->setDebugLevel(2);

// Read map from file
   PHBoolean status = TAO->FetchFromFile(infilename);

// Write info to the database
   if(status) {
     PHBoolean status2 = TAO->Update(Tbeg, Tend);
     if(status2) { cout << "Successful commit." << endl; }
       else { cout << "ERROR: Failed to commit." << endl; }
   }
   else { cout << "ERROR: Can not read from file " << infilename << endl; }
}

