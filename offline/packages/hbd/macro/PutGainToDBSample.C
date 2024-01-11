//
//
// This is a sample macro to insert Module-by-Module run-by-run
// gain constant into DB.
//
// Written by T. Sakaguchi, Jul 23, 2009
// 
// How to run:
//
// 1, source /opt/phenix/bin/phenix_setup.csh
// 2, setenv ODBCINI /opt/phenix/etc/odbc.ini.master
// 3, execute this macro like "root.exe PutGainToDBSample.C"
// 
// The "SampleFile.txt" quoted in the arguments should include
// 24 parameters which are gains for all modules
//
// If you have inserted parameters to DB mistakenly, drop an
// E-mail to Sakaguchi when you inserted what, to delete the
// corresponding DB entry.
//
//
PutGainToDBSample(int runno=236464, char *filename="SampleFile.txt")
{

//
// Load libraries.
//
gSystem->Load("libhbd.so");
gSystem->Load("libfun4allfuncs.so");

//
// HBD Adc calibration class
//
hbdAdcCalib t;

//
// Gain parameter file
//
ifstream fin(filename);

for(int i=0;i<24;i++){
   float gainvalue;
   float gainerror=0.0;

   fin>>gainvalue;
   t.setModuleGain(i,gainvalue,gainerror);
}

//
// Getting Start and End time of the given run number
//
RunToTimePg *runconv = RunToTimePg::instance();
PHTimeStamp *tStart = runconv->getBeginTime(runno);
PHTimeStamp *tStop = runconv->getEndTime(runno);

//
// Update DB
//
t.updateModuleGain(*tStart,*tStop);

//
// Read out from DB to see if the parameters are properly
// inserted
//
hbdAdcCalib t2;
t2.fetch(runno);
t2.print();
}

