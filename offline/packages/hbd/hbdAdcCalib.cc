// Class: hbdAdcCalib (implementation)
// Created by: Ilia Ravinovich
// Description: Class that generates the pad chamber geometry from
//              either a default, or from the information in the database
// last update: 04/15/03 simulation geometry in OBJY database, Indrani Ojha
// modified by C. Aidala and A. Sickles to use the database 3/6/2006
// last update: 12/20/11 Modifications to accomodate time dependent gain calibration
//             - make run number a member variable to be set only once when calibration is fetched
//                  all other methods that need run number now use this member variable 
//             - Remove previous implementation of time dependent gain that reads constants from text file
//             - RunIsCalibrated is assumed to be true, except if the banks are not found.
//             - apply time dependent or constant gains based on a user supplied flag HBD_MODBYMOD. 
//                  default is set to 1- apply constant gains as in the old days. Use 2 for time dep. gains
//             - add functions to read/write to DB/file the time dependent gain constants
// modified by E. Atomssa
//-------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <hbdAdcCalib.hh>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PdbApplication.hh>
#include <PdbBankManager.hh>
#include <PdbCalBank.hh>
#include <PdbHbdAdc.hh>
#include <PdbHbdModuleGain.hh>
#include <PdbParameterError.hh>
#include <PHTimeStamp.h>
#include <RunToTime.hh>

#include "recoConsts.h"
#include "hbdDetectorGeo.hh"
#include "TRandom2.h"

using namespace std;

//
// Default Constructor for hbdAdcCalib
//
hbdAdcCalib::hbdAdcCalib()
{
  // make this flag settable from the outside
  // so you do not piss everybody off with your blabbering
  debug=0;
  Verbose = 0;
  // floats can be set to zero by memset (but to no other value!)
  // Zero out the P/T corrections... 
  memset(PTConst,0,sizeof(PTConst));
  memset(PTError,0,sizeof(PTError));
  // Zero out the module gains...
  memset(ModuleGain,0,sizeof(ModuleGain));
  memset(ModuleGainError,0,sizeof(ModuleGainError));
  memset(ModuleGainByChan,0,sizeof(ModuleGainByChan));

  // Zero out the chan-by-gain gain and pedestals...
  memset(GainConst,0,sizeof(GainConst));
  memset(GainError,0,sizeof(GainError));
  memset(PedestalConst,0,sizeof(PedestalConst));
  memset(PedestalError,0,sizeof(PedestalError));

  //  Set up the choices for applied calibrations...
  recoConsts *rc = recoConsts::instance();
  ApplyChanByChan = rc->get_IntFlag("HBD_CHANBYCHAN",1) == 1;
  ApplyModByMod = rc->get_IntFlag("HBD_MODBYMOD",  1) == 1;
  ApplyTimeDepModByMod = rc->get_IntFlag("HBD_MODBYMOD",  1) == 2;

  runnum = 0;
  RunIsCalibrated = true;

  runFirstClockTick = 0;
  validityRangeFirstClkTick = 0;
  validityRangeLastClkTick = kMaxULong64;

  T = new TRandom2();
  T->SetSeed(12345);
}

hbdAdcCalib::~hbdAdcCalib()
{
  delete T;
  return;
}



PHBoolean
hbdAdcCalib::setClockTick(unsigned int moreSignif, unsigned int lessSignif)
{

  if (!ApplyTimeDepModByMod) return True;

  ULong64_t clk_tick = joinTwoInts(moreSignif,lessSignif);

  // Events do not necessarily come in increasing order of clock tick. Adjust the clock tick for that
  if (clk_tick<runFirstClockTick) clk_tick = runFirstClockTick;

  // If the current validity range works for this event, nothing to do
  if (validityRangeFirstClkTick<=clk_tick && clk_tick<validityRangeLastClkTick) return True;


  // if not, reset the currentModuleGain pointer to the correct one. reinitialize validity range delimiters
  bool lastValidityRange = true;
  for (gainConstIter = clkTickGainMap.begin(); gainConstIter!=clkTickGainMap.end(); ++gainConstIter)
    {      
      if (clk_tick<gainConstIter->first) 
	{
	  lastValidityRange = false;
	  validityRangeLastClkTick = gainConstIter->first;
	  break;
	}
    }
  //If last validity range is reached without finding the correct validity range, 
  //the last one must be the correct one. Otherwise the previous range is correct one
  if (lastValidityRange) 
    validityRangeLastClkTick = kMaxULong64;

  gainConstIter--;
  
  currentModuleGain = (PdbHbdModuleGain*)&gainConstIter->second;
  validityRangeFirstClkTick = gainConstIter->first;

  // call the method to spread out the module gains to all channels
  ModuleToChannel();

  return True;

}



//
// Fetch the ADC information from the database by Run Number
//
PHBoolean
hbdAdcCalib::fetch(PHTimeStamp& tInt)
{
  RunToTime * runto = RunToTime::instance();
  return fetch(runto->getRunNumber(tInt));
}


//
// Fetch the ADC information from the database
//
PHBoolean
hbdAdcCalib::fetch(int run)
{
  
  runnum = run;

   const char *calibName = "calib.hbd.adc";
   PdbBankManager* bankManager = PdbBankManager::instance();
   PdbApplication* application = bankManager->getApplication();
   if(application->startRead()){

      //
      // Here is Pedestal calibration
      //
      PdbBankID bankID(1);   // 1 = ped, 2 = chan-by-chan, 3 = module-by-module
      PdbCalBank* hbdBank = bankManager->fetchBank("PdbHbdAdcBank", bankID, calibName, runnum);

      if(hbdBank){
	 if(debug){
	    hbdBank->print();
	    cout << "# of channels = " << hbdBank->getLength() << endl;
	 }
	
	 PdbHbdAdc* hbdped = (PdbHbdAdc*)&(hbdBank->getEntry(0));
	 for(int i=0;i<2304;i++){
            hbdped->getPedestal(i,PedestalConst[i],PedestalError[i]); 
	 }
	 delete hbdBank;
      }
      else
	{
	  //RunIsCalibrated = false;
	  cout << PHWHERE << "Unable to find pedestals for runnum " << runnum << endl;
	}

      //
      // Here is channel by channel gain calibration
      //
      bankID.setInternalValue(2);   // 1 = ped, 2 = chan-by-chan, 3 = module-by-module
      hbdBank = bankManager->fetchBank("PdbHbdAdcBank", bankID, calibName, runnum);

      if(hbdBank){
	 if(debug){
	    hbdBank->print();
	    cout << "# of channels = " << hbdBank->getLength() << endl;
	 }
	
	 PdbHbdAdc* hbdgain = (PdbHbdAdc*)&(hbdBank->getEntry(0));
	 for(int i=0;i<2304;i++){
            hbdgain->getGain(i, GainConst[i], GainError[i]); 
	 }
	 delete hbdBank;
      }
      else
	{
	  //RunIsCalibrated = false;
	  cout << PHWHERE << "Unable to find chan-by-chan gains for runnum " << runnum << endl;
	}


      if (ApplyModByMod)
	{
	  //
	  // Here is Module-by-Module calibration - no time dependence (This will be kept until the time dependent gains have been validated)
	  //
	  bankID.setInternalValue(3);   // 1 = ped, 2 = chan-by-chan, 3 = module-by-module
	  hbdBank = bankManager->fetchBank("PdbHbdAdcBank", bankID, calibName, runnum);

	  if(hbdBank){
	    if(debug){
	      hbdBank->print();
	      cout << "# of channels = " << hbdBank->getLength() << endl;
	    }
	    
	    PdbHbdAdc* hbdmod = (PdbHbdAdc*)&(hbdBank->getEntry(0));
	    for(int i=0;i<24;i++){
	      hbdmod->getGain(i,ModuleGain[i],ModuleGainError[i]); 
	    }
	    
	    ModuleToChannel();  // Spread the 24 gains over the channels...
	    delete hbdBank;
	  }
	  else
	    {
	      //RunIsCalibrated = false;
	      cout << PHWHERE << "Unable to find module gains for runnum " << runnum << endl;
	    }
	}
      
      if (ApplyTimeDepModByMod)
	{
	  //
	  // Time dependent module by module gain calibration. 
	  //
	  hbdBank = bankManager->fetchBank("PdbHbdModuleGainBank", PdbBankID(0), "hbdmodulegain", runnum);
	  if (hbdBank)
	    {
	      if (Verbose>0)
		hbdBank->print();
	      
	      // just in case database is accessed more than once, delete values stored inside old map, and clear the map
	      clkTickGainMap.clear();
	      
	      for (size_t iEnt=0; iEnt<hbdBank->getLength(); iEnt++)
		{
		  if (Verbose>0)
		    cout << "Getting Entry " << iEnt << " in bank" << endl;
		  
		  PdbHbdModuleGain *moduleGain = (PdbHbdModuleGain*)&(hbdBank->getEntry(iEnt));
		  if (Verbose>0) moduleGain->print();

		  // insert a clock tick - PdbHbdModuleGain object pair in the map
		  unsigned int moreSignif, lessSignif;
		  moduleGain->getClockTick(moreSignif,lessSignif);
		  ULong64_t clk_tick = joinTwoInts(moreSignif,lessSignif);
		  clkTickGainMap.insert(std::pair<ULong64_t,PdbHbdModuleGain>(clk_tick,*moduleGain));

		  // set the current validity range to the first entry in the map
		  if (iEnt == 0) 
		    {
		      runFirstClockTick = clk_tick;
		      validityRangeFirstClkTick = clk_tick;
		      validityRangeLastClkTick = kMaxULong64; //0xffffffff;
		      currentModuleGain = moduleGain;
		    }
		  else if (iEnt == 1)
		    {
		      validityRangeLastClkTick = clk_tick;
		    }
		}
	      
	      ModuleToChannel();
	      delete hbdBank;
	    }
	  else
	    {
	      //RunIsCalibrated = false;
	      cout << PHWHERE << "Unable to find time dependent module gains for runnum " << runnum << endl;
	    }
	}

   }

   return True;

}




//
// Fetch the P/T and Module Gain information from the database
//
PHBoolean
hbdAdcCalib::fetchPT(PHTimeStamp& tInt)
{

   const char *calibName = "calib.hbd.pt";
   PdbBankManager* bankManager = PdbBankManager::instance();
   PdbApplication* application = bankManager->getApplication();
   if(application->startRead()){

      //
      // Here is Module Gain calibration
      //
      PdbBankID bankID(0);
      PdbCalBank* hbdBank = bankManager->fetchBank("PdbParameterErrorBank", bankID, calibName, tInt);

      if(hbdBank){
	 if(debug){
	    hbdBank->print();
	    cout << "# of channels = " << hbdBank->getLength() << endl;
	 }
	
	 PdbParameterError* hbdpt = (PdbParameterError*)&(hbdBank->getEntry(0));
         PTConst[0]=hbdpt->getParameter();
         PTError[0]=hbdpt->getParameterError();
	 delete hbdBank;
      }

      //
      // Here is gain calibration
      //
      bankID.setInternalValue(1);
      hbdBank = bankManager->fetchBank("PdbParameterErrorBank", bankID, calibName, tInt);

      if(hbdBank){
	 if(debug){
	    hbdBank->print();
	    cout << "# of channels = " << hbdBank->getLength() << endl;
	 }
	
	 PdbParameterError* hbdpt = (PdbParameterError*)&(hbdBank->getEntry(0));
         PTConst[1]=hbdpt->getParameter();
         PTError[1]=hbdpt->getParameterError();
	 delete hbdBank;
      }
   }
   return True;
}


//
// Put the ADC calibration into the database
//
///////////////////////////////////////////////////////////////////////////
PHBoolean hbdAdcCalib::updateGain(PHTimeStamp& tStart, PHTimeStamp& tStop)
{
   
     const char *calibName = "calib.hbd.adc";
     const char *hbddescript = "HBD ADC gain calibration";
     PdbBankManager* bankManager = PdbBankManager::instance();
     PdbApplication* application = bankManager->getApplication();
     cout << "opening db in update mode " << calibName << endl;

     if(application->startUpdate()){
	PdbBankID bankID(2); // 2 means chan-by-chan gain
	PdbCalBank* hbdBank = bankManager->createBank("PdbHbdAdcBank",bankID,hbddescript,tStart,tStop, calibName );
	
	hbdBank->setLength(1);
	if(debug) hbdBank->print();
	PdbHbdAdc* hbdgain = (PdbHbdAdc*)&(hbdBank->getEntry(0));
	for(int i=0;i<2304;i++){
	   hbdgain->setGain(i,GainConst[i], GainError[i]);
	}
      	if(debug) hbdBank->printEntry(0);

    // please put user name by hand
//     	char username[100];
// 	cout << endl << "Please put your name" << endl;
//	cin.getline(username,100);
	PHString UserName("Hbd ADC calibrator");
	hbdBank->setUserName(UserName);
	application->commit(hbdBank);
     }
     return True;
}


//
// Put P/T data into the database
//
///////////////////////////////////////////////////////////////////////////
PHBoolean hbdAdcCalib::updatePT(PHTimeStamp& tStart, PHTimeStamp& tStop)
{
   
     const char *calibName = "calib.hbd.pt";
     const char *hbddescript = "HBD P/T constants";
     PdbBankManager* bankManager = PdbBankManager::instance();
     PdbApplication* application = bankManager->getApplication();
     cout << "opening db in update mode " << calibName << endl;

     if(application->startUpdate()){
        PdbBankID bankID(0); // bankID used at subsystem discretion. 

        PdbCalBank* hbdBank = bankManager->createBank("PdbParameterErrorBank",bankID,hbddescript,tStart,tStop, calibName );
        
        hbdBank->setLength(1);
        if(debug) hbdBank->print();
        PdbParameterError* hbdpt = (PdbParameterError*)&(hbdBank->getEntry(0));
        hbdpt->setParameter(PTConst[0]);
        hbdpt->setParameterError(PTError[0]);
        if(debug) hbdBank->printEntry(0);

    // please put user name by hand
//      char username[100];
//      cout << endl << "Please put your name" << endl;
//      cin.getline(username,100);
        PHString UserName("Hbd P/T calibrator");
        hbdBank->setUserName(UserName);
        application->commit(hbdBank);

        bankID.setInternalValue(1); // bankID used at subsystem discretion. 

        hbdBank = bankManager->createBank("PdbParameterErrorBank",bankID,hbddescript,tStart,tStop, calibName );
        
        hbdBank->setLength(1);
        if(debug) hbdBank->print();
        hbdpt = (PdbParameterError*)&(hbdBank->getEntry(0));
        hbdpt->setParameter(PTConst[1]);
        hbdpt->setParameterError(PTError[1]);

        PHString UserName2("Hbd P/T calibrator");
        hbdBank->setUserName(UserName2);
        application->commit(hbdBank);
     }
     return True;
}


//
// Put Module data into the database
//
///////////////////////////////////////////////////////////////////////////
PHBoolean hbdAdcCalib::updateModuleGain(PHTimeStamp& tStart, PHTimeStamp& tStop)
{
   
     const char *calibName = "calib.hbd.adc";
     const char *hbddescript = "HBD Module gain calibration";
     PdbBankManager* bankManager = PdbBankManager::instance();
     PdbApplication* application = bankManager->getApplication();
     cout << "opening db in update mode " << calibName << endl;

     if(application->startUpdate()){
        PdbBankID bankID(3); // 3 means module gains...

        PdbCalBank* hbdBank = bankManager->createBank("PdbHbdAdcBank",bankID,hbddescript,tStart,tStop, calibName );
        
        hbdBank->setLength(1);
        if(debug) hbdBank->print();
        PdbHbdAdc* hbdgain = (PdbHbdAdc*)&(hbdBank->getEntry(0));
        for(int i=0;i<24;i++){
           hbdgain->setGain(i,ModuleGain[i], ModuleGainError[i]);
        }
        if(debug) hbdBank->printEntry(0);

    // please put user name by hand
//      char username[100];
//      cout << endl << "Please put your name" << endl;
//      cin.getline(username,100);
        PHString UserName("Hbd Module Gain calibrator");
        hbdBank->setUserName(UserName);
        application->commit(hbdBank);
     }
     return True;
}


PHBoolean hbdAdcCalib::updatePedestal(PHTimeStamp& tStart, PHTimeStamp& tStop)
{
   
     const char *calibName = "calib.hbd.adc";
     const char *hbddescript = "HBD ADC pedestal calibration";
     PdbBankManager* bankManager = PdbBankManager::instance();
     PdbApplication* application = bankManager->getApplication();
     cout << "opening db in update mode " << calibName << endl;

     if(application->startUpdate()){
        PdbBankID bankID(1); // 1 means pedestals...
        PdbCalBank* hbdBank = bankManager->createBank("PdbHbdAdcBank",bankID,hbddescript,tStart,tStop, calibName );
	
        hbdBank->setLength(1);
        if(debug) hbdBank->print();
        PdbHbdAdc* hbdped = (PdbHbdAdc*)&(hbdBank->getEntry(0));
        for(int i=0;i<2304;i++){
           hbdped->setPedestal(i,PedestalConst[i], PedestalError[i]);
        }
      	if(debug) hbdBank->printEntry(0);
    // please put user name by hand
//     	char username[100];
// 	cout << endl << "Please put your name" << endl;
//	cin.getline(username,100);
	PHString UserName("Hbd ADC calibrator");
	hbdBank->setUserName(UserName);
	application->commit(hbdBank);
     }
     return True;
}


void
hbdAdcCalib::readCalibFromFile(const char *filename)
{
  recoConsts *rc = recoConsts::instance();
  int gainsonly = rc->get_IntFlag("HBD_GAINSONLY",0);

   ifstream fgeo(filename);
   if (!fgeo.is_open())
     {
       cout << PHWHERE << "Cannot open calibration file: " << filename << endl;
       return;
     }


   int ADCch;
   float ped,gain;
   float pederr,gainerr;
   while(fgeo)
     {
       fgeo >> ADCch >> ped >> pederr >> gain >> gainerr;
       if (!fgeo.eof())
	 {
	   if (Verbose > 0)
	     {
	       cout << ADCch   << " ";
	       cout << ped     << " " ;
	       cout << pederr  << " ";
	       cout << gain    << " ";
	       cout << gainerr << endl;
	     }
	   
	   if (!gainsonly)
	     {
	       PedestalConst[ADCch]=ped;
	       PedestalError[ADCch]=pederr;
	     }
	   GainConst[ADCch]=gain;
	   GainError[ADCch]=gainerr;
	 }
     }
}


// Commit time dependent module gains in memory (read from file) to database
//______________________________________________________________________________
PHBoolean hbdAdcCalib::updateTimeDepModuleGain(PHTimeStamp& tStart, PHTimeStamp& tStop)
{
  const char *calibName = "hbdmodulegain";
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();
  cout << "opening db in update mode for table " << calibName << " as user " << std::getenv("USER") << " entry tStart= " << tStart << " tEnd= " << tStop << endl;
  
  if(application->startUpdate())
    {
      PdbCalBank* hbdBank = bankManager->createBank("PdbHbdModuleGainBank", PdbBankID(0), "HBD Module Gain Calibration", tStart, tStop, calibName);
      
      hbdBank->setLength(clkTickGainMap.size());
      if(Verbose>0) hbdBank->print();
      
      int iEnt = 0;
      
      for(gainConstIter=clkTickGainMap.begin(); gainConstIter!=clkTickGainMap.end(); gainConstIter++)
	{
	  PdbHbdModuleGain* hbdgain = (PdbHbdModuleGain*)&(hbdBank->getEntry(iEnt));
	  *hbdgain = gainConstIter->second;
	  if (Verbose>1) hbdBank->printEntry(iEnt);
	  iEnt++;
	}
      
      hbdBank->setUserName(PHString(std::getenv("USER")));
      application->commit(hbdBank);
      return True;
    }
  return False;
}

// Dump the old constant gains to text file. In cases where time dependent
// gains are not available, the old gains are used
//______________________________________________________________________
void hbdAdcCalib::dumpOldModuleGainToFile(const char *filename, int run)
{
  ofstream fout;
  fout.open(filename);
  fout << 0 << endl;
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();
  if(application->startRead())
    {
      PdbCalBank* hbdBank = bankManager->fetchBank("PdbHbdAdcBank", PdbBankID(3), "calib.hbd.adc", run);
      if(hbdBank)
	{
	  if(debug)
	    {
	      hbdBank->print();
	      cout << "# of channels = " << hbdBank->getLength() << endl;
	    }
       
	  PdbHbdAdc* hbdgain = (PdbHbdAdc*)&(hbdBank->getEntry(0));
	  for(int i=0;i<24;i++)
	    {
	      float gain_const=0,gain_err=0;
	      hbdgain->getGain(i, gain_const, gain_err);
	      fout << gain_const << endl;
	    }
	   delete hbdBank;
	 }
       else
	 {
	   cout << PHWHERE << "Unable to find module by module gains for run " << run << endl;
	 }
     }
  fout.close();
}

// read module gains from file where the start of validity range for a given 
// set of constants is formatted as two unsigned ints
//__________________________________________________________________________
void hbdAdcCalib::readModuleGainsFromFileTwoIntsFormat(const char *filename)
{
  // the text file should be formatted as follows
  // two integer start validity time values (more,less significant halves of clock bit respectively) 
  // followed by 24 float values of gain constants one value for each module each row will then be 
  // the set of gain constants for the 24 modules at a given time
  unsigned int moreSignif, lessSignif;
  float gain;
  std::vector<float> gains;
  ifstream fgains;
  fgains.open(filename);
  while(fgains)
    {
      fgains >> moreSignif >> lessSignif;
      if (Verbose>2)
	cout << "moreSignif= " << moreSignif << " lessSignif= " << lessSignif << endl;
      gains.clear();
      for (int ii=0; ii<24; ii++) {
	fgains >> gain;
	gains.push_back(gain);
      }
      addValidityStartClockTickAndModuleGain(moreSignif,lessSignif,gains);
    }
}

// read module gains from file where the start of validity range for a given 
// set of constants is formatted as an unsigned long long
//________________________________________________________________________
void hbdAdcCalib::readModuleGainsFromFileULongFormat(const char *filename)
{
  // the text file should be formatted as follows
  // one ULong64 start validity time value followed by 24 float values of gain constants one value 
  // for each module each row will then be the set of gain constants for the 24 modules at a given time
  ULong64_t clk_tick;
  float gain;
  std::vector<float> gains;
  ifstream fgains;
  fgains.open(filename);
  while(fgains)
    {
      fgains >> clk_tick;
      if (Verbose>2)
	cout << "clk_tick " << clk_tick << " gains: ";
      gains.clear();

      //
      bool goodRange =  true;
      int nDeadModule = 0;

      for (int ii=0; ii<24; ii++) {
	fgains >> gain;
	if (gain<0||gain>10000) goodRange = false;
	if (gain>1000) nDeadModule++;

	gains.push_back(gain);
	if (Verbose>2)
	  cout << gain << ", ";
      }
      if (Verbose>2)
	cout << endl;

      if (goodRange && nDeadModule<12)
	addValidityStartClockTickAndModuleGain(clk_tick,gains);
      else {
	cout << "Bad Range!!! \n";
      }
    }
}

// when reading constants from file, add a the start validity clock tick - module gain object
// pair to the map that is used to carry the constants in memory, and is accessed for correction
// and constant sumbission to DB. The clock tick is given as two unsigned int arguments
//__________________________________________________________________________________________________________________________________
void hbdAdcCalib::addValidityStartClockTickAndModuleGain(unsigned int moreSignif, unsigned int lessSignif, std::vector<float> gains)
{
  PdbHbdModuleGain modGain;
  modGain.setClockTick(moreSignif,lessSignif);
  modGain.setAllGains(gains);
  if (Verbose>0)
    {
      cout << "hbdAdcCalib::addVSCTAMG " << endl;
      modGain.Print();
    }
  ULong64_t tmp = joinTwoInts(moreSignif,lessSignif);
  clkTickGainMap.insert(pair<ULong64_t,PdbHbdModuleGain>(tmp,modGain));
}

// when reading constants from file, add a the start validity clock tick - module gain object
// pair to the map that is used to carry the constants in memory, and is accessed for correction
// and constant sumbission to DB. The clock tick is given as one unsigned long long
//____________________________________________________________________________________________________
void hbdAdcCalib::addValidityStartClockTickAndModuleGain(ULong64_t clk_tick, std::vector<float> gains)
{
  unsigned int moreSignif,lessSignif;
  splitULong64(clk_tick,moreSignif,lessSignif);
  addValidityStartClockTickAndModuleGain(moreSignif,lessSignif,gains);
}

// utility function to join two unsigned ints into one unsigned long long
//__________________________________________________________________________________
ULong64_t hbdAdcCalib::joinTwoInts(unsigned int moreSignif, unsigned int lessSignif)
{
  ULong64_t tmp = moreSignif;
  tmp = (tmp << 32) | lessSignif;
  return tmp;
}

// utility function to split one unsigned long long into two unsigned ints (more & less signif. halves)
//__________________________________________________________________________________________________
void hbdAdcCalib::splitULong64(ULong64_t oolong, unsigned int &moreSignif, unsigned int &lessSignif)
{
  moreSignif = oolong >> 32;
  lessSignif = static_cast<unsigned int>(oolong - (static_cast<ULong64_t>(moreSignif)<<32));
}

// print time dpendent module gains for diagnostics
//__________________________________
void hbdAdcCalib::printModuleGains()
{
  cout << "__________________printing module gains for run " << runnum << "__________________" << endl;
  cout << "_______ NumCalibSets (num of validity ranges for run " << runnum<< " ) = " << clkTickGainMap.size() << " __________" << endl;
  for (gainConstIter=clkTickGainMap.begin(); gainConstIter!=clkTickGainMap.end(); gainConstIter++)
    {
      const ULong64_t clk_tick = (*gainConstIter).first;
      unsigned int moreSignif, lessSignif;
      splitULong64(clk_tick, moreSignif, lessSignif);

      cout << "start validity clock tick = 0x" << hex << clk_tick << dec 
	   << " dec(" << clk_tick << ")"<< " moreSignif= 0x" << hex << moreSignif 
	   << dec << " lessSignif= 0x" << hex << lessSignif << dec << endl;

      vector<float> tmp;
      (gainConstIter->second).getAllGains(tmp);
      cout << "gains: ";
      for (size_t ii=0; ii<tmp.size(); ii++) cout << tmp.at(ii) << " ";
      cout << endl;
    }
}


void
hbdAdcCalib::set_Verbose(short setverb)
{
  Verbose = setverb;
}

short
hbdAdcCalib::get_Verbose()
{
  return Verbose;
}

void 
hbdAdcCalib::ModuleToChannel()
{
  //  This routine is silly.
  //  However, its the best way to not disturn the old architecture...
  //         TKH DS AI  3-14-2009
   hbdDetectorGeo t;
   t.fetchPad(runnum);
   
   for(int i=0;i<2304;i++){
      int seqsec,padid,arm,sec,side;
      side=0;

      t.getPadInfo(i,arm,sec,seqsec,padid);

      if(arm==0 && padid<=96)  //East south
         { side=0; }
      if(arm==1 && padid<=96)  //West north
         { side=1; }
      if(arm==0 && padid> 96)  //East north
         { side=1; }
      if(arm==1 && padid> 96)  //West south
         { side=0; }

      if (ApplyModByMod)
	ModuleGainByChan[i] = ModuleGain[arm*12+side*6+sec];
      if (ApplyTimeDepModByMod)
	ModuleGainByChan[i] = currentModuleGain->getGain((unsigned int) arm*12+side*6+sec);
   }

}


void 
hbdAdcCalib::print()
{
   for(int i=0; i<2304; i++){
      cout <<"ADCch: " << i;
      cout <<", pedstal: " << PedestalConst[i];
      cout <<", pederr: " << PedestalError[i];
      cout <<", gain: " << GainConst[i];
      cout <<", gainerr: " << GainError[i];
      cout <<", Module gain: " << ModuleGain[i/96];
      cout <<", Module gainerr: " << ModuleGainError[i/96];
      cout <<", P/T: " << PTConst[i/1152];
      cout <<", P/T err: " << PTError[i/1152];
      cout <<endl;
   }
}

void 
hbdAdcCalib::getGain(int ADCch, float& gain, float& gainerr)
{
   gain = GainConst[ADCch];
   gainerr = GainError[ADCch];
}

void 
hbdAdcCalib::setGain(int ADCch, float gain, float gainerr)
{
   GainConst[ADCch] = gain;
   GainError[ADCch] = gainerr;
}

void 
hbdAdcCalib::getModuleGain(int ADCch, float& gain, float& gainerr)
{
   gain = ModuleGain[ADCch];
   gainerr = ModuleGainError[ADCch];
}

void 
hbdAdcCalib::setModuleGain(int ADCch, float gain, float gainerr)
{
   ModuleGain[ADCch] = gain;
   ModuleGainError[ADCch] = gainerr;
}

void 
hbdAdcCalib::getPTgain(int arm, float& ptgain, float& ptgainerr)
{
   ptgain = PTConst[arm];
   ptgainerr = PTError[arm];
}

void 
hbdAdcCalib::setPTgain(int arm, float ptgain, float ptgainerr)
{
   PTConst[arm] = ptgain;
   PTError[arm] = ptgainerr;
}

void 
hbdAdcCalib::getPedestal(int ADCch, float& ped, float& pederr)
{
   ped = PedestalConst[ADCch];
   pederr = PedestalError[ADCch];
}

void 
hbdAdcCalib::setPedestal(int ADCch, float ped, float pederr)
{
   PedestalConst[ADCch] = ped;
   PedestalError[ADCch] = pederr;
}

//
// Apply calibration
//
void
hbdAdcCalib::ApplyCalib(int ADCch, int ADCval, float& charge)
{
  charge = static_cast<float>(ADCval-PedestalConst[ADCch]) + T->Rndm();
  if (ApplyChanByChan) {
    if (GainConst[ADCch]!=0)
      charge /= GainConst[ADCch];
    else
      charge = 0;
  }
  if (ApplyModByMod||ApplyTimeDepModByMod) {
    if (ModuleGainByChan[ADCch]!=0)
      charge /= ModuleGainByChan[ADCch];
    else
      charge = 0;
  }
}
