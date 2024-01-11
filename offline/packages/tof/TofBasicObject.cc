//-----------------------------------------------------------------------------
//  Implementation of class TofBasicObject
//
//  Author: Akio Kiyomichi (Univ.of Tsukuba)
//
//  History: 04/10/00  A.Kiyomichi  First Version
//           01/09/04  H.Masui Update Data base access
//
//-----------------------------------------------------------------------------

//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "PdbCalBank.hh" 
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "RunToTime.hh"
//INCLUDECHECKER: Removed this line: #include "PHTimeStamp.h"

#include "TofBasicObject.hh"

#include <iostream>

using namespace std;

const int TofBasicObject::TOFMAPBANK   = 7100;
const int TofBasicObject::TOFGEOMBANK  = 7200;
const int TofBasicObject::TOFCALIBBANK = 7300;

// Constructor
TofBasicObject::TofBasicObject()
 : Tsearch(0), CalibName(""), Geom0Name(""), Geom1Name(""), UserName(""), 
   BankID(0), BankNumber(0), iFlag(0), Debug(0)
{
}
// Destructor
TofBasicObject::~TofBasicObject()
{
}


// Get START Validity Time 
PHTimeStamp TofBasicObject::getStartValidity() { 
 
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class. 
  PdbApplication *application = bankManager->getApplication(); 

  cout << "Now opening FD in readonly mode.." << endl; 
  if (application->startRead()){

    PdbBankID bankID(BankNumber);

    PdbCalBank *tofBank;
    if (BankNumber==TOFMAPBANK)
      { 
	tofBank = bankManager->fetchBank("PdbIndexBank", bankID, CalibName.getString(), Tsearch);
      }
    else if (BankNumber==TOFGEOMBANK)
      {
	tofBank = bankManager->fetchBank("PdbCoordinateBank", bankID, CalibName.getString(), Tsearch);
      }
    else if (BankNumber==TOFCALIBBANK)
      {
	tofBank = bankManager->fetchBank("PdbCalChanBank", bankID, CalibName.getString(), Tsearch);
      }
    else
      {
        cout << PHWHERE << ": Illegal BankNumber!" << endl;
	tofBank = 0;
	return PHTimeStamp(2029,9,17,0,0,0);
      }

    PHTimeStamp Tval = tofBank->getStartValTime();
    if(Debug>0) cout << "getStartValidity(): "
		     << "Start Validity Time = ";
    Tval.print(); cout << endl;
 
    application->commit();
    return Tval;
  } else {
    application->abort(); 
    cerr << PHWHERE << " getStartValidity() " 
         << "ERROR -> Transaction aborted. Database NOT available." 
         << "^G" << endl; 
    return PHTimeStamp(2029,9,17,0,0,0);
  } 
} 
 
/// Get END Validity Time 
PHTimeStamp TofBasicObject::getEndValidity(){

  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class. 
  PdbApplication *application = bankManager->getApplication();

  if (application->startRead()){
    PdbBankID bankID(BankNumber);

    PdbCalBank *tofBank;
    if (BankNumber==TOFMAPBANK)
      { 
	tofBank = bankManager->fetchBank("PdbIndexBank", bankID, CalibName.getString(), Tsearch);
      }
    else if (BankNumber==TOFGEOMBANK)
      {
	tofBank = bankManager->fetchBank("PdbCoordinateBank", bankID, CalibName.getString(), Tsearch);
      }
    else if (BankNumber==TOFCALIBBANK)
      {
	tofBank = bankManager->fetchBank("PdbCalChanBank", bankID, CalibName.getString(), Tsearch);
      }
    else
      {
        cout << PHWHERE << ": Illegal BankNumber!" << endl;
	tofBank = 0;
	return PHTimeStamp(2029,9,17,0,0,0);
      }

    PHTimeStamp Tval = tofBank->getEndValTime();
    if(Debug>0) cout << "getEndValidity(): "
		     << "End Validity Time: ";
    Tval.print(); cout << endl;

    application->commit();
    return Tval;
  } else {
    application->abort();
    cerr << PHWHERE << " getEndValidity() "
         << "ERROR -> Transaction aborted. Database NOT available."
         << "^G" << endl;
    return PHTimeStamp(2029,9,17,0,0,0);
  }
}
 
/// Check if an object is valid at data member time Tsearch 
PHBoolean TofBasicObject::Validate(){
  return TofBasicObject::Validate( Tsearch );
}
 
/// Check if an object is valid at time TS 
PHBoolean TofBasicObject::Validate(PHTimeStamp& TS) {

  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class. 
  PdbApplication *application = bankManager->getApplication();

  if (application->startRead()) {
 
    PdbBankID bankID(BankNumber);

    PdbCalBank *tofBank;
    if(BankNumber==TOFMAPBANK)
      { 
	tofBank = bankManager->fetchBank("PdbIndexBank", bankID, CalibName.getString(), TS);
      }
    else if(BankNumber==TOFGEOMBANK)
      {
	tofBank = bankManager->fetchBank("PdbCoordinateBank", bankID, CalibName.getString(), TS);
      }
    else if(BankNumber==TOFCALIBBANK)
      {
	tofBank = bankManager->fetchBank("PdbCalChanBank", bankID, CalibName.getString(), TS);
      }
    else
      {
        cout << PHWHERE << ": Illegal BankNumber!" << endl;
	tofBank = 0;
	return False;
      }
    
    PHTimeStamp Tstart = tofBank->getStartValTime();
    PHTimeStamp Tstop = tofBank->getEndValTime();

    application->commit();

    if((TS>Tstart) && (TS<Tstop)) { return True; }
    else { return False; }
 
  } else {
    application->abort();
    cerr << PHWHERE << " Validate() "
         << "ERROR -> Transaction aborted. Database NOT available."
         << "^G" << endl;
    return False;
  }
}

PHTimeStamp TofBasicObject::getTimeStamp(const int run)
{

  if(run<0){
    cout << PHWHERE << " runNumber<0, set TimeStamp to far future..." << endl;
    PHTimeStamp ph;
    ph.setToFarFuture();
    return ph;
  }

  //auto_ptr<RunToTime> runtotime(new RunToTimePg);
  RunToTime *runtotime = RunToTime::instance();
  PHTimeStamp* ts(runtotime->getBeginTime(run));

  if(!ts){
    cout << PHWHERE << " TimeStamp was not in database for Run " << run << endl;
    return 0;
  }

  PHTimeStamp t = (*ts) + 5;
  if(Debug>0){
    cout << "Run TimeStamp was found for run : " << run << " ";
    t.print(); cout << endl;
  }

  delete ts;

  return t;
}


void TofBasicObject::Print()
{
  cout << " ---------------------------------------------------------------------- " << endl;
  cout << " Time stamp       : ";  Tsearch.print(); cout << endl;
  cout << " Calibration name : " << CalibName.getString() << endl;
  cout << " Geometry name 0  : " << Geom0Name.getString() << endl;
  cout << " Geometry name 1  : " << Geom1Name.getString() << endl;
  cout << " User name        : " << UserName.getString() << endl;
  cout << " Bank ID          : " << BankID.getInternalValue() << endl;
  cout << " Bank Number      : " << BankNumber << endl;
  cout << " Flag (0 is OK)   : " << iFlag << endl;
  cout << " Debug flag       : " << Debug << endl;
  cout << " ---------------------------------------------------------------------- " << endl;
}
