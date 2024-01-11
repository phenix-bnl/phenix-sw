//--------------------------------------------------------------- 
//                                                                
// Created by: Sasha Lebedev (ISU) lebedev@iastate.edu 01/24/00
//                                                                
// Description: Implementation of TecBasicObject class
//                                                                
//----------------------------------------------------------------

#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "TecBasicObject.hh" 
#include "RunToTime.hh"
#include <iostream>

using namespace std;

// constructor
TecBasicObject::TecBasicObject() { }

// destructor
TecBasicObject::~TecBasicObject() { }

void TecBasicObject::setRunNumber(int runnumber) {
  SetRunNumber(runnumber);
  return;
}

PHBoolean 
TecBasicObject::SetRunNumber(int runnumber) 
{

  PHBoolean iret = False;
	RunToTime *runTime = RunToTime::instance();
	if (runTime)
		{
			PHTimeStamp *TimeStp = runTime->getBeginTime(runnumber);
			if (TimeStp)
				{
					PHTimeStamp startTime = *TimeStp;
					Tsearch = startTime+30;
					iret = True;
					delete TimeStp;
				}
			else
				{
					cerr << "TecBasicObject::setRunNumber ERROR: Can NOT get Time Stamp !!!" << endl;
					cerr << "TecBasicObject::setRunNumber ERROR: TimeStamp NOT set !!!" << endl;
				}
		}
	else
		{
			cout << "TecBasicObject::setRunNumber: RunToTime transaction aborted." << endl;
		}
return iret;
}

///
void TecBasicObject::setCalibName(const char* location) {

  CalibName = location;
  char c11,c12,c13,c21,c22,c23,c31,c32,c33;
  c11='r'; c12='e'; c13='l';
  c21='a'; c22='b'; c23='s';
  c31='t'; c32='i'; c33='m';
  unsigned int len = strlen(location);

    for(unsigned int i=0; i<len; i++) CalibNameA1[i] = location[i]; CalibNameA1[len]='\0';
    for(unsigned int i=0; i<len; i++) CalibNameA2[i] = location[i]; CalibNameA2[len]='\0';
    for(unsigned int i=0; i<len; i++) CalibNameT[i] = location[i]; CalibNameT[len]='\0';
    CalibNameA1[10]=c11; CalibNameA1[11]=c12; CalibNameA1[12]=c13;
    CalibNameA2[10]=c21; CalibNameA2[11]=c22; CalibNameA2[12]=c23;
    CalibNameT[10]=c31; CalibNameT[11]=c32; CalibNameT[12]=c33;

  if(Debug>0) {
    cout << "CalibName   = " << CalibName << endl;
    cout << "CalibNameA1 = " << CalibNameA1 << endl;
    cout << "CalibNameA2 = " << CalibNameA2 << endl;
    cout << "CalibNameT  = " << CalibNameT << endl;
  }

}

/// Get START Validity Time
PHTimeStamp TecBasicObject::getStartValidity() {


  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank=0;

  PHTimeStamp Tval(2029,9,17,0,0,0);

  if (application->startRead()) {

    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID;
    const char *calname = CalibName;
    int bankid = BankNumber;
    bankID.setInternalValue(bankid);

      if(BankNumber==TECMAPBANK) {
        tecBank = bankManager->fetchBank("PdbMapHSBank", bankID, calname, tSearch);
      }
      else if(BankNumber==TECGEOMBANK) {
        tecBank = bankManager->fetchBank("PdbCoordinateBank", bankID, calname, tSearch);
      }
      else if(BankNumber==TECCAL1BANK || BankNumber==TECCAL2BANK || BankNumber==TECCALTBANK) {
        tecBank = bankManager->fetchBank("PdbCalibrateBank", bankID, calname, tSearch);
      }
      else if(BankNumber==TECHVBANK) {
        tecBank = bankManager->fetchBank("PdbHVBank", bankID, calname, tSearch);
      }
      else {
        cerr << __FILE__ << ":" << __LINE__ << ": Illegal Bank Number " << BankNumber << endl;
        application->abort();
        return Tval;
      }

      if(tecBank) Tval = tecBank->getStartValTime();
      if(Debug>0) cout << "getStartValidity(): " << "Start Validity Time = "; Tval.print(); cout << endl;

    application->commit();
    if(tecBank) delete tecBank;
    return Tval;
  }
  else {
    application->abort();
    cerr << "getStartValidity() " << "ERROR -> Transaction aborted. Database NOT available." << endl;
    return Tval;
  }

}

/// Get END Validity Time
PHTimeStamp TecBasicObject::getEndValidity() {


  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank=0;

  PHTimeStamp Tval(1990,9,17,0,0,0);

  if (application->startRead()) {

    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID;
    const char *calname = CalibName;
    int bankid = BankNumber;
    bankID.setInternalValue(bankid);

      if(BankNumber==TECMAPBANK) {
        tecBank = bankManager->fetchBank("PdbMapHSBank", bankID, calname, tSearch);
      }
      else if(BankNumber==TECGEOMBANK) {
        tecBank = bankManager->fetchBank("PdbCoordinateBank", bankID, calname, tSearch);
      }
      else if(BankNumber==TECCAL1BANK || BankNumber==TECCAL2BANK || BankNumber==TECCALTBANK) {
        tecBank = bankManager->fetchBank("PdbCalibrateBank", bankID, calname, tSearch);
      }
      else if(BankNumber==TECHVBANK) {
        tecBank = bankManager->fetchBank("PdbHVBank", bankID, calname, tSearch);
      }
      else {
        cerr << __FILE__ << ":" << __LINE__ << ": Illegal Bank Number " << BankNumber << endl;
        application->abort();
        return Tval;
      }

      if(tecBank) Tval = tecBank->getEndValTime();
      if(Debug>0) cout << "getEndValidity(): " << "End Validity Time: "; Tval.print(); cout << endl;

    application->commit();
    if(tecBank) delete tecBank;
    return Tval;
  }
  else {
    application->abort();
    cerr << "getEndValidity() " << "ERROR -> Transaction aborted. Database NOT available." << endl;
    return Tval;
  }

}

/// Check if an object is valid at data member time Tsearch
PHBoolean TecBasicObject::Validate() {
  PHTimeStamp* TS = &Tsearch;
  return TecBasicObject::Validate(TS);
}

/// Check if an object is valid at time TS
PHBoolean TecBasicObject::Validate(PHTimeStamp* TS) {


  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbCalBank *tecBank=0;

  PHTimeStamp Tstart(2029,9,17,0,0,0);
  PHTimeStamp Tstop(1990,9,17,0,0,0);

  if (application->startRead()) {

    PHTimeStamp tSearch; tSearch = *TS;
    PdbBankID bankID;
    const char *calname = CalibName;
    int bankid = BankNumber;
    bankID.setInternalValue(bankid);

      if(BankNumber==TECMAPBANK) {
        tecBank=bankManager->fetchBank("PdbMapHSBank",bankID,calname,tSearch);
      }
      else if(BankNumber==TECGEOMBANK) {
        tecBank=bankManager->fetchBank("PdbCoordinateBank",bankID,calname,tSearch);
      }
      else if(BankNumber==TECCAL1BANK || BankNumber==TECCAL2BANK || BankNumber==TECCALTBANK) {
        tecBank=bankManager->fetchBank("PdbCalibrateBank",bankID,calname,tSearch);
      }
      else if(BankNumber==TECHVBANK) {
        tecBank=bankManager->fetchBank("PdbHVBank",bankID,calname,tSearch);
      }
      else {
        cerr << __FILE__ << ":" << __LINE__ << ": Illegal Bank Number " << BankNumber << endl;
        application->abort();
        return False;
      }

    if(tecBank) {
      Tstart = tecBank->getStartValTime();
      Tstop = tecBank->getEndValTime();
    }

    application->commit();

    if(tecBank) delete tecBank;

    if((tSearch>Tstart) && (tSearch<Tstop)) { return True; }
      else { return False; }

  }
  else {
    application->abort();
    cerr << "Validate() " << "ERROR -> Transaction aborted. Database NOT available." << endl;
    return False;
  }

}




