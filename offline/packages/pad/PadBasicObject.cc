// Author: David Silvermyr

#include "PadBasicObject.hh" 

#include <PdbBankManager.hh>
#include <PdbApplication.hh>

#include <string>

using namespace std;

static 
bool
numberToName(const int& BankNumber, string &calType)
{
  switch (BankNumber)
    {
    case 4100:
      calType = "PdbPadBadChBank";
      break;
    case 4101:
      calType = "PdbPadBadROCBank";
      break;
    case 4102:
      calType = "PdbPadROCCalBank";
      break;
    case 4103:
      calType = "PdbPadROCPosBank";
      break;
    case 4104:
      calType = "PdbPadGeoChamBank";
      break;
    case 4105:
      calType = "PdbPadGeoParBank";
      break;
    case 4106:
      calType = "PdbPadHVBank";
      break;
    default:
      return false;
    }

  return true;
}

PadBasicObject::PadBasicObject():
  BankNumber(0),
  iFlag(-1),
  Debug(0)
{}

// Get START Validity Time
PHTimeStamp 
PadBasicObject::getStartValidity() 
{
  PdbCalBank *padBank;
   
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PHTimeStamp Tval;
  if (application->startRead()) 
    {
      PHTimeStamp tSearch = Tsearch;
      PdbBankID bankID;
      char *calname = CalibName.getString();
      bankID.setInternalValue(BankNumber);
      string calType;

      if (!numberToName(BankNumber, calType))
	{
	  Tval.setToFarFuture(); // not valid now..
	  cout << __FILE__ << ":" << __LINE__ 
	       << ": Unknown bank number requested!" << endl;
	  return Tval;
	}
      padBank = bankManager->fetchBank(calType.c_str(), bankID, 
				       calname, tSearch);
      Tval = padBank->getStartValTime();
      application->commit();		

    }
  else 
    {
      application->abort();
      Tval.setToFarFuture(); // not valid now..
      cerr << "getStartValidity() "
	   << "ERROR -> Transaction aborted. Database NOT available."
	   << endl;
    }

  return Tval;
}

// Get END Validity Time
PHTimeStamp 
PadBasicObject::getEndValidity() 
{
  PdbCalBank *padBank;
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  PHTimeStamp Tval;
  if (application->startRead()) 
    {
      PHTimeStamp tSearch = Tsearch;
      PdbBankID bankID;
      char *calname = CalibName.getString();
      bankID.setInternalValue(BankNumber);
      string calType;
    
      if (!numberToName(BankNumber, calType))
	{
	  Tval.setToFarFuture(); // not valid now..
	  cout << __FILE__ << ":" << __LINE__ 
	       << ": Unknown bank number requested!" << endl;
	  return Tval;
	}
      padBank = bankManager->fetchBank(calType.c_str(), bankID, 
				       calname, tSearch);
      Tval = padBank->getEndValTime();
      application->commit();

  }
  else {
    application->abort();
    Tval.set(1999,12,31,23,59,59,0); // not fit for the new millenium..
    cerr << "getEndValidity() "
         << "ERROR -> Transaction aborted. Database NOT available."
         << "^G" << endl;
  }
  return Tval;
}

// Check if an object is valid at data member time Tsearch
PHBoolean
PadBasicObject::Validate() 
{
  PHTimeStamp* TS = &Tsearch;
  return PadBasicObject::Validate(TS);
}

// Check if an object is valid at time TS
PHBoolean 
PadBasicObject::Validate(PHTimeStamp* TS) 
{
  PdbCalBank *padBank;
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (application->startRead()) 
    {
      PHTimeStamp tSearch; tSearch = *TS;
      PdbBankID bankID;
      char *calname = CalibName.getString();
      bankID.setInternalValue(BankNumber);
      string calType;
      
      if (!numberToName(BankNumber, calType))
	{
	  cout << __FILE__ << ":" << __LINE__ 
	       << ": Unknown bank number requested!" << endl;
	  return False;
	}
      padBank = bankManager->fetchBank(calType.c_str(), bankID, 
				       calname, tSearch);
      PHTimeStamp Tstart = padBank->getStartValTime();
      PHTimeStamp Tstop = padBank->getEndValTime();
      
      application->commit();
      
      if (tSearch > Tstart && tSearch < Tstop) 
	{
	  return True; 
	}
    }
  else 
    {
      application->abort();
      cerr << "Validate() "
	   << "ERROR -> Transaction aborted. Database NOT available."
	   << endl;
    }

 return False;
}
