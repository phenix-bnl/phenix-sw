#include "emcPgStorageManager.h"

#include <cstdlib>
#include <iostream>
#include "emcDataManager.h"
#include "PdbBankManagerFactory.hh"
#include "PHTimeStamp.h"
#include "PdbApplication.hh"

using namespace std;

namespace 
{
  emcDataManager* DM()
  {
    return emcDataManager::GetInstance();    
  }
}

//_____________________________________________________________________________
emcPgStorageManager::emcPgStorageManager()
{
  // default ctor: nop.
}

//_____________________________________________________________________________
emcPgStorageManager::~emcPgStorageManager()
{
}

//_____________________________________________________________________________
PdbBankManager* 
emcPgStorageManager::BankManager()
{
  static PdbBankManager* fBankManager = 
    PdbBankManagerFactory::instance().create("Pg");
  if (!fBankManager)
    {
      std::cerr << PHWHERE << "Cannot create Pg BankManager!!!"
		<< std::endl;
      exit(1);
    }
  return fBankManager;
}

//_____________________________________________________________________________
PdbApplication* 
emcPgStorageManager::DBApplication()
{
  static PdbApplication* fDBApplication = BankManager()->getApplication();
  return fDBApplication;
}

//_____________________________________________________________________________
PdbCalBank*
emcPgStorageManager::fetchBank(const char *className,
			       PdbBankID bankID, const char *bankName,
			       PHTimeStamp &searchTime,
			       string)
{
#ifdef DEBUG
  std::cout << PHWHERE << className << " " 
	    << bankID.getInternalValue()
	    << " " << bankName << " "
	    << searchTime
	    << std::endl;
#endif

  return BankManager()->fetchBank(className,bankID,bankName,searchTime);
}

//_____________________________________________________________________________
PdbCalBank*
emcPgStorageManager::fetchBank(const char *className,
			       PdbBankID bankID, const char *bankName,
			       PHTimeStamp &searchTime)
{
  std::cout << PHWHERE << className << " " 
	    << bankID.getInternalValue()
	    << " " << bankName << " "
	    << searchTime
	    << std::endl;

  return BankManager()->fetchBank(className,bankID,bankName,searchTime);
}

//_____________________________________________________________________________
void
emcPgStorageManager::Reset()
{
}
