#ifndef __EMCPGSTORAGEMANAGER_H__
#define __EMCPGSTORAGEMANAGER_H__

#include "emcManageable.h"
#include "PHTimeStamp.h"
#include "PdbCalBank.hh"
#include "PHString.h"
#include <cassert>

class PdbBankManager;
class PdbBankID;
class PHTimeStamp;
class PdbApplication;

class emcPgStorageManager
{
 public:

  emcPgStorageManager();
  ~emcPgStorageManager();

  /// Give access to the bank manager (to create/fetch banks)
  PdbBankManager* BankManager();

  /// Give access to the DB Application (to start/commit transaction)
  PdbApplication* DBApplication();

  /** Update the validity periods of banks with id=bankID,
      and valid at time tStart, prior to insert the new bank. */
  PdbCalBank* createBankWithUpdate(const char* className, 
				   PdbBankID bankID, 
				   const char* description, 
				   PHTimeStamp& tStart, 
				   PHTimeStamp& tEnd, 
				   const char* bankName) 
  { assert(0==1); return 0; }
  
  /** in pg, equivalent to the next one (at least once we figure out
      if we need to speed things up -hopefully we won't ;-) )
  */
  PdbCalBank* fetchBank(const char *, PdbBankID, const char *, 
			PHTimeStamp &, std::string category);

  PdbCalBank* fetchBank(const char *, PdbBankID, const char *, 
			PHTimeStamp &);
  
  PdbCalBank* fetchPreviousVersionBank(const char *, PdbBankID, const char *, 
				       PHTimeStamp &, std::string,
				       int )
  { return 0; }

  void Reset();

  static emcManageable::EStorage storage() 
  { return emcManageable::kDB_Pg; }

  /** Getting the class name might be useful for debugging, and as
      such is needed by classes that uses this one as a template
      paramater
  */
  static const char* name() { return "emcPgStorageManager"; }

private:

  //  PHString getRealName(const PHString & searchName);

};

#endif
