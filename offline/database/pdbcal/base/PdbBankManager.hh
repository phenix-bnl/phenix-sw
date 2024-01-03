//  Declaration of class PdbBankManager
//  Purpose: Abstract factory class to handle banks
//  Author: Matthias Messer

#ifndef __PDBBANKMANAGER_HH__
#define __PDBBANKMANAGER_HH__

#include <PdbBankID.hh>
#include <PdbBankID2.hh>
#include <PHTimeStamp.h>
#include <ctime>
#include <phool.h>

#include <map>
#include <set>
#include <string>

class PdbCalBank;
class PdbApplication;
class PdbBankList;
class PdbCalBankIterator;

class PdbBankManager 
{

protected:

  PdbBankManager();
  virtual ~PdbBankManager();

public:

  static  PdbBankManager *instance();

  /// Get an iterator to loop over banks.
  virtual PdbCalBankIterator* getIterator() = 0;

  virtual PdbCalBank* createBank(const char *,
				 PdbBankID,
				 const char *,
				 PHTimeStamp &,
				 PHTimeStamp &,
				 const char *) = 0;
  virtual PdbCalBank* createBank(const char *,
				 PdbBankID2,
				 const char *,
				 PHTimeStamp &,
				 PHTimeStamp &,
				 const char *) = 0;



  // create bank with run number as key
  virtual PdbCalBank* createBank(const int,
				 const char *,
				 PdbBankID,
				 const char *,
				 const char *,
				 const time_t duration=60) = 0;

  virtual PdbCalBank* createBank(const int,
				 const char *,
				 PdbBankID2,
				 const char *,
				 const char *,
				 const time_t duration=60) = 0;


  // create bank for a given range of run numbers rather than timestamps
  virtual PdbCalBank* createBank(const int,
				 const int,
				 const char *,
				 PdbBankID,
				 const char *,
				 const char *) = 0;
  
 virtual PdbCalBank* createBank(const int,
				 const int,
				 const char *,
				 PdbBankID2,
				 const char *,
				 const char *) = 0;

  virtual PdbCalBank* fetchBank(const char *,
				PdbBankID,
				const char *,
				const int) = 0;

  virtual PdbCalBank* fetchBank(const char *,
				PdbBankID2,
				const char *,
				const int) = 0;


  virtual PdbCalBank* fetchClosestBank(const char *,
				       PdbBankID,
				       const char *,
				       const int) = 0; 
  // virtual void fetchAllBanks(PdbBankList &,
  // 			     const char *,
  // 			     PdbBankID,
  // 			     const char *,
  // 			     const int) = 0;

  // virtual void fetchAllBanks(PdbBankList &,
  // 			     const char *,
  // 			     const char *,
  // 			     const int) = 0;

  virtual PdbCalBank* fetchBank(const char *,
				PdbBankID,
				const char *,
				const PHTimeStamp &) = 0;


  virtual PdbCalBank* fetchBank(const char *,
				PdbBankID2,
				const char *,
				const PHTimeStamp &) = 0;

  virtual PdbCalBank* fetchClosestBank(const char *,
				       PdbBankID,
				       const char *,
				       PHTimeStamp &) = 0;  

  // virtual void fetchAllBanks(PdbBankList &,
  // 			     const char *,
  // 			     PdbBankID,
  // 			     const char *,
  // 			     PHTimeStamp &) = 0;

  // virtual void fetchAllBanks(PdbBankList &,
  // 			     const char *,
  // 			     const char *,
  // 			     PHTimeStamp &) = 0;

  virtual PdbApplication* getApplication(PHBoolean pJob = False) = 0;

  virtual void fillCalibObject(PdbCalBank*,
			       const char *,
			       PHTimeStamp &) = 0;

  virtual void GetUsedBankRids(std::map<std::string,std::set<int> > &usedbanks) const {}
  virtual void ClearUsedBankRids() {}
  virtual  void SetMaxInsertTime(const PHTimeStamp &tMax) {}

protected:

  static  PdbBankManager *__instance; 


};

#endif /* __PDBBANKMANAGER_HH__ */
