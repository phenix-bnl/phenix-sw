#include "SvxMap.h"

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbInt.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>

#include <PHTimeStamp.h>

#include <string>
#include <stdlib.h>

using namespace std;


bool SvxMap::commitPixelPacketMap(const char* description, PHTimeStamp tStart, PHTimeStamp tStop, int size, int* array){
  string s_dbname("calibvtxppacketmap");

  return commitDBIntArray(s_dbname.c_str(), description, tStart, tStop, size, array);
}

bool SvxMap::commitDBIntArray(const char *tablename, const char *description, 
                          PHTimeStamp tStart, PHTimeStamp tStop, int size, int* array){

  PdbBankManager *bankManager = PdbBankManager::instance();
  if (!bankManager) {
      cout << PHWHERE<< "Could not get instance of PdbBankManager, exiting" << endl;
      exit(1);
  }


  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate()) {
      cout << PHWHERE << "Aborting ... Database not readable" << endl;
      application->abort();
      return false;
  }

  string s_dbname(tablename);
  string s_description(description);

  //  Make a bank ID...
  PdbBankID bankID(0); // lets start at zero
  PdbCalBank *newBank = bankManager->createBank("PdbIntBank",
                                                bankID,
                                                s_description.c_str(),
                                                tStart, tStop,
                                                s_dbname.c_str());
  if (newBank) {
    newBank->setLength(size);
    for (unsigned int i = 0; i < newBank->getLength(); i++) {
      PdbInt *val = (PdbInt *) & (newBank->getEntry(i));
      val->setValue(array[i]);
    }
    application->commit(newBank);
    delete newBank;
  } else {
      cout << PHWHERE << " PdbIntBank was not created, calibration failed" << endl;
      return false;
  }
  cout << "Comitted" << endl;

  return true;
}


bool SvxMap::fetchPixelPacketMap(PHTimeStamp tStart, int *size, int** array){
  string s_dbname = "calibvtxppacketmap";

  return fetchDBIntArray(s_dbname.c_str(), tStart, size, array);
}

bool SvxMap::fetchDBIntArray(const char *tablename, PHTimeStamp tStart, int *size, int** array){
  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      cout << "Aborting ... Database not readable" << endl;
      application->abort();
      return false;
    }

  string s_dbname(tablename);

  PdbBankID bankID(0);
  PdbCalBank *Bank = bankManager->fetchBank("PdbIntBank",
                                            bankID,
                                            s_dbname.c_str(),
                                            tStart);

  if (Bank) {
    *size = Bank->getLength();
    (*array) = new int[*size];
    for (int i = 0; i < *size; i++)
      {
        PdbInt *val = (PdbInt *) & Bank->getEntry(i);
        (*array)[i] = val->getValue();
      }
    //TStart = Bank->getStartValTime();
    //TStop = Bank->getEndValTime();
    delete Bank;
  }
  else {
    cout << "Failed to get bunch crossing info from DB" << endl;
    return false;
  }

  return true;
}

