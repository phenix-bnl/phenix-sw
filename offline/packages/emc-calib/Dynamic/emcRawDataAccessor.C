// Implementation of class : emcRawDataAccessor
//
// Author: Laurent Aphecetche (laurent.aphecetche@subatech.in2p3.fr)
//
//-------------------------------------------------------------------------
//
// Some notes about this Singleton's destruction and initialization.
// For convenience, initialization methods are also named GetInstance, but do have
// some parameters.
// In order to avoid (or at least to limit) dangling references,
// we only allow those initialization methods to be called when the internal
// instance is null, i.e. when nobody has such an instance in hand (this is inferred
// using reference-counting, which is far from being fullproof - e.g. same user can
// call ReleaseInstance twice...- , but that's better than
// nothing).
//
// So, the correct way of using this class is :
//
// emcRawDataAccessor* rda; 
// // Init
// rda = emcRawDataAccessor::GetInstance(PHTimeStamp(2000,1,12,0,0,0));
// // use it...
// void f1(void) {
//   emcRawDataAccessor* rda = emcRawDataAccessor::GetInstance();
//   ...
//   emcRawDataAccessor::ReleaseInstance();
// }
// void f2(void) {
//   emcRawDataAccessor* rda = emcRawDataAccessor::GetInstance();
//   ...
// }
//
// // Try to re-initialize it
// rda = emcRawDataAccessor::GetInstance(status,configfilename); // WRONG !
// // WRONG because the f2() function still holds a reference count on the previous instance.
// emcRawDataAccessor::ReleaseInstance(); // release last instance (fCount=0)
// rda = e mcRawDataAccessor::GetInstance(status,configfilename); // OK now.
//
//

#include "emcRawDataAccessor.h"
#include "emcDataManager.h"
#include "EmcDynamicData.h"
#include "emcRawDataObject.h"
#include "emcConfigurationFile.h"
#include <cassert>
#include <iostream>

using std::string;
using std::cout;
using std::cerr;
using std::endl;

emcRawDataAccessor* emcRawDataAccessor::fInstance = 0;
int emcRawDataAccessor::fCount = 0;
emcRawDataObject* emcRawDataAccessor::fRDO = 0;
EmcDynamicData* emcRawDataAccessor::fDynamicData = 0;
EMCalFEE* emcRawDataAccessor::fDataExtractor = 0;
Eventiterator* emcRawDataAccessor::fEventiterator = 0;
#ifndef __CINT__
string emcRawDataAccessor::fConfigFileName = ""; 
#endif

//_____________________________________________________________________________
emcRawDataAccessor::emcRawDataAccessor(int& status, 
		     const char* configfilename)		  
		  
{
  char* filename = const_cast<char*>(configfilename);
  fConfigFileName = configfilename;
  fDynamicData = new EmcDynamicData(status,filename,true);
  if (status==0) {
		//    FEMlimits lim;
    lim.minAmp = 20;
    lim.maxAmp = 4080;
    fDataExtractor = new EMCalFEE(fDynamicData,&lim,status);
  }
}

//_____________________________________________________________________________
emcRawDataAccessor::emcRawDataAccessor(int& status, emcConfigurationFile& configFile)
{
  fConfigFileName = "";
  fDynamicData = new EmcDynamicData(status,configFile,true);
  if (status==0) {
		//    FEMlimits lim;
    lim.minAmp = 5;
    lim.maxAmp = 4090;
    fDataExtractor = new EMCalFEE(fDynamicData,&lim,status);
  }
}

//_____________________________________________________________________________
emcRawDataAccessor::~emcRawDataAccessor()
{
  if(fDynamicData) delete fDynamicData;
  fDynamicData = 0;
  delete fDataExtractor;
  fDataExtractor = 0;
  delete fRDO;
  fRDO = 0;
  //  delete fEventiterator;
  fEventiterator = 0;
  fConfigFileName = "" ;
  assert(fCount==0);
}

//_____________________________________________________________________________
void emcRawDataAccessor::RemoveInstance(void)
{
  if (!fCount) return; // no instance 
  fCount = 0;
  delete fInstance;
  fInstance = 0;
}

//_____________________________________________________________________________
void emcRawDataAccessor::ReleaseInstance(void)
{
  if (!fCount) return; // no instance 

  fCount--;

  if (fCount) return; // instance still in use

  delete fInstance;

  fInstance = 0;
}

//_____________________________________________________________________________
emcRawDataAccessor* emcRawDataAccessor::GetInstance(void)
{
  if (fInstance==0) {
    cout << "<E> Singleton emcRawDataAccessor object should be first created"
	 << " by a call to one of the full GetInstance methods : "
	 << " (int,const char*) or (PHTimeStamp&)."
	 << endl;
    return 0;
  }
  else {
    fCount++;
    return fInstance;
  }
}

//_____________________________________________________________________________
emcRawDataAccessor* emcRawDataAccessor::GetInstance(const PHTimeStamp& ts)
{
  // Initialization using configuration file read from the DB.

  if (fCount) {
    cerr << "<E> emcRawDataAccessor::GetInstance(PHTimeStamp& ts) : reference count is not zero. Somebody is using one instance of this object. Release it before you can re-initialize it." << endl;
    return 0;
  }

  emcConfigurationFile config;
  emcConfigurationFile* configFile = 0;

  config.SetSource(emcManageable::kDB_Objy); 

  emcDataManager* dm = emcDataManager::GetInstance();

  configFile = static_cast<emcConfigurationFile*>(dm->Collect(config,ts));

  if (!configFile) {
    cerr << "<E> emcRawDataAccessor::GetInstance(...) : Cannot fetch configuration file for time stamp " 
	 << ts << endl;
    return 0;
  }
  else {
    int status;
    assert(fInstance==0);
    fInstance = new emcRawDataAccessor(status,*configFile);
    fCount++;
  }
  return fInstance;
}

//_____________________________________________________________________________
emcRawDataAccessor* emcRawDataAccessor::GetInstance(int& status, 
		     const char* configfilename)
		
{
  if (fCount) {
    cerr << "<E> emcRawDataAccessor::GetInstance(int&status,char* filename) : reference count is not zero. Somebody is using one instance of this object. Release it before you can re-initialize it." << endl;
    return 0;
  }
  assert (fInstance==0);
  fInstance = new emcRawDataAccessor(status,configfilename);
  fCount++;

  return fInstance;
}

//_____________________________________________________________________________
int emcRawDataAccessor::GetNextEvent(void)
{
  // Get the next event from the DataExtractor.
  // Note:
  // In case the RDO was already created, 
  // as soon as you have executed this method, the RDO is updated

  if (fEventiterator==0) {
    cerr << "<E> No data source selected." << endl;
    return false;
  }
  return fDataExtractor->getNextEvent(fEventiterator);
}

//_____________________________________________________________________________
emcRawDataObject * emcRawDataAccessor::GetRawDataObject(void)
{
  if (!fRDO) {
    cout << "<I> emcRawDataAccessor::GetRawDataObject : Allocating a new RDO" 
	 << endl;
    fRDO = new emcRawDataObject(fDynamicData->getEmcSize(),
				fDynamicData->getEmcMap(),
				(fDynamicData->getEmcRaw())[0],
				(fDynamicData->getEmcRaw())[1],
				(fDynamicData->getEmcRaw())[2],
				(fDynamicData->getEmcRaw())[3],
				(fDynamicData->getEmcRaw())[4],
				fDataExtractor->getCells(),
				fDataExtractor->getDataErrors()
				);
   }
  return fRDO;
}

//_____________________________________________________________________________
void emcRawDataAccessor::SetDataSource(Eventiterator* it)
{
  fEventiterator = it;
}
