//$Id: EmcStaticData.C,v 1.6 2015/03/22 15:57:53 mazsi Exp $ 
//#############################################################################
// E.Kistenev         03/01/99 
// send comments to kistenev@bnl.gov 
//#############################################################################

#include <iostream> 
#include <cstdio>
#include <cstdlib>
#include <cassert>

#include "EmcStaticData.h" 
#include "PbScCalibrationData.h"
#include "PbGlCalibrationData.h"
#include "EmcSector.h"
#include "PbScSector.h"
#include "PbGlSector.h"
#include "emcException.h"

EmcStaticData * EmcStaticData::single       = 0 ;
int             EmcStaticData::access_count = 0 ;

//____________________________________________________________________
EmcStaticData::EmcStaticData()
{
  PbScData = NULL;
  PbGlData = NULL;
}

EmcStaticData::~EmcStaticData()
{
}

//____________________________________________________________________
EmcStaticData * EmcStaticData::buildEmcStaticData() 
{ 
  if(single==0) { 
    single   = new EmcStaticData(); 
    single->PbScData = 0 ;
    single->PbGlData = 0 ;
    // we build SM static data if, and only if, we
    // have some environment variables set.
    // FIXME: should we also test the emcDataManager::GetSourceDir()
    // here ?
    char* pbsc = getenv("EMCAL_DATA_PBSC") ;
    if (pbsc) {
      single->PbScData = new PbScCalibrationData(); 
    } else {std::cout<<"<EmcStaticData::buildEmcStaticData> Hardware DB EMCAL_DATA_PBSC not defined. Legacy data not collected "<<std::endl; }
    char* pbgl = getenv("EMCAL_DATA_PBGL") ;
    if (pbgl) {
      single->PbGlData = new PbGlCalibrationData();
    } else {std::cout<<"<EmcStaticData::buildEmcStaticData> Hardware DB EMCAL_DATA_PBGL not defined. Legacy data not collected "<<std::endl; }
    single->Sectors.resize(8) ;
    for(int iS = 0; iS<8; iS++) single->Sectors[iS] = 0;
  } 
  access_count ++; 
  return single; 
}; 
 
//____________________________________________________________________
int EmcStaticData::deleteEmcStaticData() 
{ 
  //  It would be appropriate to check access_count - decrease it by one 
  //  and decide if singleton must be deleted on the basis of result - but -
  //  how can we make sure that anyone who cared to ask for the pointer also
  //  delets the object associated to that pointer- for that reason
  //  access counter is returned and you are free to use it to judge your
  //  programming style 
  access_count--;
  if(single){
    if(access_count<=0){
      for (int i=0; i<7; i++) {
	delete single->Sectors[i];
      } 
      single->Sectors.resize(8);
      if(single->PbScData) delete single->PbScData; 
      if(single->PbGlData) delete single->PbGlData; 
      single->PbScData = NULL;
      single->PbGlData = NULL;
      delete single;
      single = 0;
    }
  }
  return access_count;
}; 
 
//____________________________________________________________________
void 
EmcStaticData::buildEmcSector(const char * sectorName, PHTimeStamp* ts) 
  //try 
{ 
  // This method is critical. If it fails, initial calibration
  // will _not_ be available, and thus, no meaningfull calibration
  // can be expected...

  int SectorNumber = EmcIndexer::EmcSectorNumber(sectorName);

  assert(0 <= SectorNumber && SectorNumber < single->Sectors.size());
  delete single->Sectors[SectorNumber] ; 
  single->Sectors[SectorNumber] = 0 ;

  EmcSector* sector ;

  if(SectorNumber==6 || SectorNumber==7) { 
      sector = new PbGlSector(SectorNumber,ts); 
  } else { 
      sector = new PbScSector(SectorNumber,ts); 
  } 
  if (sector->IsOK()) {
    single->Sectors[SectorNumber] = sector ;
    single->Sectors[SectorNumber]->setSectorId(sectorName) ;
  }
  else {
    delete sector ;
    //    single->Sectors[SectorNumber] = 0 ;
    //    throw emcException("<EmcStaticData::buildEmcSector> : failed sector build. Calibration will be nonsense !!!") ;
    //    std::cout<<"<EmcStaticData::buildEmcSector> : failed to collect hardware legacy data !!!"<<std::endl;
  }
} 
//catch (...) { throw; }













