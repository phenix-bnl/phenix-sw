//$Id: PbScSector.C,v 1.14 2015/03/22 15:57:53 mazsi Exp $ 
//#############################################################################
// E.Kistenev         6/10/99 
// send comments to kistenev@bnl.gov 
//#############################################################################

#include <iostream>
#include <cstdio>

#include "EmcStaticData.h"
#include "PbScCalibrationData.h"
#include "PbScSuperModule.h"
#include "PbScIndexer.h"
#include "PbScSector.h"
#include "emcDataManager.h"
#include "emcCalibrationData.h"
#include "emcDefines.h"
#include <cassert>
#include <fstream>
#include <string>

using std::cout ;
using std::cerr ;
using std::endl ;
using std::ifstream ;
using std::string;

size_t PbScSector::fgSize = 2592 ;
size_t PbScSector::fgNumberOfSuperModules = 18 ;

//_____________________________________________________________________________
PbScSector::PbScSector(int & SectorNumber, PHTimeStamp* ts) 
{
  SectorN = SectorNumber ;

  fECalib.resize(fgSize) ;
  fNorm0.resize(fgSize) ;

  SM.resize(fgNumberOfSuperModules) ;
  for (size_t i = 0 ; i < fgNumberOfSuperModules ; i++ ) SM[i]=0;

  string source = "Files";
  if (ts) {
    BuildFromDB(ts) ;
    source = "DB";
  }
  else {
    BuildFromFiles() ;
  }

  if (!fIsOK) { // the fIsOK flag is set by the BuildFromXXX methods
    //    cerr << EMC_ERROR_MSG << "<PbScSector::PbScSector> : Sector "
    //	 << SectorNumber << " failed in build from "
    //	 << source << endl;
    Reset() ;
  }
}

//_____________________________________________________________________________
PbScSector::~PbScSector() 
{ 
  Reset() ;
}

//_____________________________________________________________________________
void 
PbScSector::Reset(void)
{
  for (size_t i = 0 ; i < SM.size() ; i++ ) {
    delete SM[i]; 
  }
  SM.clear() ;

  for (size_t i = 0 ; i < fECalib.size() ; i++ ) {
    fECalib[i] = 0.0 ;
  }

  for (size_t i = 0 ; i < fNorm0.size() ; i++ ) {
    fNorm0[i] = 0.0 ;
  }

  fIsOK = false ;
}

//_____________________________________________________________________________
void
PbScSector::BuildFromFiles(void)
{ 
  EmcStaticData* gEmcStaticData = EmcStaticData::buildEmcStaticData() ;
  PbScCalibrationData* PbScData = gEmcStaticData->getPbScData() ;
  
  if (PbScData==0) {
    fIsOK = false ;
  }
  else if (PbScData->getStatus()==false) {
    fIsOK = false ;
  }
  else {

    PbScIndexer* gPbSc = PbScIndexer::buildPbScIndexer() ;

    int j = fgNumberOfSuperModules*SectorN; 

    for (int iSM=0; iSM<static_cast<int>(fgNumberOfSuperModules); iSM++){ 
      
      int SMId = PbScData->getSMId(j+iSM); 
    
      SM[iSM] = new PbScSuperModule(SMId); 
      
      if (SM[iSM]->LoadSMData()) { 
	//  convert calibration data from 902 format to that suited for  
	//  analysis chain 
	for (int iSMT=0; iSMT<144; iSMT++){ 

	  int iS = gPbSc->SMiSMTiST(iSM, iSMT); 

	  // FACTOR 1.1 is responsible for the N2-to-YAG laser swap
	  //	PbScECalib[iS] = 0.038/SM[iSM]->getMuPeak(iSMT)*1.1; 
	  // Corrected based upon Edwards studies, Nov. 28, 2000
	  // Nov. 29, 2000 G. D.
	  fECalib[iS] = 0.038/SM[iSM]->getMuPeak(iSMT)*1.026; 
	  fNorm0[iS]  = SM[iSM]->getLaserRaw(iSMT)/ 
	    SM[iSM]->getIntSPD()* 
	    SM[iSM]->getIntSPDTP(); 
	} 
      }
    }
    fIsOK = true ;
  }
}

//_____________________________________________________________________________
void 
PbScSector::BuildFromDB(PHTimeStamp* when)
{
  emcCalibrationData* inical ; 
  emcCalibrationData driver(emcCalibrationData::kIniCal,SectorN) ;
  driver.SetSource(emcManageable::kDB_Objy);
  emcDataManager* dm = emcDataManager::GetInstance() ;

  inical =  dynamic_cast<emcCalibrationData*>(dm->Collect(driver,*when)) ;

  if (!inical) {
    fIsOK = false ;
    cerr << EMC_ERROR_MSG << "PbScSector::BuildFromDB() : Failed to fetch "
	 << "initial calibration data " 
	 << " for sector " << SectorN << " at time " << *when << endl ;
  }
  else {

    size_t iSM ;
    size_t iSMT ; 

    PbScIndexer* gPbSc = PbScIndexer::buildPbScIndexer() ;

    for ( iSM = 0 ; iSM < fgNumberOfSuperModules ; iSM++ ) { 
      for ( iSMT = 0 ; iSMT < 144 ; iSMT++ ) { 
	int iS = gPbSc->SMiSMTiST(iSM, iSMT) ; 
	fECalib[iS] = inical->GetValue(iS,0) ;
	fNorm0[iS] = inical->GetValue(iS,1) ;
      }
    }
    fIsOK = true ;   
  }
}
 
//_____________________________________________________________________________
void 
PbScSector::GetEnergyCalibration(int iTower, float& EScale, 
				 float& Normalization, float& Nothing)
{ 
  // This method should only be called after checking
  // that this sector is OK (using the IsOK() method).
  
  assert(iTower>=0 && static_cast<size_t>(iTower)<fECalib.size());
  assert(iTower>=0 && static_cast<size_t>(iTower)<fNorm0.size());

  EScale = fECalib[iTower]; 
  Normalization = fNorm0[iTower]; 
  Nothing = 1.0 ;
}
  
//_____________________________________________________________________________
void 
PbScSector::CorrectEnergyCalibration(const char * fname)
{
  // LOAD AND APPLY CORRECTION COEFFICIENTS BASED UPON MIP PEAKS 
  // IN INDIVIDUAL SUPERMODULES.
  // This method should only be called after checking
  // that this sector is OK (using the IsOK() method).
  
  ifstream fin;

  fin.open(fname,std::ios::in);

  if (!fin) {
    cerr << EMC_ERROR_MSG << "File " << fname 
	 << " not found - proceed with original calibration" << endl ;
    fIsOK = false ;
  }
  else {
    int sect, smz, smy, status, ii;
    float corr, eCorr ;
    //  Skip title line
    char c;
    while(fin.get(c)&&c!='\n');
    while(fin>>smz>>smy>>sect>>ii>>corr>>eCorr>>status){
      smz /=-12; smz += 5; smy /=12;
      cout<<sect<<" "<<smz<<" "<<smy<<" "<<corr<<" "<<eCorr<<" "<<status<<endl;
      if(!status||sect!=SectorN) continue;
      int iSM = smy*6+smz;
      for(int iCh=0; iCh<144;iCh++) {
	int iTwr = EmcIndexer::iSiSMiSMTiST(SectorN, iSM, EmcIndexer::iCHiSMT(iCh));
	assert(0 <=iTwr && iTwr < fECalib.size());
	fECalib[iTwr] *=corr;
      }
    }
    fin.close();
  }
}
