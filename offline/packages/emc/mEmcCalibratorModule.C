//-------------------------------------------------------------------------
// $Header: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/packages/emc/mEmcCalibratorModule.C,v 2.31 2009/08/21 03:58:07 pinkenbu Exp $
// 
// Package: offline/packages/emc
// 
// Copyright (C) PHENIX collaboration, 2000 
//
// Implementation of class : mEmcCalibratorModule.h
//
// Gines MARTINEZ SUBATECH, Broohaven Aug-00
// Last modification : 08-Feb-2001, L. Aphecetche
// Last modification : 22-May-2001, A.Bazilevsky
//                     channel dead map propogated to dEmcCalibTower table
//-------------------------------------------------------------------------

#include <Fun4AllReturnCodes.h>
#include "mEmcCalibratorModule.h"
#include "emcRawDataAccessor.h"
#include "emcDataFormatter.h"
#include "emcRawDataObject.h"
#include "emcCalibratedDataObject.h"
#include "emcCalibratorFactory.h"
#include "emcCalibrator.h"
#include "dEmcCalibTowerWrapper.h"
#include "EmcDynamicData.h"


#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHDataNodeIterator.h"
#include "Event.h"

#include <iostream>

using namespace std;

//-------------------------------------------------------------------------
mEmcCalibratorModule::mEmcCalibratorModule(const char* configfilename,  
					   const char* calibrator,
					   emcManageable::EStorage source):
  SubsysReco("mEmcCalibratorModule")
{ 
  // default ctor
  Init(configfilename,calibrator,source);
}

//-------------------------------------------------------------------------
mEmcCalibratorModule::mEmcCalibratorModule(const PHTimeStamp& ts,
					   const char* calibrator,
					   emcManageable::EStorage source):
  SubsysReco("mEmcCalibratorModule")
{ 
  // default ctor
  Init(ts,calibrator,source);
}

//-------------------------------------------------------------------------
int mEmcCalibratorModule::process_event(PHCompositeNode *root) 
{  
  emcDataFormatter fDataFormatter;
  int j,arm,sector,yrow,zrow;
  int temp1;
  int type = 1;
  float energy,tof;

  // MV 2001/09/25
  float adc, tac;

  // should be provided by rdo 
  PHTimeStamp*  when ;
  
  PHNodeIterator i(root);
  PHDataNodeIterator DNiter(root);
  
  if (fVerbose > 0) 
    cout << "mEmcCalibrator>>>  Starting..." << endl;

  // Get Raw Data from the PRDF Node 
  if (fVerbose > 0) 
    cout << "mEmcCalibrator>>> Getting raw data from PRDF node " << endl;
  PHDataNode<Event> * thisEventNode =  (PHDataNode<Event>*) (i.findFirst("PHDataNode","PRDF")) ;

 // Set up dEmcCalibTower and fill it 
  if (fVerbose > 0) 
    cout << "mEmcCalibrator>>> Getting raw data from PRDF node dstNode " << endl;
  PHIODataNode<PHTable>* dEmcCalibTowerNode = (PHIODataNode<PHTable>*)i.findFirst("PHIODataNode","dEmcCalibTower");
  dEmcCalibTowerWrapper * dEmcCalibTower = static_cast<dEmcCalibTowerWrapper*>(dEmcCalibTowerNode->getData());

 // Proccessing the event
 if(fVerbose>0) 
   {
     cout << "mEmcCalibrator>>> Processing event ..." << endl;
   }
 fDataFormatter.fillRDO(thisEventNode->getData());
 if (fVerbose)
   {
     cout << "RDO size=" << fRdo->GetSize() << endl;
   }

 if (!fUseTimeStamp) {
   when =  new PHTimeStamp(thisEventNode->getData()->getTime());
 }
 else {
   when = &fTimeStamp ;
 }

 //Doing Calibration
 if (fVerbose > 0) 
   cout << "mEmcCalibrator>>> Doing Calibration " << endl;
 PHIODataNode<emcCalibratedDataObject>* CdoNode = 0; 
 CdoNode = DNiter.FindIODataNode(CdoNode,"EmcCdo") ; 
 fCdo = CdoNode->getData();
 fRdc->Calibrate(*fRdo, fMdo, *when);
 if (fVerbose)
   {
     cout << "MDO size=" << fMdo.GetSize() << endl;
   }

 fRdc->Calibrate(fMdo, *fCdo, *when);

 if (fVerbose)
   {
     cout << "CDO size=" << fCdo->GetSize() << endl;
   }

 if (fRdc->GetCollectionStatus("*")==false) {
   cerr << "<E><I> Disabling collection of calibration parameters !!!" << endl ;   
 } 

 if (!fUseTimeStamp) {
   delete when;
 }
 
 long softkey ;
 short outrow = 0 ;
 int dead ;
 int warn; // MV 2001/12/08

 for(j = 0 ; j < fCdo->GetSize(); j++) {

   fCdo->Get( j, energy, tof) ;
   softkey = fCdo->GetSoftwareKey(j) ;
   dead = fCdo->GetDead(j) ;

   // MV 2001/12/08

   warn=fCdo->GetWarn(j);

   if(energy > 0.001) {
     arm = softkey / 100000 ;
     sector = (softkey - arm * 100000) / 10000 ;
     temp1 = softkey - arm * 100000 - sector * 10000 ;
     yrow = temp1 / 100 ;
     zrow = temp1 - yrow * 100 ;

     // fill the CalibTower table from cdo
     dEmcCalibTower->set_id(outrow,outrow) ;
     dEmcCalibTower->set_hwkey(outrow,0) ;
     dEmcCalibTower->set_swkey(outrow,softkey) ;
     dEmcCalibTower->set_type(outrow,type) ;
     dEmcCalibTower->set_arm(outrow,arm) ;
     dEmcCalibTower->set_sector(outrow,sector) ;
     dEmcCalibTower->set_ind(1,outrow,yrow) ;
     dEmcCalibTower->set_ind(0,outrow,zrow) ;
     dEmcCalibTower->set_ecal(outrow,energy) ;
     dEmcCalibTower->set_tof(outrow,tof) ;
     dEmcCalibTower->set_deadmap(outrow,dead) ;
     // MV 2001/12/06
     dEmcCalibTower->set_warnmap(outrow, warn);

     // MV 2001/09/24
     int towerID=fCdo->GetItemId(j);
     fIndexMapIter=fIndexMap.find(towerID);
     if(fIndexMapIter == fIndexMap.end()){
       
       cerr<<"<E>: mEmcCalibratorModule::Event(): index for towerID="<<towerID<<" not found"<<endl;
       adc=0.;
       tac=0.;
       
     } else{

       int index=fIndexMap[towerID];
       fMdo.Get(index, adc, tac);

     }

     dEmcCalibTower->set_adc(outrow, adc);
     dEmcCalibTower->set_tac(outrow, tac);

     outrow++ ;

     if (fVerbose>1) {
       cout << "dEmcCalibTower(id,hwkey,swkey,type,arm,sector,yrow,zrow,ecal,tof, adc, tac) = ("
	    << outrow << "," << 0 << "," 
	    << softkey<< "," 
	    << type << "," 
	    << arm << "," 
	    << sector<< "," 
	    << yrow<< "," 
	    << zrow<< "," 
	    << energy<< ","   
	    << tof<<","
	    << adc<<","
	    << tac<<")" 
	    << endl;
     }
   }
 }
 dEmcCalibTower->SetRowCount(outrow);

  if ( fVerbose ) 
   {
     cout << "dEmcCalibTower size=" << outrow << endl;
   }
  return EVENT_OK;
}

//-------------------------------------------------------------------------
bool mEmcCalibratorModule::Init(const char* configfilename,				
				const char* calibrator,
				emcManageable::EStorage source )
{
  int emc_status ;
  fVerbose = 0 ; 
  /// get the raw data accessor unique instance
  fRda = emcRawDataAccessor::GetInstance(emc_status, configfilename);
  if (!fRda) return false ;
  return Config(calibrator,source) ;
}

//-------------------------------------------------------------------------
bool mEmcCalibratorModule::Init(const PHTimeStamp& ts,
				const char* calibrator,
				emcManageable::EStorage source )
{
  fVerbose = 0 ; 
  /// get the raw data accessor unique instance, from DB
  fRda = emcRawDataAccessor::GetInstance(ts) ;
  if (!fRda) return false ;
  return Config(calibrator,source) ;
}


//-------------------------------------------------------------------------
bool mEmcCalibratorModule::Config(const char* calibrator, 
				  emcManageable::EStorage source )
{

  cout<<"<mEmcCalibratorModule::Config> Calibrator: "<<calibrator<<"  Data Source "<<source<<endl;
  /// tell which calibration method to use
  emcCalibratorFactory::Initialize(calibrator) ;
  /// get the corresponding calibrator object
  fRdc = emcCalibratorFactory::GetCalibrator() ;
  if (!fRdc) return false ;
  /// set a few parameters needed by the calibrator
  SelectSource(source) ; 
  SetHighLowLimit() ; 
  SetZeroSuppression() ; 
  fMdo.UseHGLG(true) ;
  fRdo = fRda->GetRawDataObject() ;

  // MV 2001/09/24
  EmcDynamicData *dynamicData = fRda->GetDynamicData();
  int* dataMap=dynamicData->getEmcMap(); // get list of detector cells
  int nchannel=dynamicData->getEmcSize(); // get size of data arrays

  for(int ind=0; ind<nchannel; ind++) fIndexMap[dataMap[ind]]=ind;

  fUseTimeStamp = false ;
  fTimeStamp.setToFarFuture() ;

  return true ;
}

//-------------------------------------------------------------------------
void mEmcCalibratorModule::SelectSource(emcManageable::EStorage source)
{
  fRdc->SelectSource("*", source) ; 
}

//-------------------------------------------------------------------------
bool mEmcCalibratorModule::SetCalibrator(const char* calibrator) 
{
  if (fRdc) {
    emcCalibratorFactory::Initialize(calibrator) ;
    /// get the corresponding calibrator object
    fRdc = emcCalibratorFactory::GetCalibrator() ;
    if (!fRdc) return false ;
    return true ;
  }
  else {
    return false ;
  }
}

//-------------------------------------------------------------------------
bool mEmcCalibratorModule::SetHighLowLimit(int limit)
{
  if (fRdc) {
    fHighLowLimit = limit ;
    fRdc->SetHighLowLimit(fHighLowLimit) ;
    return true ;
  }
  else {
    return false ;
  }
}

//-------------------------------------------------------------------------
bool mEmcCalibratorModule::SetZeroSuppression(bool val)
{
  if (fRdc) {
    fZeroSuppression = val ;
    fRdc->SetZeroSuppression(fZeroSuppression) ;
    return true ;
  }
  else {
    return false ;
  }
}

//_____________________________________________________________________________
void
mEmcCalibratorModule::UseTimeStamp(const PHTimeStamp& when)
{
  fTimeStamp = when ;
  fUseTimeStamp = true ;
}

//_____________________________________________________________________________
void 
mEmcCalibratorModule::ForceDBCollection()
{
  if ( !fRdc ) 
    {
      cerr << "<E> mEmcCalibratorModule::ForceDBCollection : uh ? fRdc is "
	   << " null ?! That's bad, will crash sooner or later" 
	   << endl;
      return;
    }

  cout << "<I> mEmcCalibratorModule::ForceDBCollection TimeStamp="
       << fTimeStamp
       << endl;
    
  fRdc->ForceDBCollection(fTimeStamp);
}
