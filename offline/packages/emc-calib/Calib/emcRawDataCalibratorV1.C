//-------------------------------------------------------------------------
// $Id: emcRawDataCalibratorV1.C,v 1.27 2014/12/22 03:02:02 mazsi Exp $
// 
// Package: Calib
// 
// Copyright (C) PHENIX collaboration, 1999-2001
//
// Implementation of class : emcRawDataCalibratorV1
//
// Authors: Real calculation content = E. Kistenev (kistenev@bnl.gov)
//          C++ consulting = L. Aphecetche (aphecetc@in2p3.fr)
//
//-------------------------------------------------------------------------

#include "emcRawDataCalibratorV1.h"

#include "emcRawDataAccessor.h"
#include "emcMixedDataObject.h"
#include "emcCalibratedDataObject.h"
#include "EmcDynamicData.h"
#include "emcHLRatios.h"
#include "emcPedestals5.h"
#include "emcGains.h"
#include "emcGainFEM.h" 
#include "emcLCTofs.h"
#include "emcQAs.h"
#include "emcWalkTofs.h" 
//#include "emcTofT0s.h"
#include "emcTacPeds.h"
#include "EmcStaticData.h"
#include "emcDataManager.h"
#include "EmcSector.h"
#include "emcCalibrationData.h"
#include "EmcIndexer.h"
#include "emcFEMtupleFactory.h"
#include "emcHLRatioFEM.h"
#include "emcPedestalFEM.h"
#include "pbscTimingFixes.h"

#include <cassert>
#include <iostream>
#include <fstream>
#include <cmath>
#include <cstdlib>
#include <vector>

#include "emcException.h"
#include "emcDefines.h"

using std::vector ;
using std::cout ;
using std::cerr ;
using std::endl ;
using std::ifstream ;
using std::string;

static const string kTACPED = "TAC";

//-------------------------------------------------------------------------
emcRawDataCalibratorV1::emcRawDataCalibratorV1() : emcCalibrator()		  
{
  fName = "emcRawDataCalibratorV1" ; // must be the classname (because it's used by the Factory)
  Reset() ;
}

//-------------------------------------------------------------------------
emcRawDataCalibratorV1::~emcRawDataCalibratorV1()
{
}

//_____________________________________________________________________________
bool emcRawDataCalibratorV1::Calibrate(const emcRawDataObject& const_rdo,
			 emcMixedDataObject& mdo,
			 const PHTimeStamp& when)
{
  /* Make ADC and TDC values out of the RawDataObject.

     To perform this, we need to get access to some calibration objects :
     a) pedestals
     b) high/low gain ratios

     Those objects are collected from the database (being file or Objy)
     using the DataManager.

     NOTE: mdo can't be zero suppressed, if done - it will create a problem 
     in calibrating data. Associating suppressed data with Gains will 
     require search what is simply too time consuming.
   */

  // This method lies about it's use of rdo.  It claims it'll be
  // treated as const, but then it modifies it.  This line is a HACK
  // and should be properly fixed by an expert.
  emcRawDataObject& rdo = const_cast<emcRawDataObject&>(const_rdo);
    
  bool rv = true ; 

  if (fMustCollectForMDO) {
    CollectForMDO(when) ;
    fMustCollectForMDO = false ;
  }

  // Sets the corresponding flags of the MDO
  mdo.SetPedestalFlag(fCollectPedestalStatus) ; 
  mdo.SetHLRatioFlag(fCollectHLRatioStatus) ; 

  // Initialize the MDO from the RDO
  int* datamap ;
  long* softkeys ;
  int* dataerrors ;
  rdo.GetDataDescriptionPointers(datamap,softkeys,dataerrors) ;
  mdo.SetMaxSize(rdo.GetMaxSize()) ;
  Int_t* deadmap = 0;
  Int_t* warnmap = 0 ;

  if(fQA){
    deadmap = fQA->GetDeadMap();
    assert(deadmap!=0) ;
    warnmap = fQA->GetWarnMap() ;
    assert(warnmap!=0) ;
  }
  mdo.SetDataDescriptionPointers(datamap,softkeys,dataerrors,deadmap,warnmap) ;

  ////////////// COMPUTE LINEARIZED ADC AND TDC ///////////////////////
  Int_t index ;
  Float_t adc,tdc ;
  Float_t hgpost,hgpre,lgpost,lgpre ;
  Float_t phg, plg;
  Float_t lg, hg ;
  int amupre,amupost,amutac ;
  int data_error ;
  float tacped;
  float scale=0.0;

  // Loop over towers

  for (index = 0; index < rdo.GetSize(); index++ ) {

    // Is the channel to be declared dead ?
    if ( mdo.GetDead(index) & emcQAs::IamDeadMask() ) {
      mdo.AddDataError(index,0x2000);
      mdo.SetToZero(index) ;
      continue;
    }

    //  rdo.IsZero should identify the channels which are pedestal 
    //	suppressed in DCM. We are using it at all stages of data processing 
    //	so - there is no real need to reset to zero locations in mdo which 
    //	contain non-zero values from previous event (time saving). 
    //	We'll keep this for the time being but - it can be removed later.
    
    if (rdo.IsZero(index)) {
      mdo.SetToZero(index) ;
      continue;
    }

    rdo.Get(index,tdc,hgpost,lgpost,hgpre,lgpre,
    	    amupre,amupost,amutac,data_error) ;
    rdo.Get(index,tdc,hgpost,lgpost,hgpre,lgpre,data_error) ;

    // ADC 


    //=====> different treatment for PbSc and PbGl MV 2001/08/24
    int TowerId = rdo.GetItemId(index); 
    bool chooseLowGain=true;

    if(EmcIndexer::isPbSc(TowerId)){

      chooseLowGain=data_error&0x003c || (lgpre-lgpost)>192.;

      scale = (fCollectHLRatioStatus? fHLRatios->getValue(index) : 15.22) ;
      if(scale<12.||scale>18.) scale = 15.4; 
      
    } else if(EmcIndexer::isPbGl(TowerId)){
      
      bool badHighGain=(hgpre-hgpost)<0. && (lgpre-lgpost)>50.;
      bool goodLowGain=(lgpre-lgpost)>170.;
      chooseLowGain= data_error&0x003c || badHighGain || hgpost<1024. || goodLowGain;

      scale = (fCollectHLRatioStatus? fHLRatios->getValue(index) : 15.33);
      if(scale<12.||scale>18.) scale = 15.33;
      
    }

    // Current decision - if both High and Low are flagged as bad :
    // use Low but set a flag of corrupted data
    adc = lgpre-lgpost ;
    if(chooseLowGain) {
      //  Low Gain leg selected
      if (fCollectPedestalStatus&&fUseAmpPedestals) {
	plg = (-fPedestals->getValue(index, amupre, "LG_Pre")+
	       fPedestals->getValue(index, amupost, "LG_Post")) ;
	if(fabs(plg)<20.)   adc += plg;
      }
      adc *= scale;
    } else {
      //  High Gain leg selected
      adc =  hgpre-hgpost ;
      if (fCollectPedestalStatus&&fUseAmpPedestals) { 
	phg = (-fPedestals->getValue(index, amupre, "HG_Pre")+
	       fPedestals->getValue(index, amupost, "HG_Post")) ;
	if(fabs(phg)<20.)   adc += phg;
      }
    }
    if (data_error&0x23c0) {
      rdo.AddDataError(index,0x2000);
    }

    // TDC
    if(tdc>=4095) tdc = 0;

    if (fCollectPedestalStatus) {
      tacped   = fPedestals->getValue(index,rdo.GetTACCell(index), kTACPED) ; 
      //  FIXME: this is to protect against zero pedestals in TAC
      tdc -= ((tacped>0)? tacped : fPedestals->getValue(index, 0, kTACPED)) ; 
    }

    if (mdo.IsUsingHGLG()) {
      hg = hgpre-hgpost ;
      lg = lgpre-lgpost ;
      rdo.GetCells(index,amupre,amupost,amutac);
      if (fCollectPedestalStatus) {	
	//	cout<<index<<" "<<amupre<<" "<<amupost<<endl;
	phg = (-fPedestals->getValue(index, amupre, "HG_Pre")+
	       fPedestals->getValue(index, amupost, "HG_Post")) ;
	plg = (-fPedestals->getValue(index, amupre, "LG_Pre")+
	       fPedestals->getValue(index, amupost, "LG_Post")) ;

	hg += phg;
	lg += plg;
      }
      mdo.Set(index, adc, tdc, hg, lg) ;
    }
    else {
      mdo.Set(index, adc, tdc) ;
    }

  }

  return rv ; 
} 

//_____________________________________________________________________________
bool emcRawDataCalibratorV1::Calibrate(const emcMixedDataObject& mdo,
			 emcCalibratedDataObject& cdo,
			 const PHTimeStamp& when)
{
  /* Make Energie (GeV) and Time (ns) values out of the MixedDataObject 

     To perform this, we need to get access to some calibration objects :
     a) time-dependant gains
     b) least-count tofs
     c) walk tofs

     Those objects are collected from the database (being file or Objy)
     using the DataManager.

     We also need time-zero calibrations to get the final GeV values.

   */

  if ( fECalAtT0.empty() ) {
    if(!GetECalAtT0(when,true)) return false;
  }
  bool rv = true ; 
  Int_t index ; 
  Int_t eventTime   = when.getTics();

  if (fMustCollectForCDO) {
    CollectForCDO(when) ;
    tf = pbscTimingFixes::getInstance();
    if(!(tf->areFixesLoaded())) tf = 0;
    fMustCollectForCDO = false ;
  }

  bool energycalibrated = true; 
  bool timecalibrated   = true;
  
  cdo.Reset();

  Float_t adc, tdc ; 
  Int_t   outindex =0; 
  Float_t etotal   =0.;

  // Loop over towers
  for (index = 0; index < mdo.GetSize(); index++ ) {

    int TowerId = mdo.GetItemId(index) ; 

    // Skip reference towers, if any.
    if (EmcIndexer::isReference(TowerId)) continue;

    adc = mdo.GetADC(index) ; 

    int errorFlag = mdo.GetErrorFlag(index);

    if( adc > GetThresholdADC() && !(errorFlag&0x2000)) { 
      tdc = mdo.GetTDC(index) ; 
      if (!(errorFlag&0x2400)) {
	timecalibrated &= CalibrateTime(tdc, adc, index, TowerId, eventTime );
      }
      energycalibrated &= CalibrateEnergy( adc, index, TowerId, eventTime) ;
      cdo.Set(outindex, TowerId, mdo.GetSoftwareKey(index), errorFlag, 
	      adc, tdc,mdo.GetDead(index),mdo.GetWarn(index)) ;
      etotal += adc ;
      outindex++;
    } 
    else {
      if(!fZeroSuppression) {
	cdo.Set(outindex, TowerId, mdo.GetSoftwareKey(index), 
		errorFlag, 0., 0.,mdo.GetDead(index),mdo.GetWarn(index)) ;
	outindex++;
       }
    }
  }  
  cdo.SetTotalEnergy(etotal);
  cdo.SetZeroSuppressedFlag(fZeroSuppression);
  cdo.SetEnergyCalibratedFlag(energycalibrated);
  cdo.SetTimeCalibratedFlag(timecalibrated) ; 

  return rv ; 
 } 

//_____________________________________________________________________________
bool emcRawDataCalibratorV1::Calibrate(const emcRawDataObject& rdo,
			 emcCalibratedDataObject& cdo,
			 const PHTimeStamp& when)
{
  /** This method only call the 2 other Calibrate methods. It makes
      the calling program simpler, but has an efficiency pay-off, due
      to the re-allocation of a new mdo at each call.
   */
  emcMixedDataObject mdo ;
  bool rv = Calibrate(rdo,mdo,when) ;
  rv &= Calibrate(mdo,cdo,when) ;
  return rv ;
}

//-------------------------------------------------------------------------
bool 
emcRawDataCalibratorV1::CalibrateEnergy(Float_t & adc, const Int_t index, 
				      const Int_t TowerId,int incrementalTime) 
{
  if (fCollectGainStatus==false) { return false; }

  float normt, dummy;
  // dummy = adc;
  if(EmcIndexer::isPbSc(TowerId)) {
    // PBSC
    normt = fGains->getValue(index,incrementalTime) ;
    adc  *= ((normt>0.01)?  fECalAtT0[index]/normt : 0.) ; 
  } else {
    assert(EmcIndexer::isPbGl(TowerId)) ;
    // PBGL  - EXPLICITLY NO TIMING DEPENDENCE FOR NOW 
    normt = fGains->getValue(index,0) ;
    adc  *= ((normt>0.01)?  fECalAtT0[index]/normt : 0.) ; 
  }
  //  cout<<"<CE> Index "<<index<<" TowerId "<<TowerId<<" ADC "<<dummy<<" NORM "<<normt<<" ECal "<<fECalAtT0[index]<<" Energy "<<adc<<endl;
  return true ;
} 

//_________________________________________________________________________
bool  
emcRawDataCalibratorV1::CalibrateTime(Float_t & tdc, float adc, 
				    const Int_t index, const Int_t TowerId,
				    int incrementalTime)
{
  if (fCollectTofStatus==false) { return false; }
  float lc = fLCTofs->GetValue1(index);
  float t0 = 0.;
  //  EK 01/11/02
  //  we will now use time-dependent TAC pedestals to correct TAC first
  if(fTacPeds) {
    //    cout<<"TAC ped "<<index<<" "<<TowerId<<" "<<incrementalTime<<" "<<tdc<<" "<<fTacPeds->getValue((int)index, incrementalTime)<<endl;
    tdc -= fTacPeds->getValue((int)index, incrementalTime);
  }



//    if(!fTofT0s) {
//    //  constant T0 computed at a time of walk calibration
//      t0 = fWalkTofs->GetValue1(index);
//    } else {
//    //  variable T0 computed using the data used to prodce gains
//      t0 = fTofT0s->getValue(index, incrementalTime);
//      //    cout<<index<<" "<<t0<<" "<<fWalkTofs->GetValue1(index)<<endl;
//    }




  float wk = fWalkTofs->GetValue2(index);
  float dt;

  //=====> different functions for PbGl and PbSc MV 2001/08/24  
  if(TowerId<15552){
    lc = ((lc>25.&&lc<45.)? lc : 38.2)/1000.;    
    dt = ((adc>0.)? wk*log(adc)*1000. : 0.);
    tdc =  - (tdc-t0-dt)*lc;
    if(tf){
      int iS, iSM, iSMT;
      EmcIndexer::iPXiSiSMiSMT(TowerId,iS,iSM,iSMT);
      tdc -= (tf->getSectorT0(iS)+tf->getSMT0(iSM)+tf->getFEMTPattern(iSMT)+tf->getTowerT0(iSM,iSMT));
    }
    //    cout<<lc<<" "<<dt<<endl;
  } else if(TowerId<24768){

    // Changed at Maxim's request, Sep 28, 2001 GD
    //    //=====> recalculate adc to low gain MV 2001/08/24
    //    float scale;


    //    if(scale<12.|| scale>18.) scale = 15.33;

    //    lc = ((lc>20. && lc<55.)? lc : 38.9)/1000.;
    //    float lgadc=adc/scale;
    //    dt=((lgadc>0.)? wk*pow(lgadc, -0.2): 0.);

    // MV 2001/09/27 walk correction is calculated using physics data,
    // so T0 should be taken care of automatically
    t0 = fWalkTofs->GetValue1(index);
    lc = ((lc>20. && lc<55.)? lc : 38.9)/1000.;
    dt=((adc>0.)? wk*log(adc): 0.);
    tdc =  - (tdc-t0-dt)*lc + GetGlobalT0() ;
  }

  //  if(adc>250.) cerr<<"adc="<<adc<<" TOF t="<<t<<" lc="<<lc<<" t0="<<t0<<" wk="<<wk<<" dt="<<dt<<" T0="<<GetGlobalT0()<<" tdc="<<tdc<<endl;
  //  cout<<"<CT> Index "<<index<<" TowerId "<<TowerId<<" ADC "<<adc<<" TDC "<<dummy<<" TIME "<<tdc<<endl;
  return true ;

}

//_____________________________________________________________________________
void emcRawDataCalibratorV1::CollectForCDO(const PHTimeStamp& when)
{
  // Collection of gain and time calibration data.
  cout << "emcRawDataCalibratorV1::CollectForCDO" << endl ;

  static emcGains    gainDriver ;
  static emcLCTofs   lctofDriver ;
  static emcWalkTofs walktofDriver ;

  //  EK 01/11/02
  //  static emcTofT0s   drifttofDriver;
  static emcTacPeds   drifttofDriver;
  
  emcDataManager* dm = emcDataManager::GetInstance() ;
  
  // FIXME : THIS WILL WORK ONLY IF ALL DATA HAVE THE SAME START 
  // OF VALIDITY TIME 
  // Try to collect Tof-related data
  if ( fTofSource != emcManageable::kNone && maxFailTof ) {
    // Try to collect ToF drifts
    drifttofDriver.SetSource( fTofSource ) ;
    
    //  EK 01/11/02
    //    fTofT0s   = dynamic_cast<emcTofT0s*>( dm->Collect(drifttofDriver, when)) ; 
    fTacPeds   = dynamic_cast<emcTacPeds*>( dm->Collect(drifttofDriver, when)) ; 
    if(!fTacPeds) cout<<"failed to collect drifts"<<endl;
    // Try to collect least counts for ToF 
    lctofDriver.SetSource( fTofSource ) ;
    //    cout<<"Collecting LCTOFs"<<endl;
    fLCTofs = dynamic_cast<emcLCTofs*>( dm->Collect(lctofDriver, when) ) ;
    //    cout<<"Collecting LCTOFs - done"<<endl;

    // Try to collect walks for ToF 
    walktofDriver.SetSource( fTofSource ) ;
    fWalkTofs = dynamic_cast<emcWalkTofs*>( dm->Collect(walktofDriver, when)) ;



    // (*1) ... here it might be that collection was not ok ...
    fCollectTofStatus = (fLCTofs!=0 && fWalkTofs!=0) ;
    maxFailTof -= !fCollectTofStatus ;
  }


  // by default we consider that collection never happened... (*1)
  if (maxFailGain) fCollectGainStatus = false ;
  
  // Try to collect Gains
  if ( fGainsSource != emcManageable::kNone && maxFailGain ) {
    gainDriver.SetSource( fGainsSource ) ;
    fGains = dynamic_cast<emcGains*>( dm->Collect(gainDriver, when) ) ;
    // (*1) ... here it might be that collection was not ok ...
    fCollectGainStatus = (fGains!=0) ;
    maxFailGain -= !fCollectGainStatus ;
  } 
  
  if ( fVerbose>=1 ) {
    if (!fCollectTofStatus) {
      cerr << "<E> emcRawDataCalibratorV1::CollectForCDO " << endl 
	   << "    ToFs collection failed. " << endl ;
    }
    if (!fCollectGainStatus) {
      cerr << "<E> emcRawDataCalibratorV1::CollectForCDO() " << endl
	   << "    Gains collection failed. " << endl ;
    }
  }
}

//_____________________________________________________________________________
void emcRawDataCalibratorV1::CollectForMDO(const PHTimeStamp& when)
{
  // Collection of pedestals, hlratios and Q&As
  cout << "emcRawDataCalibratorV1::CollectForMDO" << endl ;

  static emcPedestals5 pedDriver ;
  static emcHLRatios hlrDriver ;
  static emcQAs qaDriver ;

  emcDataManager* dm = emcDataManager::GetInstance() ;

  // by default we consider that collection never happened... (*1)
  fCollectPedestalStatus = fCollectHLRatioStatus = false ;

  // Try to collect pedestals
  if ( fPedestalsSource != emcManageable::kNone && maxFailPed ) {
    pedDriver.SetSource( fPedestalsSource ) ;
    fPedestals = dynamic_cast<emcPedestals5*>( dm->Collect(pedDriver, when) ) ;
    // (*1) ... here it might be that collection was not ok ...
    fCollectPedestalStatus = (fPedestals!=0) ;
    maxFailPed -= !fCollectPedestalStatus ;
  }

  if( fVerbose>=1 && !fCollectPedestalStatus) {
    cout << "<E> emcRawDataCalibratorV1::CollectFromMDO() " << endl
	 << "    Pedestal collection failed." << endl ;
  }

  // Try to collect H/L Ratios
  if ( fHLRatiosSource != emcManageable::kNone && maxFailHLR ) {
    // (*1) ... here it might also be that collection was not ok ...
    hlrDriver.SetSource( fHLRatiosSource) ;
    fHLRatios = dynamic_cast<emcHLRatios*>( dm->Collect(hlrDriver, when) ) ;
    // (*1) ... here it might be that collection was not ok ...
    fCollectHLRatioStatus = (fHLRatios!=0) ;
    maxFailHLR -= !fCollectHLRatioStatus ;
  }

  // Try to collect Q&A objects
  if ( fQASource != emcManageable::kNone ) {

    // Set some parameters for the qa object we want
    qaDriver.SetSource(fQASource) ;

    // FIXME: unclear if this is correctly made - call on every event
    qaDriver.SetExtraRejectListFilename(fExtraRejectListFilename.c_str()) ;

    // Collect it
    fQA = dynamic_cast<emcQAs*>( dm->Collect(qaDriver,when) ) ;
    if (!fQA) {
      cerr << "<E> Could not collect Q&A ?!" << endl ;
    }
    assert(fQA!=0) ;
  }

  if ( fVerbose>=1 && !fCollectHLRatioStatus) {
    cout << "<E> emcRawDataCalibratorV1::CollectForMDO(r) " << endl
	 << "    HLRatio collection failed." << endl ;
  }
}

//_____________________________________________________________________________
void
emcRawDataCalibratorV1::ForceDBCollection(const PHTimeStamp& when)
{
  CollectForMDO(when);
  fMustCollectForMDO = false;
  GetECalAtT0(when,true);
  CollectForCDO(when);
  fMustCollectForCDO = false;
}

//_____________________________________________________________________________
bool 
emcRawDataCalibratorV1::GetECalAtT0(const PHTimeStamp& when, 
 				  bool normalizationON) 
{
  // Get initial calibration data.

  if (!fECalAtT0.empty()) { 
    // we already have it 
    return true ;
  }

  emcRawDataAccessor * rda = emcRawDataAccessor::GetInstance() ; 

  if (!rda) {
    cerr << "<E> emcRawDataCalibratorV1:: GetECalAtT0 " 
	 << "- Cannot fetch time 0 calibration - No data accessor " << endl ;
    return false ; 
  }

  if (GetVerbose()>0) {
    cout << "<I> emcRawDataCalibratorV1::GetECalAtT0 : *** Reading from [" ;
    cout << emcManageable::GetStorageName(fIniCalSource) << "]" << endl ;
  }

  size_t ntowers = 144*rda->GetDynamicData()->getnSM() ;

  fECalAtT0.resize(ntowers) ;

  PHTimeStamp* timestamp = 0 ;
  EmcStaticData* sd = 0 ;

  // here we're just exercizing C++ exceptions.
  // Might not be the most suitable place for that, so 
  // we might revert to old error handling later.
  // Anyway, if the sector can not be built, we should
  // exit asap (because init. calibrations are simply
  // not there).
  try {
    sd = EmcStaticData::buildEmcStaticData() ;
  }
  catch (emcException& err) {
    cerr << "Got exception : " << err.what() << endl ;
    exit(1) ;
  }

  const int * DataMap        = rda->GetDynamicData()->getEmcMap();
  int item ;
  int sn, ist ;
  EmcSector * sector ;
  float encal, norm0 ;
  float nothing ;
  float c0,g0,cf ;
  // this is "WA98" kappa factor
  float kappa = 1.0/5.9950 ; 

  if ( fIniCalSource == emcManageable::kDB_Objy ) { 
    PHTimeStamp& ts = const_cast<PHTimeStamp&>(when) ;
    timestamp = &ts ; 
  }

  for (item = 0; item < rda->GetDynamicData()->getEmcSize(); item++) {

    EmcIndexer::iPXiSiST(DataMap[item], sn, ist) ;

    assert(sn>=0) ;

    // skip references
    if (sn>=8) continue ;

    sector = sd->getSector(sn) ;

    if( !sector ) {
      // Sector will be built from file if timestamp=0, 
      // from Objy DB otherwise (assuming there's calibration
      // constants valid at timestamp in the DB, of course!)
      sd ->buildEmcSector( EmcIndexer::EmcSectorId(sn), timestamp ) ;
      sector = sd->getSector(sn) ;
    }
 
    assert(sector->IsOK()) ;

    if ( sn < 6 ) {      
      // PbSc
      sector->GetEnergyCalibration(ist,encal,norm0,nothing) ;
      assert(nothing==1.0) ;
    }
    else {
      // PbGl
      sector->GetEnergyCalibration(ist,c0,g0,cf) ;
      encal = c0*g0*cf ; 
      norm0 = kappa ;
    }	

    fECalAtT0[item] = encal * ( normalizationON ? norm0 : 1. ) ;      

  } // end of loop over item

  return true ;
}

//_____________________________________________________________________________
bool emcRawDataCalibratorV1::GetCollectionStatus(const char* type) const
{
  string stype = type ;
  if ( stype == "Pedestals" ) {
    return fCollectPedestalStatus ;
  }
  else if ( stype == "HLRatios" ) {
    return fCollectHLRatioStatus ;
  }
  else if ( stype == "Gains" ) {
    return fCollectGainStatus ;
  }
  else if ( stype == "Tofs") {
    return fCollectTofStatus ;
  }
  else if ( stype == "*" ) {
    return 
      fCollectPedestalStatus &
      fCollectHLRatioStatus & 
      fCollectGainStatus &
      fCollectTofStatus ;
  }
  return false ;
}


//_____________________________________________________________________________
void emcRawDataCalibratorV1::SetCollectionStatus(const char* type)
{
  string stype = type ;
  if ( stype == "Pedestals" ) {
    fCollectPedestalStatus = true;
  }
  else if ( stype == "HLRatios" ) {
    fCollectHLRatioStatus  = true;
  }
  else if ( stype == "Gains" ) {
    fCollectGainStatus = true;
  }
  else if ( stype == "Tofs") {
    fCollectTofStatus = true;
  }
  else if ( stype == "*" ) {
    fCollectPedestalStatus = true;
    fCollectHLRatioStatus  = true; 
    fCollectGainStatus     = true;
    fCollectTofStatus      = true;
  }
}

//_____________________________________________________________________________

void emcRawDataCalibratorV1::Print() const
{
  cout << " emcRawDataCalibratorV1 setup : " << endl ;  

  cout  << " **** Pedestals  " ; 
  if ( fPedestalsSource == emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ; 
  }
  else if ( fPedestalsSource == emcManageable::kDB_Objy  ) {
    cout << "are read from Database " << endl ; 
  }
  else {
    cout << " = 0 (collection disabled)" << endl ; 
  }

  cout << " **** high/low gain ratios  " ; 
  if ( fHLRatiosSource ==  emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ; 
  }
  else if ( fHLRatiosSource ==  emcManageable::kDB_Objy ) {
    cout << "are read from Database " << endl ; 
  }
  else {
    cout << " = 16 " << endl ; 
  }

  cout << " **** Gains (current normalization)  " ; 
  if ( fGainsSource ==  emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ; 
  }
  else if ( fGainsSource ==  emcManageable::kDB_Objy ) {
    cout << "are read from Database " << endl ; 
  }
  else {
    cout << " = 1 " << endl ; 
  }

  cout << " **** ToF calibration (least counts and walks)  " ; 
  if ( fTofSource ==  emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ; 
  }
  else if ( fTofSource ==  emcManageable::kDB_Objy ) {
    cout << "are read from Database " << endl ; 
  }
  else {
    cout << " = 1 " << endl ; 
  }

  cout << " **** Q&A " ;
  if ( fQASource == emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ;
  }
  else if ( fQASource == emcManageable::kDB_Objy ) {
    cout << "are read from Database " << endl ;
  }
  else {
    cout << "are not read (perfect towers assumed) " << endl ;
  } 

  cout << " **** Initial Calibration " ;
  if ( fIniCalSource == emcManageable::kFile_ASCII) { 
    cout << "are read from files " << endl ;
  }
  else if ( fIniCalSource == emcManageable::kDB_Objy ) {
    cout << "are read from Database " << endl ;
  }
  else {
    cout << " Huh ? : check your IniCal source !" << endl ;
  }

  cout << " **** High gain threshold = " <<  GetHighLowLimit() << endl ; 
  cout << " **** ADC threshold = " << GetThresholdADC() << endl ;
  cout << " **** Zero suppression is " << 
    ((fZeroSuppression==true)?"ON":"OFF") << endl ;

}

//_____________________________________________________________________________
void emcRawDataCalibratorV1::Reset(void)
{
  fPedestalsSource = emcManageable::kNone ; 
  fHLRatiosSource  = emcManageable::kNone ;  
  fGainsSource     = emcManageable::kNone ;  
  fTofSource       = emcManageable::kNone ;  
  fQASource        = emcManageable::kNone ;
  fIniCalSource    = emcManageable::kNone ;
  fCollectPedestalStatus = fCollectHLRatioStatus = false ;
  fCollectGainStatus    = fCollectTofStatus     = false;
  maxFailPed = maxFailHLR = maxFailGain = maxFailTof = 10;
  fECalAtT0.clear() ;
  fZeroSuppression = false;
  fQA = 0 ;
  SetExtraRejectListFilename();
  fPedestals = 0 ;
  fHLRatios = 0 ;
  fGains = 0 ;
  fLCTofs = 0 ;
  fWalkTofs = 0 ;
  fTofT0s = 0 ;
  fMustCollectForCDO = true ;
  fMustCollectForMDO = true ;
  tf=0;
}

//-------------------------------------------------------------------------
bool emcRawDataCalibratorV1::SelectSource(const char* type, 
					emcManageable::EStorage source)
{
  /* The validity of the source parameter depends on the mapping style
     of the calorimeter. If 144 words per FEM, all sources allowed. 
     If 192 words per FEM, no source at all allowed.
  */
  
  emcRawDataAccessor * rda = emcRawDataAccessor::GetInstance() ; 

  if (!rda) {
    cerr << "<E> emcRawDataCalibratorV1:: SelectSource - Cannot check source is valid because I cannot get access to RDA. So I am NOT changing the source." << endl ;
    return false ;
  }
  else {
    /* Check that the source is valid. 
       Basically, if we deal with 144 words per FEM, all sources allowed,
       if 192 words per FEM, valid source = none. */
    if ( rda->GetDynamicData()->getMapStyle() == false &&
	 source != emcManageable::kNone ) {
      cerr << "<E> emcRawDataCalibratorV1::SelectSource - Map style is 192 words per FEM. Cannot use any source." << endl ;
      return false ;
    }
  }
 
  bool rv = true ; 
  string stype = type ;
  
  if ( stype == "Pedestals" ) 
    fPedestalsSource = source ; 
  
  else if ( stype == "HLRatios" ) 
    fHLRatiosSource = source ;

  else if ( stype == "Gains" ) 
    fGainsSource    = source ;

  else if ( stype == "ToF" ) 
    fTofSource    = source ;

  else if ( stype == "QAs" )
    fQASource = source ;

  else if ( stype == "IniCal" ) 
    fIniCalSource = source ; 

  else if ( stype == "*" ) {
    fPedestalsSource = source ; 
    fHLRatiosSource = source ;
    fGainsSource    = source ;
    fQASource = source ;
    fTofSource    = source ;   
    fIniCalSource = source ;
  }
  else {
    cerr << "emcRawDataCalibratorV1::SelectSource: " << type << " is an unknown type " << endl
	 << "                                    Valid types are Pedestals, HLRatios, Gains, ToF " << endl ; 
    rv = false ; 
  }
  
  return rv ; 
  
}

//_________________________________________________________________________
// Set Global T0 for every calorimeter Tower 
// The simplest solution for now was to write an ASCII file without any DB connectivity and load it at will
void emcRawDataCalibratorV1::SetTwrGlobalT0(char * filename){
  if (fTwrGlobalT0) {
    delete [] fTwrGlobalT0;
    fTwrGlobalT0 = NULL;
  }
  if(!filename) 	return;
  ifstream fin(filename, std::ios::in);
  emcRawDataAccessor * rda = emcRawDataAccessor::GetInstance();
  assert(rda!=0);
  emcRawDataObject   * rdo = rda->GetRawDataObject() ;
  assert(rdo!=0);
  fTwrGlobalT0 = new float [rdo->GetMaxSize()];
  int lines = 0;
  int twr, sector, iX, iY, entries;
  float t0, rms;
  while(fin>>twr>>sector>>iX>>iY>>t0>>rms>>entries){
    // cout<<lines<<" "<<twr<<" "<<sector<<" "<<iX<<" "<<iY<<" "<<t0<<" "<<rms<<" "<<entries<<endl;
    assert(lines<rdo->GetMaxSize());
    fTwrGlobalT0[lines] = ((entries>2)? t0 : 0.);
    lines++;
  }
  assert(lines==rdo->GetMaxSize());
}
