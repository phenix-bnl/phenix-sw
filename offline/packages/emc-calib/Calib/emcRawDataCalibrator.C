// Implementation of class : emcRawDataCalibrator
//
// Authors: Real calculation content = E. Kistenev (kistenev@bnl.gov)
//          C++ consulting = L. Aphecetche (aphecetc@in2p3.fr)

#include "emcRawDataCalibrator.h"

#include "emcRawDataAccessor.h"
#include "emcMixedDataObject.h"
#include "emcCalibratedDataObject.h"
#include "EmcDynamicData.h"
#include "emcHLRatios.h"
#include "emcPedestals.h"
#include "emcGains.h"
#include "emcGainFEM.h" 
#include "emcLCTofs.h"
#include "emcQAs.h"
#include "emcWalkTofs.h" 
#include "emcTofT0s.h" 
#include "EmcStaticData.h"
#include "emcDataManager.h"
#include "EmcSector.h"
#include "emcCalibrationData.h"
#include "EmcIndexer.h"
#include "emcFEMtupleFactory.h"
#include "emcHLRatioFEM.h"
#include "emcPedestalFEM.h"

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

//-------------------------------------------------------------------------
emcRawDataCalibrator::emcRawDataCalibrator() : emcCalibrator()		  
{
  fName = "emcRawDataCalibrator" ; // must be the classname (because it's used by the Factory)
  Reset() ;
}

//-------------------------------------------------------------------------
emcRawDataCalibrator::~emcRawDataCalibrator()
{
}

//_____________________________________________________________________________
bool emcRawDataCalibrator::Calibrate(const emcRawDataObject& const_rdo,
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

  float INVALID_FLOAT = -9999 ;

  // This method lies about it's use of rdo.  It claims it'll be
  // treated as const, but then it modifies it.  This line is a HACK
  // and should be properly fixed by an expert.
  emcRawDataObject& rdo = const_cast<emcRawDataObject&>(const_rdo);
    
  bool rv = true ; 

  CollectForMDO(when) ;

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
  if(fQA){
    deadmap = fQA->GetDeadMap();
    assert(deadmap!=0) ;
  }
  mdo.SetDataDescriptionPointers(datamap,softkeys,dataerrors,deadmap) ;

  ////////////// COMPUTE LINEARIZED ADC AND TDC ///////////////////////
  Int_t index ;
  Float_t adc,tdc ;
  Float_t hgpost,hgpre,lgpost,lgpre ;
  Float_t lg, hg ;
  int amupre,amupost,amutac ;
  int data_error ;
  float newTDCValue, tacped;
  float scale ;

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

    scale = (fCollectHLRatioStatus? fHLRatios->getValue(index) : 15.22) ;
    if(scale<12.||scale>18.) scale = 15.4; 

    // ADC 
    // In making decision on the choice between High/Low gain legs 
    // we depend on the assesment of the data quality made earlier by the 
    // data accessor (bit pattern in the DataErrors). 
    // If set - it has an absolute priority over any other tests.
    adc = lgpre-lgpost ;
    // FIXME: We have a bit of a problem with gain selection - 
    // some channels do have very low saturation level in high gain 
    // (our current HighLowLimit is set to 1024 and it seems that even 
    // that high is still not high enough). 
    // For the moment - we select low gain every time when it is 
    // above 128 channels 

    if ( adc>128 || data_error&0x203c || 
	 hgpost < static_cast<float>(GetHighLowLimit()) ) {
      //  Low Gain leg selected
      if (data_error&0x23c0) {
	adc = 0.;
	tdc = 0.;
	// Use DataErrors to indicate that this data word is complete failure
	if(data_error<0x2000) {
	  mdo.AddDataError(index,0x2000) ;
	}
      } else {
	if (fCollectPedestalStatus) {
	  adc -= fPedestals->getValue(index, amupost, "LG_Pre-Post") ;
	}
	adc *= scale;
      }
    }
    else {           
      //  High Gain leg selected
      adc =  hgpre-hgpost ;
      if (fCollectPedestalStatus) { 
	adc -= fPedestals->getValue(index,amupost, "HG_Pre-Post") ; 
      }
    }

    newTDCValue = INVALID_FLOAT ;

    // TDC
    if (fCollectPedestalStatus && !(data_error&0x2400)) {

      newTDCValue = tdc;
      tacped   = fPedestals->getValue(index,amutac, "TAC") ; 
      //  FIXME: this is to protect against zero pedestals in TAC
      tdc -= ((tacped>0)? tacped : fPedestals->getValue(index, 0, "TAC")) ; 
    } else if (data_error&0x2400) {
      tdc = 0.0 ;
    }

    // If amplitude in the tower is low - measured TDC value can be used to update timing pedestals
    if(fabs(tdc)<200. && adc<200. && newTDCValue!=INVALID_FLOAT)  {
      fPedestals->updateValue(index, amutac, newTDCValue, "TAC");
    }
    if (mdo.IsUsingHGLG()) {
      hg =  hgpre-hgpost ;
      lg = lgpre-lgpost ;
      if (fCollectPedestalStatus) {
	hg -= fPedestals->getValue(index,amupost,"HG_Pre-Post") ;
	lg -= fPedestals->getValue(index,amupost,"LG_Pre-Post") ;
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
bool emcRawDataCalibrator::Calibrate(const emcMixedDataObject& mdo,
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

  CollectForCDO(when) ;

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
	      adc, tdc,mdo.GetDead(index)) ;
      etotal += adc ;
      outindex++;
    } 
    else {
      if(!fZeroSuppression) {
	cdo.Set(outindex, TowerId, mdo.GetSoftwareKey(index), 
		errorFlag, 0., 0.,mdo.GetDead(index)) ;
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
bool emcRawDataCalibrator::Calibrate(const emcRawDataObject& rdo,
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
emcRawDataCalibrator::CalibrateEnergy(Float_t & adc, const Int_t index, 
				      const Int_t TowerId,int incrementalTime) 
{
  if (fCollectGainStatus==false) { return false; }

  float normt;

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
  return true ;
} 

//_________________________________________________________________________
bool  
emcRawDataCalibrator::CalibrateTime(Float_t & tdc, float adc, 
				    const Int_t index, 
				    const Int_t /*TowerId*/,
				    int /*incrementalTime*/)
{
  if (fCollectTofStatus==false) { return false; }

  float lc   = fLCTofs->GetValue1(index) * 1.075 ;
  lc         = ((lc>25.&&lc<45.)? lc : 35.)/1000.;
  float t0 = 0.;
  float dt = 0.;
  float wk = 0.;

  t0 = fWalkTofs->GetValue1(index);
  wk  = fWalkTofs->GetValue2(index);
  dt  = ((adc>0.)? wk*adc : 0.);

  tdc =  (t0 - (tdc-dt))*lc - GetGlobalT0() ;
  return true ;
}

//_____________________________________________________________________________
void emcRawDataCalibrator::CollectForCDO(const PHTimeStamp& when)
{
  // Collection of gain and time calibration data.

  static emcGains gainDriver ;
  static emcLCTofs lctofDriver ;
  static emcWalkTofs walktofDriver ;
  
  emcDataManager* dm = emcDataManager::GetInstance() ;
  
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
  // FIXME : THIS WILL WORK ONLY IF ALL DATA HAVE THE SAME START 
  // OF VALIDITY TIME 
  // Try to collect Tof-related data
  if ( fTofSource != emcManageable::kNone && maxFailTof ) {
    // Try to collect least counts for ToF 
    lctofDriver.SetSource( fTofSource ) ;
    fLCTofs = dynamic_cast<emcLCTofs*>( dm->Collect(lctofDriver, when) ) ;
    // Try to collect walks for ToF 
    walktofDriver.SetSource( fTofSource ) ;
    fWalkTofs = dynamic_cast<emcWalkTofs*>( dm->Collect(walktofDriver, when)) ;
    // (*1) ... here it might be that collection was not ok ...
    fCollectTofStatus = (fLCTofs!=0 && fWalkTofs!=0) ;
    maxFailTof -= !fCollectTofStatus ;
  }
  
  if ( fVerbose>=1 ) {
    if (!fCollectTofStatus) {
      cerr << "<E> emcRawDataCalibrator::CollectForCDO " << endl 
	   << "    ToFs collection failed. " << endl ;
    }
    if (!fCollectGainStatus) {
      cerr << "<E> emcRawDataCalibrator::CollectForCDO() " << endl
	   << "    Gains collection failed. " << endl ;
    }
  }
}

//_____________________________________________________________________________
void emcRawDataCalibrator::CollectForMDO(const PHTimeStamp& when)
{
  // Collection of pedestals, hlratios and Q&As

  static emcPedestals pedDriver ;
  static emcHLRatios hlrDriver ;
  static emcQAs qaDriver ;

  emcDataManager* dm = emcDataManager::GetInstance() ;

  // by default we consider that collection never happened... (*1)
  fCollectPedestalStatus = fCollectHLRatioStatus = false ;

  // Try to collect pedestals
  if ( fPedestalsSource != emcManageable::kNone && maxFailPed ) {
    pedDriver.SetSource( fPedestalsSource ) ;
    fPedestals = dynamic_cast<emcPedestals*>( dm->Collect(pedDriver, when) ) ;
    // (*1) ... here it might be that collection was not ok ...
    fCollectPedestalStatus = (fPedestals!=0) ;
    maxFailPed -= !fCollectPedestalStatus ;
  }

  if( fVerbose>=1 && !fCollectPedestalStatus) {
    cout << "<E> emcRawDataCalibrator::CollectFromMDO() " << endl
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
    cout << "<E> emcRawDataCalibrator::CollectForMDO(r) " << endl
	 << "    HLRatio collection failed." << endl ;
  }

}

//_____________________________________________________________________________
bool 
emcRawDataCalibrator::GetECalAtT0(const PHTimeStamp& when, 
 				  bool normalizationON) 
{
  // Get initial calibration data.

  if (!fECalAtT0.empty()) { 
    // we already have it 
    return true ;
  }

  emcRawDataAccessor * rda = emcRawDataAccessor::GetInstance() ; 

  if (!rda) {
    cerr << "<E> emcRawDataCalibrator:: GetECalAtT0 " 
	 << "- Cannot fetch time 0 calibration - No data accessor " << endl ;
    return false ; 
  }

  if (GetVerbose()>0) {
    cout << "<I> emcRawDataCalibrator::GetECalAtT0 : *** Reading from [" ;
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
bool emcRawDataCalibrator::GetCollectionStatus(const char* type) const
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

void emcRawDataCalibrator::Print() const
{
  cout << " emcRawDataCalibrator setup : " << endl ;  

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
    cout << " Huh ? : check your Q&A source !" << endl ;
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
void emcRawDataCalibrator::Reset(void)
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
}

//-------------------------------------------------------------------------
bool emcRawDataCalibrator::SelectSource(const char* type, 
					emcManageable::EStorage source)
{
  /* The validity of the source parameter depends on the mapping style
     of the calorimeter. If 144 words per FEM, all sources allowed. 
     If 192 words per FEM, no source at all allowed.
  */
  
  emcRawDataAccessor * rda = emcRawDataAccessor::GetInstance() ; 

  if (!rda) {
    cerr << "<E> emcRawDataCalibrator:: SelectSource - Cannot check source is valid because I cannot get access to RDA. So I am NOT changing the source." << endl ;
    return false ;
  }
  else {
    /* Check that the source is valid. 
       Basically, if we deal with 144 words per FEM, all sources allowed,
       if 192 words per FEM, valid source = none. */
    if ( rda->GetDynamicData()->getMapStyle() == false &&
	 source != emcManageable::kNone ) {
      cerr << "<E> emcRawDataCalibrator::SelectSource - Map style is 192 words per FEM. Cannot use any source." << endl ;
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
    cerr << "emcRawDataCalibrator::SelectSource: " << type << " is an unknown type " << endl
	 << "                                    Valid types are Pedestals, HLRatios, Gains, ToF " << endl ; 
    rv = false ; 
  }
  
  return rv ; 
  
}


//_________________________________________________________________________
// Set Global T0 for every calorimeter Tower 
// The simplest solution for now was to write an ASCII file without any DB connectivity and load it at will
void emcRawDataCalibrator::SetTwrGlobalT0(char * filename){
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
