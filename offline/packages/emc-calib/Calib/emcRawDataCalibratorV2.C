//-------------------------------------------------------------------------
// $Id: emcRawDataCalibratorV2.C,v 1.35 2015/03/22 17:23:30 mazsi Exp $
// <EK> 111302 Implementation for Run3
// 
// Package: Calib
// 
// Copyright (C) PHENIX collaboration, 1999-2001
//
// Implementation of class : emcRawDataCalibratorV2
//
// Authors: Real calculation content = E. Kistenev (kistenev@bnl.gov)
//          C++ consulting = L. Aphecetche (aphecetc@in2p3.fr)
//
//-------------------------------------------------------------------------

#include "emcRawDataCalibratorV2.h"

#include "emcRawDataAccessor.h"
#include "emcMixedDataObject.h"
#include "emcCalibratedDataObject.h"
#include "EmcDynamicData.h"
#include "emcDataFormatter.h"
#include "emcHLRatios.h"
#include "emcPedestals5.h"
#include "emcGains.h"
#include "emcGainFEM.h" 
#include "emcLCTofs.h"
#include "emcQAs.h"
#include "emcWalkTofs.h" 
#include "emcTofT0s.h"
#include "emcTacPeds.h"
#include "EmcStaticData.h"
#include "emcDataManager.h"
#include "EmcSector.h"
#include "emcCalibrationData.h"
#include "emcCalibrationDataHelper.h"
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
static const string kHG_Pre = "HG_Pre";
static const string kHG_Post = "HG_Post";
static const string kLG_Pre = "LG_Pre";
static const string kLG_Post = "LG_Post";

namespace {

  float walkCorrection(float amp)
  {
    static const float walkCorrections[] = {
      //  the first 6 or 7 numbers in this table are badly defined (no data). Unless the data are available - there was no point in trying to improve the parametrisation
      0.0277778, 0.107143, 0.173529, 0.0543478, 0.116667, 0.10, 0.0744804, 0.0957627, 0.164296, 0.077351, 0.153276, 0.205479, 0.164593, 0.165495, 0.21152, 0.24188, 0.26691, 0.264707, 0.260216, 0.235105, 0.170128, 0.101093, 0.0506275, 0.0129709, -0.0310411, -0.0710098, -0.107253, -0.155408, -0.190727, -0.217815, -0.236577, -0.241337, -0.241553, -0.242847, -0.232342, -0.215285, -0.206976, -0.191099, -0.172913, -0.152418, -0.126642, -0.102625, -0.083642, -0.0752175, -0.0562135, -0.0406164, -0.0194486, -0.00347076, 0.0184414, 0.0312518, 0.0502009, 0.0678629, 0.0832042, 0.0936487, 0.103101, 0.114466, 0.137864, 0.156677, 0.169112, 0.174417, 0.173401, 0.17573, 0.185781, 0.201714, 0.215833, 0.22749, 0.240455, 0.246843, 0.252532, 0.256536, 0.262321, 0.26874, 0.278161, 0.285608, 0.289092, 0.290117, 0.301697, 0.308295, 0.316562, 0.331756, 0.334545, 0.34805, 0.362435, 0.36986, 0.373365, 0.382179, 0.405925, 0.396395, 0.406755, 0.421493, 0.430347, 0.440847, 0.457075, 0.463535, 0.458969, 0.466124, 0.479996, 0.487251, 0.489726, 0.494271
    };
    
    int bin=int(amp/10);
    if(bin>=100) bin=99;
    return walkCorrections[bin];
  }
}

//-------------------------------------------------------------------------
emcRawDataCalibratorV2::emcRawDataCalibratorV2() : emcCalibrator()		  
{
  fName = "emcRawDataCalibratorV2" ; // must be the classname (because it's used by the Factory)
  Reset() ;
  fCH = NULL;
}
//-------------------------------------------------------------------------
emcRawDataCalibratorV2::emcRawDataCalibratorV2(emcCalibrationDataHelper * fch) : emcCalibrator()		  
{
  fName = "emcRawDataCalibratorV2" ; // must be the classname (because it's used by the Factory)
  Reset() ;
  fCH = fch;
}

//-------------------------------------------------------------------------
emcRawDataCalibratorV2::~emcRawDataCalibratorV2()
{
}

//_____________________________________________________________________________
bool emcRawDataCalibratorV2::Calibrate(const emcRawDataObject& const_rdo,
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
  int*   datamap ;
  long*  softkeys ;
  int*   dataerrors ;
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

    int TowerId = rdo.GetItemId(index); 

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
    //    rdo.Get(index,tdc,hgpost,lgpost,hgpre,lgpre,data_error) ;

    // ADC 


    //=====> different treatment for PbSc and PbGl MV 2001/08/24
    bool chooseLowGain =true;
    if(fUseOnlyLowGain) {
      scale              = 1.;
    } else {
      if(EmcIndexer::isPbSc(TowerId)){

	//	chooseLowGain = data_error&0x003c || (lgpre-lgpost)>192.;
	chooseLowGain = data_error&(emcDataFormatter::HG_PRE_OUT+emcDataFormatter::HG_POST_OUT) || (lgpre-lgpost)>192.;

	scale = (fCollectHLRatioStatus? fHLRatios->getValue(index) : 15.22) ;
	if(scale<12.||scale>18.) scale = 15.4; 
      
      } else if(EmcIndexer::isPbGl(TowerId)){
      
	bool badHighGain=(hgpre-hgpost)<0. && (lgpre-lgpost)>50.;
	bool goodLowGain=(lgpre-lgpost)>170.;
	chooseLowGain= data_error&0x003f || badHighGain || hgpost<1024. || goodLowGain;

	scale = (fCollectHLRatioStatus? fHLRatios->getValue(index,3) : 15.33);
	if(scale<10.||scale>21.) scale = 15.33;
      
      }
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
//     if (data_error&0x23c0) {
//       rdo.AddDataError(index,0x2000);
//     }

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
	phg = fUseAmpPedestals? (-fPedestals->getValue(index, amupre, kHG_Pre)+
				 fPedestals->getValue(index, amupost, kHG_Post)) : 0.;
	plg = fUseAmpPedestals? (-fPedestals->getValue(index, amupre, kLG_Pre)+
				 fPedestals->getValue(index, amupost, kLG_Post)) : 0. ;

	hg += phg;
	lg += plg;
      }
      int iadc = static_cast<int>(floor(adc+0.5));
      int itdc = static_cast<int>(floor(tdc+0.5));
      mdo.Set(index, iadc, itdc, hg, lg) ;
    }
    else {
      mdo.Set(index, adc, tdc) ;
    } 

  }

  return rv ; 
} 

//_____________________________________________________________________________
bool emcRawDataCalibratorV2::Calibrate(const emcMixedDataObject& mdo,
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
  emcRawDataAccessor *frda   = emcRawDataAccessor::GetInstance();
  EmcDynamicData     *fdd    = frda->GetDynamicData() ;
  const SuperModule  *femmap = fdd->getSMMap();
 
//   if ( fECalAtT0.empty() ) {
//     if(!GetECalAtT0(when,true)) return false;
//   }
//   bool rv = true ; 
  Int_t index ; 


//   if (fMustCollectForCDO) {
//     CollectForCDO(when) ;
//     tf = pbscTimingFixes::getInstance();
//     if(!(tf->areFixesLoaded())) tf = 0;
//     fMustCollectForCDO = false ;
//   }

  bool energycalibrated = true; 
  bool timecalibrated   = true;
  
  cdo.Reset();

  Int_t   outindex =0; 
  Float_t etotal   =0.;

  // Loop over towers
  for (index = 0; index < mdo.GetSize(); index++ ) {

    int TowerId = mdo.GetItemId(index) ;
    int iFEM    = femmap[index/144].absPosition;

    // Skip reference towers, if any.
    if (EmcIndexer::isReference(TowerId)) continue;

    Float_t adc = mdo.GetADC(index) ; 
    float asv = adc;
    float tsv = mdo.GetTDC(index) ;
    Float_t tdc = tsv; 
    
    int errorFlag = mdo.GetErrorFlag(index);

    if( adc > GetThresholdADC() && !(errorFlag&0x2000)) { 
      if (!(errorFlag&0x2400)) {
	//	timecalibrated &= CalibrateTime(tdc, adc, index, TowerId, eventTime );
	//	timecalibrated &= CalibrateTime(tdc, adc, index, TowerId, 0);
	//   01/18/2010 - copied implementation from emcDCProcessorv3
	timecalibrated = (TowerId<15552? 
			  calibrateTimePbSc(adc, tdc, index, iFEM, 0) :
			  calibrateTimePbGl(adc, tdc, index, iFEM, 0) );
      }
      //      energycalibrated &= CalibrateEnergy( adc, index, TowerId, eventTime) ;
      //      energycalibrated &= CalibrateEnergy( adc, index, TowerId, 0) ;
      energycalibrated = (TowerId<15552? 
			  calibrateEnergyPbSc(adc, index, TowerId, iFEM, 0) :
			  calibrateEnergyPbGl(adc, index, TowerId, iFEM, 0) );
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
    if ( fVerbose>=1 && asv>10.) cout<<"Calib "<<index<<" ADC "<<asv<<" E "<<adc<<" TDC "<<tsv<<" T"<<tdc<<endl;
  }  
  cdo.SetTotalEnergy(etotal);
  cdo.SetZeroSuppressedFlag(fZeroSuppression);
  cdo.SetEnergyCalibratedFlag(energycalibrated);
  cdo.SetTimeCalibratedFlag(timecalibrated) ; 

  return true ; 
 } 

//_____________________________________________________________________________
bool emcRawDataCalibratorV2::Calibrate(const emcRawDataObject& rdo,
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

//_____________________________________________________________________________
//  iFEM - is an absolute FEM number in the 0-172 range, stupid CDH takes it as a position inside its internal vector of FEM's which it sets to 172. FEM knows its real absolute number and collects correct data from DB. Essential difference between this Calibrator and one used in reconstruction is ordering of the data in fECalAtT0. This calibrator stores only towers present in conf. file and orders them to match data in ADC list. Reconstruction stores initial calibration for every tower and orders those by TowerId (but it is doing it only if CH is created with iniall=true). 
//_____________________________________________________________________________

bool
emcRawDataCalibratorV2::calibrateEnergyPbGl(float & adc, int index, int twrId, int ifem, time_t ti)
{  
  const emcCalFEM* GN  = fCH->getCalibration(ifem,"Gains");
  assert(GN!=0);
  float normt          = GN->getValue(index%144, ti);
  //  return (normt>0.? adc*fECalAtT0[index]/normt : 0.);
  adc = (normt>0.? adc*fCH->getEnergyCalibration(twrId)/normt : 0.);
  return true;
}

//_____________________________________________________________________________
bool
emcRawDataCalibratorV2::calibrateEnergyPbSc(float & adc, int index, int twrId, int ifem, time_t ti)  
{
  //  if base-line corrections are required - we will need to chose different gain kind but let's keep it for the future - at least for now in production it is "Gains" prompt
  const emcCalFEM* GN = fCH->getCalibration(ifem,"Gains");
  assert(GN!=0);
  float normt          = GN->getValue(index%144, ti);
  //  cout<<"calibrateEnergyPbSc index "<<index<<" twrId "<<twrId<<" ifem "<<ifem<<" normt "<<normt<<" cal0 "<<fCH->getEnergyCalibration(twrId)<<" adc "<<adc<<"  Energy "<<adc*fCH->getEnergyCalibration(twrId)/normt<<endl;
  //  return (normt>0.? adc*fECalAtT0[index]/normt : 0.);
  adc = (normt>0.? adc*fCH->getEnergyCalibration(twrId)/normt : 0.);
  return true;
}

//_____________________________________________________________________________
bool 
emcRawDataCalibratorV2::calibrateTimePbGl(float adc, float & tdc, int index, int ifem, time_t ti)
{
  const emcCalFEM* LC = fCH->getCalibration(ifem,"LCTofs");
  const emcCalFEM* WT = fCH->getCalibration(ifem,"WalkTofs");
  const emcCalFEM* T0 = fCH->getCalibration(ifem,"TofT0Bs");
  
  assert(LC!=0);
  assert(WT!=0);
  assert(T0!=0);

  int channel = index%144;
  float t0 = T0->getValueFast(channel,0);
  float lc = LC->getValueFast(channel,0);
  lc = ((lc>20. && lc<55.)? lc : 38.9)/1000.;

  float wk = WT->getValueFast(channel,1);
  float dt = (adc>0.? wk*log(adc) : 0.);

  tdc = - (tdc-dt)*lc - t0;
  return true;
}

//_____________________________________________________________________________
bool
emcRawDataCalibratorV2::calibrateTimePbSc(float adc, float & tdc, int index, int ifem, time_t ti)
{

  const emcCalFEM* LC = fCH->getCalibration(ifem,"LCTofs");
  const emcCalFEM* WT = fCH->getCalibration(ifem,"WalkTofs");
  const emcCalFEM* T0 = fCH->getCalibration(ifem,"TofT0Bs");
  
  assert(LC!=0);
  assert(WT!=0);
  assert(T0!=0);

  int channel = index%144;
  float lc = LC->getValueFast(channel,0);
  float wk = WT->getValueFast(channel,1);
  float t0 = T0->getValueFast(channel,0);

  lc = ((lc>25.&&lc<65.)? lc : 40.0)/1000.;  


  float walk;
//   if (fCH->runNumber()<213889) // Run 4..6
//     {
//       walk = (adc>0.&&wk<0.)? wk*4000./adc : 0.;
//     }
//   else if (fCH->runNumber()<275899}// Run 7, 8 (walk in low gain mode)
//     {
//       walk = (adc>0.&&wk<0.)? wk*1000./cbrt((float)(adc/16.)) : 0.;
//     }
//   else     //  Run 9 and later
//     {
  walk = (adc>0.)? wk*6000./cbrt((float)adc) : 0.;
      //    }
  //  cout<<"calibrateTimePbSc index "<<index<<" ifem "<<ifem<<" adc "<<adc<<" tdc "<<tdc<<" lc "<<lc<<" wk "<<wk<<" t0 "<<t0<<" Time "<<- lc*(tdc-walk) - t0<<endl;

  tdc = - lc*(tdc-walk) - t0;
  return true;
}


//  ORIGINAL IMPLEMENTATION FROM TIME LONG PAST
//-------------------------------------------------------------------------
bool emcRawDataCalibratorV2::CalibrateEnergy(Float_t & adc, const Int_t index, 
					const Int_t TowerId,
					int incrementalTime) 
{
  if (fCollectGainStatus==false) { return false; }

  float normt, dummy;
  dummy = adc;
  if(EmcIndexer::isPbSc(TowerId)) {
    // PBSC
    normt = fGains->getValue(index,incrementalTime) ;
    //  we changed the range for Gains in pp-run5 - no more scaling for test-pulse
    adc  *= ((normt>0.01)?  fECalAtT0[index]/normt : 0.) ; 
    if ( fVerbose>=1 && dummy>10.) cout<<"<CE> Index "<<index<<" TowerId "<<TowerId<<" ADC "<<dummy<<" Time "<<incrementalTime<<" NORM "<<normt<<"/"<<fGains->getValue(index,0)<<" ECal "<<fECalAtT0[index]<<" Energy "<<adc<<endl;
  } else {
    assert(EmcIndexer::isPbGl(TowerId)) ;
    // PBGL  - EXPLICITLY NO TIMING DEPENDENCE FOR NOW 
    normt = fGains->getValue(index,0) ;
    adc  *= ((normt>0.)?  fECalAtT0[index]/normt : 0.) ; 
  }
  //  cout<<"<CE> Index "<<index<<" TowerId "<<TowerId<<" ADC "<<dummy<<" NORM "<<normt<<" ECal "<<fECalAtT0[index]<<" Energy "<<adc<<endl;
  return true ;
} 

//_________________________________________________________________________
bool  
emcRawDataCalibratorV2::CalibrateTime(Float_t & tdc, float adc, 
				    const Int_t index, const Int_t TowerId,
				      int /*incrementalTime*/)
{ 
  if (fCollectTofStatus==false) { return false; }
  float lc = fLCTofs->GetValue1(index);
  //  float t0 = 0.;
  float t0 = TowerId < 15552 ? fTofT0s->getValue(index,0) : fWalkTofs->GetValue1(index);;
  float wk = fWalkTofs->GetValue2(index);
  float dt;

  //=====> different functions for PbGl and PbSc MV 2001/08/24  
  if ( TowerId < 15552 )
    {
//       //  new set of 2003 calibration data has all LC defined. 
//       // We'll keep this test for just in case
//       lc = ((lc>32.&&lc<48.)? lc : 40)/1000.;    
//       //  EK 04/10/03
//       //  first attempt to introduce global corrections for 
//       // nonlinearities in the walk values
   
//       float walk        = (adc>0.)? wk*log(adc)*1000. : 0.;
//       //  next line is hopefully valid for all runs but functional 
//       //  form needs to be redefined
//       //  correction function depends on LG adc - it is enough to 
//       // use a common scale everywhere
//       float walkCorr    = (lc>0)? walkCorrection(adc/15.7) : 0.; 
//       //  due to nonlinear amplitude dependence
//       //  we will probably find some better way of explaining/presenting 
//       // this correction 
//       float walkStretch = (lc>0)? (wk-0.0469)*6.26     : 0.;   
//       //  due to residual walk stretching
//       tdc =  - ((tdc-t0-walk)*lc -walkCorr-walkStretch);
      

      lc = ((lc>25.&&lc<65.)? lc : 40.0)/1000.;  


      float walk = (adc>0.&&wk<0.)? wk*4000./adc : 0.;

      tdc =  - lc*(tdc-walk) - t0;
    } 
  else if ( TowerId < 24768 )
    {
      // Changed at Maxim's request, Sep 28, 2001 GD
      //    //=====> recalculate adc to low gain MV 2001/08/24
      //    float scale;
      
      
      //    if(scale<12.|| scale>18.) scale = 15.33;
      
      //    lc = ((lc>20. && lc<55.)? lc : 38.9)/1000.;
      //    float lgadc=adc/scale;
      //    dt=((lgadc>0.)? wk*pow(lgadc, -0.2): 0.);
      
      // MV 2001/09/27 walk correction is calculated using physics data,
      // so T0 should be taken care of automatically
      //      t0 = fWalkTofs->GetValue1(index);
      lc = ((lc>20. && lc<55.)? lc : 38.9)/1000.;
      dt=((adc>0.)? wk*log(adc): 0.);
      tdc =  - (tdc-t0-dt)*lc + GetGlobalT0() ;
    }
  
  return true;
}

//_____________________________________________________________________________
void emcRawDataCalibratorV2::CollectForCDO(const PHTimeStamp& when)
{
  // Collection of gain and time calibration data.
  cout << "emcRawDataCalibratorV2::CollectForCDO" << endl ;

  static emcGains    gainDriver ;
  static emcLCTofs   lctofDriver ;
  static emcWalkTofs walktofDriver ;

  //  EK 01/11/02
  static emcTofT0s    t0tofDriver;
  static emcTacPeds   drifttofDriver;
  
  emcDataManager* dm = emcDataManager::GetInstance() ;
  
  // FIXME : THIS WILL WORK ONLY IF ALL DATA HAVE THE SAME START 
  // OF VALIDITY TIME 
  // Try to collect Tof-related data
  if ( fTofSource != emcManageable::kNone && maxFailTof ) {
    // Try to collect T0 
    t0tofDriver.SetSource( fTofSource ) ;
    fTofT0s   = dynamic_cast<emcTofT0s*>( dm->Collect(t0tofDriver, when)) ; 
    //    fTacPeds   = dynamic_cast<emcTacPeds*>( dm->Collect(drifttofDriver, when)) ; 
    //    if(!fTacPeds) cout<<"failed to collect drifts"<<endl;
    // Try to collect least counts for ToF 
    lctofDriver.SetSource( fTofSource ) ;
    fLCTofs = dynamic_cast<emcLCTofs*>( dm->Collect(lctofDriver, when) ) ;
    //    cout<<"Collecting LCTOFs - done"<<endl;

    // Try to collect walks for ToF 
    walktofDriver.SetSource( fTofSource ) ;
    fWalkTofs = dynamic_cast<emcWalkTofs*>( dm->Collect(walktofDriver, when)) ;



    // (*1) ... here it might be that collection was not ok ...
    fCollectTofStatus = (fLCTofs!=0 && fWalkTofs!=0 && fTofT0s!=0) ;
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
      cerr << "<E> emcRawDataCalibratorV2::CollectForCDO " << endl 
	   << "    ToFs collection failed. Source: " <<fTofSource<< endl ;
    }
    if (!fCollectGainStatus) {
      cerr << "<E> emcRawDataCalibratorV2::CollectForCDO() " << endl
	   << "    Gains collection failed. Source: " <<fGainsSource<< endl ;
    }
  }
}

//_____________________________________________________________________________
void emcRawDataCalibratorV2::CollectForMDO(const PHTimeStamp& when)
{
  // Collection of pedestals, hlratios and Q&As
  cout << "emcRawDataCalibratorV2::CollectForMDO" << endl ;

  static emcPedestals5 pedDriver ;
  static emcHLRatios hlrDriver ;
  static emcQAs qaDriver ;

  emcDataManager* dm = emcDataManager::GetInstance() ;

  // by default we consider that collection never happened... (*1)
  fCollectPedestalStatus = fCollectHLRatioStatus = false ;

  // Try to collect pedestals
  if ( fPedestalsSource != emcManageable::kNone && maxFailPed ) {
    pedDriver.SetSource( fPedestalsSource  ) ;
    fPedestals = dynamic_cast<emcPedestals5*>( dm->Collect(pedDriver, when) ) ;
    // (*1) ... here it might be that collection was not ok ...
    fCollectPedestalStatus = (fPedestals!=0) ;
    maxFailPed -= !fCollectPedestalStatus ;
    if(!fCollectPedestalStatus) {
      cout << "<E> emcRawDataCalibratorV2::CollectFromMDO() " << endl
	   << "    Pedestal collection at "<<when<<" failed. Source: " <<fPedestalsSource<< endl ;
    } else {cout<<"<I> Pedestal collection succeded " << endl;}
  } else {cout<<"<I> Pedestal collection inhibited " << endl;}

  //  if( fVerbose>=1 && !fCollectPedestalStatus) {

  // Try to collect H/L Ratios
  if (!fUseOnlyLowGain && fHLRatiosSource != emcManageable::kNone && maxFailHLR ) {
    // (*1) ... here it might also be that collection was not ok ...
    hlrDriver.SetSource( fHLRatiosSource) ;
    fHLRatios = dynamic_cast<emcHLRatios*>( dm->Collect(hlrDriver, when) ) ;
    // (*1) ... here it might be that collection was not ok ...
    fCollectHLRatioStatus = (fHLRatios!=0) ;
    maxFailHLR -= !fCollectHLRatioStatus ;
    if(!fCollectHLRatioStatus) {
      cout << "<E> emcRawDataCalibratorV2::CollectFromMDO() " << endl
	   << "    HLRatio collection failed." << endl ;
    } else {cout<<"<I> HLRatio collection succeded " << endl;}
  } else {cout<<"<I> HLRatio collection inhibited " << endl;}

  // Try to collect Q&A objects
  if ( fQASource != emcManageable::kNone ) {

    // Set some parameters for the qa object we want
    qaDriver.SetSource(fQASource) ;

    // FIXME: unclear if this is correctly made - call on every event
    qaDriver.SetExtraRejectListFilename(fExtraRejectListFilename.c_str()) ;

    // Collect it
    fQA = dynamic_cast<emcQAs*>( dm->Collect(qaDriver,when) ) ;
    if (!fQA) {
      cout << "<E> emcRawDataCalibratorV2::CollectFromMDO() " << endl
	   << "    QA collection failed." << endl ;
    } else {cout<<"<I> QA collection succeded " << endl;}
    assert(fQA!=0) ;
  } else {cout<<"<I> QA collection inhibited " << endl;}

} 

//_____________________________________________________________________________
//  IMPORTANT - fEcalAtT0 is ordered by FEM channel numbers in the data (defined by configuration file)

bool 
emcRawDataCalibratorV2::GetECalAtT0(const PHTimeStamp& when, 
				    bool normalizationON) 
{
  if (!fECalAtT0.empty()) 
    { 
      // we already have it 
      return true ;
    }

  //  emcDataManager* dm = emcDataManager::GetInstance();
  emcRawDataAccessor * rda = emcRawDataAccessor::GetInstance() ; 
  size_t ntowers = 144*rda->GetDynamicData()->getnSM() ;
  int * DataMap  = rda->GetDynamicData()->getEmcMap();

  fECalAtT0.resize(ntowers,1.0) ;

  emcCalibrationData * sector[8];

  // Load the sectors initial calibration from the required source.

  for ( int isector = 0; isector < 8; ++isector ) 
    {
      sector[isector] = (emcCalibrationData *)fCH->getCalibrationData("IniCal",isector);
      if (!sector[isector]) 
	{
	  cerr << "<E> emcRawDataCalibratorV2::GetECalAtT0 : "
	       << "cannot get sector " << isector 
	       << " from source=" 
	       << emcManageable::GetStorageName(fIniCalSource)
	       << endl;
	}
    }

  // Populate the fECalAtT0 array from sectors initial calibrations

  for ( int item = 0; item < rda->GetDynamicData()->getEmcSize(); item++) 
    {
      int sn,ist;
      //  DataMap is TowerId
      EmcIndexer::iPXiSiST(DataMap[item], sn, ist) ;
      
      float encal,norm0,one;
      
      if ( sn < 6 ) 
	{
	  encal = sector[sn]->GetValue(ist,0);
	  norm0 = sector[sn]->GetValue(ist,1);
	  one = sector[sn]->GetValue(ist,2);
	  assert(one==1.0);
	  fECalAtT0[item] = encal*( normalizationON ? norm0 : 1. ) ;   
	}
      else if(sn>=6&&sn<8)
	{
	  encal = sector[sn]->GetValue(ist,0) *
	    sector[sn]->GetValue(ist,1)*
	    sector[sn]->GetValue(ist,2);
	  fECalAtT0[item] = encal;
	}
      
      fECalAtT0[item] *= fUseOnlyLowGain? 16. : 1.;
    }
  
  // clean up after ourselves.
  for ( int isector = 0; isector < 8; ++isector ) 
    {
      delete sector[isector];
    }

  return true ;
}

//_____________________________________________________________________________
bool emcRawDataCalibratorV2::GetCollectionStatus(const char* type) const
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
void emcRawDataCalibratorV2::SetCollectionStatus(const char* type)
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

void emcRawDataCalibratorV2::Print() const
{
  cout << " emcRawDataCalibratorV2 setup : " << endl ;  

  cout  << " **** Pedestals  " ; 
  if ( fPedestalsSource == emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ; 
  }
  else if ( fPedestalsSource == emcManageable::kDB_Objy  ) {
    cout << "are read from Database (Objy)" << endl ; 
  }
  else if ( fPedestalsSource == emcManageable::kDB_Pg  ) {
    cout << "are read from Database (PG)" << endl ; 
  }
  else {
    cout << " = 0 (collection disabled)" << endl ; 
  }

  cout << " **** high/low gain ratios  " ; 
  if ( fHLRatiosSource ==  emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ; 
  }
  else if ( fHLRatiosSource ==  emcManageable::kDB_Objy ) {
    cout << "are read from Database (Objy)" << endl ; 
  }
  else if ( fHLRatiosSource ==  emcManageable::kDB_Pg ) {
    cout << "are read from Database (PG)" << endl ; 
  }
  else {
    cout << " = 16 " << endl ; 
  }

  cout << " **** Gains (current normalization)  " ; 
  if ( fGainsSource ==  emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ; 
  }
  else if ( fGainsSource ==  emcManageable::kDB_Objy ) {
    cout << "are read from Database (Objy)" << endl ; 
  }
  else if ( fGainsSource ==  emcManageable::kDB_Pg ) {
    cout << "are read from Database (PG)" << endl ; 
  }
  else {
    cout << " = 1 " << endl ; 
  }

  cout << " **** ToF calibration (least counts and walks)  " ; 
  if ( fTofSource ==  emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ; 
  }
  else if ( fTofSource ==  emcManageable::kDB_Objy ) {
    cout << "are read from Database (Objy)" << endl ; 
  }
  else if ( fTofSource ==  emcManageable::kDB_Pg ) {
    cout << "are read from Database (PG)" << endl ; 
  }
  else {
    cout << " = 1 " << endl ; 
  }

  cout << " **** Q&A " ;
  if ( fQASource == emcManageable::kFile_ASCII ) {
    cout << "are read from files " << endl ;
  }
  else if ( fQASource == emcManageable::kDB_Objy ) {
    cout << "are read from Database (Objy)" << endl ;
  }
  else if ( fQASource == emcManageable::kDB_Pg ) {
    cout << "are read from Database (PG)" << endl ;
  }
  else {
    cout << "are not read (perfect towers assumed) " << endl ;
  } 

  cout << " **** Initial Calibration " ;
  if ( fIniCalSource == emcManageable::kFile_ASCII) { 
    cout << "are read from files " << endl ;
  }
  else if ( fIniCalSource == emcManageable::kDB_Objy ) {
    cout << "are read from Database (Objy)" << endl ;
  }
  else if ( fIniCalSource == emcManageable::kDB_Pg ) {
    cout << "are read from Database (PG)" << endl ;
  }
  else if ( fIniCalSource == emcManageable::kDB_Construction ) {
    cout << "are read from construction DB " << endl ;
  }
  else {
    cout << " Huh ? : check your IniCal source !" << endl ;
  }

  cout << " **** High gain threshold = " <<  GetHighLowLimit() << endl ; 
  cout << " **** ADC threshold = " << GetThresholdADC() << endl ;
  cout << " **** Zero suppression is " <<     ((fZeroSuppression==true)?"ON":"OFF") << endl ;
  cout << " **** Will " <<((fUseAmpPedestals==true)? "SUBTRUCT ":"NOT SUBTRUCT") << " amplitude pedestals "<<endl ;
  cout << " **** Will use " <<((fUseOnlyLowGain==true)? " LOW GAIN LEG ONLY ":" BOTH GAIN LEGS") <<endl ;

}
//_____________________________________________________________________________
//  Prints debug information for individual tower
void  emcRawDataCalibratorV2::printData(const emcRawDataObject& rdo, const int item) const
{
  if(item<0 || item>=rdo.GetMaxSize()) return;
  Float_t tdc ;
  Float_t hgpost,hgpre,lgpost,lgpre ;
  int amupre,amupost,amutac ;
  int data_error ;
  //  we need AMU cell addresses to correctly extract pedestals
  rdo.Get(item,tdc,hgpost,lgpost,hgpre,lgpre,
	    amupre,amupost,amutac,data_error) ;
  cout<<"ITEM "<<item<<" AMU(pre,post,tac) " <<amupre<<"/"<<amupost<<"/"<<amutac;
  cout<<" H/L ";
  if(fCollectHLRatioStatus) cout<<fHLRatios->getValue(item); else cout<<"none";
  cout<<" C0 ";
  if(!fECalAtT0.empty()) cout<< fECalAtT0[item]; else cout<<"none ";
  cout<<" NORM(t) ";
  if(fCollectGainStatus) cout<< fGains->getValue(item,0)<<endl; else cout<<" none "<<endl;
  cout<<"HG: (post/pre) "<<hgpost<<"/"<<hgpre<<" LG: (post/pre) "<<lgpost<<"/"<<lgpre<<"  TAC "<<tdc<<endl;
  if(fCollectPedestalStatus) 
    cout<<"PEDESTALS: HG Post "<<
      fPedestals->getValue(item, amupost, "HG_Post")<<" HG pre "<<fPedestals->getValue(item, amupre, "HG_Pre")<<
      " LG Post "<<
      fPedestals->getValue(item, amupost, "LG_Post")<<" LG pre "<<fPedestals->getValue(item, amupre, "LG_Pre")<<
      " TAC "<<fPedestals->getValue(item, amutac, "TAC")<<endl; 
}

//_____________________________________________________________________________
void emcRawDataCalibratorV2::Reset(void)
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
  fCH = NULL;
}

//-------------------------------------------------------------------------
bool emcRawDataCalibratorV2::SelectSource(const char* type, 
					emcManageable::EStorage source)
{
  /* The validity of the source parameter depends on the mapping style
     of the calorimeter. If 144 words per FEM, all sources allowed. 
     If 192 words per FEM, no source at all allowed.
  */
  
  emcRawDataAccessor * rda = emcRawDataAccessor::GetInstance() ; 

  if (!rda) {
    cerr << "<E> emcRawDataCalibratorV2:: SelectSource - Cannot check source is valid because I cannot get access to RDA. So I am NOT changing the source." << endl ;
    return false ;
  }
  else {
    /* Check that the source is valid. 
       Basically, if we deal with 144 words per FEM, all sources allowed,
       if 192 words per FEM, valid source = none. */
    if ( rda->GetDynamicData()->getMapStyle() == false &&
	 source != emcManageable::kNone ) {
      cerr << "<E> emcRawDataCalibratorV2::SelectSource - Map style is 192 words per FEM. Cannot use any source." << endl ;
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
    cerr << "emcRawDataCalibratorV2::SelectSource: " << type << " is an unknown type " << endl
	 << "                                    Valid types are Pedestals, HLRatios, Gains, ToF " << endl ; 
    rv = false ; 
  }
  
  return rv ; 
  
}

//_________________________________________________________________________
// Set Global T0 for every calorimeter Tower 
// The simplest solution for now was to write an ASCII file without any DB connectivity and load it at will
void emcRawDataCalibratorV2::SetTwrGlobalT0(char * filename){
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
