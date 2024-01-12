#include <EmcTofWalkRecalReco.h>
#include <emcCalibrationData.h>
#include <emcCalibrationDataHelper.h>
#include <emcCalFEM.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <emcDataManager.h>
#include <emcManageable.h>
#include <EmcIndexer.h>
#include <PhPhotonList.h>
#include <PHGlobal.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <RunHeader.h>
#include <Event.h>
#include <EventHeader.h>
#include <MasterRecalibrator.h>
#include <recoConsts.h>
#include <RunToTime.hh>
#include <RunNumberRanges.h>

#include <gsl/gsl_const_cgsm.h>
#include <cassert>
#include <iostream>

using namespace std;

//_____________________________________________________________________________
EmcTofWalkRecalReco::EmcTofWalkRecalReco(const string &name): Recalibrator(name)
{
  baseclasses.insert("emcClusterContainer");
  setLoadPbSc();
  setLoadPbGl();
  setLoadSectorOffsets(false);
  setSourceTofT0Bs(emcManageable::kDB_Pg);
  setSourceWalkTofs(emcManageable::kDB_Pg);
  setSourceSectorOffsets(emcManageable::kDB_Pg);
  setUseDirectADC(true);
  fdeactivated = false;
}

//_____________________________________________________________________________
int
EmcTofWalkRecalReco::isValidRun(const int runno) const
{
  // for run7 and run8
  if (runno > BEGIN_OF_RUN7 && runno < BEGIN_OF_RUN9)
  {
    return 1;
  }
  return 0;
}

//_____________________________________________________________________________
void EmcTofWalkRecalReco::Deactivate() 
{
  std::cerr << PHWHERE << " WARNING: Can't work on node " << inputnodename << ". Deactivated!" << endl;
  fdeactivated = true;
}


//_____________________________________________________________________________

void EmcTofWalkRecalReco::CacheGainConstants(int runnumber) {
  /// \brief Cache gain constants into memory
  static const char * fksGainsBLR = "Gains:BLR:0:x:ZS:AVOFRATIO:164777"; //runno>=164777;

  // incremental time (from begin-of-run timestamp, for gain tracing)
  RunToTime *run_time=RunToTime::instance();
  PHTimeStamp * begintime = run_time->getBeginTime(runnumber);
  time_t incremental_time=begintime->getTics();
  delete begintime;

  emcCalibrationDataHelper * CDH = new emcCalibrationDataHelper(runnumber,false);

  for (int iTwr=0; iTwr<ntowers; iTwr++) {
    int fem, ch;
    EmcIndexer::PXPXSM144CH(iTwr, fem, ch);
 
    // Gains & Normt
    const emcCalFEM* calfem = CDH->getCalibration(fem, fksGainsBLR);
    fnormt[iTwr] = calfem->getValue(ch,incremental_time);
    fgain[iTwr] = CDH->getEnergyCalibration(iTwr);
  }
  
  delete CDH;
}

//_____________________________________________________________________________

void EmcTofWalkRecalReco::CacheOldConstants(int runnumber) {
  /// \brief Cache old calibration constants into memory
  emcCalibrationDataHelper * CDH = new emcCalibrationDataHelper(runnumber,false);
  CDH->setSource("TofT0Bs", fsrcOldTofT0Bs); 
  CDH->setSource("WalkTofs", fsrcOldWalkTofs);
  CDH->setSource("TofSectorOffset",fsrcOldSectorTofs);
  
  for (int iTwr=0; iTwr<ntowers; iTwr++) {
    int fem, ch;
    EmcIndexer::PXPXSM144CH(iTwr, fem, ch);
    bool isPbsc = EmcIndexer::isPbScFEM(fem);
    
    // Walk
    if ( (fpbscwalk && isPbsc) || (fpbglwalk && !isPbsc) ) {
      const emcCalFEM* WK = CDH->getCalibration( fem ,"WalkTofs");
      assert(WK!=0);
      fOwk[iTwr] = WK->getValueFast( ch, 1 );
    }
    
    if ( (fpbsctwr && isPbsc) || (fpbgltwr && !isPbsc) ) {
      // Tower Offset
      const emcCalFEM* T0 = CDH->getCalibration( fem,"TofT0Bs");
      assert(T0!=0);
      fOtwroff[iTwr] = T0->getValueFast(ch,0);
    }
  }
  
  if (fdosec) {
    for (int isec=0; isec<nsectors; isec++) {
      const emcCalibrationData* so = CDH->getCalibrationData("TofSectorOffset",isec);
      fOsecoff[isec] = so->GetValue(0,2) - so->GetValue(0,3);   // sectort0 - <bbct0>
      cout << fOsecoff[isec] << endl;
    }
  }
  
  delete CDH;
}

//_____________________________________________________________________________

void
EmcTofWalkRecalReco::CacheConstants(int runnumber) {
  /// \brief Cache (re)calibration constants into memory
  emcCalibrationDataHelper * CDH = new emcCalibrationDataHelper(runnumber,false);
  CDH->setSource("TofT0Bs", fsrcTofT0Bs); 
  CDH->setSource("WalkTofs", fsrcWalkTofs);
  CDH->setSource("TofSectorOffset",fsrcSectorTofs);

  for (int iTwr=0; iTwr<ntowers; iTwr++) {
    int fem, ch;
    EmcIndexer::PXPXSM144CH(iTwr, fem, ch);
    bool isPbsc = EmcIndexer::isPbScFEM(fem);
 
    // Walk
    if ( (fpbscwalk && isPbsc) || (fpbglwalk && !isPbsc) ) {
      const emcCalFEM* WK = CDH->getCalibration( fem ,"WalkTofs");
      assert(WK!=0);
      fwk[iTwr] = WK->getValueFast( ch, 1 );
      if (fafterburn==2)
	fOwk[iTwr] = WK->getValueFast( ch, 0 );
    }
    
    if ( (fpbsctwr && isPbsc) || (fpbgltwr && !isPbsc) ) {
      // Tower Offset
      const emcCalFEM* T0 = CDH->getCalibration( fem,"TofT0Bs");
      assert(T0!=0);
      ftwroff[iTwr] = T0->getValueFast(ch,0);
    }
    
    if ( (fpbsclc && isPbsc) || (fpbgllc && !isPbsc) ) {
      // Least Count
      const emcCalFEM* LC = CDH->getCalibration( fem, "LCTofs");
      assert(LC!=0);
      float lc = LC->getValueFast( ch, 0 );
      lc = ((lc>25.&&lc<65.)? lc : 40.0)/1000. ;
      flc[iTwr] = lc;
    }
    
  }
  
  if (fdosec) {
    for (int isec=0; isec<nsectors; isec++) {
      const emcCalibrationData* so = CDH->getCalibrationData("TofSectorOffset",isec);
      fsecoff[isec] = so->GetValue(0,2) - so->GetValue(0,3);   // sectort0 - <bbct0>
    }
  }

  delete CDH;
}

//_____________________________________________________________________________
int
EmcTofWalkRecalReco::InitRun(PHCompositeNode* topNode)
{

  //******************************************
  // here comes the switch...case later 
  // when there'll be anything to decide about

  setUseAfterburn(2);

  //******************************************

  recoConsts *rc = recoConsts::instance();
  int runnumber =  rc->get_IntFlag("RUNNUMBER");
  
  if (verbosity) 
    std::cout << PHWHERE << " caching constants for run #" 
	      << runnumber << "." << std::endl;

  flc.clear();  fgain.clear();    fnormt.clear();
  fwk.clear();  ftwroff.clear();  fsecoff.clear();
  fOwk.clear(); fOtwroff.clear(); fOsecoff.clear();

  flc.resize(ntowers,0);  fgain.resize(ntowers,0);    fnormt.resize(ntowers,0);
  fwk.resize(ntowers,0);  ftwroff.resize(ntowers,0);  fsecoff.resize(nsectors,0);
  fOwk.resize(ntowers,0); fOtwroff.resize(ntowers,0); fOsecoff.resize(nsectors,0);
 
  // test inputnodename
  if ( strncmp( inputnodename.c_str(), "emcClusterContainer", 19) ) {
    if (fafterburn) {
      setUseDirectADC(false);  // must recompute ADC
      CacheGainConstants(runnumber);
    }
    else { // unable to go from raw if PhPhotonlist!
      Deactivate();
      return 0;
    }
  }

  if (fafterburn==1) {
    setLoadPbGl(false);

    setSourceTofT0Bs(emcManageable::kNone, emcManageable::kDB_Pg);
    setSourceWalkTofs(emcManageable::kNone, emcManageable::kDB_Pg);
    setSourceSectorOffsets(emcManageable::kNone, emcManageable::kNone);
    emcDataManager* dm = emcDataManager::GetInstance(); 
    // this is an ascii reading patch... brooaghh... used only for testing
    dm->SetSourceDir("/afs/rhic.bnl.gov/phenix/users/vertesi/public/recal/");
    CacheOldConstants(runnumber); // load old wk, sect0, twrt0 constants
 }
  else if (fafterburn==2) {
    setLoadPbGlWalks(false);
    setLoadPbGlLeastCounts(false);
  }
  else {
  }

  CacheConstants(runnumber); // load needed wk, sect0, twrt0, lc, gain, normt constants
  
  return 0;
}

//_____________________________________________________________________________
int
EmcTofWalkRecalReco::EndRun(const int)
{ 
  return 0;
}


//_____________________________________________________________________________
float
EmcTofWalkRecalReco::restoreADC(const int twrid, const float ecent) {
  static const float fgNormtLimitPbSc = 0.01;

  float normt = fnormt[twrid];
  float energycal = fgain[twrid];

  float adc;
  if ( normt > fgNormtLimitPbSc )
    adc = ecent*normt/energycal;
  else
    adc = 0.;
  
  return adc;
}

//_____________________________________________________________________________
float EmcTofWalkRecalReco::flashTime(const float xgl, const float ygl, const float zgl, const float zvertex) 
{
  const double kSpeedOfLight = GSL_CONST_CGSM_SPEED_OF_LIGHT*1E-9; //cm.ns-1
  
  // find flashtime
  float vx = -xgl;
  float vy = -ygl;
  float vz = zvertex-zgl;
  float lactual = sqrt( vx*vx + vy*vy + vz*vz );
  float lnominal = sqrt(xgl * xgl + ygl * ygl + zgl * zgl);
  float dd = lactual - lnominal;
  
  return dd/kSpeedOfLight;
}


//_____________________________________________________________________________
float
EmcTofWalkRecalReco::calibrateTime(const int twrid, const float adc, const float tdc, const float lc, const float wk, const float t0)
{
  int fem, ch;
  EmcIndexer::PXPXSM144CH(twrid, fem, ch);

  float walk;
  if ( EmcIndexer::isPbScFEM(fem) ) { // PbSc
    recoConsts *rc = recoConsts::instance();
    int runnumber =  rc->get_IntFlag("RUNNUMBER");
    
    if (runnumber<213889) { // Run 4..6
      walk = (adc>0.&&wk<0.)? wk*4000./adc : 0.;
      }
    else { // Run 7...
      walk = (adc>0.&&wk<0.)? wk*1000./cbrt((float)adc) : 0.;
    }
  }
  else //PbGl
    walk = wk*std::log(adc);
  
  return -(tdc-walk)*lc - t0;
}

//_____________________________________________________________________________
void
EmcTofWalkRecalReco::SetClusterDeltaT(emcClusterContent * c, float toffset, float tcorroffset) {
  //c->set_tof( c->tof() + toffset ); 
  //if ( c->has_Tofmin() ) c->set_tofmin( c->tofmin() + toffset );
  //if ( c->has_Tofmax() ) c->set_tofmax( c->tofmax() + toffset );  

  c->set_tofcorr( c->tofcorr() + tcorroffset );      
  if ( c->has_Tofcorrmin() ) c->set_tofcorrmin( c->tofcorrmin() + tcorroffset );      
  if ( c->has_Tofcorrmax() ) c->set_tofcorrmax( c->tofcorrmax() + tcorroffset );  
}

//_____________________________________________________________________________
int
EmcTofWalkRecalReco::process_event(PHCompositeNode* topNode)
{
  /// \brief Adjust cluster time with the recalibration consts
  
  if (fdeactivated) return 0;
  
  // find vertex
  PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  if (!global)
    {
      std::cerr << PHWHERE << " No PHGlobal, skipping event ! " << std::endl;
      return 0;
    }
  float zvtx = global->getBbcZVertex();
  
  // get cluster container and work on it
  emcClusterContainer *clusters =
    findNode::getClass<emcClusterContainer>(topNode, inputnodename.c_str());
  if (!clusters)
    {
      std::cerr << PHWHERE << " No emcClusterContainer object, skipping event !" << std::endl;
      return 0;
    }

  for (unsigned int i=0; i<clusters->size(); i++) {
    emcClusterContent* c = clusters->getCluster(i);
   
    int iTwr = c->towerid(0);
 
    if ( fdirectadc && !c->has_adc() ) {
      setUseDirectADC(false);
      recoConsts *rc = recoConsts::instance();
      int runnumber =  rc->get_IntFlag("RUNNUMBER");
      CacheGainConstants(runnumber);
    }
    
    float adc;
    if (fdirectadc)
      adc = c->adc();
    else
      adc = restoreADC( iTwr, c->ecent() );
 
    int isec, dummy;
    EmcIndexer::decodeTowerId(iTwr,isec,dummy,dummy);
 
    c->set_tof( c->tof() + global->getBbcTimeZero()/2 );
    if (fafterburn) {
      // relative correction ("afterburner")
      float told=calibrateTime(iTwr, adc, 0., flc[iTwr], fOwk[iTwr], fOtwroff[iTwr]+fOsecoff[isec]);
      float tnew=calibrateTime(iTwr, adc, 0., flc[iTwr], fwk[iTwr], ftwroff[iTwr]+fsecoff[isec]);
      //cout << tnew-told << "  " << tnew-told << endl;
      SetClusterDeltaT ( c, tnew-told, tnew-told );
    }
    else {
      // absolute correction ("direct")
     
      if ( !c->has_rawtdc() ) {
	Deactivate();
	return 0;
      }
      
      float tdc = c->rawtdc();
      float tof = calibrateTime(iTwr, adc, tdc, flc[iTwr], fwk[iTwr], ftwroff[iTwr]+fsecoff[isec])
	- flashTime(c->x(),c->y(),c->z(),zvtx);
      float dt = tof - c->tof();
      float dtcorr = tof - c->tofcorr();

      SetClusterDeltaT( c, dt, dtcorr );
    }
  }


  
  return 0;
}
