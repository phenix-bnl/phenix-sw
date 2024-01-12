// The recalreco module corrects the energy scales in pairs of
// sectors in the EMCal. The functions were derived using
// pi0 peak positions from the Run4 200 GeV data set.
// The class is extended from EmcAbsEScaleRecalReco
// Original Author: Peter Tarjan.
// Modified  by T. Sakaguchi. The code adopted to database

#include "EmcGenericEScaleRecalReco.h"

#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <emcTowerContainer.h>
#include <emcTowerContent.h>
#include <emcGeaTowerContent.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <EmcIndexer.h>

#include <PHCompositeNode.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbFloatVector.hh>
#include <RunToTime.hh>
#include <PdbEmcEScaleTowerRecal.hh>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <getClass.h>
#include <recoConsts.h>
#include <PHPanel.h>
#include <PHVector.h>
#include <PHPoint.h>
#include <PHLine.h>

#include <mEmcGeometryModule.h>

#include "TOAD.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>

using namespace std;


EmcGenericEScaleRecalReco::EmcGenericEScaleRecalReco(const string &name): Recalibrator(name)
{
  baseclasses.insert("emcClusterContainer");
  baseclasses.insert("PHCentralTrack");
  baseclasses.insert("emcTowerContainer");
}

int
EmcGenericEScaleRecalReco::isValidRun(const int runno) const
{
  if ((runno >= 107445 && runno <= 122223) ||
      (runno >= 168000 && runno <= 180000) ||
      (runno >= 185000 && runno <= 207000) ||
      (runno >= 228042 && runno <= 240121) || // Run7 Au+Au
      (runno >= 246444 && runno <= 253701) || // Run8 d+Au
      (runno >= 256450 && runno <= 259575) || // Run8 p+p
      (runno >= 310698 && runno <= 313322) || // Run10 62GeV Au+Au
      (runno >= 313591 && runno <= 314994) || // Run10 39GeV Au+Au
      (runno >= 300475 && runno <= 310454) || // Run10 200GeV Au+Au
      (runno >= 372402 && runno <= 377310) || // Run12 200GeV Cu+Au
      (runno >= 405860 && runno <= 414988) ||  // Run14 200GeV Au+Au
      (runno >= 415370 && runno <= 416893) ||  // Run14 200GeV He3+Au
      (runno >= 421707 && runno <= 438422) ||  // Run15 200GeV p+p and p+Au
      (runno >= 454774 && runno <= 455639) ||  // Run16 200GeV d+Au
      (runno >= 443112 && runno <= 454387) ||  // Run16 200GeV Au+Au 1st period
      (runno >= 458390 && runno <= 459344)) // Run16 200GeV Au+Au 2nd period
    {
      return 1;
    }
  return 0;
}

int
EmcGenericEScaleRecalReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  run = rc->get_IntFlag("RUNNUMBER");

  nonlincorr=0;
  poscorr=0;
  RbyRcorr=0;
  SecbySecCorr=0;
  Run10RRSSCorrs = 0; 
  Run14Corr = 0;
  Run15Corr = 0;
  Run16Corr = 0;
  Run16dAu200Corr = 0;
  Run12CuAuCorr = 0;

  //
  // If the flags are not set, use default values of "1"
  //
  if(run >= 107445 && run <= 122223){
     nonlincorr = rc->get_IntFlag("EmcNonLinCorr",1);
     poscorr = rc->get_IntFlag("EmcPosCorr",1);
     RbyRcorr = 0;
  }

  //
  // For Run7 Au+Au, we don't apply nonlinear or position correction for now.
  //   We apply RbyR correction now (Nov 7, 2008)
  //
  //   We decided to apply position correction if PosCorr is defined.
  //   (Nov 11, 2009)
  //
  //   add new flag for sector-by-sector correction (Feb 25, 2010)
  //   We apply new sec-by-sec correction now (Mar. 23, 2010)
  //   by Yoki Aramaki    
  //
  if(run >= 228042 && run <= 240121){
    // Run7 Au+Au
    nonlincorr=0;
    poscorr = rc->get_IntFlag("EmcPosCorr",0);
    RbyRcorr = rc->get_IntFlag("EmcRbyRCorr",1);
    SecbySecCorr = rc->get_IntFlag("EmcSecbySecCorr",1);
  }

  //
  // For Run8, we don't apply nonlinear or position correction for now.
  //   We apply RbyR correction now (Apr 22, 2009)
  //
  if((run >= 246444 && run <= 253701) || // Run8 d+Au
     (run >= 256450 && run <= 259575)){   // Run8 p+p
    poscorr=0; nonlincorr=0;
    RbyRcorr = rc->get_IntFlag("EmcRbyRCorr",1);
  }

  //
  // For Run10 39GeV, we don't apply nonlinear or position correction for now.
  //   We apply RbyR correction now (Jul 9, 2010)
  //
  // For Run10 200GeV too (Apr 23, 2011)
  //

  if((run >= 313591 && run <= 314994) ||   // Run10 39GeV
     (run >= 310698 && run <= 313322) ||   // Run10 62GeV Au+Au
     (run >= 300475 && run <= 310454)){     // Run10 200GeV Au+Au
    poscorr=0; nonlincorr=0;
    RbyRcorr = rc->get_IntFlag("EmcRbyRCorr",1);
    Run10RRSSCorrs = 1;            
  }

   if(run >=372402 && run <=377310){
    // Run12 Cu+Au
    nonlincorr=0;
    SecbySecCorr = 1;
    Run12CuAuCorr = 1;
  }
  if(run >= 415370 && run <= 416893){
    // Run14 He3+Au
    nonlincorr=0;
    SecbySecCorr = 1;
    Run14Corr = 1;
  }
  if(run >= 405860 && run <= 414988){
    // Run14 Au+Au
    nonlincorr=0;
    SecbySecCorr = 1;
    Run14Corr = 1;
  }

  if(run >= 421707 && run <= 438422){
    // Run15 p+p and p+Au
    nonlincorr=0;
    SecbySecCorr = 1;
    Run15Corr = 1;
  }

  if(run >= 454774 && run <= 455639){
    // Run16 d+Au 200GeV
    nonlincorr=0;
    SecbySecCorr = 1;
    Run16dAu200Corr = 1;
  }

  if((run >= 443112 && run <= 454387) || (run >=  458390 && run <= 459344)) {
    // Run16 Au+Au
    nonlincorr=0;
    SecbySecCorr = 1;
    Run16Corr = 1;
  }
  
  cout << endl;
  if(nonlincorr==1) cout <<"EMCal Energy Non linearity correction is in." << endl;
  if(poscorr==1) cout <<"EMCal Position shift correction is in." << endl;
  if(RbyRcorr==1) cout <<"EMCal Run-by-Run correction is in." << endl;
  if(SecbySecCorr==1) cout <<"EMCal Sector-by-Sector correction is in." << endl;
  cout << endl;

  int version = 1;
  recalemc = 0;
  recalcnt = 0;
  recaltwr = 0;
  if (mybaseclass == "emcClusterContainer")
    {
      recalemc = 1;
    }
  else if (mybaseclass == "PHCentralTrack")
    {
      recalcnt = 1;

  if((run>=168000 && run<=180000) || (run>=185000 && run<=207000)){
     cout << "\n!!!!! This is Run5 and Run6 p+p. EMCal energy in CNT may be wrong. !!!!!"<<endl;
     cout << "!!!!! Load PWG for obtaining correct energy. !!!!!\n" << endl;
  }

    }
  else if (mybaseclass == "emcTowerContainer")
    {
      recaltwr = 1;
    }

  // Do the position correction for Run4 Au+Au
  if((run >= 107445 && run <= 122223)
   || (run >= 228042 && run <= 240121 && poscorr==1)){
     double corrfac[8]={1.005,1.004,1.009,1.011,1.010,1.008,0.995,0.996};
     int RevArm[2]={1,0};

     mEmcGeometryModule *emcgeo = new mEmcGeometryModule();
     cout << "Position Correction Setup!!"<< endl;

     for(int i=0;i<8;i++){
        short arm,sec;
        emcgeo->emcToPhenix(i,arm,sec); 
        PHPanel panel = emcgeo->GetPanel(arm,sec);
        PHVector normal = panel.getNormal();
        PHLine line(PHPoint(0,0,0),normal);
        PHPoint proj;
        emcgeo->Intersection(line,i,proj);
        PHVector currvec(proj);
        PHVector shiftR = currvec*(corrfac[i]-1.0);
        corrposx[RevArm[arm]][sec]= shiftR.getX();
        corrposy[RevArm[arm]][sec]= shiftR.getY();
        corrposz[RevArm[arm]][sec]= shiftR.getZ();
        cout <<"sector " << i <<": ";
        cout <<"shiftX " << shiftR.getX() <<", ";
        cout <<"shiftY " << shiftR.getY() <<", ";
        cout <<"shiftZ " << shiftR.getZ() <<endl;
     }
     delete emcgeo;
  }

  //
  // Here is the common tower-by-tower calibration
  //

  // but this isn't used by Run 10 sector by sector cals
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbBankID bankID;
  bankID.setInternalValue (version);
  PHString calibname = "calib.emc.escale.recal";
  
  
  PdbCalBank *recalBank = 0;

  
  if (!Run10RRSSCorrs && !Run14Corr & !Run16Corr & !Run15Corr & !Run12CuAuCorr & !Run16dAu200Corr)
    {

      recalBank = bankManager->fetchBank ("PdbEmcEScaleTowerRecalBank", bankID, calibname.getString (), run);

	  
      if (recalBank)
	{
	  recalBank->printHeader ();
	  PHTimeStamp StartTime = recalBank->getStartValTime ();
	  PHTimeStamp EndTime = recalBank->getEndValTime ();
	  
	  PdbEmcEScaleTowerRecal recalpar = (PdbEmcEScaleTowerRecal &) recalBank->getEntry(0);
	  
	  for (int arm = 0;arm < 2;arm++)
	    {
	      for (int sect = 0;sect < 4;sect++)
		{
		  for (int iz = 0;iz < 96;iz++)
		    {
		      for (int iy = 0;iy < 48;iy++)
			{
			  _e_scale_status[arm][sect][iz][iy] = recalpar.getTwrEScaleStatus(arm, sect, iy, iz);
			  recalpar.getTwrEScaleFactor(arm, sect, iy, iz, _e_scale[arm][sect][iz][iy]);
			}
		    }
		}
	    }
	  if (verbosity > 0)
	    {
	      cout << _e_scale[0][0][0][0][0] << " ";
	      cout << _e_scale[0][1][0][0][0] << " ";
	      cout << _e_scale[0][2][0][0][0] << " ";
	      cout << _e_scale[0][3][0][0][0] << " ";
	      cout << _e_scale[1][0][0][0][0] << " ";
	      cout << _e_scale[1][1][0][0][0] << " ";
	      cout << _e_scale[1][2][0][0][0] << " ";
	      cout << _e_scale[1][3][0][0][0] << endl;
	      fflush(stdout);
	    }
	  delete recalBank;
	  //      return EVENT_OK;
	}
      else{
	cout << PHWHERE << " Could not load calibration from calib.emc.escale.recal for run "
	     << run << endl;
	exit(1);
      }
    }

  //
  // If Run10 Au+Au 39GeV, fetch sec-by-sec, run-by-run cal
  //
  // Run10 Au+Au 200GeV as well.
  //

  if((run >= 313591 && run <= 314994) ||  // Run10 39GeV
     (run >= 310698 && run <= 313322) ||  // Run10 62GeV Au+Au
     (run >= 300475 && run <= 310454)){   // Run10 200GeV Au+Au
      fetchSectorCal(run);
      return EVENT_OK;
  }
  
  if(run >=372402 && run <=377310)
  {    
      cout << "Run 12 Cu Au EMC E recalibration sector by sector" << endl;
      cout << run << endl;
      fetchSectorCal(run);
      return EVENT_OK;
  }
  
  if(run>=415370 && run<=416893)
    {    
      cout << "Run 14 EMC E recalibration by sector" << endl;
      fetchSectorCal(run);
      //return EVENT_OK;
    }
  if(run>=405860 && run<=414988) //Run14 AuAu calibration
    {    
      cout << "Run 14 EMC E recalibration by sector" << endl;
      fetchSectorCal(run);
      return EVENT_OK;
    }

  if(run >= 421707 && run <= 438422)
    {    
      cout << "Run 15 EMC E recalibration by sector" << endl;
      cout << run << endl;
      fetchSectorCal(run);
      return EVENT_OK;
    }

  if(run >= 454774 && run <= 455639)
    {    
      cout << "Run 16 d+Au 200 GeV EMC E recalibration by sector" << endl;
      cout << run << endl;
      fetchSectorCal(run);
      return EVENT_OK;
    }

  if((run >= 443112 && run <= 454387) || (run >=  458390 && run <= 459344))  //Run16 AuAu calibration
    {    
      cout << "Run 16 EMC E recalibration by sector" << endl;
      fetchSectorCal(run);
      return EVENT_OK;
    }

	  
  // If not Run7 Au+Au or discard Run-by-Run correction, return
  if((!(run >= 228042 && run <= 240121) && // Run7 Au+Au
      !(run >= 246444 && run <= 253701) && // Run8 d+Au
      !(run >= 256450 && run <= 259575)) || // Run8 p+p
       RbyRcorr==0 ) return EVENT_OK;

  //
  // Here, this is the special run-by-run calibration for Run7 PbGl
  // As well as run-by-run Run8 PbSc
  //
  version = 11001;
  bankID.setInternalValue (version);
  calibname = "calib.emc.gains.recal";

  recalBank = bankManager->fetchBank ("PdbEmcEScaleTowerRecalBank", bankID, calibname.getString (), run);

  if (recalBank)
    {
      recalBank->printHeader ();
      PHTimeStamp StartTime = recalBank->getStartValTime ();
      PHTimeStamp EndTime = recalBank->getEndValTime ();

      PdbEmcEScaleTowerRecal recalpar = (PdbEmcEScaleTowerRecal &) recalBank->getEntry(0);

      for (int arm = 0;arm < 2;arm++)
        {
          for (int sect = 0;sect < 4;sect++)
            {
              for (int iz = 0;iz < 96;iz++)
                {
                  for (int iy = 0;iy < 48;iy++)
                    {
                      _e_scale_RbyR[arm][sect][iz][iy][0]=0.0;
                      _e_scale_RbyR_status[arm][sect][iz][iy] = recalpar.getTwrEScaleStatus(arm, sect, iy, iz);
                      recalpar.getTwrEScaleFactor(arm, sect, iy, iz, _e_scale_RbyR[arm][sect][iz][iy]);
                      if(fabs(_e_scale_RbyR[arm][sect][iz][iy][0])<1.0e-3) _e_scale_RbyR[arm][sect][iz][iy][0]=1.0;
                    }
                }
            }
        }
      if (verbosity > 0)
        {
          cout << _e_scale_RbyR[0][0][0][0][0] << " ";
          cout << _e_scale_RbyR[0][1][0][0][0] << " ";
          cout << _e_scale_RbyR[0][2][0][0][0] << " ";
          cout << _e_scale_RbyR[0][3][0][0][0] << " ";
          cout << _e_scale_RbyR[1][0][0][0][0] << " ";
          cout << _e_scale_RbyR[1][1][0][0][0] << " ";
          cout << _e_scale_RbyR[1][2][0][0][0] << " ";
          cout << _e_scale_RbyR[1][3][0][0][0] << endl;
          fflush(stdout);
        }
      delete recalBank;
      return EVENT_OK;
    }
  else{
      cout << PHWHERE << " Could not load Run7 Run-by-Run calibration from calib.emc.gains.recal for run "
       << run << endl;
      exit(1);
    }
}


float EmcGenericEScaleRecalReco::get_correction(const int arm, const int sect, const int iz, const int iy,float _ecore) const
{
  float corr=1.0; //1.085
  //float r14_sec_escale[8] = {1, 1, 1.085, 1.069, 1.053, 1.03, 1.022, 1.022};

  
  if(run >= 415370 && run <= 416893){ // Run14 200GeV He3+Au
    if(arm==0) corr = r14_sec_escale[4+sect];
    else corr = r14_sec_escale[sect];
  }
  else if(run >= 405860 && run <= 414988){ // Run14 200GeV Au+Au
    if(arm==0) corr = r14_sec_escale[4+sect];
    else corr = r14_sec_escale[sect];
    return corr;
  }
  else if(run >= 421707 && run <= 438422){ // Run15 200GeV p+p and p+Au
    if(arm==0) corr = r15_sec_escale[4+sect];
    else corr = r15_sec_escale[sect];
    return corr;
  }
  else if(run >= 454774 && run <= 455639){ //Run16 200GeV d+Au
    if(arm==0) corr = r16dAu200_sec_escale[4+sect];
    else corr = r16dAu200_sec_escale[sect];
    return corr;
  }
  else if(run >= 372402 && run <= 377310){ // Run12 200GeV Cu+Au
    if(arm==0) corr = r12_CuAu_sec_escale[4+sect];
    else corr = r12_CuAu_sec_escale[sect];
    return corr;
  }
  else if((run >= 443112 && run <= 454387) || (run >=  458390 && run <= 459344)) { // Run16 200GeV Au+Au
    if(arm==0) corr = r16_sec_escale[4+sect];
    else corr = r16_sec_escale[sect];
    return corr;
  }

  else if((run>=168000 && run<=180000) || (run>=185000 && run<=207000)){

    // This is Run5 and Run6 p+p (updated July 31, 2006)
    corr = _e_scale[arm][sect][iz][iy][0];

  }
  else if(run >= 107445 && run <= 122223){
// They are Run4 Au+Au 200GeV
   
     float E0corr_a0 = 9.68014e-01;
     float E0corr_a1 = 1.05081e-01;
     float E0corr_a2 = -5.55101e-01;

     float E1corr_a0 = 9.90379e-01;
     float E1corr_a1 = 9.89748e-02;
     float E1corr_a2 = -9.09784e-01;

     float E0Addcorr_a0 = 9.98499e-01;
     float E0Addcorr_a1 = 1.80671e-02;
     float E0Addcorr_a2 = -5.18684e-01;

     float E1Addcorr_a0 = 9.83061e-01;
     float E1Addcorr_a1 = 6.11122e-02;
     float E1Addcorr_a2 = -1.78745e+00;


     // This is common to sectors except PbGl!
     float PbScPar0 = 0.99028;
     float PbScPar1 = -0.0681364;
     float PbScPar2 = -2.92568;


     if(arm==1 && sect==0)
        corr = (E0corr_a0+E0corr_a1*exp(E0corr_a2*_ecore))*(E0Addcorr_a0+E0Addcorr_a1*exp(E0Addcorr_a2*_ecore));

     else if(arm==1 && sect==1)
        corr = (E1corr_a0+E1corr_a1*exp(E1corr_a2*_ecore))*(E1Addcorr_a0+E1Addcorr_a1*exp(E1Addcorr_a2*_ecore));

     else if(nonlincorr==1)
        corr = _e_scale[arm][sect][iz][iy][0]/((PbScPar0+PbScPar1*exp(PbScPar2*_ecore))*1.01);

//     else corr = _e_scale[arm][sect][iz][iy][0];
  }
  else if(run >= 228042 && run <= 240121){


    float Adhoc_P0[4][6]={

       // E2, E3, W0, W1, W2, W3
       {0.249204, 0.293274, 0.279609, 0.251246, 0.330372, 0.259269},
       {0.263558, 0.349945, 0.360555, 0.392311, 0.342561, 0.366313},
       {-0.00598368, -0.0141662, 0.087728, 0.105516, 0.194672, 0.206094},
       {0.449152, 0.335631, 0.238202, 0.265092, 0.255464, 0.261995}};

    float Adhoc_P1[4][6]={
       // E2, E3, W0, W1, W2, W3
       {-4.808e-07,-6.746e-07,-6.169e-07,-4.830e-07,-8.315e-07,-5.224e-07},
       {-5.413e-07,-9.170e-07,-9.665e-07,-1.091e-06,-8.819e-07,-9.829e-07},
       {6.140e-07,6.432e-07,2.069e-07,1.440e-07,-2.401e-07,-2.877e-07}, 
       {-1.303e-06,-8.286e-07,-4.245e-07,-5.262e-07,-4.937e-07,-5.206e-07}};


    //run group: [228042,231500),[231500,234000),[234000,237550),[237550,240122);
    //fit function: f = p0 + p1 * runnumer;
    //column:
    //group, arm, sector, p0, p1
    //arm: 1-east, 0-west; only 6  pbsc sectors are here.
    
    // They are Run7 Au+Au 200GeV
    
    //     corr = _e_scale[arm][sect][iz][iy][0];
    
    // It was wrong..
    //    corr = _e_scale[arm][sect][iz][iy][0]*_e_scale_RbyR[arm][sect][iz][iy][0];
    
    /*
      Correction made on March 5.
      
      W3: divide by 1.0127
      W2: divide by 1.0127
      W1: divide by 1.0127 and   E_new = 9.88583e-01+7.25074e-02*exp(-1.*E_old)
      W0: divide by 1.0127 and   E_new = 1.00890e+00*E_old
      E3: divide by 1.0127
      E2: divide by 1.0127
      E1: E_new = 9.83495e-01+8.04533e-02*exp(-1.*E_old)
      E0: E_new = 9.72489e-01+1.07201e-01*exp(-1.*E_old)
      
      and multiply by 1.005 as an overall scale shift.
    */

     if(arm==1&&sect==0){

        // E0, PbGl correction
        corr = _e_scale[arm][sect][iz][iy][0]/_e_scale_RbyR[arm][sect][iz][iy][0]*(0.9725+0.1072*exp(-1.0*_ecore))*1.005;
     }
     else if(arm==1&&sect==1){
        // E1, PbGl correction
        corr = _e_scale[arm][sect][iz][iy][0]/_e_scale_RbyR[arm][sect][iz][iy][0]*(0.9835+0.08045*exp(-1.0*_ecore))*1.005;
     }
     else{
       // This is adhoc additional R-by-R calibration.
       int rungroup=0,secgroup=0;

       if(run>=228042&&run<231500) rungroup=0;
       else if(run>=231500&&run<234000) rungroup=1;
       else if(run>=234000&&run<237550) rungroup=2;
       else if(run>=237550&&run<240122) rungroup=3;

       if(arm==1&&sect>=2) secgroup =sect-2; 
       else secgroup =sect+2; 

       // 
       // The last factor of 1.0127 is to adjust overall scale
       // By T. Sakaguchi (Jan 14, 2008)
       // 
       corr = _e_scale[arm][sect][iz][iy][0]/_e_scale_RbyR[arm][sect][iz][iy][0]/(Adhoc_P1[rungroup][secgroup]*run+Adhoc_P0[rungroup][secgroup])*0.1389*1.005;

       if(arm==0&&sect==0) corr = corr *1.0089;
       if(arm==0&&sect==1) corr = corr * (0.9886+0.07251*exp(-1.0*_ecore));
    }
   
     // New additional sector-by-sector correction
     // by Yoki Aramaki (Feb. 25, 2010)
     if(SecbySecCorr==1){
       if(arm==1&&sect==0) corr = corr * 1.00145; // E0
       if(arm==1&&sect==1) corr = corr * 0.996749; // E1
       if(arm==1&&sect==2) corr = corr * 0.996207; // E2
       if(arm==1&&sect==3) corr = corr * 1.0031; // E3
       if(arm==0&&sect==0) corr = corr * 0.986406; // W0
       if(arm==0&&sect==1) corr = corr * 1.00824; // W1
       if(arm==0&&sect==2) corr = corr * 0.999947; // W2
       if(arm==0&&sect==3) corr = corr * 1.00487; // W3
     }
   
  }
  else if(run >= 246444 && run <= 259575){
     corr = _e_scale_RbyR[arm][sect][iz][iy][0];
  }
  else if(run >= 313591 && run <= 314994){ // Run10 39GeV
     if(arm==0) corr = sector_escale[4+sect];
     else corr = sector_escale[sect];
  }
  else if(run >= 310698 && run <= 313322){ // Run10 62GeV Au+Au
     if(arm==0) corr = sector_escale[4+sect];
     else corr = sector_escale[sect];
  }
  else if(run >= 300475 && run <= 310454){ // Run10 200GeV Au+Au
    // if(arm==1) corr = sector_escale[7-sect];
     if(arm==0) corr = r10_sector_escale[4+sect];
     else corr = r10_sector_escale[sect];
     //cout << PHWHERE << "R10.200G:" << arm << " " << sect << " " << 7*arm - sect << endl;
  }

//   cout << "arm = " << arm << ", sec = " << sect << ", fE = " << _ecore << ", corr = " << corr << endl;

  return corr;

}

void EmcGenericEScaleRecalReco::get_pos_correction(const int arm, const int sect,
                                                 float inx, float iny, float inz,
                                                 float& outx, float& outy, float& outz)
{
   outx = inx+ corrposx[arm][sect];
   outy = iny+ corrposy[arm][sect];
   outz = inz+ corrposz[arm][sect];
}


int EmcGenericEScaleRecalReco::process_event(PHCompositeNode *topNode)
{

  if (verbosity > 1)
    {
      cout << "Calling process_event in EmcGenericEScaleRecalReco......." << endl;
    }
  int iret = EVENT_OK;

  if (recalcnt)
    {
      // Recalibration for CNT first
      PHCentralTrack *phtrack = NULL;
      phtrack =
        findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());

      if (phtrack)
        {
          unsigned int ntracks = phtrack->get_npart();
          for (size_t i = 0; i < ntracks; i++)
            {
              PHSnglCentralTrack *sngltrk = phtrack->get_track(i);

              float corr;
              int emcarm;

              int sector = sngltrk->get_sect();
              if (!sngltrk->isValid(sngltrk->get_sect()))
                {
                  continue;
		  cout << "something is not valid" << endl;
                }
              int arm = sngltrk->get_dcarm();
              if (arm == 1)
                emcarm = 0;
              else
                emcarm = 1;


              // For Normal
              // since only the y/z sectors are critical to be implemented only these ones
              // are checked. The others (ecorr,ecore,emce,ecent,e9) are updated using
              // their own get method (they are not used to calculate other values)
              // so the virtual set will be called if they don't exist
              // and no damage is done
              sngltrk->ShutUp();
              if (sngltrk->isImplemented(sngltrk->get_ysect()) && sngltrk->isImplemented(sngltrk->get_zsect()))
                {
                  int ysect = sngltrk->get_ysect();
                  int zsect = sngltrk->get_zsect();
                  corr = get_correction(emcarm, sector, zsect, ysect,sngltrk->get_ecore());

                  sngltrk->set_ecorr(corr*sngltrk->get_ecorr());
                  sngltrk->set_ecore(corr*sngltrk->get_ecore());
                  sngltrk->set_emce(corr*sngltrk->get_emce());
                  sngltrk->set_ecent(corr*sngltrk->get_ecent());
                  sngltrk->set_e9(corr*sngltrk->get_e9());
                }

              // For Swapped
              if (sngltrk->isImplemented(sngltrk->get_sysect()) && sngltrk->isImplemented(sngltrk->get_szsect()))
                {
                  int sysect = sngltrk->get_sysect();
                  int szsect = sngltrk->get_szsect();
                  corr = get_correction(emcarm, sector, szsect, sysect,sngltrk->get_ecore());

                  sngltrk->set_secorr(corr*sngltrk->get_secorr());
                  sngltrk->set_secore(corr*sngltrk->get_secore());
                  sngltrk->set_semce(corr*sngltrk->get_semce());
                  sngltrk->set_secent(corr*sngltrk->get_secent());
                  sngltrk->set_se9(corr*sngltrk->get_se9());
                }
              sngltrk->ShutUp(0);
            }
        }
    }
  if (recalemc)
    {

      emcClusterContainer *clusters =
        findNode::getClass<emcClusterContainer>(topNode, inputnodename.c_str());

      if (!clusters)
        {
          std::cerr << PHWHERE << " No emcClusterContainer object !" << std::endl;
          return 0;
        }


      for (size_t i = 0; i < clusters->size();i++)
        {
          emcClusterContent *clus = clusters->getCluster(i);
          int arm = (int)clus->arm();
          int sector = (int)clus->sector();
          int iypos = (int)clus->iypos();
          int izpos = (int)clus->izpos();

          float corr,corrcent;
          float corrtmp;
          float towerEtmp[100];
          float OldEsum=0.0;
          float NewEsum=0.0;
          float Newpartesum[100];

          for(int j=0;j<100;j++) Newpartesum[j]=0.0;
          for(int j=0;j<100;j++) towerEtmp[j]=0.0;

          int ntowers = clus->multiplicity();
          if (ntowers > 99) ntowers = 99;

          //
          // Reconstruct energy for each tower from partesum.
          //
          for (int j = 0;j < ntowers;j++){
	      EmcIndexer::TowerLocation(clus->towerid(j),arm,sector,iypos,izpos);

	       corrtmp = get_correction(arm,sector,izpos,iypos,clus->ecore());
	       if(corrtmp <= 0.0) corrtmp = 1.0;
		
             // Extract energy for each tower 
             if(j==0) towerEtmp[j]= clus->partesum(j);
             else towerEtmp[j]= clus->partesum(j)-clus->partesum(j-1);
             if(towerEtmp[j]<0.0) cout << PHWHERE <<"Partesum error!!! in : "<< j << ", out of " << ntowers <<", " << towerEtmp[j] << endl;

             // Calculate before and after correction esum
             OldEsum+= towerEtmp[j];
             NewEsum+= towerEtmp[j] * corrtmp;
             Newpartesum[j] = NewEsum;

             if(corrtmp <=0.0) 
	     { 
	       cout << "corrtmp Error!"<< j;
	       cout <<": " << corrtmp; 
	       cout << " iy: " << iypos;
	       cout  << ", iz: " << izpos;
	       cout << ", arm: " << arm ;
               cout << ", sec: " << sector << endl;
	     }
          }

          for(int j=0;j<ntowers;j++){

             // Resetting partesum
             clus->set_partesum(j, Newpartesum[j]);
          }

          if((run>=168000 && run<=180000)
             || (run>=185000 && run<=207000)
             || (run>=228042 && run<=240121)
             || (run>=246444 && run<=253701)
             || (run>=256450 && run<=259575)){

             //
             // It is the right way for Run5 p+p (Provided by Kenichi Nakano)
             //   Added Run6. (Provided by Kenichi Nakano)
             //   Added Run7 Au+Au. (Provided by Rui Wei)
             //   Added Run8 p+p, d+Au. (Provided by Ondrej Chvala)
             //     by T. Sakaguchi
             //
             corr = NewEsum/OldEsum;
//             cout << "New: "<< NewEsum << ", Old: " <<OldEsum << endl;
//             cout << "Run5 p+p corr fact: " << corr << endl;

             EmcIndexer::TowerLocation(clus->towerid(0),arm,sector,iypos,izpos);
             corrcent = get_correction(arm,sector,izpos,iypos,clus->ecore());

             // Position correction if needed
             if(poscorr){
                float inx = clus->x();
                float iny = clus->y();
                float inz = clus->z();
                float outx,outy,outz;

                get_pos_correction(arm,sector,inx,iny,inz,outx,outy,outz);
                clus->set_xyz(outx,outy,outz);
             }

             clus->set_e(clus->e() * corr);
             clus->set_ecore(clus->ecore()*corr);
             clus->set_ecent(towerEtmp[0]*corrcent);
             clus->ShutUp();
             if (clus->isValid(clus->e9()))
               clus->set_e9(clus->e9()*corr);
             if (clus->isValid(clus->etofmin()))
               clus->set_etofmin(clus->etofmin()*corrcent);
             if (clus->isValid(clus->etofmax()))
               clus->set_etofmax(clus->etofmax()*corrcent);
             clus->ShutUp(0);
          }
          else{  

             if ((corr = get_correction(arm, sector, izpos, iypos,clus->ecore())) > -9998){
                float inx = clus->x();
                float iny = clus->y();
                float inz = clus->z();
                float outx,outy,outz;

                if(poscorr) get_pos_correction(arm,sector,inx,iny,inz,outx,outy,outz);
                else{outx=inx; outy=iny; outz=inz;}

                clus->set_xyz(outx,outy,outz);

                clus->set_e(clus->e() * corr);
                clus->set_ecore(clus->ecore()*corr);
                clus->set_ecent(clus->ecent()*corr);
                clus->ShutUp();
                if (clus->isValid(clus->e9()))
                  clus->set_e9(clus->e9()*corr);
                if (clus->isValid(clus->etofmin()))
                  clus->set_etofmin(clus->etofmin()*corr);
                if (clus->isValid(clus->etofmax()))
                  clus->set_etofmax(clus->etofmax()*corr);
                clus->ShutUp(0);
              }
          }

        }
    }

  if (0&&recaltwr)
    {
      //if (!Run10RRSSCorrs){
      
      //
      // From Here, Tower Container Recal
	  emcTowerContainer *towers =
         findNode::getClass<emcTowerContainer>(topNode, inputnodename.c_str());
      if (towers)
        {

          for (size_t i = 0; i < towers->size();i++)
            {
              emcTowerContent *tow = towers->getTower(i);
	      emcGeaTowerContent *geatow = dynamic_cast<emcGeaTowerContent *>( towers->getTower(i) );
              if (tow->hasCalib())
                {
                  int arm, sector, iypos, izpos;
                  float corr;

                  EmcIndexer::TowerLocation(tow->TowerID(), arm, sector, iypos, izpos);
                  if ((corr = get_correction(arm, sector, izpos, iypos, tow->Energy())) > -9998)
                    {
		      if( geatow ) geatow->scale_edep( corr );
		      else tow->SetCalibrated(tow->Energy() * corr, tow->ToF());
                    }
                }
            }
        }
      //}
    }
  return iret;

}


int
EmcGenericEScaleRecalReco::AddSectorCal(const float e0, const float e1, const float e2, const float e3,
					const float w0, const float w1, const float w2, const float w3,
                                        const int runmin, const int runmax)
{
  sector_escale_runmin = runmin;
  if (sector_escale_runmin <=0)
    {
      cout << PHWHERE << ": invalid minimum runnumber " << sector_escale_runmin << endl;
      exit(1);
    }
  sector_escale_runmax = runmax;
  if (sector_escale_runmax == 0)
    {
      sector_escale_runmax = sector_escale_runmin;
    }
  sector_escale.clear();
  sector_escale.push_back(e0);
  sector_escale.push_back(e1);
  sector_escale.push_back(e2);
  sector_escale.push_back(e3);
  sector_escale.push_back(w0);
  sector_escale.push_back(w1);
  sector_escale.push_back(w2);
  sector_escale.push_back(w3);

  return 0;
}

int
EmcGenericEScaleRecalReco::CommitSectorCal(const int commit, const float e0, const float e1, const float e2, const float e3,
					const float w0, const float w1, const float w2, const float w3,
                                        const int runmin, const int runmax)
{
  sector_escale_runmin = runmin;
  if (sector_escale_runmin <=0)
    {
      cout << PHWHERE << ": invalid minimum runnumber " << sector_escale_runmin << endl;
      exit(1);
    }
  sector_escale_runmax = runmax;
  if (sector_escale_runmax == 0)
    {
      sector_escale_runmax = sector_escale_runmin;
    }
  sector_escale.clear();
  sector_escale.push_back(e0);
  sector_escale.push_back(e1);
  sector_escale.push_back(e2);
  sector_escale.push_back(e3);
  sector_escale.push_back(w0);
  sector_escale.push_back(w1);
  sector_escale.push_back(w2);
  sector_escale.push_back(w3);


  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *BorTime = rt->getBeginTime(sector_escale_runmin);
  PHTimeStamp *EorTime = rt->getEndTime(sector_escale_runmax);
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  cout << sector_escale_runmin << " to " << sector_escale_runmax << endl;

  if (!application->startUpdate())
    {
      cout << PHWHERE << ": Aborting ... Database not writable" << endl;
      application->abort();
      exit(1);
    }
  PdbBankID bankID(0);
  ostringstream descrip;
  descrip << "Sector escale from run " << sector_escale_runmin << " to " << sector_escale_runmax;
  PdbCalBank *NewBank = bankManager->createBank("PdbFloatVectorBank",
						bankID,
						descrip.str().c_str(),
						*BorTime, *EorTime,
						"calibemcescale_sector");
  if (NewBank && commit)
    {
      NewBank->setLength(1);
      PdbFloatVector *fvec = (PdbFloatVector *) & (NewBank->getEntry(0));
      for (size_t n = 0; n < sector_escale.size(); n++)
        {
          fvec->add_float(sector_escale[n]);
	  //cout << "Sector " << n << ": " << sector_escale[n] << endl;
        }
      application->commit(NewBank);
      delete NewBank;
    }
  return 0;
}

int
EmcGenericEScaleRecalReco::fetchSectorCal(const int runnumber)
{
  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbApplication *application = bankManager->getApplication();
  //  application->setDBName("oncal");
  //  TString savDB = application->getDBName();
  //  application->setDBName("calibrations_scratch");


  if (!application->startRead())
    {
      cout << PHWHERE << ": Aborting ... Database not readable" << endl;
      application->abort();
      exit(1);
    }
  PdbBankID bankID(0);
  PdbCalBank *Bank = bankManager->fetchBank("PdbFloatVectorBank",
					    bankID,
					    "calibemcescale_sector",
					    runnumber);
  if (Bank)
    {
      PdbFloatVector *fvec = (PdbFloatVector *) & (Bank->getEntry(0));
      sector_escale = fvec->getVector();
      for (int ijk = 0; ijk < 8; ijk++)
      {
          if(run >= 415370 && run <= 416893){
              r14_sec_escale[ijk] = sector_escale[ijk];
          }
          else if(run >= 405860 && run <= 414988){ //Run-14 Au+Au
              r14_sec_escale[ijk] = sector_escale[ijk];
          }
          else if(run >= 421707 && run <= 438422){ //Run-15 p+p and p+Au
              r15_sec_escale[ijk] = sector_escale[ijk];
              cout << run << "   " << sector_escale[ijk] << endl;
          }
          else if(run >= 372402 && run <= 377310){ //Run-12 Cu+Au
              r12_CuAu_sec_escale[ijk] = sector_escale[ijk];
              cout << "This is Run12 run# "<<run << "   " << sector_escale[ijk] << endl;
          }
          else if(run >= 454774 && run <= 455639){
            r16dAu200_sec_escale[ijk] = sector_escale[ijk];
            cout << run << "   " << sector_escale[ijk] << endl;
          }
	  else if((run >= 443112 && run <= 454387) || (run >=  458390 && run <= 459344)) { //Run-16 Au+Au
              r16_sec_escale[ijk] = sector_escale[ijk];
          }

          
          else
              r10_sector_escale[ijk] = sector_escale[ijk];
          //cout << r10_sector_escale[ijk] << endl;
      }
      delete Bank;
      return 0;
    }
  else
    {
      cout << PHWHERE << ": Could not fetch sector escale from calibemcescale_sector table, exiting now" << endl;
      
      if(run >= 421707 && run <= 438422){ //Run-15 p+p and p+Au
          for (int ijk = 0; ijk < 8; ijk++) r15_sec_escale[ijk] = 1.00;
      }
      if(runnumber >= 405860 && runnumber <= 414988) { // Run14 Au+Au
          for (int ijk = 0; ijk < 8; ijk++) r14_sec_escale[ijk] = 1.00;
      }
      if(run >= 372402 && run <= 377310){ //Run-12 Cu+Au
          for (int ijk = 0; ijk < 8; ijk++) r12_CuAu_sec_escale[ijk] = 1.00;
      }
      if(run >= 454774 && run <= 455639){ //Run16 d+Au
          for (int ijk = 0; ijk < 8; ijk++) r16dAu200_sec_escale[ijk] = 1.00;
      }
      if((run >= 443112 && run <= 454387) || (run >=  458390 && run <= 459344))  { // Run16 Au+Au
          for (int ijk = 0; ijk < 8; ijk++) r16_sec_escale[ijk] = 1.00;
      }
      
      // We know that 39GeV is not complete, so we should not exit
      if((runnumber >= 313591 && runnumber <= 314994) || // Run10 39GeV
         (runnumber >= 310698 && runnumber <= 313322) || 
         (runnumber >= 300475 && runnumber <= 310454)) {// Run10 zero field runs
          sector_escale.clear();
          sector_escale.push_back(1.0);
          sector_escale.push_back(1.0);
          sector_escale.push_back(1.0);
          sector_escale.push_back(1.0);
          sector_escale.push_back(1.0);
          sector_escale.push_back(1.0);
          sector_escale.push_back(1.0);
          sector_escale.push_back(1.0);
          
          r10_sector_escale[0]=1.0;
          r10_sector_escale[1]=1.0;
          r10_sector_escale[2]=1.0;
          r10_sector_escale[3]=1.0;
          r10_sector_escale[4]=1.0;
          r10_sector_escale[5]=1.0;
          r10_sector_escale[6]=1.0;
          r10_sector_escale[7]=1.0;
          
          
          cout <<PHWHERE <<"Au+Au 39GeV run, we will use constant of 1.0 instead." <<endl;
      } 
//      else exit(1);
    }

  return 0;
}

void
EmcGenericEScaleRecalReco::Print(const std::string &what) const
{
  cout << "sector_escale, number of entries " << sector_escale.size() << endl;
  for (size_t n = 0; n < sector_escale.size(); n++)
    {
      cout << "entry " << n << ", value " << sector_escale[n] << endl;
    }
  return;
}

