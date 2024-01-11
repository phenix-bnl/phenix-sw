#include "CrkPID.hh"
#include "dCrkHitWrapper.h"
#include "CrkGeometryObject.hh"
#include "PHGeometry.h"
#include "getClass.h"
#include "CrkHitExt.h"
#include "recoConsts.h"

// include files for handling database fetch of t0 constants.
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include "PdbADCChan.hh"

#include "gsl/gsl_randist.h"

#include <iostream>

using namespace std;
using namespace PHGeometry;

//**************************
// CrkPIDout member functions
//**************************

CrkPIDout::CrkPIDout():npmt0(0),npe0(0),chi2(-1),disp(-1),npmt1(0),npe1(0),
		       npmt2(0),npe2(0),npmt3(0),npe3(0), arm(-1),
		       side(-1),panel(-1),
		       cross_phi(-10),cross_z(0),cross_phi_cor(-10),
		       cross_z_cor(0), path(0)
{
  int i;
  for(i=0;i<3;i++) {
    cross_mirror[i] = 0;
    cross_array[i]  =0.;
    center[i] = 0.;
  }
  for(i=0;i<MAXPMTHIT;i++) {
    pmt[i]=NULL;
    rpmt[i] = 0;
  }
  accepted = false;
  
}

void CrkPIDout::get_pmt_info(int i, int *p_pmtid, float *p_npe, float *p_time,
			     float *p_x, float *p_y, float *p_z) const {
  if(i < 0 || i >= npmt1) return;
  DCRKHIT_ST *hit = pmt[i];
  int pmt = hit->pmt;
  *p_pmtid = pmt;
  *p_npe   = hit->npe;
  *p_time  = hit->time;
  PHPoint pmt_pos =
    d_cgo->GetPmtPosition(d_cgo->IdToArm(pmt), d_cgo->IdToSide(pmt),
			  d_cgo->IdToSm(pmt), d_cgo->IdToPmt(pmt));
  *p_x = pmt_pos.getX();
  *p_y = pmt_pos.getY();
  *p_z = pmt_pos.getZ();			   
}


//*************************************
// CrkPID member functions
//*************************************

CrkPID::CrkPID(const char *alignment_file, int fUseSurvey) {

  d_cgo = new CrkGeometryObject();
  if(fUseSurvey==1){
    d_cgo->UseSurvey();
  }
  d_align_status = False;
  if(alignment_file != NULL) {
    d_align_status = d_cgo->FetchAlignFromFile(alignment_file);
  }
  d_crkw=NULL;
  //
  // Set default analysis parameters (for CO2 ratidator)
  //  SetPidParameters(5.9,3.0,8.4,11.0,20.0);
  //
  // Note 5/22/2002 Y. Akiba
  // The value below has been used for CO2 radiator analysis until
  // RUN-2 "aferburner" (i.e V03 DST production)
  //  SetPidParameters(5.9,3.4,8.4,11.0,15.0);
  //
  // New values that helps high pt pion analysis.
  // Note that the R2 parameter (the last argument) is now
  // identical to Rmax parameter.
  SetPidParameters(5.9,3.4,8.4,11.0,8.4);
  d_MaxNpe = 8.0;

  bbct0 = 0;  //User should set bbct0 prior to each use!!!
  for (int i=0; i<NCRKPMT; i++) {
    t0[i]=0;
  }
  for (int pmtid = 0; pmtid < NCRKPMT;pmtid++)
    {
      pmt_pos1[pmtid] =
	d_cgo->GetPmtPosition
	(d_cgo->IdToArm(pmtid), d_cgo->IdToSide(pmtid),
	 d_cgo->IdToSm(pmtid), d_cgo->IdToPmt(pmtid));
    }

  rng = gsl_rng_alloc (gsl_rng_mt19937);
  gsl_rng_set(rng, 6534); // use fixed seed -- this is ok since this is reconstruction, not simulation
}

CrkPID::CrkPID(int RunNumber,int fUseSurvey) {
 
  recoConsts *rc = recoConsts::instance();
  if (rc->FlagExist("SIMULATIONFLAG")){
    if (rc->get_IntFlag("SIMULATIONFLAG") >= 1){
      RunNumber= -1;
    }
  }

  if( rc->get_IntFlag("LOOKATSIMFROMEMBED",1) == 0)
    {
      RunNumber = rc->get_IntFlag("RUNNUMBER");
    }

  d_cgo = new CrkGeometryObject();

  switch (fUseSurvey)
    {
    case 0:             //Select to use survey or not using run number
      if(RunNumber>0){
	d_cgo->UseSurvey();
      }
      break;
    case 1:             //force to use survey
      d_cgo->UseSurvey();
      break;
    case -1:            //force not to use survey
      break;
    default:
      if(RunNumber>0){
	d_cgo->UseSurvey();
      }
    }

  SimFlag_CrkPID=0;
  if(RunNumber<0){
    SimFlag_CrkPID=1;
  }

  SimFlag_CrkPID_VTX=0;
  if (rc->FlagExist("SIMULATION_CRKPID_VTX")){
    SimFlag_CrkPID_VTX = rc->get_IntFlag("SIMULATION_CRKPID_VTX");
  }


  d_align_status = d_cgo->Fetch(RunNumber);
  d_crkw=NULL;
  //
  // Set default analysis parameters (for CO2 ratidator)
  //  SetPidParameters(5.9,3.0,8.4,11.0,20.0);
  //
  // Note 5/22/2002 Y. Akiba
  // The value below has been used for CO2 radiator analysis until
  // RUN-2 "aferburner" (i.e V03 DST production)
  //  SetPidParameters(5.9,3.4,8.4,11.0,15.0);
  //
  // New values that helps high pt pion analysis.
  // Note that the R2 parameter (the last argument) is now
  // identical to Rmax parameter.
  SetPidParameters(5.9,3.4,8.4,11.0,8.4);
  d_MaxNpe = 8.0;

  bbct0 = 0;  //User should set bbct0 prior to each use!!!
  for (int i=0; i<NCRKPMT; i++) {
    t0[i]=0;
  }
  if(RunNumber>0) FetchFromDatabase(RunNumber);
  else cout << "Run Number set -1 --> Use defaut t0 values" << endl;
  for (int pmtid = 0; pmtid < NCRKPMT;pmtid++)
    {
      pmt_pos1[pmtid] =
	d_cgo->GetPmtPosition
	(d_cgo->IdToArm(pmtid), d_cgo->IdToSide(pmtid),
	 d_cgo->IdToSm(pmtid), d_cgo->IdToPmt(pmtid));
    }

  rng = gsl_rng_alloc (gsl_rng_mt19937);
  gsl_rng_set(rng, 6534); // use fixed seed -- this is ok since this is reconstruction, not simulation


  ///////// 
  cout<<"CrkPID :: runnumber  = "<<RunNumber         <<endl;
  cout<<"       :: SimFlag    = "<<SimFlag_CrkPID    <<endl;
  cout<<"       :: SimFlagVTX = "<<SimFlag_CrkPID_VTX<<endl;
}

CrkPID::~CrkPID() {
  delete d_cgo;
  gsl_rng_free(rng);
}

void CrkPID::SetPidParameters(float R0, float Rmin, float Rmax, float R1,
			      float R2) {
  d_R0   = R0;
  d_Rmin = Rmin;
  d_Rmax = Rmax;
  d_R1   = R1;
  d_R2   = R2;

}

void CrkPID::GetPidParameters(float& R0, float& Rmin, float& Rmax, float& R1,
			      float& R2) {
  R0 = d_R0;
  Rmin = d_Rmin;
  Rmax = d_Rmax;
  R1   = d_R1;
  R2   = d_R2;

}

void CrkPID::SetCrkHitFromTop(PHCompositeNode *top) {
  dCrkHitWrapper *crkw = findNode::getClass<dCrkHitWrapper>(top,"dCrkHit");
  if(crkw) SetCrkHit(crkw);
}

void CrkPID::SetCrkHit(dCrkHitWrapper *crkw) {
  d_crkw = crkw;
}

bool CrkPID::TrackOnMirrorAndPMT(const PHLine& track, PHLine& reftrack, PHPoint& mcross, PHPoint& cross)
{
  int arm, side, panel;
  double path;
  bool accepted = false;

  if(track.getBasepoint().getX() > 0) arm = 0;  //West Arm
  else arm = 1;                                 //East Arm

  PHLine ref = d_cgo->Reflect (arm, track, side, panel, path);
  mcross = ref.getBasepoint();

  if (ref.length() > 0.) {
    cross = d_cgo->HitArray (arm, ref, side);
    if (!(cross == PHPoint())){
      accepted = true;
      float xcross = cross.getX();
      float ycross = cross.getY();
      float cross_phi = atan (ycross /xcross);
      float cross_r = sqrtf(xcross*xcross+ycross*ycross);
      float cross_z   = cross.getZ();
      if (arm == 1) cross_phi += Pi;
      float cross_phi_cor = cross_phi + d_cgo->GetAlignDphi(arm,side,panel);
      float cross_z_cor   = cross_z   + d_cgo->GetAlignDz(arm,side,panel);

      // Make alignment correction on track level
      cross.setX(cross_r*cos(cross_phi_cor));
      cross.setY(cross_r*sin(cross_phi_cor));
      cross.setX(cross_z_cor);

      PHVector F(cross.getX()-mcross.getX(),
                 cross.getY()-mcross.getY(),
                 cross.getZ()-mcross.getZ());

      reftrack.setBasepoint(mcross);
      reftrack.setDirection(F);
    }
    else{
      accepted = false;
    }
  }
  return accepted;
}


void CrkPID::GetPMThit(CrkHitExt& crkhitext)
{
  int npmt = d_crkw->RowCount();

  crkhitext.set_CrkNHit(npmt);

  DCRKHIT_ST *crk = d_crkw->TableData();

  for (int ihit = 0; ihit < npmt; ihit ++) {
    DCRKHIT_ST *chit = crk + ihit;
    int pmtid  = chit->pmt;
    float npe  = chit->npe;
    float time = chit->time;

    PHPoint pmt_pos = pmt_pos1[pmtid];

    crkhitext.set_pmt(ihit,pmtid);
    crkhitext.set_npe(ihit,npe);
    crkhitext.set_time(ihit,time);
    crkhitext.set_posX(ihit,pmt_pos.getX());
    crkhitext.set_posY(ihit,pmt_pos.getY());
    crkhitext.set_posZ(ihit,pmt_pos.getZ());
  }
}

bool CrkPID::AssociateTrack(const PHLine& track, CrkPIDout *result) {

  int npmt = d_crkw->RowCount();
  DCRKHIT_ST *crk = d_crkw->TableData();

  // CrkPIDout *result returns the ring parameters AND the associated RICH hits
  
  bool accepted = Associate(track, npmt, crk, result);

  return accepted;
}


bool CrkPID::Associate(const PHLine& track, const int npmt, DCRKHIT_ST *crk, CrkPIDout *result) {

  //cout << "Entering CrkPID::Associate()" << endl;

  int arm;
  int side;
  int panel;
  double path;

  int   npmt0 = 0;
  int   npmt1 = 0;
  int   npmt2 = 0;
  int   npmt3 = 0;
  float npe0  = 0.;
  float npe1  = 0.;
  float npe2  = 0.;
  float npe3  = 0.;
  float tcrk  = 0.;
  float chisqr= 0.;
  float xcenter0=0.;
  float ycenter0=0.;
  float zcenter0=0.;
  float xcenter1=0.;
  float ycenter1=0.;
  float zcenter1=0.;
  PHPoint cross;

  float timeSum   = 0;
  float energySum = 0;

  bool accepted = false;

  if(track.getBasepoint().getX() > 0) arm = 0;  //West Arm
  else arm = 1;                                 //East Arm

  PHLine ref = d_cgo->Reflect (arm, track, side, panel, path);
  if (ref.length() > 0.) {
    cross = d_cgo->HitArray (arm, ref, side);
    if (!(cross == PHPoint())) {
      accepted = true;

      // The RICH hits data are now passed as input arguments, instead of found in dCrkHits (ADF 8/14/2009)
      // int npmt = d_crkw->RowCount();
      // DCRKHIT_ST *crk = d_crkw->TableData();

      for (int ihit = 0; ihit < npmt; ihit ++) {
	DCRKHIT_ST *chit = crk + ihit;
	int pmtid  = chit->pmt;
	float npe  = chit->npe;
	float time = chit->time;
	if (d_cgo->IdToArm(pmtid) == arm) {
	  PHPoint pmt_pos = pmt_pos1[pmtid];
	  float pmt_r = sqrtf (pmt_pos.getX() * pmt_pos.getX() +
			       pmt_pos.getY() * pmt_pos.getY());
	  float pmt_phi = atan (pmt_pos.getY() / pmt_pos.getX());
	  if (arm == 1) pmt_phi += M_PI; // Pi from gsl/gsl_math.h
	  //
	  // Apply small alignment correctoin to the geometry
	  //
	  PHPoint pmt_pos_cor
	    (pmt_r * cos(pmt_phi - d_cgo->GetAlignDphi(arm,side,panel)),
	     pmt_r * sin(pmt_phi - d_cgo->GetAlignDphi(arm,side,panel)),
	     pmt_pos.getZ() - d_cgo->GetAlignDz(arm,side,panel));
	  
	  double r_cor = distanceLinePoint(ref, pmt_pos_cor);

          if(SimFlag_CrkPID){
            if(SimFlag_CrkPID_VTX == 0) {
              // diffution emulation for simulated ring
              // RMS difference between real and sim is around 1.2
              //Double_t sumtemp=0.0;
              //for(int j=0;j<12;j++) sumtemp += gsl_rng_uniform(rand);
              r_cor=gsl_ran_gaussian(rng,1.2) + r_cor - 0.3; 
              //r_cor=(sumtemp-6)*1.2 + r_cor - 0.3;
            } 
            else {
              r_cor = gsl_ran_gaussian(rng,1.0) + r_cor;  // TH. 20140805 for run11VTX analysis
              npe   = npe*0.95; // TH. 20140805
            }

            if(r_cor<0)
              r_cor=0.;
          }
	  if(d_Rmin < r_cor && r_cor < d_Rmax && npe < d_MaxNpe) {
	    npmt0++;
	    npe0 += npe;
	    xcenter0 += (pmt_pos_cor.getX() * npe);
	    ycenter0 += (pmt_pos_cor.getY() * npe);
	    zcenter0 += (pmt_pos_cor.getZ() * npe);
	  }
	  if(r_cor < d_R1 && npe < d_MaxNpe) {
	    if(npmt1 < CrkPIDout::MAXPMTHIT) {
	      result->pmt[npmt1]=chit;
	      result->rpmt[npmt1]=r_cor;
	    }
	    npmt1++;
	    npe1 += npe;
	    xcenter1 += (pmt_pos_cor.getX() * npe);
	    ycenter1 += (pmt_pos_cor.getY() * npe);
	    zcenter1 += (pmt_pos_cor.getZ() * npe);
	    chisqr += (r_cor - d_R0)*(r_cor - d_R0)*npe;
	  }
	  if(r_cor < d_R2 && npe < d_MaxNpe) {
	    npmt2 ++;
	    npe2 += npe;
	  }
	  if(3.9 < r_cor && r_cor < 7.9 && npe < d_MaxNpe) {
	    npmt3++;
	    npe3 += npe;
	    // time_corr is slew, t0, and bbct0 corrected time...
	    float time_corr = 1.12*(time-t0[pmtid]) - bbct0 - 3.4/npe + 1.5;
	    if (-2<time_corr && time_corr<4) {
	      timeSum += npe*time_corr;
	      energySum += npe;
	    }
	  }
	} // if PMT in same arm as track
      } // loop on PMT hit
    }//if(cross !=(0,0,0)) <--> reflected ray hits the PMT array
  }//if(ref.length()>0) <--> track hits the mirror
//  delete rand;
  // Calculate the weighted average slew-corrected time
  if (energySum>0) {
    tcrk = timeSum/energySum;
  }
  else {
    tcrk = -1000;
  }

  if(accepted) {
    // here 
    // ref ... reflected ray
    // cross ... cross point of ref and PMT array
    // But no alignment correction is applied.
    float xcross = cross.getX();
    float ycross = cross.getY();
    float cross_phi = atan (ycross /xcross);
    //    if(xcross < 0) cross_phi += Pi; // I agree with Takao this is a bug
    float cross_z   = cross.getZ();
    if (arm == 1) cross_phi += Pi;
    float cross_phi_cor = cross_phi + d_cgo->GetAlignDphi(arm,side,panel);
    float cross_z_cor   = cross_z   + d_cgo->GetAlignDz(arm,side,panel);
    PHPoint center0;
    if(npe0>0) center0 = PHPoint(xcenter0/npe0,ycenter0/npe0,zcenter0/npe0);

    result->accepted = accepted;
    result->npmt0 = npmt0;
    result->npe0  = npe0;
    result->chi2  = chisqr;
    result->disp  = distanceLinePoint(ref,center0);
    result->npmt1 = npmt1;
    result->npe1  = npe1;
    result->npmt2 = npmt2;
    result->npe2  = npe2;
    result->npmt3 = npmt3;
    result->npe3  = npe3;
    result->time  = tcrk;
    result->arm   = arm;
    result->side  = side;
    result->panel = panel;
    PHPoint mcross = ref.getBasepoint();
    result->cross_mirror[0]= mcross.getX();
    result->cross_mirror[1]= mcross.getY();
    result->cross_mirror[2]= mcross.getZ();
    result->cross_array[0] = cross.getX();
    result->cross_array[1] = cross.getY();
    result->cross_array[2] = cross.getZ();
    result->cross_phi     = cross_phi;
    result->cross_z       = cross_z;
    result->cross_phi_cor = cross_phi_cor;
    result->cross_z_cor   = cross_z_cor;

    if(npe1>0) {
      result->center[0] = xcenter1/npe1;
      result->center[1] = ycenter1/npe1;
      result->center[2] = zcenter1/npe1;
    }
    result->path  = path;
    result->d_cgo = d_cgo;
  }
  //cout << "Leaving CrkPID::Associate()" << endl;
  return accepted;
}


// SL 5-23-2002
PHBoolean CrkPID::FetchFromDatabase(int RunNumber) {

  // This routine is stolen from the afterBurner and 
  //  fetches the very same t0 constants.
  //                        TKH 1-7-2003


  PdbADCChan* achan = 0;
  const char* calibname = "afterburner.crk.tzero";

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID(0);

  PHTimeStamp tLatest(2030,1,1,0,0,0);


  if(application->startRead()) {

    PdbCalBank *Bank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, RunNumber);

    if(!Bank) {
      Bank = bankManager->fetchClosestBank("PdbADCChanBank", bankID, calibname, tLatest);
      cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
      cout << PHWHERE << "Warning: Can not get the period of run "
	   << RunNumber 
	   << " from the database." << endl << "Using latest ("
	   << Bank->getStartValTime()
	   << ") database for t0." << endl;
        }    

    if(Bank) {
      int banklength = Bank->getLength();
      for(int i=0; i<banklength; i++) {
        achan = (PdbADCChan*)&(Bank->getEntry(i));
        t0[i] = achan->getParameter(0);
      }
      delete Bank;
    }
    else {
      cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
      cout << PHWHERE << "ERROR: Can not get rich t0 from the database." << endl;
      return False;
    }
        application->commit();
        
  }
  else {
    cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
    cout << PHWHERE << "ERROR: Can not access the database." << endl;
    return False;
  }

  return True;
}


