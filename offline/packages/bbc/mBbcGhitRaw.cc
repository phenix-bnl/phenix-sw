#include "mBbcGhitRaw.h"
#include "bbc_common.h"
#include <emlLib.h>

#include <phool.h>

#include <gsl/gsl_randist.h>

#include <cmath>
#include <cassert>
#include <cstddef>

const gsl_rng_type *T;
gsl_rng *rng = 0;

int
ds2ReallocTable(TABLE_HEAD_ST** ppHeader, char** ppData, size_t newRowCount);

/********************************************************************/
/* forward declarations of static functions                         */ 
/********************************************************************/
static int   add_hit  ( DBBCGHITRAWPAR_ST *par, 
                        DBBCGEO_ST        *geo,
                        DBBCUCAL_ST       *ucal,
                        TABLE_HEAD_ST     *ghit_h,
                        BBCGHIT_ST        *ghit,
                        TABLE_HEAD_ST     *ghitraw_h,
                        DBBCGHITRAW_ST    *ghitraw,
                        float             *fadc, 
                        float             *ftdc,
                        int               *ighit,
                        int               *electron,
                        int               *pion,
                        int               *kaon,
                        int               *proton );
static void add_reso  ( DBBCGHITRAWPAR_ST *par, 
                        DBBCGEO_ST        *geo,
                        DBBCUCAL_ST       *ucal,
                        TABLE_HEAD_ST     *raw_h,
                        DBBCRAW_ST        *raw,
                        float             *fadc, 
                        float             *ftdc );
static void add_null  ( DBBCGHITRAWPAR_ST *par, 
                        DBBCGEO_ST        *geo,
                        DBBCUCAL_ST       *ucal,
                        TABLE_HEAD_ST     *raw_h,
                        DBBCRAW_ST        *raw,
                        float             *fadc, 
                        float             *ftdc );
static float incident_angle(
			DBBCGHITRAWPAR_ST *par, 
			BBCGHIT_ST        *ghit  );
static float calc_beta(BBCGHIT_ST        *ghit  );
static float get_mass (int                pid   );


/* Discription:                                         */ 
/* Position ID converting table from GEANT to real BBC  */ 
/* Please refer to http://hiroh2.hepl.hiroshima-u.ac.jp */

static int convPisaIDtoBBCID[128] =
     {
     13,18,10,16,21, 6,14,19,24, 3,
     11,17,22,28, 7,15,20,25, 4,12,
     23,29, 1, 8,26,31, 5,30, 2, 9,
     27,32,
     64,59,41,34,62,37,63,58,40,33,
     61,55,44,36,57,52,47,39,60,54,
     49,43,35,56,51,46,38,53,48,42,
     50,45,
     18,13,21,16,10,24,19,14, 6,28,
     22,17,11, 3,25,20,15, 7,29,23,
     12, 4,31,26, 8, 1,30, 5,32,27,
      9, 2,
     34,41,59,64,37,62,33,40,58,63,
     36,44,55,61,39,47,52,57,35,43,
     49,54,60,38,46,51,56,42,48,53,
     45,50
     };

/********************************************************************/
/* Implementation of PAM                                            */
/********************************************************************/
long 
mBbcGhitRaw_(TABLE_HEAD_ST *dBbcGhitRawPar_h,  DBBCGHITRAWPAR_ST *dBbcGhitRawPar,
	     TABLE_HEAD_ST *dBbcGeo_h,         DBBCGEO_ST        *dBbcGeo,
	     TABLE_HEAD_ST *dBbcUcal_h,        DBBCUCAL_ST       *dBbcUcal,
	     TABLE_HEAD_ST *bbcghit_h,         BBCGHIT_ST        *bbcghit,
	     TABLE_HEAD_ST *dBbcGhitRaw_h,     DBBCGHITRAW_ST    *dBbcGhitRaw,
	     TABLE_HEAD_ST *dBbcRaw_h,         DBBCRAW_ST        *dBbcRaw  )
{
  float fadc[BBCPMTMAX];
  float ftdc[BBCPMTMAX];

  int   ighit[BBCPMTMAX];
  int   electron[BBCPMTMAX];
  int   pion[BBCPMTMAX];
  int   kaon[BBCPMTMAX];
  int   proton[BBCPMTMAX];

  /* Initialize */
  int i;
  for ( i=0;i<128;i++ ) {
    fadc[i]     = 0.0;
    ftdc[i]     = 0.0;
    ighit[i]    = 0.0;
    electron[i] = 0.0;
    pion[i]     = 0.0;
    kaon[i]     = 0.0;
    proton[i]   = 0.0;
  } 
  T = gsl_rng_default;
  if (rng == 0)
    {
      rng = gsl_rng_alloc (T);
      gsl_rng_set(rng, dBbcGhitRawPar[0].randseed);
    }

  if (bbcghit_h->nok > 0) {

    dBbcGhitRaw_h->nok = bbcghit_h->nok;

    /* Initialize */
    for (i=0;i<dBbcGeo->MaxPmtNo;i++) {
      DBBCUCAL_ST *u_cal;
      u_cal=dBbcUcal+i;
      fadc[i] = 0.;
      ftdc[i]=dBbcGhitRawPar->MaxTdc*u_cal->TdcChGain0 + 5*u_cal->TimeReso; 
    }

    /* calcurate ADC sum and find fastest TDC for each PMT*/
    for (i=0;i<bbcghit_h->nok;i++) {

      if ( add_hit(dBbcGhitRawPar,dBbcGeo,dBbcUcal,bbcghit_h,bbcghit+i,dBbcGhitRaw_h,
                   dBbcGhitRaw+i,fadc,ftdc,ighit,electron,pion,kaon,proton) == 1 ) {
        return STAFCV_BAD;
      } 
    }

    /* add_reso digitizes signals into ADC and TDC ch and fill into dBbcRaw */
    add_reso(dBbcGhitRawPar,dBbcGeo,dBbcUcal,dBbcRaw_h,dBbcRaw,fadc,ftdc);


  } else {

    /* add_null fills dBbcRaw with overflow values */
    add_null(dBbcGhitRawPar,dBbcGeo,dBbcUcal,dBbcRaw_h,dBbcRaw,fadc,ftdc);
    // std::cout << PHWHERE << "No entry in ghit" << std::endl; (excess output, use verbosity check, CFM)
  }
  // std::cout << PHWHERE << "BbcRaw = " << dBbcRaw_h->nok << std::endl;  (excess output, use verbosity check, CFM)

  return STAFCV_OK;
}

static int 
add_hit( DBBCGHITRAWPAR_ST *par,
	 DBBCGEO_ST        *geo,
	 DBBCUCAL_ST       *ucal,
	 TABLE_HEAD_ST     *ghit_h,
	 BBCGHIT_ST        *ghit,
	 TABLE_HEAD_ST     *ghitraw_h,
	 DBBCGHITRAW_ST    *ghitraw,
	 float *fadc, float *ftdc,
	 int   *ighit,
	 int   *electron,
	 int   *pion,
	 int   *kaon,
	 int   *proton ) 
{

  /* Discription : calculate sum up photons yielded and    */ 
  /*               find the fastest hit by all tracks      */
  /*               and save to fadc, ftdc.                 */

  //  float photo_e_num;           // avarage photo-electron number
  int   pmt_no_gea;            // serial number of PMT in GEANT order
  int   pmt_no;                // serial number of PMT
  float theta;                 // incident angle to PMT by all tracks
  float beta;                  // beta by all tracks
  int   arm=1;
 
  if (ghit->tof < 0)
    {
      std::cout << PHWHERE << "Error: BBC time of flight from PISA is negative!" << std::endl;
      return 1;
    }

  /* Setting Arm ID (North : arm = 1) */
  /*                (South : arm = 2) */
  arm = (ghit->pmt < 2000) ? 1 : 2;
      
  if ( arm == 1 ) {        //printf("------>North\n");
    if ( ghit->pmt < arm*1000+33 ) {
      pmt_no_gea = ghit->pmt - 1000 - 1;
    } else if ( ghit->pmt > arm*1000+34 ) {
      pmt_no_gea = ghit->pmt - 1000 - 2 -1;
    } else {
      // skip add_hit for this hit since 33 and 34 correspond to laser fiber holders
      return 0;
    }
  } else {                 //printf("------>South\n");
    if ( ghit->pmt < arm*1000+33 ) {
      pmt_no_gea = ghit->pmt - 2000 - 1 + 64;
    } else if ( ghit->pmt > arm*1000+34 ) {
      pmt_no_gea = ghit->pmt - 2000 -2 -1 + 64;
    } else {
      // skip add_hit for this hit since 33 and 34 correspond to laser fiber holders
      return 0;
    }
  }   

  /* convert from GEANT ID to real position ID */
  if ( arm == 1 ) 
    {
      if (pmt_no_gea >= 128)
	{
	  std::cout << PHWHERE << "invalid pmt id: " << pmt_no_gea << std::endl;
	  exit(1);
	}
      pmt_no = convPisaIDtoBBCID[pmt_no_gea]-1;
    }
  if ( arm == 2 ) 
    {
      if (pmt_no_gea >= 128)
	{
	  std::cout << PHWHERE << "invalid pmt id: " << pmt_no_gea << std::endl;
	  exit(1);
	}

      pmt_no = convPisaIDtoBBCID[pmt_no_gea]-1+64;
    }

  ghitraw->rawid  = pmt_no;
  ghitraw->ghitid = ghit->mctrack;

  theta   = incident_angle(par, ghit);
  beta    = calc_beta(ghit);
  if ( fabs(theta) < par->AngleCut || 
       fabs(theta) > 180.0-par->AngleCut ) {
    if ( beta*par->Nindex > 1 ) {
      ighit[pmt_no]++; 
      if ( ghit->pid== 2||ghit->pid== 3 ) electron[pmt_no]++;
      if ( ghit->pid== 8||ghit->pid== 9 ) pion[pmt_no]++;
      if ( ghit->pid==11||ghit->pid==12 ) kaon[pmt_no]++;
      if ( ghit->pid==14||ghit->pid==15 ) proton[pmt_no]++;
    

      /* Discription : N0 is defined by this parameters.                    */         
      /*               (N0 coefficeint)*(#p.e.)*(PMTgain)*(Spliter in FEM)  */
      /*               This dimention is [pC] according to  this definition */


      float photo_e_num = par->N0*( 1-1/(beta*par->Nindex)/
			          (beta*par->Nindex) )*ghit->len;
      fadc[pmt_no] += photo_e_num;

      if ( ftdc[pmt_no] > ghit->tof ) {
	ftdc[pmt_no] = ghit->tof;
      }
    }
  }

  return 0;
}

static void 
add_reso( DBBCGHITRAWPAR_ST *par,
	  DBBCGEO_ST        *geo,
	  DBBCUCAL_ST       *ucal,
	  TABLE_HEAD_ST     *raw_h,
	  DBBCRAW_ST        *raw, 
	  float *fadc, float *ftdc ) 
{
  
  /* Discription : calculate degitaized ADC and TDC values with       */
  /*               resolution of PMTs( pulse height resolution and    */
  /*               intrinsic time resolution.), and fill into dBbcRaw.*/ 
  /*               fadc is in [pC] and ftdc is in [ns].               */

  DBBCUCAL_ST *u_cal;

  //  int   pmt;          // serial number of PMT
  //float adc_tmp;      // ADC values before dizitize with resolution [pC]
  //  int   adc_noped;    // dizitized ADC values without pedestal [ch]
  float adc_fake;     //
  //int   adc;          // dizitized ADC values with pedestal [ch]
  float tdc0_slw;     // TDC0 values within slewing effect [ns]
  float tdc1_slw;     // TDC1 values within slewing effect [ns]
  float tdc0_reso = NAN;    // TDC0 values within resolution [ns]
  //  float tdc1_reso = NAN;    // TDC1 values within resolution [ns]
  float Arm_off;      // arm offset [ns]
  int   tdc0;         // dizitized TDC0 value [ch]
  int   tdc1 = -9999;         // dizitized TDC1 value [ch]
  float tdc0_over;    // TDC0 value when it is overflow [ch]
  float tdc1_over;    // TDC1value when it is overflow [ch]
  //  float mean, sigma;

  if (geo->MaxPmtNo > raw_h->maxlen)
    {
      ds2ReallocTable(&raw_h, (char**)&raw, geo->MaxPmtNo);
    }
  raw_h->nok=geo->MaxPmtNo;

  
  for (int pmt=0;pmt<geo->MaxPmtNo;pmt++) {

    //DBBCRAW_ST  *raw_ptr=raw+pmt;
    u_cal   = ucal + pmt;
    DBBCRAW_ST  *raw_ptr = raw  + pmt;

    if ( pmt < (geo->MaxPmtNo/2) ) {
      Arm_off = par->Z0overC_off; // [ns]
      raw_ptr->Arm = 0;
      raw_ptr->Ring = geo->Ring[pmt];
      raw_ptr->Tube = geo->PMT[pmt];
      if ( pmt < (geo->MaxPmtNo/4) ) {
        raw_ptr->Half=0;
      }
      else {
        raw_ptr->Half=1;
      }
    } 
    else {
      Arm_off = -par->Z0overC_off; // [ns]
      raw_ptr->Arm = 1;
      raw_ptr->Ring = geo->Ring[pmt-(geo->MaxPmtNo/2)];
      raw_ptr->Tube = geo->PMT[pmt-(geo->MaxPmtNo/2)];
      if ( pmt < (geo->MaxPmtNo*3/4) ) {
        raw_ptr->Half=0;
      } 
      else {
        raw_ptr->Half=1;
      }
    }

    /* --- NOTE for Y2 simulation --- */
    // fadc is simulated output charge from PMT in [pC].
    // PMTGain is [pC/MIP] in real data of Y2
    // 40. is expected average [pC/MIP] in Y2 HV adjustment. 
    // Therefore PMTGain/40. is dimensionless PMT-by-PMT relative factor.
    // Then mean becomes [pC].
    // PulseHeightReso is [pC] in MIP peak.

    // AdcGainFactor is set to 1.0 which is a reserved parameter
    float mean = fadc[pmt]*(u_cal->PMTGain / 40.)*u_cal->AdcGainFac;

    // PulseHeightReso means sigma of MIP peak in [pC]
    float sigma = u_cal->PulseHeightReso;

    // append fluctuation: adc_tmp is [pC]

    float adc_tmp = gsl_ran_gaussian(rng, sigma) + mean;

    // digitize: AdcChGain is [pC/ch]
    int adc_noped = (int) ( adc_tmp / u_cal->AdcChGain );

    // add pedestal: Pedestal is [ch]
    int adc = (int) ( adc_tmp / u_cal->AdcChGain + u_cal->Pedestal );



    /* --- NOTE for Y2 simulation --- */
    // TDC0 threshold typically corresponds to ~20 ADC ch above pedestal.
    // TDC1 threshold typically corresponds to ~20 ADC ch above TDC0 threshold.
    // In online configuration we set thresholds at 10 mV for TDC0
    // and at 30 mV for TDC1 as a default Au+Au setting in Y2.
    // In the following, threshold comparisons are made after converting to pC from ADC ch.
    // TdcThreshold0/1 is [pC] after pedestal subtracted.
    // ThresholdFactor is a dimensionless factor which is used to see effects of threshold
    // uncertainties for the trigger efficiency study.
    //
    // After adding slewing effects and time resolutions, several time offsets must be add
    // to reproduce observed TDC ch as in the following;
    // Arm_off[ns] is obtained by +- global Z0 offset/c/2 which has no effect on T0 in net.
    // RunByRun_off[ns] is a reserved offset which is 0 as the default.
    // MeanTDC_off[ch] is the typical TDC peak position in TDC window for Y2, which is
    // around ~1500 ch in the ~3000 ch window. 
   
    /* TDC0 */
    if ( adc_tmp > 0 && adc_tmp > (float)u_cal->TdcThreshold0 * (float)par->ThresholdFactor ) {

      /* add slewing effects */
      tdc0_slw = ftdc[pmt] - (u_cal->SlewParA0 + u_cal->SlewParB0/adc_noped + u_cal->SlewParC0*log(adc_noped));
      sigma = u_cal->TimeReso; // [ns]
      mean  = tdc0_slw;        // [ns]

      /* append intrinsic timing resorution */
      tdc0_reso = gsl_ran_gaussian(rng, sigma) + mean;

      //tdc0 = (int) ( (tdc0_reso + Arm_off + par->RunByRun_off) / (u_cal->TdcChGain0) + par->MeanTDC_off );
      /* Fix time offset to avoid negative TDC when t0 offset = 0.0 */
      tdc0 = (int) ( (tdc0_reso + Arm_off + par->RunByRun_off) / (u_cal->TdcChGain0) + 1500.0);

      /* Procedure when TDC0 exceeds dynamic ranges after implementing slewing and resolution effects */
      if (tdc0_slw > par->MaxTdc*(u_cal->TdcChGain0) || tdc0 > par->MaxTdc) {
        mean  = u_cal->TdcOver0_mean;
        sigma = u_cal->TdcOver0_sigma;
        tdc0_over = gsl_ran_gaussian(rng, sigma) + mean;
        tdc0=(int)tdc0_over;
      }

    } else {

      mean  = u_cal->TdcOver0_mean;
      sigma = u_cal->TdcOver0_sigma;
      tdc0_over = gsl_ran_gaussian(rng, sigma) + mean;
      tdc0=(int)tdc0_over;

      mean  = u_cal->FakePede_mean;
      sigma = u_cal->FakePede_sigma;
      adc_fake = gsl_ran_gaussian(rng, sigma) + mean;
      adc=(int)adc_fake;

      /* In case fake adc value is funny */
      if (adc > par->MaxAdc) {
        adc=par->MaxAdc;
      }

    }

    /* TDC1 */
    if ( adc_tmp > 0 && adc_tmp > (float)u_cal->TdcThreshold1 * (float)par->ThresholdFactor ) {

      /* add slewing effects */
      tdc1_slw = ftdc[pmt] - (u_cal->SlewParA1 + u_cal->SlewParB1/adc_noped + u_cal->SlewParC1*log(adc_noped));
      sigma = u_cal->TimeReso; // [ns]
      mean  = tdc1_slw;        // [ns]

      /* append intrinsic timing resorution */
      //tdc1_reso = gsl_ran_gaussian(rng, sigma) + mean;
      //tdc1 = (int) ( (tdc1_reso + Arm_off + par->RunByRun_off) / (u_cal->TdcChGain1) + par->MeanTDC_off );
      /* Fix time offset to avoid negative TDC when t0 offset = 0.0 */
      tdc0 = (int) ( (tdc0_reso + Arm_off + par->RunByRun_off) / (u_cal->TdcChGain0) + 1500.0);

      /* Procedure when TDC1 exceeds dynamic ranges after implementing slewing and resolution effects */
      if (tdc1_slw > par->MaxTdc*(u_cal->TdcChGain1) || tdc1 > par->MaxTdc) {
        mean  = u_cal->TdcOver1_mean;
        sigma = u_cal->TdcOver1_sigma;
        tdc1_over = gsl_ran_gaussian(rng, sigma) + mean;
        tdc1=(int)tdc1_over;
      }

    } else {

      mean  = u_cal->TdcOver1_mean;
      sigma = u_cal->TdcOver1_sigma;
      tdc1_over = gsl_ran_gaussian(rng, sigma) + mean;
      tdc1=(int)tdc1_over;

    }

    raw_ptr->Pmt = pmt;
    raw_ptr->Adc = adc;
    raw_ptr->Tdc0= tdc0;
    raw_ptr->Tdc1= tdc1;
    
  }
}

static void add_null( DBBCGHITRAWPAR_ST *par,
		      DBBCGEO_ST        *geo,
		      DBBCUCAL_ST       *ucal,
		      TABLE_HEAD_ST     *raw_h,
		      DBBCRAW_ST        *raw, 
                      float *fadc, float *ftdc ) {

  DBBCUCAL_ST *u_cal;

  //int   pmt;          // serial number of PMT
  //  float adc_fake;     //
  //float tdc0_over;    // TDC0 value when it is overflow [ch]
  //  float tdc1_over;    // TDC1value when it is overflow [ch]
  //int   adc;          // dizitized ADC values with pedestal [ch]
  //int   tdc0;         // dizitized TDC0 value [ch]
  //  int   tdc1;         // dizitized TDC1 value [ch]

  if(geo->MaxPmtNo > raw_h->maxlen) {
    ds2ReallocTable(&raw_h,(char**)&raw,geo->MaxPmtNo); 
  }
  raw_h->nok=geo->MaxPmtNo;

  for (int pmt=0;pmt<geo->MaxPmtNo;pmt++) {

    u_cal   = ucal + pmt;
    DBBCRAW_ST  *raw_ptr = raw  + pmt;

    float mean  = u_cal->FakePede_mean;
    float sigma = u_cal->FakePede_sigma;
    float adc_fake = gsl_ran_gaussian(rng, sigma) + mean;
    int adc=(int)adc_fake;

    mean  = u_cal->TdcOver0_mean;
    sigma = u_cal->TdcOver0_sigma;
    float tdc0_over = gsl_ran_gaussian(rng, sigma) + mean;
    int tdc0=(int)tdc0_over;

    mean  = u_cal->TdcOver1_mean;
    sigma = u_cal->TdcOver1_sigma;
    float tdc1_over = gsl_ran_gaussian(rng, sigma) + mean;
    int tdc1=(int)tdc1_over;

    raw_ptr->Pmt = pmt;
    raw_ptr->Adc = adc;
    raw_ptr->Tdc0= tdc0;
    raw_ptr->Tdc1= tdc1;
    
  }
}

static float incident_angle(DBBCGHITRAWPAR_ST *par, BBCGHIT_ST *ghit)
{
  /* Discription: calculate incident angle to the BBC modules of the track. */

  float ptot;
  float pper;
  float theta;
  float px = ghit->mom[0];
  float py = ghit->mom[1];
  float pz = ghit->mom[2];
  ptot   = sqrt(px*px + py*py + pz*pz);   
  pper   = sqrt(px*px + py*py);   
  if (ptot <= par->MomLowerLim){
    theta=0.;
  }else{ 
    theta=atan2(pper,pz)*180./M_PI;
  }
  return(theta);
}

static float calc_beta(BBCGHIT_ST *ghit)
{
  /* Discription: calculate beta of the track          */

  float mass;
  float pp;
  float ptot;
  float px = ghit->mom[0];
  float py = ghit->mom[1];
  float pz = ghit->mom[2];
  mass   = get_mass( ghit->pid );
  pp     = (px*px + py*py + pz*pz);   
  ptot   = sqrt(px*px + py*py + pz*pz);   
  return(ptot/sqrt(pp+mass*mass));
}

static float get_mass(int pid)
{
  /* Discription: give a particle mass from Geant PID. */

  float mass;
  if (pid == 1 || pid == 4)
    {
      mass = 0.0;
    }
  else if (pid == 2 || pid == 3)
    {
      mass = 0.000511;
    }
  else if (pid == 5 || pid == 6)
    {
      mass = 0.1057;
    }
  else if (pid == 7 || pid == 8 || pid == 9 )
    {
      mass = 0.1396;
    }
  else if (pid == 10 || pid == 11 || pid == 12 || pid == 16 )
    {
      mass = 0.4936;
    }
  else if (pid == 17)
    {
      mass = 0.5488;
    }
  else if (pid == 14 || pid == 15)
    {
      mass = 0.9383;
    }
  else
    {
      mass = 100.;
    }
  return (mass);
}
