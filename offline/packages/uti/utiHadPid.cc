///////////////////////////////////////////////////////////////////////
//
// utiHadPid class
//
// Author:  J. Burward-Hoy
// 
// Description:  
//  class that determines the PID of hadrons (pi+/-, K+/-, (anti)p)
//
//  by default, the class works on the assumption that the momentum,
//  TOF, and pathlength come from the TOF detector.  If you wish to
//  use the EMC for pid, you must call set_EmcPID() prior to setting
//  the t,pl,p values...OR call calcSigmaM2() if they had been set.
//       -Tim Miller 21 May 2002
// 
//  Additions to use this class for pp data:
//  we have 3 different ways to measure time-of-flight
//  1) BBC - TOF   : flag TofPID
//  2) NTC - TOF   : flag TofNtcPID
//  3) Tzr - TOF   : flag TzrTofPID
// August - 2002, Julia Velkovska
//
//
// June 2003 - Felix Matathias
// A new constructor was added that takes as an argument the topnode
// from there it gets the runnumber and it determines what year/species
// this runnumber corresponds to and then loads the appropriate parameters.
// Variables like ppdata, ppfactor and their corresponding getters and setters
// were removed since the determination of species has become automatic.
// 
// The parameters for Year 3 dAu have been studied by F.M., Anuj Purwar and 
// Hiroshi Masui.
// For Year 3 pp I used the same values as dAu. This is justified by the fact
// that the all detectors involved in this analysis were in good standing
// during the pp run and there were no sighnificant changes.
// When systematic studies of year 3 pp data becfome available I will 
// update these parameters.
// For year 2 pp I put new parameters that were provided by Anuj Purwar and 
// Mark Harvey. Concerning the PID using other detectors than TOF, I havent
// changed anything since there were no updates for these numbers.
// These only apply for Year 2 pp.
//
// The Nomenclature for the new parameter PIDYEARSPECIESFLAG is simple:
//                  Year/Species where for dAu or AuAu I use species = 1 
//               (mnemonic: the species that was run first in the ring)
//            
//   201 = Year 2 AuAu
//   202 = Year 2 pp
//   301 = Year 3 dAu
//   
//////////////////////////////////////////////////////////////////////

#include "utiHadPid.h"
#include "RunHeader.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "getClass.h"

#include "gsl/gsl_const.h"

#include <cmath>
#include <iostream>

using namespace std;

void utiHadPid::Load_Default() {

  TofPID = true;
  TofNtcPID = false;
  TzrTofPID = false;
  EmcPID = false;   

  if( PIDYEARSPECIESFLAG == 201 ) //YEAR 2 AuAu 
    { 
      cout << "UTIHADPID: Year 02, AuAu parameters will be loaded." << endl;
    
      // index 0:  positive particle
      // index 1:  negative particle
      
      /*
	NOTE:  the mom values are the middle of the bin!  These are the values
	sent to me by Anuj.  I am averaging all momentum bins for a given
	particle to place into the simply mean*[] arrays.  Btw, that's a
	weighted average!
	
	mom     m**2 mean       m**2 mean err   m**2 sigma      m**2 sigma err
	
	PI PLUS
	0.5     0.0194215       2.00429e-05     0.0035968       0.000386353
	0.7     0.019035        9.16824e-05     0.0064488       0.000387157
	0.9     0.018753        0.000741576     0.0108889       0.00126892
	1.1     0.0180029       0.00133857      0.0154582       0.00221104
	1.3     0.0195147       0.00137942      0.0244727       0.00181892
	1.5     0.0184124       0.00239849      0.0319714       0.00221038
	1.7     0.0187741       0.00176557      0.0432917       0.00271338
	1.9     0.0176279       0.00154049      0.0552711       0.00585317
	2.1     0.0175722       0.00320579      0.069952        0.0167426
	
	PI MINUS
	0.5     0.0194162       2.38517e-05     0.00360119      0.000386789
	0.7     0.019164        9.51824e-05     0.00643114      0.000416988
	0.9     0.018803        0.000778558     0.0105948       0.00136424
	1.1     0.0182425       0.00110828      0.0157579       0.00189913
	1.3     0.0194163       0.0015096       0.0242895       0.00238007
	1.5     0.0184965       0.00186483      0.0310717       0.00289654
	1.7     0.0189909       0.00221166      0.0429859       0.00280118
	1.9     0.0168308       0.00112677      0.054925        0.00659116
	2.1     0.0177763       0.00322802      0.0677456       0.0123362
	
	K PLUS
	0.5     0.243980        0.000193446     0.00749331      0.000450243
	0.7     0.242412        0.000148902     0.00937925      0.000747798
	0.9     0.242528        0.000146589     0.0143834       0.000649791
	1.1     0.241989        0.000286024     0.0183709       0.00145488
	1.3     0.243003        0.000511223     0.0269694       0.00149717
	1.5     0.242254        0.000765667     0.0347522       0.00358727
	1.7     0.239255        0.000807371     0.0457187       0.00143507
	1.9     0.233664        0.00234443      0.0579808       0.00201833
	2.1     0.229440        0.00426951      0.0727064       0.00586815
	
	K MINUS
	0.5     0.243935        0.000196567     0.00811234      0.000342701
	0.7     0.242701        0.000138058     0.0095499       0.000552287
	0.9     0.242566        0.000148957     0.0139741       0.000802554
	1.1     0.243134        0.000695403     0.0201243       0.000467401
	1.3     0.243147        0.00235781      0.0269789       0.00661034
	1.5     0.241892        0.000734833     0.0340682       0.0056797
	1.7     0.238936        0.00104731      0.0453735       0.00181108
	1.9     0.236632        0.00144294      0.0536321       0.00214346
	2.1     0.232681        0.00399915      0.0641735       0.00539795
	
	PROTON
	0.5     0.893713        0.00226118      0.0442044       0.00250541
	0.7     0.876690        0.000725753     0.037573        0.000987371
	0.9     0.873850        0.00275202      0.0372282       0.00160942
	1.1     0.872301        0.00242111      0.0373542       0.00153646
	1.3     0.872917        0.000570474     0.0443958       0.00202836
	1.5     0.872438        0.00117042      0.050156        0.00151982
	1.7     0.872152        0.00370057      0.0564372       0.00698502
	1.9     0.872826        0.00472376      0.063951        0.00829048
	2.1     0.881155        0.0071031       0.0842789       0.0114261
	
	ANTI-PROTON
	0.5     0.894657        0.00272887      0.0341212       0.000854222
	0.7     0.879319        0.000773699     0.032676        0.00147313
	0.9     0.872594        0.00197265      0.0316229       0.00113215
	1.1     0.872932        0.0011718       0.0364638       0.000982889
	1.3     0.872426        0.00104764      0.0424622       0.00264301
	1.5     0.870873        0.0019029       0.0472426       0.00351262
	1.7     0.872780        0.00366738      0.0554094       0.00644719
	1.9     0.873427        0.00284694      0.0663017       0.00444346
	2.1     0.876824        0.00654132      0.0847619       0.0101496
	
      */
      
      //  Average centroid for m**2 distribution over a range from 0.4 to 2.2GeV
      meanPi[0] = 0.01940; // +- 0.00002
      meanPi[1] = 0.01939; // +- 0.00002
      
      meanK[0] = 0.24268;  // +- 0.00008
      meanK[1] = 0.24284;  // +- 0.00008
      
      meanP[0] = 0.8764;   // +- 0.0004
      meanP[1] = 0.8759;   // +- 0.0005
      
      /*
      **  The following values for TOF, MS, and ALPHA were extracted from
      **  Anuj Purwar's talk to the Global/Hadron Working group on 29 May 2002
      ** 
      **  These values are the averages of the positive and negative values 
      **  in that talk (as suggested by Anuj).  The only caveat is the MS 
      **  constant (sigmaDC[0])...the positive track value is 0.92 and the
      **  negative track value is 0.75.  This means the sigma(m**2) isn't quite
      **  right for low momentum protons...but not by much.
      */
      
      sigmaTOF   = 0.120;  // in ns (+- 2ps)
      sigmaDC[0] = 0.835;  // multiple scattering constant in mrad GeV/c
      sigmaDC[1] = 0.86;   // angular resolution in mrad
      /*
	NOTE:  The 'Default' value taken from PHCentralTrackv3.C on 21 May 2002
	was 340ps.  I've had several notes and seen several talks that this is
	not correct.  The PbGl is apparently 600ps while the PbSc is mostly 
	400ps and some is 450ps.  
	
	SO, I'm setting it to 600ps, since the majority of EmCal is PbGl.
	Those using PbSc should adjust by using tight sigma cuts OR when 
	analyzing the data, setSigmaEMC() prior to calling IsPion, etc.
      */
      sigmaEMC = 0.600;  
      EmcPID = true;
    } //YEAR 2 AuAu 

 
       
  else if( PIDYEARSPECIESFLAG == 202) //YEAR 2 pp
    {
      
      cout << "UTIHADPID: Year 02, pp parameters will be loaded." << endl;

      //Values by Anuj Purwar, Mark Harvey
      /*
	Positive Particles  (0.5 - 1.9 -- 200 MeV Bins)
	
	1  SigAlpha     1.17886e+00   7.94763e-02 
	2  SigMS        9.76835e-01   3.21441e-02 
	3  SigToF       1.36459e+02   6.80856e-01 
	
	Negative Particles  (0.5 - 1.9 -- 200 MeV Bins)
	
	1  SigAlpha     1.09247e+00   8.55943e-02 
	2  SigMS        1.02928e+00   3.73612e-02 
	3  SigToF       1.36481e+02   6.78364e-01 
	
	
	Positive Particles
	pi+                       K+                          Proton
	0.020064 +/- 0.00000037   0.24545774 +/- 0.00004755   0.88841665 +/- 0.00053310 
	
	Negative Particles
	pi-                       K-                          Anti-Proton
	0.019921 +/- 0.00000037   0.24344725 +/- 0.00004941   0.87638148 +/- 0.00054919
      */


      // index 0:  positive particle
      // index 1:  negative particle

      //values for centroids
      meanPi[0] = 0.0200640;
      meanPi[1] = 0.0199210;
                 
      meanK[0] = 0.24545774; 
      meanK[1] = 0.24344725;
             
      meanP[0] = 0.88841665;
      meanP[1] = 0.87638148;

      //averaged for negatives and positives
      sigmaTOF   = 0.13647;     // in ns (+- 2ps)
      sigmaDC[0] = 1.00305;    // multiple scattering constant in mrad GeV/c
      sigmaDC[1] = 1.13566;   // angular resolution in mrad
      
           
      //TOF resolution for the rest of the detectors
      sigmaNtcTof= 0.300;  // this is a made-up number for now, need to study it
      sigmaTzr   = 0.220;  // this is a made-up number for now, need to study it
      sigmaNtcDC = 1;      // the DC resolution with NTC vertex is worse - need to study what it is exactly
      
      /*
	NOTE:  The 'Default' value taken from PHCentralTrackv3.C on 21 May 2002
	was 340ps.  I've had several notes and seen several talks that this is
	not correct.  The PbGl is apparently 600ps while the PbSc is mostly 
	400ps and some is 450ps.  
	
	SO, I'm setting it to 600ps, since the majority of EmCal is PbGl.
	Those using PbSc should adjust by using tight sigma cuts OR when 
	analyzing the data, setSigmaEMC() prior to calling IsPion, etc.
      */
      sigmaEMC = 0.600;  
      EmcPID = true;

            
    }//YEAR 2 pp




  else if( PIDYEARSPECIESFLAG == 301) //YEAR 3 dAu
    {

      cout << "UTIHADPID: Year 03, dAu parameters will be loaded." <<endl;
      cout << "           Use EMC at your own risk and update it if you know better" << endl;
      cout << "This is the last time someone took a closer look at these values" << endl;
      cout << "But they are deemed to be good enough with the advent of recalibrators" << endl;
      //Values by Anuj Purwar for resolutions
      
      // index 0:  positive particle
      // index 1:  negative particle

      //using pro41+mom afterburning it was determined that 
      //the centroids should be set at their PDG values July, 2003

      //average of centroids
      meanPi[0] = 0.0194795; 
      meanPi[1] = 0.0194795;
      
      meanK[0] = 0.2437169;
      meanK[1] = 0.2437169;
      
      meanP[0] = 0.8803543;
      meanP[1] = 0.8803543;

      //averaged for negatives and positives
      sigmaTOF   = 0.130 ;     // in ns (+- 2ps)
      sigmaDC[0] = 0.720;    // multiple scattering constant in mrad GeV/c
      sigmaDC[1] = 1.0950;   // angular resolution in mrad
      // chp: this is just a wild guess since noone else bothered so far
      // and I need it
      sigmaEMC = 0.500;  
      EmcPID = true;
            
      
    }//YEAR 3 dAu


  

  cout << "UTIHADPID: sigmaTof= " << sigmaTOF << endl << endl;
  
  K1 = 87.0; 
  c = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9;
  L = 0.0;
  p = 0.0;
  t = 0.0;
  m2 = 0.0;  
  sigmaM2 = 999.0;
  
}

utiHadPid::utiHadPid()
{
  
  cout << "***************                 UTIHADPID                                              ***************" << endl;
  cout << "*************** ALERT: ARE YOU SURE YOU ARE USING THE RIGHT CONSTRUCTOR  ?             ***************" << endl;
  cout << "*************** UTIHADPID HAS A NEW CONSTRUCTOR: utiHadPid(PHCompositeNode *topNode);  ***************" << endl;
  cout << "*************** IN THAT WAY IT CAN READ THE RUNNUMBER AND SET PARAMETERS ACCORDINGLY   ***************" << endl;
  cout << "*************** CAUTION:       YEAR 2 PARAMETERS FOR AUAU WILL BE USED                 ***************" << endl;

  PIDYEARSPECIESFLAG = 201;
  Load_Default();
}

utiHadPid::utiHadPid(float TOF, float PL, float MOM)
{

  cout << "***************                 UTIHADPID                                              ***************" << endl;
  cout << "*************** ALERT: ARE YOU SURE YOU ARE USING THE RIGHT CONSTRUCTOR  ?             ***************" << endl;
  cout << "*************** UTIHADPID HAS A NEW CONSTRUCTOR: utiHadPid(PHCompositeNode *topNode);  ***************" << endl;
  cout << "*************** IN THAT WAY IT CAN READ THE RUNNUMBER AND SET PARAMETERS ACCORDINGLY   ***************" << endl;
  cout << "*************** CAUTION:       YEAR 2 PARAMETERS FOR AUAU WILL BE USED                 ***************" << endl;

  PIDYEARSPECIESFLAG = 201;
  Load_Default();

  t = TOF;
  L = PL;
  p = MOM;

  calcMeasM2();
  calcSigmaM2();
}

utiHadPid::utiHadPid(PHCompositeNode *topNode)
{
  if(topNode==NULL) 
    {
      cout << "*************************************************************" << endl;
      cout << "UTIHADPID: ALERT, RECEIVED NULL TOPNODE." << endl;
      cout << "YEAR 2 PARAMETERS WILL BE LOADED AS DEFAULT BUT PLEASE CHECK!" << endl;
      cout << "*************************************************************" << endl;
      PIDYEARSPECIESFLAG = 201;
      Load_Default();
      return;
    }
  

  //Run Header
  RunHeader *d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");
  
  int runnumber = 0;

  if (d_runhdr) 
    {
      runnumber = d_runhdr->get_RunNumber();
      cout << "***************" << endl;
      cout << "UTIHADPID: RUN_NUMBER  " << runnumber << endl;
      cout << "***************" << endl;
    }
  else
    {
      runnumber = -1;
      cout << "***************        UTIHADPID             ***************" << endl;
      cout << "*************** ALERT: NO RUN INFORMATION    ***************" << endl;
      cout << "*************** COULD NOT SPECIFY RUN NUMBER ***************" << endl;
      cout << "*************** ALL INFORMATION WILL BE INVALID ************" << endl;
    }

  

  ////////////////////////////////////////
  //  Run2 AuAu 20578 - 33768
  //  Run2 pp   35758 - 40655
  //
  //  Run3 dAu  65145 - 80312
  //  Run3 pp   82544 - 92446
  ///////////////////////////////////////

  if (runnumber >= 20578 && runnumber <= 33768)
    {
      PIDYEARSPECIESFLAG = 201;
    }
  else if (runnumber >= 35758 && runnumber <= 40655)
    {
      PIDYEARSPECIESFLAG = 202;
    }
  else if (runnumber >= 65145)
    {
      PIDYEARSPECIESFLAG = 301;
    }
  else
    {
      PIDYEARSPECIESFLAG = 0;
    }

  if(PIDYEARSPECIESFLAG == 0)
    {
      cout << "UTIHADPID: ALERT, COULD NOT CATEGORIZE THE RUNNUMBER" << endl;
      cout << "IT DOES NOT APPEAR TO BE A VALID RUN02 OR RUN03 RUNNUMBER " << endl;
      cout << "YEAR 2 PARAMETERS WILL BE LOADED AS DEFAULT BUT PLEASE CHECK!" << endl;
      PIDYEARSPECIESFLAG = 201;
    }
  
  Load_Default();
  
}


void utiHadPid::setM2MeanPi(int charge,float pi)
{

  if (charge>0) {
    meanPi[0] = pi;
  } else {
    meanPi[1] = pi;
  }

}

float utiHadPid::getM2MeanPi(int charge)
{
  if (charge>0) {
    return meanPi[0];
   
  } else {
    return meanPi[1];
  }


}

float utiHadPid::getM2MeanK(int charge)
{
  if (charge>0) {
    return meanK[0];
  } else {
    return meanK[1];
  }


}


float utiHadPid::getM2MeanP(int charge)
{
  if (charge>0) {
    return meanP[0];
  } else {
    return meanP[1];
  }

}


void utiHadPid::setM2MeanK(int charge,float k)
{

  if (charge>0) {
    meanK[0] = k;
  } else {
    meanK[1] = k;
  }


}


void utiHadPid::setM2MeanP(int charge,float p)
{

  if (charge>0) {
    meanP[0] = p;
  } else {
    meanP[1] = p;
  }


}

void utiHadPid::setSigmaTOF(float sigma)
{
  sigmaTOF = sigma;
}

void utiHadPid::setSigmaEMC(float sigma)
{
  sigmaEMC = sigma;
  EmcPID = true;
}

void utiHadPid::setSigmaDC(float sigmaMS, float sigmaAlpha)
{
  sigmaDC[0]  = sigmaMS;
  sigmaDC[1]  = sigmaAlpha;
  
}

void utiHadPid::calcMeasM2()
{
  // must call setHadronTimeOfFlight(..), setHadronPathlength(..), 
  // and setHadronMomentum(..) functions first!!

  // this function calculates the measured M2 
  // given the time-of-flight, pathlength, and momentum

   m2 = 0.0;

   if (L > 0) {
     m2 =(::pow(t*c/L,2.0)-1)*p*p;
   } 

}

void utiHadPid::set_TofPlMom(float TOF, float PL, float MOM) 
{
  t = TOF;
  L = PL;
  p = MOM;

  calcMeasM2();
  calcSigmaM2();
}

void utiHadPid::calcSigmaM2()
{
 
  // this function calculates the measured M2 width using 
  // the measured time-of-flight, pathlength, and momentum
  // and the values of sigmaTOF, sigmaDC[] measured detector 
  // resolutions

  sigmaM2 = 999.0;

  if (p > 0.0 && t > 0.0)
    {

      if (TofPID)
	{
	  sigmaM2 = 2 * sqrt(m2 * pow(p * sigmaDC[1] / K1, 2) +
			     pow(sigmaDC[0] * m2 / K1, 2) * (1.0 + m2 / (p * p)) +
			     pow(sigmaTOF * p / t, 2) * (m2 + p * p));
	}
      else if (TofNtcPID)
	{
	  sigmaM2 = 2 * sqrt(m2 * pow(p * sigmaNtcDC / K1, 2) +
			     pow(sigmaDC[0] * m2 / K1, 2) * (1.0 + m2 / (p * p)) +
			     pow(sigmaNtcTof * p / t, 2) * (m2 + p * p));
	}
      else if (TzrTofPID)
	{
	  sigmaM2 = 2 * sqrt(m2 * pow(p * sigmaDC[1] / K1, 2) +
			     pow(sigmaDC[0] * m2 / K1, 2) * (1.0 + m2 / (p * p)) +
			     pow(sigmaTzr * p / t, 2) * (m2 + p * p));
	}
      else if (EmcPID)
	{
	  sigmaM2 = 2 * sqrt(m2 * pow(p * sigmaDC[1] / K1, 2) +
			     pow(sigmaDC[0] * m2 / K1, 2) * (1.0 + m2 / (p * p)) +
			     pow(sigmaEMC * p / t, 2) * (m2 + p * p));
	}
    }
  return;
}

float utiHadPid::IsPion(int charge)
{ 

  float centroid = meanPi[0];
  if (charge<0) {
    centroid = meanPi[1];
  }

  float nPi = (m2-centroid)/sigmaM2;
   
  return nPi;
}

float utiHadPid::IsKaon(int charge)
{ 

  float centroid = meanK[0];
  if (charge<0) {
    centroid = meanK[1];
  }

  float nK = (m2-centroid)/sigmaM2;

  return nK;  
}

float utiHadPid::IsProton(int charge)
{ 

  float centroid = meanP[0];
  if (charge<0) {
    centroid = meanP[1];
  }

  float nP = (m2-centroid)/sigmaM2;
   
  return nP;
}

float utiHadPid::IsPion(int charge,float TOF, float PL, float MOM)
{ 

  t = TOF;
  L = PL;
  p = MOM;
  
  calcMeasM2();
  calcSigmaM2();

  return IsPion(charge);
}



float utiHadPid::IsKaon(int charge,float TOF, float PL, float MOM)
{ 

  t = TOF;
  L = PL;
  p = MOM;
  
  calcMeasM2();
  calcSigmaM2();

  return IsKaon(charge);
}

float utiHadPid::IsProton(int charge,float TOF, float PL, float MOM)
{ 

  t = TOF;
  L = PL;
  p = MOM;
  
  calcMeasM2();
  calcSigmaM2();

  return IsProton(charge);
}

