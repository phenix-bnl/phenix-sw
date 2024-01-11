#include "utiCentrality.h"
#include "CentralityParameters.h"

#include "ZdcOut.h"
#include "BbcOut.h"
#include "Bbc.hh"
#include "FclOut.h"
#include "PHGlobal.h"
#include "RunHeader.h"

#include "getClass.h"

#include "TF1.h"

#include <cstdlib>
#include <iostream>

using namespace std;

namespace PhUtilities
{


//===========================================================================
//  Run 5 Copper-Copper centrality functions - j.nagle
//===========================================================================

int getCentralityByBBCRun5CuCu(PHCompositeNode *topNode) {
  PHGlobal* d_global =   findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  RunHeader *d_run = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if(!d_global || !d_run){
    std::cout<< PHWHERE <<" ERROR: PHGlobal not in the Node Tree"<<std::endl;
    return -1;
  }
  float bbc1 = d_global->getBbcChargeN();
  float bbc2 = d_global->getBbcChargeS();
  float zdc1 = d_global->getZdcEnergyN();
  float zdc2 = d_global->getZdcEnergyS();
  //  float zvertex = d_global->getZvertex(); // ? real method
  float zvertex = 0.0;
  int  runno = d_run->get_RunNumber();
  return getCentralityByBBCRun5CuCu(bbc1, bbc2, zdc1, zdc2, zvertex, runno);
}

int getCentralityByBBCRun5CuCu(float bbc1, float bbc2, float zdc1, float zdc2, float zvertex, int runno) {

  // originally we had coded the centrality method here (J.Nagle)
  // However, now that there is a more up to date RECO module and we
  // do not want to risk duplication of code - instructions have been
  // given to not using PHUtilities at all

  std::cerr<< PHWHERE << "!!! ERROR WITH RUN5 CUCU 200 GeV CENTRALITY !!!" << std::endl; 
  std::cerr<< PHWHERE << "!!! J.Nagle - do not use PHUtilitities to get centrality anymore." << std::endl; 
  std::cerr<< PHWHERE << "!!! This is now superceded by the RECO module which must be used (06-01-2005)." << std::endl; 
  std::cerr<< PHWHERE << "!!! Now executing an exit(1)." << std::endl; 

  exit(1);

  // Calculates event centrality in percent for run5 CuCu.
  // Uses "bbc only" method
  // Assumes that we see 88% of total cross section.
  // Returned value of 1 means that this event is from 0% to 1% most central,
  // Returned value of 50 means that this event is from 49% to 50% most central, etc.

  // Other comments about how calculated and where documented here....
  // note that it currently does not use zvertex or runnumber...

  // j.nagle - last update 02-08-2005
  // for right now there is no zvertex or runno dependence in place

  //-------------------------------------------------------------------------
  // vector of cut values determined from run 151577 with ZDCMAX=700, BBCPOW=0.5, BBCDIV=24.0
  

  Float_t bbcCut[90];

  bbcCut[ 0] = 1999.90000;
  bbcCut[ 1] = 543.164978;
  bbcCut[ 2] = 510.894989;
  bbcCut[ 3] = 491.225006;
  bbcCut[ 4] = 473.375000;
  bbcCut[ 5] = 456.154999;
  bbcCut[ 6] = 441.524994;
  bbcCut[ 7] = 426.195007;
  bbcCut[ 8] = 412.964996;
  bbcCut[ 9] = 399.174988;
  bbcCut[10] = 385.315002;
  bbcCut[11] = 372.015015;
  bbcCut[12] = 359.345001;
  bbcCut[13] = 348.214996;
  bbcCut[14] = 336.804993;
  bbcCut[15] = 324.834991;
  bbcCut[16] = 314.545013;
  bbcCut[17] = 303.065002;
  bbcCut[18] = 293.964996;
  bbcCut[19] = 283.325012;
  bbcCut[20] = 274.154999;
  bbcCut[21] = 263.795013;
  bbcCut[22] = 254.345001;
  bbcCut[23] = 245.664993;
  bbcCut[24] = 236.565002;
  bbcCut[25] = 228.725006;
  bbcCut[26] = 220.535004;
  bbcCut[27] = 212.695007;
  bbcCut[28] = 204.785004;
  bbcCut[29] = 196.455002;
  bbcCut[30] = 188.895004;
  bbcCut[31] = 181.964996;
  bbcCut[32] = 174.684998;
  bbcCut[33] = 168.244995;
  bbcCut[34] = 162.014999;
  bbcCut[35] = 155.994995;
  bbcCut[36] = 149.695007;
  bbcCut[37] = 143.535004;
  bbcCut[38] = 137.585007;
  bbcCut[39] = 131.705002;
  bbcCut[40] = 126.665001;
  bbcCut[41] = 121.275002;
  bbcCut[42] = 116.445000;
  bbcCut[43] = 111.334999;
  bbcCut[44] = 106.644997;
  bbcCut[45] = 101.535004;
  bbcCut[46] = 96.845001;
  bbcCut[47] = 92.574997;
  bbcCut[48] = 88.305000;
  bbcCut[49] = 84.105003;
  bbcCut[50] = 79.904999;
  bbcCut[51] = 76.264999;
  bbcCut[52] = 72.625000;
  bbcCut[53] = 69.055000;
  bbcCut[54] = 65.485001;
  bbcCut[55] = 62.334999;
  bbcCut[56] = 59.325001;
  bbcCut[57] = 56.105000;
  bbcCut[58] = 52.884998;
  bbcCut[59] = 50.014999;
  bbcCut[60] = 47.145000;
  bbcCut[61] = 44.695000;
  bbcCut[62] = 42.035000;
  bbcCut[63] = 39.794998;
  bbcCut[64] = 37.625000;
  bbcCut[65] = 35.525002;
  bbcCut[66] = 33.634998;
  bbcCut[67] = 31.465000;
  bbcCut[68] = 29.504999;
  bbcCut[69] = 27.754999;
  bbcCut[70] = 25.934999;
  bbcCut[71] = 24.325001;
  bbcCut[72] = 22.855000;
  bbcCut[73] = 21.385000;
  bbcCut[74] = 19.915001;
  bbcCut[75] = 18.445000;
  bbcCut[76] = 17.045000;
  bbcCut[77] = 15.715000;
  bbcCut[78] = 14.455000;
  bbcCut[79] = 13.265000;
  bbcCut[80] = 12.075000;
  bbcCut[81] = 10.885000;
  bbcCut[82] = 9.695000;
  bbcCut[83] = 8.365000;
  bbcCut[84] = 7.245000;
  bbcCut[85] = 5.985000;
  bbcCut[86] = 4.725000;
  bbcCut[87] = 3.115000;
  bbcCut[88] = -0.5;

  Float_t bbcsum = bbc1 + bbc2;

  Float_t centrality = -1.0;
  // use lookup table to determine centrality of this event using clock method
  for (int jjj = 0; jjj<89; jjj++) {
    if (bbcsum < bbcCut[jjj] && bbcsum >= bbcCut[jjj+1]) {
      centrality = (float) (jjj + 1);
      break;
    }
  }

  if (centrality == -1.0) {
    std::cout<< PHWHERE << "ERROR WITH RUN5 CUCU CENTRALITY - bbc1=" << bbc1 << " bbc2=" << bbc2 << " return -1" << std::endl;
  }

  return ((int) centrality);

}

//===========================================================================
//  Run 4 centrality functions
//===========================================================================

//______________________________________________________________
int getCentralityByClockRun4(PHCompositeNode *topNode) {
	
	// phglobal node
  PHGlobal* d_global =   findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if(!d_global)
	{ 
    std::cout<< PHWHERE <<" ERROR: PHGlobal not in the Node Tree"<<std::endl;
    return -1;
  }

	// run header node
  RunHeader *d_run = findNode::getClass<RunHeader>(topNode,"RunHeader");
	if( !d_run)
	{ 
    std::cout<< PHWHERE <<" ERROR: RunHeader not in the Node Tree"<<std::endl;
    return -1;
  }
			
  float bbc1 = d_global->getBbcChargeN();
  float bbc2 = d_global->getBbcChargeS();
  float zdc1 = d_global->getZdcEnergyN();
  float zdc2 = d_global->getZdcEnergyS();
  int  runno = d_run->get_RunNumber();
  return getCentralityByClockRun4(bbc1, bbc2, zdc1, zdc2, runno);
}

//______________________________________________________________
int getCentralityByPerpRun4(PHCompositeNode *topNode) 
{

	// phglobal node
  PHGlobal* d_global =   findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if(!d_global)
	{ 
    std::cout<< PHWHERE <<" ERROR: PHGlobal not in the Node Tree"<<std::endl;
    return -1;
  }

	// run header node
  RunHeader *d_run = findNode::getClass<RunHeader>(topNode,"RunHeader");
	if( !d_run)
	{ 
    std::cout<< PHWHERE <<" ERROR: RunHeader not in the Node Tree"<<std::endl;
    return -1;
  }

  float bbc1  = d_global->getBbcChargeN();
  float bbc2  = d_global->getBbcChargeS();
  float zdc1  = d_global->getZdcEnergyN();
  float zdc2  = d_global->getZdcEnergyS();
  int   runno = d_run->get_RunNumber();
  return getCentralityByPerpRun4(bbc1, bbc2, zdc1, zdc2, runno);
}

//______________________________________________________________
int getCentralityByClockRun4(float bbc1, float bbc2, float zdc1, float zdc2, int runno) {

  // Calculates event centrality in percent for run4 AuAu.
  // Uses "clock" method (zdc vs bbc angle).
  // Assumes that we see 93% of total cross section.
  // Returned value of 1 means that this event is from 0% to 1% most central,
  // Returned value of 50 means that this event is from 49% to 50% most central, etc.
  // Event selection: abs(Zvertex)<30cm
  // Clock calibration is based on run4 minimum bias nanoDSTs (PHGlobal object), Run# 116830-118898
  // Run dependence was checked and quick correction is applied
  
  float phiCut[94];
  
  float bbcMax = 1700;
  float zdcMax = 8500;
  float bbcCenter = 0.15;

  phiCut[0] = 1.99998;
  phiCut[1] = 1.41902;
  phiCut[2] = 1.3713;
  phiCut[3] = 1.32578;
  phiCut[4] = 1.28002;
  phiCut[5] = 1.23386;
  phiCut[6] = 1.1875;
  phiCut[7] = 1.14094;
  phiCut[8] = 1.09434;
  phiCut[9] = 1.04794;
  phiCut[10] = 1.0017;
  phiCut[11] = 0.95578;
  phiCut[12] = 0.91054;
  phiCut[13] = 0.86594;
  phiCut[14] = 0.8223;
  phiCut[15] = 0.77982;
  phiCut[16] = 0.73846;
  phiCut[17] = 0.69822;
  phiCut[18] = 0.65918;
  phiCut[19] = 0.62162;
  phiCut[20] = 0.5853;
  phiCut[21] = 0.55038;
  phiCut[22] = 0.51686;
  phiCut[23] = 0.48474;
  phiCut[24] = 0.45406;
  phiCut[25] = 0.42458;
  phiCut[26] = 0.39626;
  phiCut[27] = 0.36922;
  phiCut[28] = 0.34338;
  phiCut[29] = 0.3187;
  phiCut[30] = 0.2951;
  phiCut[31] = 0.27258;
  phiCut[32] = 0.25106;
  phiCut[33] = 0.23046;
  phiCut[34] = 0.21078;
  phiCut[35] = 0.19194;
  phiCut[36] = 0.17398;
  phiCut[37] = 0.15674;
  phiCut[38] = 0.1403;
  phiCut[39] = 0.12454;
  phiCut[40] = 0.10946;
  phiCut[41] = 0.09494;
  phiCut[42] = 0.08106;
  phiCut[43] = 0.06778;
  phiCut[44] = 0.05502;
  phiCut[45] = 0.04278;
  phiCut[46] = 0.0309;
  phiCut[47] = 0.01954;
  phiCut[48] = 0.00858;
  phiCut[49] = -0.00202;
  phiCut[50] = -0.01222;
  phiCut[51] = -0.0221;
  phiCut[52] = -0.03162;
  phiCut[53] = -0.0409;
  phiCut[54] = -0.0499;
  phiCut[55] = -0.0587;
  phiCut[56] = -0.06722;
  phiCut[57] = -0.07554;
  phiCut[58] = -0.0837;
  phiCut[59] = -0.0917;
  phiCut[60] = -0.09954;
  phiCut[61] = -0.1073;
  phiCut[62] = -0.11494;
  phiCut[63] = -0.12254;
  phiCut[64] = -0.13006;
  phiCut[65] = -0.13754;
  phiCut[66] = -0.14506;
  phiCut[67] = -0.15258;
  phiCut[68] = -0.16014;
  phiCut[69] = -0.16774;
  phiCut[70] = -0.17546;
  phiCut[71] = -0.1833;
  phiCut[72] = -0.19134;
  phiCut[73] = -0.19954;
  phiCut[74] = -0.20802;
  phiCut[75] = -0.21682;
  phiCut[76] = -0.22598;
  phiCut[77] = -0.23566;
  phiCut[78] = -0.24582;
  phiCut[79] = -0.2567;
  phiCut[80] = -0.2683;
  phiCut[81] = -0.2809;
  phiCut[82] = -0.29466;
  phiCut[83] = -0.30982;
  phiCut[84] = -0.32678;
  phiCut[85] = -0.34598;
  phiCut[86] = -0.36806;
  phiCut[87] = -0.39434;
  phiCut[88] = -0.4265;
  phiCut[89] = -0.46758;
  phiCut[90] = -0.52382;
  phiCut[91] = -0.6093;
  phiCut[92] = -0.77366;
  phiCut[93] = -1.54286;

  Float_t BBC0 = bbcCenter * bbcMax;
  Int_t cent = -1;
  Float_t czdc = (zdc1+zdc2)*4300./(8720.8-3.707*0.01*runno);
  Float_t cbbc;

  if(runno<106935){
    cbbc = -1;
  }
  else if(runno<107788){
    cbbc = (bbc1+bbc2)*1.04688;
  }
  else if(runno<111611){
    cbbc = (bbc1+bbc2)*435./(-8.493e-3*runno+1383.1);
  }
  else if(runno<112600){
    cbbc = (bbc1+bbc2)*435./(1.7485e-2*runno-1536);
  }
  else if(runno<122224){
    cbbc = (bbc1 + bbc2)*435./(1.6659e-4*runno+414.1);
  }
  else{
    cbbc = -1;
  }

  if (czdc>0 && cbbc > 0 ) 
    {
      float phi_bbczdc = atan2((cbbc - BBC0) / bbcMax, czdc / zdcMax);
      for (int j = 0; j<93; j++)
        {
          if (phi_bbczdc > phiCut[j + 1] && phi_bbczdc < phiCut[j])
            cent = j + 1;
        }
    }

  return cent;
}
                                                                                                                             
//______________________________________________________________
int getCentralityByPerpRun4(float bbc1, float bbc2, float zdc1, float zdc2, int runno) {

    // Calculates event centrality in percent for run4 AuAu.
    // Uses slices perpendicular to zdc/bbc distribution.
    // Assumes that we see 93% of total cross section.
    // Returned value of 1 means that this event is from 0% to 1% most central,
    // Returned value of 50 means that this event is from 49% to 50% most central, etc.
    // Event selection: abs(Zvertex)<30cm
    // Clock calibration is based on run4 minimum bias nanoDSTs (PHGlobal object),
    // Run dependence was not checked yet.
                                                                                                                             
    float phiCut[94];

    phiCut[0] = 2e-05;
    phiCut[1] = 0.05758;
    phiCut[2] = 0.06806;
    phiCut[3] = 0.07778;
    phiCut[4] = 0.08698;
    phiCut[5] = 0.0963;
    phiCut[6] = 0.10542;
    phiCut[7] = 0.11422;
    phiCut[8] = 0.12322;
    phiCut[9] = 0.13202;
    phiCut[10] = 0.14026;
    phiCut[11] = 0.14898;
    phiCut[12] = 0.1577;
    phiCut[13] = 0.16654;
    phiCut[14] = 0.17502;
    phiCut[15] = 0.18342;
    phiCut[16] = 0.19142;
    phiCut[17] = 0.19978;
    phiCut[18] = 0.20802;
    phiCut[19] = 0.21618;
    phiCut[20] = 0.22438;
    phiCut[21] = 0.23246;
    phiCut[22] = 0.24042;
    phiCut[23] = 0.24846;
    phiCut[24] = 0.25654;
    phiCut[25] = 0.26454;
    phiCut[26] = 0.27266;
    phiCut[27] = 0.28078;
    phiCut[28] = 0.28858;
    phiCut[29] = 0.29638;
    phiCut[30] = 0.30462;
    phiCut[31] = 0.31282;
    phiCut[32] = 0.32126;
    phiCut[33] = 0.32962;
    phiCut[34] = 0.3381;
    phiCut[35] = 0.34646;
    phiCut[36] = 0.35466;
    phiCut[37] = 0.36306;
    phiCut[38] = 0.37138;
    phiCut[39] = 0.37998;
    phiCut[40] = 0.38878;
    phiCut[41] = 0.39734;
    phiCut[42] = 0.40582;
    phiCut[43] = 0.41442;
    phiCut[44] = 0.42306;
    phiCut[45] = 0.4317;
    phiCut[46] = 0.44054;
    phiCut[47] = 0.44906;
    phiCut[48] = 0.45766;
    phiCut[49] = 0.46606;
    phiCut[50] = 0.47426;
    phiCut[51] = 0.48278;
    phiCut[52] = 0.49122;
    phiCut[53] = 0.49978;
    phiCut[54] = 0.50818;
    phiCut[55] = 0.51686;
    phiCut[56] = 0.52554;
    phiCut[57] = 0.53394;
    phiCut[58] = 0.54226;
    phiCut[59] = 0.55122;
    phiCut[60] = 0.55994;
    phiCut[61] = 0.56854;
    phiCut[62] = 0.57758;
    phiCut[63] = 0.58666;
    phiCut[64] = 0.5957;
    phiCut[65] = 0.60474;
    phiCut[66] = 0.6139;
    phiCut[67] = 0.62302;
    phiCut[68] = 0.63238;
    phiCut[69] = 0.6415;
    phiCut[70] = 0.65018;
    phiCut[71] = 0.65902;
    phiCut[72] = 0.6675;
    phiCut[73] = 0.67634;
    phiCut[74] = 0.68498;
    phiCut[75] = 0.69394;
    phiCut[76] = 0.7031;
    phiCut[77] = 0.7119;
    phiCut[78] = 0.72102;
    phiCut[79] = 0.72998;
    phiCut[80] = 0.73898;
    phiCut[81] = 0.74794;
    phiCut[82] = 0.75702;
    phiCut[83] = 0.76662;
    phiCut[84] = 0.77598;
    phiCut[85] = 0.78598;
    phiCut[86] = 0.79546;
    phiCut[87] = 0.80598;
    phiCut[88] = 0.81666;
    phiCut[89] = 0.82798;
    phiCut[90] = 0.84054;
    phiCut[91] = 0.85598;
    phiCut[92] = 0.87598;
    phiCut[93] = 0.92802;

    float bbcScale=5.6;
    float bbcShift=1.0;
    float bbcPower=0.25;
    float zdcMax=9000.;
    int cent = -1;

    float zdcCalibS, zdcCalibN, bbcCalibS, bbcCalibN;
    CentralityParameters *parameters = CentralityParameters::getPointer();
    parameters->getCalibForPerpRun4(runno, &zdcCalibS, &zdcCalibN, &bbcCalibS, &bbcCalibN);

    // --- Correction for bbc & zdc ---
    bbc1 = bbc1 * bbcCalibN;
    bbc2 = bbc2 * bbcCalibS;
    zdc1 = zdc1 * zdcCalibN;
    zdc2 = zdc2 * zdcCalibS;

    // run2
    //TF1 *fit0 = new TF1("fit0","-6.973e-01+9.464e+00*x-2.297e+01*x*x+2.619e+01*x*x*x-1.214e+01*x*x*x*x",0.0,1.0);
    // run4, 20 files
    TF1 *fit0 = new TF1("fit0","-2.56447e-01+3.97986e+00*x-5.16252e+00*x*x+2.53577e+00*x*x*x-1.15994e+00*x*x*x*x",0.0,1.0);
                                                                                                                             
    if(zdc1!=0 && zdc2!=0 && bbc1!=0 && bbc2!=0) {
                                                                                                                             
      float xx = (pow(bbc1+bbc2,bbcPower)-bbcShift)/bbcScale;
      float yy = (zdc1+zdc2)/zdcMax;
                                                                                                                             
      float error = 9999.; int i = 0; float X = 0., Y = -999.;
      if(xx > 0.2) { X = xx - 0.2; }

      while (i < 400) {
        float value = yy - fit0->Eval(X);
        value = (xx - X) * (xx - X) + value * value;
        if(value < error) { error = value; Y = X; }
          X += 0.001; i++;
      }
                                                                                                                             
      for(int k=0; k<93; k++) {
        if((1.-Y)>phiCut[k] && (1.-Y)<phiCut[k+1]) {
          cent = k + 1; break;
        }
      }
    }
                                                                                                                             
    delete fit0;

    return cent;
}

//===============================================================================
// END of Run4 functions
//===============================================================================
                                                                                                                             
//======= run2 functions start ==================================================

  int getCentralityByPerp(float bbce1, float bbce2, float zdce1, float zdce2)
  {

    // Calculates event centrality in percent.
    // Uses slices perpendicular to zdc/bbc distribution.
    // Assumes that we see 93% of total cross section.
    // Returned value of 1 means that this event is from 0% to 1% most central,
    // Returned value of 50 means that this event is from 49% to 50% most central, etc.
    // Event selection: abs(Zvertex)<30cm && TriggerHelper::IsEventMinBias()
    // Clock calibration is based on run2 v03 microDSTs, 
    // Run # 29459, 29510, 30650, 31232, 31824, 32043, 32242
    // Run dependence was not checked yet.

    float phiCut[94];

    phiCut[0] = 0.0;
    phiCut[1] = 0.0647;
    phiCut[2] = 0.07562;
    phiCut[3] = 0.0861;
    phiCut[4] = 0.09586;
    phiCut[5] = 0.10534;
    phiCut[6] = 0.11478;
    phiCut[7] = 0.12414;
    phiCut[8] = 0.13326;
    phiCut[9] = 0.14246;
    phiCut[10] = 0.15122;
    phiCut[11] = 0.15978;
    phiCut[12] = 0.16838;
    phiCut[13] = 0.17726;
    phiCut[14] = 0.18622;
    phiCut[15] = 0.19454;
    phiCut[16] = 0.20306;
    phiCut[17] = 0.21138;
    phiCut[18] = 0.2197;
    phiCut[19] = 0.22766;
    phiCut[20] = 0.2361;
    phiCut[21] = 0.2443;
    phiCut[22] = 0.2521;
    phiCut[23] = 0.25978;
    phiCut[24] = 0.2679;
    phiCut[25] = 0.27586;
    phiCut[26] = 0.28422;
    phiCut[27] = 0.29242;
    phiCut[28] = 0.3003;
    phiCut[29] = 0.30818;
    phiCut[30] = 0.31646;
    phiCut[31] = 0.3247;
    phiCut[32] = 0.3327;
    phiCut[33] = 0.3407;
    phiCut[34] = 0.34882;
    phiCut[35] = 0.3567;
    phiCut[36] = 0.36474;
    phiCut[37] = 0.37362;
    phiCut[38] = 0.38178;
    phiCut[39] = 0.39014;
    phiCut[40] = 0.39818;
    phiCut[41] = 0.40642;
    phiCut[42] = 0.41498;
    phiCut[43] = 0.42378;
    phiCut[44] = 0.4321;
    phiCut[45] = 0.44026;
    phiCut[46] = 0.44878;
    phiCut[47] = 0.45782;
    phiCut[48] = 0.46662;
    phiCut[49] = 0.47518;
    phiCut[50] = 0.48374;
    phiCut[51] = 0.49218;
    phiCut[52] = 0.50094;
    phiCut[53] = 0.50966;
    phiCut[54] = 0.51834;
    phiCut[55] = 0.52686;
    phiCut[56] = 0.53562;
    phiCut[57] = 0.54398;
    phiCut[58] = 0.55266;
    phiCut[59] = 0.5617;
    phiCut[60] = 0.57038;
    phiCut[61] = 0.57918;
    phiCut[62] = 0.58842;
    phiCut[63] = 0.59734;
    phiCut[64] = 0.60622;
    phiCut[65] = 0.61498;
    phiCut[66] = 0.62366;
    phiCut[67] = 0.63242;
    phiCut[68] = 0.6415;
    phiCut[69] = 0.64958;
    phiCut[70] = 0.65794;
    phiCut[71] = 0.66638;
    phiCut[72] = 0.6747;
    phiCut[73] = 0.68302;
    phiCut[74] = 0.69154;
    phiCut[75] = 0.70002;
    phiCut[76] = 0.70874;
    phiCut[77] = 0.71702;
    phiCut[78] = 0.72598;
    phiCut[79] = 0.73458;
    phiCut[80] = 0.74358;
    phiCut[81] = 0.75222;
    phiCut[82] = 0.76166;
    phiCut[83] = 0.77118;
    phiCut[84] = 0.78114;
    phiCut[85] = 0.79098;
    phiCut[86] = 0.80098;
    phiCut[87] = 0.81098;
    phiCut[88] = 0.82006;
    phiCut[89] = 0.82998;
    phiCut[90] = 0.84198;
    phiCut[91] = 0.85806;
    phiCut[92] = 0.88298;
    phiCut[93] = 0.90898;

    // fit to normalized zdc/bbc distribution

    TF1 *fit0 = new TF1("fit0", "-6.97310e-01+9.46360e+00*x-2.29659e+01*x*x+2.61893e+01*x*x*x-1.21407e+01*x*x*x*x", 0.0, 1.0);

    int cent = -1;

    if (zdce1 != 0 && zdce2 != 0 && bbce1 != 0 && bbce2 != 0) {

      float xx = (::pow(bbce1+bbce2,0.25)-1.0)/5.6;
      float yy = (zdce1 + zdce2) / 4500.;

      float error = 9999.;
      int i = 0;
      float X = 0., Y = -999.;
      if (xx > 0.2)
        {
          X = xx - 0.2;
        }
      while (i < 400)
        {
          float value = yy - fit0->Eval(X);
          value = (xx - X) * (xx - X) + value * value;
          if (value < error)
            {
              error = value;
              Y = X;
            }
          X += 0.001;
          i++;
        }

      for (int k = 0; k < 93; k++)
        {
          if ((1 - Y) > phiCut[k] && (1 - Y) < phiCut[k + 1])
            {
              cent = k + 1;
              break;
            }
        }
    }  

    delete fit0;
    return cent;

  }

  int getCentralityByPerp(BbcOut* bbc, ZdcOut* zdc)
  {

    float bbce1 = bbc->get_ChargeSum(Bbc::North);
    float bbce2 = bbc->get_ChargeSum(Bbc::South);
    float zdce1 = zdc->get_Energy(Zdc::North);
    float zdce2 = zdc->get_Energy(Zdc::South);

    return getCentralityByPerp(bbce1, bbce2, zdce1, zdce2);

  }

  int getCentralityByPerp(PHCompositeNode* topNode)
  {
    // starting from the topNode one should allways look for the dst
    // node which is guaranteed by pdst to be there (pdst allways maps
    // the input onto the dst node). If there is another
    // node tree being filled from the dst node with the same name it
    // might not be up to date yet and we might grab the wrong Node if
    // we start from the topNode

    PHNodeIterator iter(topNode);
    PHCompositeNode *dstNode;
    dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

    if (!dstNode)
      {
        cout << PHWHERE << "No Dst Node bailing out" << endl;
        return -1;
      }

    // BbcOut in DST
    BbcOut *bbcout = findNode::getClass<BbcOut>(dstNode,"BbcOut");
    // ZdcOut in DST
    ZdcOut *zdcout = findNode::getClass<ZdcOut>(dstNode,"ZdcOut");

    int cent = -1;

    // first try microDST
    if (bbcout && zdcout)
      {
        return getCentralityByPerp(bbcout, zdcout);
      }

    return cent;

  }

  //==========================================================================

  int getCentralityByClock(float bbce1, float bbce2, float zdce1, float zdce2)
  {

    // Calculates event centrality in percent.
    // Uses "clock" method (zdc vs bbc angle).
    // Assumes that we see 93% of total cross section.
    // Returned value of 1 means that this event is from 0% to 1% most central,
    // Returned value of 50 means that this event is from 49% to 50% most central, etc.
    // Event selection: abs(Zvertex)<30cm && TriggerHelper::IsEventMinBias()
    // Clock calibration is based on run2 v03 microDSTs, 
    // Run # 29459, 29510, 30650, 31232, 31824, 32043, 32242
    // Run dependence was not checked yet.

    float phiCut[94];

    float bbcMax = 1700;
    float zdcMax = 4500;
    float bbcCenter = 0.15;

    phiCut[0] = 1.99998;
    phiCut[1] = 1.29966;
    phiCut[2] = 1.2239;
    phiCut[3] = 1.15502;
    phiCut[4] = 1.09154;
    phiCut[5] = 1.03066;
    phiCut[6] = 0.97202;
    phiCut[7] = 0.91638;
    phiCut[8] = 0.86558;
    phiCut[9] = 0.81794;
    phiCut[10] = 0.77178;
    phiCut[11] = 0.7297;
    phiCut[12] = 0.68818;
    phiCut[13] = 0.64842;
    phiCut[14] = 0.61222;
    phiCut[15] = 0.57718;
    phiCut[16] = 0.54386;
    phiCut[17] = 0.51166;
    phiCut[18] = 0.48274;
    phiCut[19] = 0.45478;
    phiCut[20] = 0.42818;
    phiCut[21] = 0.40366;
    phiCut[22] = 0.38002;
    phiCut[23] = 0.35674;
    phiCut[24] = 0.33462;
    phiCut[25] = 0.31438;
    phiCut[26] = 0.29458;
    phiCut[27] = 0.27542;
    phiCut[28] = 0.2559;
    phiCut[29] = 0.23758;
    phiCut[30] = 0.2207;
    phiCut[31] = 0.20422;
    phiCut[32] = 0.1879;
    phiCut[33] = 0.1725;
    phiCut[34] = 0.15842;
    phiCut[35] = 0.14386;
    phiCut[36] = 0.13042;
    phiCut[37] = 0.1175;
    phiCut[38] = 0.10494;
    phiCut[39] = 0.09242;
    phiCut[40] = 0.08034;
    phiCut[41] = 0.06898;
    phiCut[42] = 0.05738;
    phiCut[43] = 0.04686;
    phiCut[44] = 0.03718;
    phiCut[45] = 0.0273;
    phiCut[46] = 0.01766;
    phiCut[47] = 0.00786;
    phiCut[48] = -0.00162;
    phiCut[49] = -0.01034;
    phiCut[50] = -0.01874;
    phiCut[51] = -0.02674;
    phiCut[52] = -0.03494;
    phiCut[53] = -0.04238;
    phiCut[54] = -0.04994;
    phiCut[55] = -0.0571;
    phiCut[56] = -0.0639;
    phiCut[57] = -0.0705;
    phiCut[58] = -0.07674;
    phiCut[59] = -0.08322;
    phiCut[60] = -0.08966;
    phiCut[61] = -0.09598;
    phiCut[62] = -0.10202;
    phiCut[63] = -0.10794;
    phiCut[64] = -0.11362;
    phiCut[65] = -0.11946;
    phiCut[66] = -0.12494;
    phiCut[67] = -0.13026;
    phiCut[68] = -0.13554;
    phiCut[69] = -0.14102;
    phiCut[70] = -0.14638;
    phiCut[71] = -0.1517;
    phiCut[72] = -0.15706;
    phiCut[73] = -0.16234;
    phiCut[74] = -0.16798;
    phiCut[75] = -0.17386;
    phiCut[76] = -0.17982;
    phiCut[77] = -0.1861;
    phiCut[78] = -0.19266;
    phiCut[79] = -0.19986;
    phiCut[80] = -0.20766;
    phiCut[81] = -0.21646;
    phiCut[82] = -0.22658;
    phiCut[83] = -0.23818;
    phiCut[84] = -0.2523;
    phiCut[85] = -0.2675;
    phiCut[86] = -0.28674;
    phiCut[87] = -0.3085;
    phiCut[88] = -0.33402;
    phiCut[89] = -0.36666;
    phiCut[90] = -0.41802;
    phiCut[91] = -0.52014;
    phiCut[92] = -0.85538;
    phiCut[93] = -1.4957;

    float bbce = bbce1 + bbce2;
    float zdce = zdce1 + zdce2;
    float BBC0 = bbcCenter * bbcMax;

    int cent = -1;

    if (zdce1 != 0 && zdce2 != 0 && bbce1 != 0 && bbce2 != 0)
      {
        float phi_bbczdc = atan2((bbce - BBC0) / bbcMax, zdce / zdcMax);
        for (int j = 0; j<93; j++)
          {
            if (phi_bbczdc > phiCut[j + 1] && phi_bbczdc < phiCut[j])
              cent = j + 1;
          }
      }

    return cent;

  }

  int getCentralityByClock(BbcOut* bbc, ZdcOut* zdc)
  {

    float bbce1 = bbc->get_ChargeSum(Bbc::North);
    float bbce2 = bbc->get_ChargeSum(Bbc::South);
    float zdce1 = zdc->get_Energy(Zdc::North);
    float zdce2 = zdc->get_Energy(Zdc::South);

    return getCentralityByClock(bbce1, bbce2, zdce1, zdce2);

  }

  int getCentralityByClock(PHCompositeNode* topNode)
  {
    // starting from the topNode one should allways look for the dst
    // node which is guaranteed by pdst to be there (pdst allways maps
    // the input onto the dst node). If there is another
    // node tree being filled from the dst node with the same name it
    // might not be up to date yet and we might grab the wrong Node if
    // we start from the topNode


    PHNodeIterator iter(topNode);
    PHCompositeNode *dstNode;
    dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

    if (!dstNode)
      {
        cout << PHWHERE << "No Dst Node bailing out" << endl;
        return -1;
      }
    // BbcOut in DST
    BbcOut *bbcout = findNode::getClass<BbcOut>(dstNode,"BbcOut");
    // ZdcOut in DST
    ZdcOut *zdcout = findNode::getClass<ZdcOut>(dstNode,"ZdcOut");


    //---------------------------------------------------------------------

    int cent = -1;

    // first try microDST
    if (bbcout && zdcout)
      {
        return getCentralityByClock(bbcout, zdcout);
      }

    return cent;

  }




  //------------------------
  // d Au functions
  //-------------------------
  
  
  
  int getdAuCentrality(BbcOut* bbc, ZdcOut* zdc)
  {  
    float bbce1 = bbc->get_ChargeSum(Bbc::North);
    float bbce2 = bbc->get_ChargeSum(Bbc::South);
    float zdce1 = zdc->get_Energy(Zdc::North);
    float zdce2 = zdc->get_Energy(Zdc::South);
    
    return getdAuCentralityBbcZdc(bbce1, bbce2, zdce1, zdce2);
  }
  
  int getdAuCentrality(FclOut* fclSouth, ZdcOut* zdc)
  {  
    float fcle = fclSouth->getSumGrey();
    float zdce1 = zdc->get_Energy(Zdc::North);
    float zdce2 = zdc->get_Energy(Zdc::South);
    
    return getdAuCentralityFclZdc(fcle, zdce1, zdce2);
  }
  
  int getdAuCentrality(FclOut* fclSouth, BbcOut* bbc)
  {  
    float fcle = fclSouth->getSumGrey();
    float bbce1 = bbc->get_ChargeSum(Bbc::North);
    float bbce2 = bbc->get_ChargeSum(Bbc::South);
    
    return getdAuCentralityFclBbc(fcle, bbce1, bbce2);
  }
  
  int getdAuCentrality(FclOut* fclSouth, BbcOut* bbc, ZdcOut* zdc)
  {  
    float fcle = (float)fclSouth->getSumGrey();
    float bbce1 = bbc->get_ChargeSum(Bbc::North);
    float bbce2 = bbc->get_ChargeSum(Bbc::South);
    float zdce1 = zdc->get_Energy(Zdc::North);
    float zdce2 = zdc->get_Energy(Zdc::South);
    
    return getdAuCentralityFclBbcZdc(fcle, bbce1, bbce2, zdce1, zdce2);
  }

  int getdAuCentralityFclZdc(float fclEnergyS, float zdcEnergyN, float zdcEnergyS)
  {

    //check for bad events and return error
    if(fclEnergyS==0 || zdcEnergyS==0) return -1;

    //scale the input energies
    float fcl = fclEnergyS/800.0;
    float zdc = zdcEnergyS/2500.0;

    //load the cumulant array
    float cumulant[90];
    cumulant[0]=0.0;
    cumulant[1]=0.0;
    cumulant[2]=0.0;
    cumulant[3] = 0.0005;
    cumulant[4] = 0.0055;
    cumulant[5] = 0.0075;
    cumulant[6] = 0.0115;
    cumulant[7] = 0.0155;
    cumulant[8] = 0.0205;
    cumulant[9] = 0.0265;
    cumulant[10] = 0.0325;
    cumulant[11] = 0.0385;
    cumulant[12] = 0.0455;
    cumulant[13] = 0.0525;
    cumulant[14] = 0.0595;
    cumulant[15] = 0.0675;
    cumulant[16] = 0.0745;
    cumulant[17] = 0.0815;
    cumulant[18] = 0.0895;
    cumulant[19] = 0.0975;
    cumulant[20] = 0.1045;
    cumulant[21] = 0.1105;
    cumulant[22] = 0.1175;
    cumulant[23] = 0.1235;
    cumulant[24] = 0.1305;
    cumulant[25] = 0.1365;
    cumulant[26] = 0.1435;
    cumulant[27] = 0.1495;
    cumulant[28] = 0.1555;
    cumulant[29] = 0.1615;
    cumulant[30] = 0.1685;
    cumulant[31] = 0.1745;
    cumulant[32] = 0.1815;
    cumulant[33] = 0.1875;
    cumulant[34] = 0.1935;
    cumulant[35] = 0.1985;
    cumulant[36] = 0.2045;
    cumulant[37] = 0.2105;
    cumulant[38] = 0.2165;
    cumulant[39] = 0.2215;
    cumulant[40] = 0.2275;
    cumulant[41] = 0.2325;
    cumulant[42] = 0.2385;
    cumulant[43] = 0.2435;
    cumulant[44] = 0.2495;
    cumulant[45] = 0.2565;
    cumulant[46] = 0.2625;
    cumulant[47] = 0.2695;
    cumulant[48] = 0.2755;
    cumulant[49] = 0.2805;
    cumulant[50] = 0.2875;
    cumulant[51] = 0.2935;
    cumulant[52] = 0.2995;
    cumulant[53] = 0.3065;
    cumulant[54] = 0.3125;
    cumulant[55] = 0.3195;
    cumulant[56] = 0.3255;
    cumulant[57] = 0.3325;
    cumulant[58] = 0.3395;
    cumulant[59] = 0.3465;
    cumulant[60] = 0.3535;
    cumulant[61] = 0.3605;
    cumulant[62] = 0.3675;
    cumulant[63] = 0.3755;
    cumulant[64] = 0.3825;
    cumulant[65] = 0.3895;
    cumulant[66] = 0.3975;
    cumulant[67] = 0.4045;
    cumulant[68] = 0.4125;
    cumulant[69] = 0.4205;
    cumulant[70] = 0.4285;
    cumulant[71] = 0.4375;
    cumulant[72] = 0.4455;
    cumulant[73] = 0.4545;
    cumulant[74] = 0.4635;
    cumulant[75] = 0.4725;
    cumulant[76] = 0.4825;
    cumulant[77] = 0.4935;
    cumulant[78] = 0.5035;
    cumulant[79] = 0.5155;
    cumulant[80] = 0.5275;
    cumulant[81] = 0.5405;
    cumulant[82] = 0.5555;
    cumulant[83] = 0.5695;
    cumulant[84] = 0.5865;
    cumulant[85] = 0.6055;
    cumulant[86] = 0.6265;
    cumulant[87] = 0.6535;
    cumulant[88] = 0.6895;
    cumulant[89] = 0.7435;

    //fit function to the fcl-zdc correlation
    TF1 *fit = new TF1("fit", "0.839500*(1-exp(-x/0.129000))", 0.0, 1.0);

    float distance_test=9999999.;  //dummy variable
    float distance=9999999.;  //final distance from point to function
    int step=0;  //minimizing step
    float fcl_test = 1/1000.0; //value of fcl at the step
    float fcl_min = 0;

    while(step<1000){
      distance_test = (zdc-fit->Eval(fcl_test))*(zdc-fit->Eval(fcl_test));
      distance_test = distance_test + (fcl-fcl_test)*(fcl-fcl_test);
      if(distance_test < distance){
	distance=distance_test;
	fcl_min=fcl_test;
      }
      step++;
      fcl_test=step/1000.0;
    }

    for(int k=0;k<89;k++)
      {
	if( fcl_min > cumulant[k] && fcl_min <cumulant[k+1])
	  {
	    return k;
	  }
      }
    return -1;
  }
  
  int getdAuCentralityFclBbc(float fclEnergyS, float bbcEnergyN, float bbcEnergyS)
  {
    cout<<"Warning, Fcl centrality is not ready yet.  The number you get is just for testing."<<endl;
    return int(fclEnergyS);
  }
  
  int getdAuCentralityBbcZdc(float bbcEnergyS, float bbcEnergyN, float zdcEnergyN, float zdcEnergyS)
  {
    cout<<"Warning, Fcl centrality is not ready yet.  The number you get is just for testing."<<endl;
    return int(bbcEnergyS);
  }
  
  int getdAuCentralityFclBbcZdc(float fclEnergyS, float bbcEnergyN, float bbcEnergyS, float zdcEnergyN, float zdcEnergyS)
  {
    cout<<"Warning, Fcl centrality is not ready yet.  The number you get is just for testing."<<endl;
    return int(fclEnergyS);
  }
  
  
}
    
    


