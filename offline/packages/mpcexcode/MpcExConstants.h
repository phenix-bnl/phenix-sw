#ifndef __MPCEXCONSTANTS_H__
#define __MPCEXCONSTANTS_H__

#include <cmath>
#include <iostream>
#include <map>
#include <TRandom.h>
#include <phool.h>

namespace MpcExConstants {

  //! number of arms (0=south and 1=north a la PHENIX)
  static const unsigned short NARMS = 2;

  //! number of packets per arm
  static const unsigned short NPACKETS_PER_ARM = 8;

  //! number of total packets
  static const unsigned short NPACKETS = NARMS*NPACKETS_PER_ARM;

  //! number of chains (quadrant of 6 micromodules in a layer and a carrier board) read out in each packet
  static const unsigned short NCHAINS_PER_PACKET = 4;

  //! number of SVX4 chips in each chain from the carrier board
  static const unsigned short NCHIPS_PER_CHAIN = 12;

  //! number of SVX4 chips per micromodule
  static const unsigned short NCHIPS_PER_MODULE = 2;

  //! number of minipads connected to each svx4 chip
  static const unsigned short NROCBONDS_PER_CHIP = 64;

  //! number of minipads in each packet
  static const unsigned int NMINIPADS_PER_PACKET = NROCBONDS_PER_CHIP*NCHIPS_PER_CHAIN*NCHAINS_PER_PACKET;

  //! total number of channels
  static const unsigned int NMINIPADS = NMINIPADS_PER_PACKET*NPACKETS;

  //! number of minipads per sensor
  static const unsigned short NMINIPADS_PER_MODULE = NROCBONDS_PER_CHIP*NCHIPS_PER_MODULE;

  //! The LUT for minipad number (0-127) of high-gain channels in an SVX
  static const unsigned short HIGHGAINMINIPAD_IN_CHIP[NROCBONDS_PER_CHIP] = {61, 57, 53, 49, 45, 41, 37, 33, 29, 25, 21, 17, 13, 9, 5, 1, 2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86, 90, 94, 98, 102, 106, 110, 114, 118, 122, 126, 125, 121, 117, 113, 109, 105, 101, 97, 93, 89, 85, 81, 77, 73, 69, 65};

  //! The LUT for minipad number (0-127) of low-gain channels in an SVX
  static const unsigned short LOWGAINMINIPAD_IN_CHIP[NROCBONDS_PER_CHIP] = {63, 59, 55, 51, 47, 43, 39, 35, 31, 27, 23, 19, 15, 11, 7, 3, 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96, 100, 104, 108, 112, 116, 120, 124, 127, 123, 119, 115, 111, 107, 103, 99, 95, 91, 87, 83, 79, 75, 71, 67};

  //! the roc bond (0-63) given a minipad (0-127) on a sensor generated as a reverse look up from HIGHGAINMINIPAD_IN_CHIP and LOWGAINMINIPAD_IN_CHIP
  static const unsigned short ROCBOND_FROM_MINIPAD[NROCBONDS_PER_CHIP*2] = {16,15,16,15,17,14,17,14,18,13,18,13,19,12,19,12,20,11,20,11,21,10,21,10,22,9,22,9,23,8,23,8,24,7,24,7,25,6,25,6,26,5,26,5,27,4,27,4,28,3,28,3,29,2,29,2,30,1,30,1,31,0,31,0,32,63,32,63,33,62,33,62,34,61,34,61,35,60,35,60,36,59,36,59,37,58,37,58,38,57,38,57,39,56,39,56,40,55,40,55,41,54,41,54,42,53,42,53,43,52,43,52,44,51,44,51,45,50,45,50,46,49,46,49,47,48,47,48};

  //! The number of layers per arm
  static const unsigned short NLAYERS = 8;

  //! The maximum number of minipads in either direction in a layer
  static const unsigned short NN = 192;

  //! The number of quadrants per layer
  static const unsigned short NQUADRANTS_PER_LAYER = 4;

  //! The number of modules per quadrant
  static const unsigned short NMODULES_PER_QUADRANT = 6;

  //! The maximum number of "x" minipads per module
  static const unsigned short NLX_PER_MODULE = 32;

  //! The maximum number of "y" minipads per module
  static const unsigned short NLY_PER_MODULE = 4;

  //! The LUT for layer number (0-7) given a packet index in the arm and the chain within the packet. both arms are read out the same which is why arm doesn't show up here
  static const unsigned short LAYER_IN_PACKET_CHAIN[NPACKETS_PER_ARM][NCHAINS_PER_PACKET] = {{4,4,5,5},{6,6,7,7},{0,0,1,1},{2,2,3,3},{4,4,5,5},{6,6,7,7},{0,0,1,1},{2,2,3,3}};

  //! The LUT for packet index (0-7) given the layer number and whether it is top/bottom 0=top, 1=bottom
  static const unsigned short PACKET_IN_LAYER_TOPBOTTOM[2][NLAYERS] = {{2,2,3,3,0,0,1,1},{6,6,7,7,4,4,5,5}};

  //! The side length of a the square micromodule
  static const float MODULE_LENGTH = 6.2; //cm
  //! The length of dead area on each side of the micromodule
  static const float MODULE_DEAD_LENGTH = 0.12; //cm
  static const float MINIPAD_SHORT_LENGTH = (MODULE_LENGTH-2*MODULE_DEAD_LENGTH)/NLX_PER_MODULE;
  static const float MINIPAD_LONG_LENGTH = (MODULE_LENGTH-2*MODULE_DEAD_LENGTH)/NLY_PER_MODULE;

  // Cuts above Pedestal in the Data
  static const float NORTH_DCM_LOW_SIGMA_CUT = 3.0;
  static const float NORTH_DCM_HIGH_SIGMA_CUT = 3.0;
  static const float SOUTH_DCM_LOW_SIGMA_CUT = 5.0;
  static const float SOUTH_DCM_HIGH_SIGMA_CUT = 5.0;

  static const float CAL_L01_LOW_SIGMA_CUT = 5.0;
  static const float CAL_L27_LOW_SIGMA_CUT = 5.0;
  static const float CAL_L01_HIGH_SIGMA_CUT = 16.0;
  static const float CAL_L27_HIGH_SIGMA_CUT = 10.0;

  //Energy scale information
  static const float MIP_IN_keV = 147.6;
  
  //Max high ADC value for combining 
  static const float MAX_ADC_COMBINE = 245.0; 

  // Fixed Calibrations Constants
  static const float FIXED_LOW_PEDESTAL = 20.0;
  static const float FIXED_HIGH_PEDESTAL = 20.0;
  static const float FIXED_LOW_PEDESTAL_WIDTH = 0.6;
  static const float FIXED_HIGH_PEDESTAL_WIDTH = 0.8;
  static const float FIXED_MIP_L01 = 18.0;
  static const float FIXED_MIP_L27 = 13.0;
  static const float FIXED_HL_RATIO = 4.5;
 
  // Detector geometry information:
  
  // Location of front face of MPC, MPC-EX
  static const float MPC_REFERENCE_Z = 220.9;
  static const float PS_REFERENCE_Z_N = 203.66;
  static const float PS_REFERENCE_Z_S = -203.66;

  static const double layerZ[2][8] = {{-203.982, -204.636, -205.290, -205.944, -206.598, -207.252, -207.906, -208.560},
				      {203.982, 204.636, 205.290, 205.944, 206.598, 207.252, 207.906, 208.560}};

  static const float fitUpLimit[8] = {0.45, 0.13, 0.09, 0.05, 0.035, 0.012, 0.005, 0.0015};
  static const float mpcfitUpLimit[5] = {8.0, 45.0, 45.0, 45.0, 50.};

  /**
   * convert energy (in keV) to adc units 
   * using the ADC values of the mip, pedestal mean and width
   * esat is the amount of energy above the max adc value
   */
  inline float make_adc(float mip, float mean, float width, float energy, TRandom* dice, float *esat=NULL){
    static float NCHANNELS = 256;
    float pedestal = dice->Gaus(mean,width);
    float adc = std::floor( (energy/MpcExConstants::MIP_IN_keV)*mip + pedestal );

    if(adc<0) adc = 0; 
    if(adc>=NCHANNELS) {
      if(esat!=NULL) {
	*esat = energy - ( (NCHANNELS-1) - pedestal)*(MpcExConstants::MIP_IN_keV/mip);
	if(*esat<0.0) *esat = 0.0; 
      }
      adc=NCHANNELS-1;
    }
    else{
      if(esat!=NULL) *esat = 0.0; 
    }

    return adc;
  }


  /**
   * return the factor increase to calculate the sampled events
   * the float returned should be multiplied to the analyzed events
   * in order to determine the number of sampled events.
   * input the runnumber, and, if found in the map, return the factor
   * a second input for which mpc trigger (0=MPC_N_A, 1=MPC_N_B)
   * the default is MPC_N_B
   */
  inline float MPCTriggerNEventFactor(int runnumber, int whichTrigger=1){
    if(whichTrigger!=0 && whichTrigger!=1){
      std::cout<<PHWHERE<<" :: Invalid trigger. Only 0=MPC_N_A or 1=MPC_N_B allowed. Returning garbage."<<std::endl;
      return -9999.;
    }

    static bool firstCall = true;
    static std::map<int,float> eventFactorMap_MPCA;
    static std::map<int,float> eventFactorMap_MPCB;
    if(firstCall){
      firstCall = false;

      //populate MPC_N_B
      //This is Run-16 d+Au 200 GeV
      eventFactorMap_MPCB.insert(std::pair<int,float>(454784,1077.67));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454785,1105.78));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454786,1277.94));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454789,1259.46));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454794,1908.87));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454797,1336.09));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454798,1343.78));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454799,1335.07));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454800,1304.51));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454802,962.053));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454808,1028.82));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454809,1040));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454810,1060.38));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454811,979.774));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454933,1014.55));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454934,954.78));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454935,1050.01));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454936,1000.92));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454937,966.394));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454943,890.893));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454944,1014.53));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454945,971.838));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454946,965.831));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454947,967.904));
      eventFactorMap_MPCB.insert(std::pair<int,float>(454948,1011.52));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455049,952.757));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455050,944.593));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455051,888.237));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455052,947.483));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455053,997.317));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455056,933.634));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455060,958.39));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455062,1007.16));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455063,951.683));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455064,993.947));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455065,990.257));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455066,933.866));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455071,978.473));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455073,948.789));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455074,1024.34));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455077,949.298));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455078,973.226));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455080,924.805));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455081,922.989));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455082,974.07));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455083,962.905));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455200,945.451));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455201,997.646));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455202,981.931));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455203,999.039));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455206,959.649));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455207,991.481));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455208,1044.13));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455209,977.375));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455211,931.656));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455212,1060.64));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455213,978.792));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455218,1002.9));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455220,988.382));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455221,1038.4));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455222,1034.61));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455223,1009.78));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455302,1013.65));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455303,1024.78));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455304,963.417));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455306,1043.03));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455344,1149.2));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455346,1069.26));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455351,1071.42));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455352,1035.6));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455353,979.64));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455355,1004.09));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455362,953.169));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455363,1027.49));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455364,1041.34));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455366,1033.65));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455367,970.831));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455446,989.97));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455450,1073.53));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455451,1031.59));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455452,945.975));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455476,912.674));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455477,918.866));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455478,1017.81));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455545,988.183));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455547,982.874));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455550,1008.02));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455551,1073.03));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455552,1105.54));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455585,1005.58));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455586,1182.46));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455587,1077.11));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455589,1026.56));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455590,1019.92));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455592,1035.8));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455604,957.771));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455605,1000.81));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455637,1173.19));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455638,1000.83));
      eventFactorMap_MPCB.insert(std::pair<int,float>(455639,956.572));

      //populate MPC_N_A
      //this is Run-16 d+Au 200 GeV
      eventFactorMap_MPCA.insert(std::pair<int,float>(454774,13.944));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454777,471.3));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454778,7.3788));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454782,378.611));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454783,47.0794));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454784,48.9788));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454785,373.605));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454786,392.319));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454789,416.595));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454794,85.1048));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454797,202.808));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454798,80.8534));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454799,49.0337));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454800,198.294));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454802,184.945));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454808,23.9907));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454809,24.4182));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454810,24.1475));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454811,96.8113));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454933,3.69865));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454934,42.2583));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454935,26.761));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454936,96.6971));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454937,94.4978));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454943,25.7588));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454944,93.3115));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454945,188.638));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454946,189.883));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454947,93.7341));
      eventFactorMap_MPCA.insert(std::pair<int,float>(454948,197.233));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455049,44.5724));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455050,25.6687));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455051,90.3438));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455052,185.226));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455053,93.3488));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455056,36.1815));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455060,98.118));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455062,198.603));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455063,189.929));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455064,200.665));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455065,93.8841));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455066,97.5944));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455071,57.0456));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455073,36.3349));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455074,95.8955));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455077,93.5109));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455078,96.4011));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455080,193.357));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455081,190.947));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455082,95.4661));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455083,190.594));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455200,53.386));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455201,37.8213));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455202,194.4));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455203,173.019));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455206,3.68899));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455207,65.0407));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455208,66.3089));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455209,38.3878));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455211,94.4373));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455212,102.677));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455213,101.991));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455218,66.4069));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455220,65.4044));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455221,40.4244));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455222,40.626));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455223,101.119));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455302,65.2728));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455303,39.9406));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455304,98.743));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455306,98.7039));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455344,59.3297));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455346,63.7659));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455351,39.5635));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455352,40.2756));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455353,38.6954));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455355,99.8306));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455362,3.40995));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455363,3.69156));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455364,68.4734));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455366,40.3533));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455367,103.588));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455446,3.81221));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455450,41.212));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455451,42.4982));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455452,40.342));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455476,3.77665));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455477,39.0981));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455478,40.5366));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455545,3.87587));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455547,41.3216));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455550,41.9134));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455551,108.798));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455552,108.009));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455585,3.72722));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455586,4.2219));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455587,4.13057));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455589,3.86372));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455590,4.07054));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455592,68.8489));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455604,40.5724));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455605,102.469));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455637,3.85895));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455638,3.95801));
      eventFactorMap_MPCA.insert(std::pair<int,float>(455639,3.8485));
    }

    std::map<int,float> factorMap = eventFactorMap_MPCB;
    if(whichTrigger == 0) factorMap = eventFactorMap_MPCA;

    std::map<int,float>::const_iterator itr = factorMap.find(runnumber);
    if(itr == factorMap.end()){
      std::cout<<PHWHERE<<" :: Run Number "<<runnumber<<" not in the map. Returning garbage."<<std::endl;
      return -9999.;
    }
    return itr->second;
  }

  // list of bad towers that can't be calibrated - JGL 11/20/2019

  inline bool isBadMPCTower(int feech, int iarm){

    bool retVal = false;

    if(iarm==1){

      if ( (feech==358) ||
	   (feech==335) ||
	   (feech==477) ||
	   (feech==468) ||
	   (feech==453) ||
	   (feech==454) ||
	   (feech==469) ||
	   //added by milap
	   (feech==328) ||
	   (feech==402) ||
	   (feech==306) ||
	   (feech==307) ||
	   (feech==396) ||
	   (feech==342) ||
	   (feech==389) ||
	   (feech==390) ||
	   (feech==539) ||
	   (feech==553) || 
	   (feech==560) || 
	   (feech==505) || 
	   (feech==489))
	retVal = true; 

    }
    
    return retVal; 

  }

  // check ix,iy entries valid - JGL 11/20/2019

  inline bool isValidMPCTower(int ix, int iy, int arm){

    bool retVal = false;

    if(arm == 0){
    
      // south arm

      switch(ix){

      case 0:
      case 17:
	if( (iy>=6) && (iy<=11) ) retVal = true; 
	break; 
      case 1:
      case 16:
	if( (iy>=4) && (iy<=13) ) retVal = true; 
	break; 
      case 2:
      case 15:
	if( (iy>=3) && (iy<=14) ) retVal = true; 
	break; 
      case 3:
      case 14:
	if( (iy>=2) && (iy<=15) ) retVal = true; 
	break; 
      case 4:
      case 13:
	if( (iy>=1) && (iy<=16) ) retVal = true; 
	break; 
      case 5:
      case 12:
	if( ((iy>=1) && (iy<=5)) || ((iy>=12) && (iy<=16)) ) retVal = true; 
	break; 
      case 6:
      case 7:
      case 8:
      case 9:
      case 10:
      case 11:
	if( ((iy>=0) && (iy<=4)) || ((iy>=13) && (iy<=17)) ) retVal = true; 
	break; 

      default: 
	retVal = false; 
      
      }

    }
    else if (arm==1){

      switch(ix){

      case 0:
      case 17:
	if( (iy>=7) && (iy<=10) ) retVal = true; 
	break; 
      case 1:
      case 16:
	if( (iy>=4) && (iy<=13) ) retVal = true; 
	break; 
      case 2:
      case 15:
	if( (iy>=3) && (iy<=14) ) retVal = true; 
	break; 
      case 3:
      case 14:
	if( (iy>=2) && (iy<=15) ) retVal = true; 
	break; 
      case 4:
      case 5:
      case 12:
      case 13:
	if( (iy>=1) && (iy<=16) ) retVal = true; 
	break; 
      case 6:
      case 11:
	if( ((iy>=0) && (iy<=6)) || ((iy>=11) && (iy<=17)) ) retVal = true; 
	break; 
      case 7:
      case 8:
      case 9:
      case 10:
	if( ((iy>=0) && (iy<=5)) || ((iy>=12) && (iy<=17)) ) retVal = true; 
	break; 

      default: 
	retVal = false; 
      
      }

    }

    return retVal; 

  }

  inline double GetMPCTowerCutoff(int arm, int ix, int iy){

    /*

    double cutoff = 4.0;//default value
    
    float ixTower[31] = {5,5,6,7,7,4,5,6,7,7,4,5,6,7,7,14,14,13,13,12,12,14,14,13,13,12,12,15,15,15,15};
    float iyTower[31] = {3,14,3,3,14,4,4,4,4,13,5,5,5,5,12,10,7,7,10,10,7,9,8,9,8,9,8,10,7,9,8};
    float cutoffTower[31] = {8,8,8,7,7,8,9,8,8,8,8,9,8,8,8,9,9,9,9,9,9,8,8,9,9,10,10,9,9,9,9};

    if(arm == 1){

      for(int i = 0; i < 31; i++){

	if((ixTower[i] == ix) &&(iyTower[i] == iy)) {
	  cutoff = cutoffTower[i];
	  break; 
	}

      }//i

    }//arm 1

    return cutoff;

    */

    return 0.0; 

  }


}

#endif /* __MPCEXCONSTANTS_H__ */
