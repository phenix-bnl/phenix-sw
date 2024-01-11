#include <MpcSectorRecV2.h>
#include <MpcCluster.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <MpcMap.h>

#include <PHPoint.h>
#include <recoConsts.h>

#include <TH2.h>
#include <TPad.h>

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <string>


using namespace std;

// // Define and initialize static members



// float MpcSectorRecV2::fShowPar[0][0]=0.213284;  
// float MpcSectorRecV2::fShowPar[0][1]=-0.115813;  
// float MpcSectorRecV2::fShowPar[0][2]=0.0283893;  
// float MpcSectorRecV2::fShowPar[0][3]=-0.00241434;  
// float MpcSectorRecV2::fShowPar[0][4]=0.0736381;  
// float MpcSectorRecV2::fShowPar[0][5]=0.00126043;  
// float MpcSectorRecV2::fShowPar[0][6]=-0.00398389;  
// float MpcSectorRecV2::fShowPar[0][7]=0.000707255;  
// float MpcSectorRecV2::fShowPar[0][8]=-4.77296e-05;  
// float MpcSectorRecV2::fShowPar[0][9]=1.13614e-06;  
// float MpcSectorRecV2::fShowPar[0][10]=0.0305248;  
// float MpcSectorRecV2::fShowPar[0][11]=-2.64698e-05;  
// float MpcSectorRecV2::fShowPar[1][0]=0.575686;  
// float MpcSectorRecV2::fShowPar[1][1]=0.0121302;  
// float MpcSectorRecV2::fShowPar[1][2]=0.580206;  
// float MpcSectorRecV2::fShowPar[1][3]=0.0137781;  
// float MpcSectorRecV2::fShowPar[1][4]=0;  
// float MpcSectorRecV2::fShowPar[1][5]=0;  
// float MpcSectorRecV2::fShowPar[1][6]=0;  
// float MpcSectorRecV2::fShowPar[1][7]=0;  
// float MpcSectorRecV2::fShowPar[1][8]=0;  
// float MpcSectorRecV2::fShowPar[1][9]=0;  
// float MpcSectorRecV2::fShowPar[1][10]=0;  
// float MpcSectorRecV2::fShowPar[1][11]=0;  
// float MpcSectorRecV2::fShowPar[2][0]=0.589431;  
// float MpcSectorRecV2::fShowPar[2][1]=0.0313041;  
// float MpcSectorRecV2::fShowPar[2][2]=8.37768;  
// float MpcSectorRecV2::fShowPar[2][3]=-3.90471;  
// float MpcSectorRecV2::fShowPar[2][4]=0.740759;  
// float MpcSectorRecV2::fShowPar[2][5]=-0.0693573;  
// float MpcSectorRecV2::fShowPar[2][6]=0.00319739;  
// float MpcSectorRecV2::fShowPar[2][7]=-5.80882e-05;  
// float MpcSectorRecV2::fShowPar[2][8]=0.217928;  
// float MpcSectorRecV2::fShowPar[2][9]=-0.00337812;  
// float MpcSectorRecV2::fShowPar[2][10]=0;  
// float MpcSectorRecV2::fShowPar[2][11]=0;  
// float MpcSectorRecV2::fShowPar[3][0]=3.95523;  
// float MpcSectorRecV2::fShowPar[3][1]=0.113908;  
// float MpcSectorRecV2::fShowPar[3][2]=28.3696;  
// float MpcSectorRecV2::fShowPar[3][3]=-11.6083;  
// float MpcSectorRecV2::fShowPar[3][4]=2.08708;  
// float MpcSectorRecV2::fShowPar[3][5]=-0.185196;  
// float MpcSectorRecV2::fShowPar[3][6]=0.00806067;  
// float MpcSectorRecV2::fShowPar[3][7]=-0.000137791;  
// float MpcSectorRecV2::fShowPar[3][8]=2.75312;  
// float MpcSectorRecV2::fShowPar[3][9]=-0.0322946;  
// float MpcSectorRecV2::fShowPar[3][10]=0;  
// float MpcSectorRecV2::fShowPar[3][11]=0;  
// float MpcSectorRecV2::fShowPar[4][0]=-5.435;  
// float MpcSectorRecV2::fShowPar[4][1]=2.77518;  
// float MpcSectorRecV2::fShowPar[4][2]=-0.516744;  
// float MpcSectorRecV2::fShowPar[4][3]=0.0478535;  
// float MpcSectorRecV2::fShowPar[4][4]=-0.0021897;  
// float MpcSectorRecV2::fShowPar[4][5]=3.95587e-05;  
// float MpcSectorRecV2::fShowPar[4][6]=0.554435;  
// float MpcSectorRecV2::fShowPar[4][7]=0.00387074;  
// float MpcSectorRecV2::fShowPar[4][8]=0;  
// float MpcSectorRecV2::fShowPar[4][9]=0;  
// float MpcSectorRecV2::fShowPar[4][10]=0;  
// float MpcSectorRecV2::fShowPar[4][11]=0;  
// float MpcSectorRecV2::fShowPar[5][0]=157.947;  
// float MpcSectorRecV2::fShowPar[5][1]=-67.7084;  
// float MpcSectorRecV2::fShowPar[5][2]=12.8579;  
// float MpcSectorRecV2::fShowPar[5][3]=-1.19931;  
// float MpcSectorRecV2::fShowPar[5][4]=0.0549216;  
// float MpcSectorRecV2::fShowPar[5][5]=-0.000990001;  
// float MpcSectorRecV2::fShowPar[5][6]=17.5042;  
// float MpcSectorRecV2::fShowPar[5][7]=-0.0520582;  
// float MpcSectorRecV2::fShowPar[5][8]=0;  
// float MpcSectorRecV2::fShowPar[5][9]=0;  
// float MpcSectorRecV2::fShowPar[5][10]=0;  
// float MpcSectorRecV2::fShowPar[5][11]=0;  
// float MpcSectorRecV2::fRMSPar[0][0]=0.246633;  
// float MpcSectorRecV2::fRMSPar[0][1]=-0.00985166;  
// float MpcSectorRecV2::fRMSPar[0][2]=0.000989706;  
// float MpcSectorRecV2::fRMSPar[0][3]=-0.000346406;  
// float MpcSectorRecV2::fRMSPar[0][4]=3.32312e-05;  
// float MpcSectorRecV2::fRMSPar[0][5]=-9.33818e-07;  
// float MpcSectorRecV2::fRMSPar[0][6]=0.132772;  
// float MpcSectorRecV2::fRMSPar[0][7]=-0.000672923;  
// float MpcSectorRecV2::fRMSPar[0][8]=0;  
// float MpcSectorRecV2::fRMSPar[0][9]=0;  
// float MpcSectorRecV2::fRMSPar[0][10]=0;  
// float MpcSectorRecV2::fRMSPar[0][11]=0;  
// float MpcSectorRecV2::fRMSPar[1][0]=0.216101;  
// float MpcSectorRecV2::fRMSPar[1][1]=-0.0622254;  
// float MpcSectorRecV2::fRMSPar[1][2]=0.00948224;  
// float MpcSectorRecV2::fRMSPar[1][3]=-0.000709574;  
// float MpcSectorRecV2::fRMSPar[1][4]=2.21932e-05;  
// float MpcSectorRecV2::fRMSPar[1][5]=-1.59413e-07;  
// float MpcSectorRecV2::fRMSPar[1][6]=0.0391432;  
// float MpcSectorRecV2::fRMSPar[1][7]=-0.000951411;  
// float MpcSectorRecV2::fRMSPar[1][8]=0;  
// float MpcSectorRecV2::fRMSPar[1][9]=0;  
// float MpcSectorRecV2::fRMSPar[1][10]=0;  
// float MpcSectorRecV2::fRMSPar[1][11]=0;  
// float MpcSectorRecV2::fRMSPar[2][0]=-0.0291626;  
// float MpcSectorRecV2::fRMSPar[2][1]=-0.251015;  
// float MpcSectorRecV2::fRMSPar[2][2]=10.1247;  
// float MpcSectorRecV2::fRMSPar[2][3]=-5.67474;  
// float MpcSectorRecV2::fRMSPar[2][4]=1.04623;  
// float MpcSectorRecV2::fRMSPar[2][5]=-0.091684;  
// float MpcSectorRecV2::fRMSPar[2][6]=0.00386578;  
// float MpcSectorRecV2::fRMSPar[2][7]=-6.31645e-05;  
// float MpcSectorRecV2::fRMSPar[2][8]=-1.34825;  
// float MpcSectorRecV2::fRMSPar[2][9]=0.00542219;  
// float MpcSectorRecV2::fRMSPar[2][10]=0;  
// float MpcSectorRecV2::fRMSPar[2][11]=0;  
// float MpcSectorRecV2::fRMSPar[3][0]=-1.28838;  
// float MpcSectorRecV2::fRMSPar[3][1]=0.33319;  
// float MpcSectorRecV2::fRMSPar[3][2]=0.000595198;  
// float MpcSectorRecV2::fRMSPar[3][3]=4;  
// float MpcSectorRecV2::fRMSPar[3][4]=0;  
// float MpcSectorRecV2::fRMSPar[3][5]=0;  
// float MpcSectorRecV2::fRMSPar[3][6]=0;  
// float MpcSectorRecV2::fRMSPar[3][7]=0;  
// float MpcSectorRecV2::fRMSPar[3][8]=0;  
// float MpcSectorRecV2::fRMSPar[3][9]=0;  
// float MpcSectorRecV2::fRMSPar[3][10]=0;  
// float MpcSectorRecV2::fRMSPar[3][11]=0;  
// float MpcSectorRecV2::fRMSPar[4][0]=-12.0199;  
// float MpcSectorRecV2::fRMSPar[4][1]=2.72188;  
// float MpcSectorRecV2::fRMSPar[4][2]=-0.0298989;  
// float MpcSectorRecV2::fRMSPar[4][3]=5.91284;  
// float MpcSectorRecV2::fRMSPar[4][4]=0;  
// float MpcSectorRecV2::fRMSPar[4][5]=0;  
// float MpcSectorRecV2::fRMSPar[4][6]=0;  
// float MpcSectorRecV2::fRMSPar[4][7]=0;  
// float MpcSectorRecV2::fRMSPar[4][8]=0;  
// float MpcSectorRecV2::fRMSPar[4][9]=0;  
// float MpcSectorRecV2::fRMSPar[4][10]=0;  
// float MpcSectorRecV2::fRMSPar[4][11]=0;  
// float MpcSectorRecV2::fRMSPar[5][0]=0.100441;  
// float MpcSectorRecV2::fRMSPar[5][1]=-0.0409774;  
// float MpcSectorRecV2::fRMSPar[5][2]=0.00823801;  
// float MpcSectorRecV2::fRMSPar[5][3]=-0.000846907;  
// float MpcSectorRecV2::fRMSPar[5][4]=4.32519e-05;  
// float MpcSectorRecV2::fRMSPar[5][5]=-8.68752e-07;  
// float MpcSectorRecV2::fRMSPar[5][6]=0.0133544;  
// float MpcSectorRecV2::fRMSPar[5][7]=-0.000143683;  
// float MpcSectorRecV2::fRMSPar[5][8]=0;  
// float MpcSectorRecV2::fRMSPar[5][9]=0;  
// float MpcSectorRecV2::fRMSPar[5][10]=0;  
// float MpcSectorRecV2::fRMSPar[5][11]=0;  
// float MpcSectorRecV2::fRMSPar[6][0]=0.641487;  
// float MpcSectorRecV2::fRMSPar[6][1]=-0.0849921;  
// float MpcSectorRecV2::fRMSPar[6][2]=0.0125019;  
// float MpcSectorRecV2::fRMSPar[6][3]=-0.000747369;  
// float MpcSectorRecV2::fRMSPar[6][4]=2.11309e-05;  
// float MpcSectorRecV2::fRMSPar[6][5]=-2.31237e-07;  
// float MpcSectorRecV2::fRMSPar[6][6]=0.538055;  
// float MpcSectorRecV2::fRMSPar[6][7]=0.00447083;  
// float MpcSectorRecV2::fRMSPar[6][8]=0;  
// float MpcSectorRecV2::fRMSPar[6][9]=0;  
// float MpcSectorRecV2::fRMSPar[6][10]=0;  
// float MpcSectorRecV2::fRMSPar[6][11]=0; 





// // Minimal shower energy when splitting peakarea onto showers, used in Gamma()
// float const MpcSectorRec::fgMinShowerEnergy=0.1;

// // Max number of clusters in sector, used in Find_Clusters()
// int const MpcSectorRec::fgMaxLen=220;

//   // Parameters for sigma in Hi2 calculations (p.36-37 v.3)
// float MpcSectorRec::fgEpar00 = 0.005;
// float MpcSectorRec::fgEpar0 = 0.0014;
// float MpcSectorRec::fgEpar1 = 0.03;
// float MpcSectorRec::fgEpar2 = -0.03;
//   // This is for PPLO mode !!!
// float MpcSectorRec::fgEpar3 = 0.;

// float MpcSectorRec::fgEpar4 = 4.0;

// // Default level, now for the Conf Level: 1% for GEANT, 2%-5% for TestBeam

// float MpcSectorRec::fgChi2Level[50]={
//     6.634899, 4.605171, 3.780564, 3.318915, 3.017103,    
//     2.801872, 2.639259, 2.511249, 2.407341, 2.320967,    
//     2.247720, 2.184744, 2.129863, 2.081515, 2.038526,    
//     1.999994, 1.965214, 1.933627, 1.904781, 1.878311,    
//     1.853912, 1.831334, 1.810365, 1.790825, 1.772564, 
//     1.755449, 1.739367, 1.724222, 1.709926, 1.696406,    
//     1.683593, 1.671430, 1.659864, 1.648850, 1.638344,    
//     1.628311, 1.618716, 1.609528, 1.600721, 1.592268,    
//     1.584148, 1.576338, 1.568822, 1.561579, 1.554596,    
//     1.547856, 1.541346, 1.535055, 1.528968, 1.523077 };   

// // For the Conf Level: 1% for GEANT, 2%-5% for TestBeam
// float MpcSectorRec::fgChi2Level1[50]={
//     6.634899, 4.605171, 3.780564, 3.318915, 3.017103,    
//     2.801872, 2.639259, 2.511249, 2.407341, 2.320967,    
//     2.247720, 2.184744, 2.129863, 2.081515, 2.038526,    
//     1.999994, 1.965214, 1.933627, 1.904781, 1.878311,    
//     1.853912, 1.831334, 1.810365, 1.790825, 1.772564, 
//     1.755449, 1.739367, 1.724222, 1.709926, 1.696406,    
//     1.683593, 1.671430, 1.659864, 1.648850, 1.638344,    
//     1.628311, 1.618716, 1.609528, 1.600721, 1.592268,    
//     1.584148, 1.576338, 1.568822, 1.561579, 1.554596,    
//     1.547856, 1.541346, 1.535055, 1.528968, 1.523077 };   

// // For the Conf Level: 2% for GEANT, 4%-7% for TestBeam
// float MpcSectorRec::fgChi2Level2[50]={
//     5.411895, 3.912024, 3.278443, 2.916812, 2.677547,    
//     2.505458, 2.374582, 2.271008, 2.186567, 2.116065,    
//     2.056169, 2.004491, 1.959343, 1.919481, 1.883964,    
//     1.852072, 1.823237, 1.797008, 1.773021, 1.750981,    
//     1.730640, 1.711795, 1.694274, 1.677931, 1.662643,    
//     1.648301, 1.634814, 1.622101, 1.610093, 1.598727,    
//     1.587948, 1.577709, 1.567968, 1.558684, 1.549824,    
//     1.541357, 1.533256, 1.525494, 1.518051, 1.510903,    
//     1.504033, 1.497424, 1.491059, 1.484924, 1.479006,    
//     1.473292, 1.467771, 1.462433, 1.457267, 1.452265 };

// ///////////////////////////////////////////////////////////////////////////
// MpcSectorRecV2 member functions

MpcSectorRecV2::MpcSectorRecV2(const int iarm) : MpcSectorRec(iarm)
{
  split_clus[0] = reco_consts->get_IntFlag("MPC_SPLITS",0);
  split_clus[1] = reco_consts->get_IntFlag("MPC_SPLITN",0);

  
fShowPar[0][0]=0.213284;  
fShowPar[0][1]=-0.115813;  
fShowPar[0][2]=0.0283893;  
fShowPar[0][3]=-0.00241434;  
fShowPar[0][4]=0.0736381;  
fShowPar[0][5]=0.00126043;  
fShowPar[0][6]=-0.00398389;  
fShowPar[0][7]=0.000707255;  
fShowPar[0][8]=-4.77296e-05;  
fShowPar[0][9]=1.13614e-06;  
fShowPar[0][10]=0.0305248;  
fShowPar[0][11]=-2.64698e-05;  
fShowPar[1][0]=0.575686;  
fShowPar[1][1]=0.0121302;  
fShowPar[1][2]=0.580206;  
fShowPar[1][3]=0.0137781;  
fShowPar[1][4]=0;  
fShowPar[1][5]=0;  
fShowPar[1][6]=0;  
fShowPar[1][7]=0;  
fShowPar[1][8]=0;  
fShowPar[1][9]=0;  
fShowPar[1][10]=0;  
fShowPar[1][11]=0;  
fShowPar[2][0]=0.589431;  
fShowPar[2][1]=0.0313041;  
fShowPar[2][2]=8.37768;  
fShowPar[2][3]=-3.90471;  
fShowPar[2][4]=0.740759;  
fShowPar[2][5]=-0.0693573;  
fShowPar[2][6]=0.00319739;  
fShowPar[2][7]=-5.80882e-05;  
fShowPar[2][8]=0.217928;  
fShowPar[2][9]=-0.00337812;  
fShowPar[2][10]=0;  
fShowPar[2][11]=0;  
fShowPar[3][0]=3.95523;  
fShowPar[3][1]=0.113908;  
fShowPar[3][2]=28.3696;  
fShowPar[3][3]=-11.6083;  
fShowPar[3][4]=2.08708;  
fShowPar[3][5]=-0.185196;  
fShowPar[3][6]=0.00806067;  
fShowPar[3][7]=-0.000137791;  
fShowPar[3][8]=2.75312;  
fShowPar[3][9]=-0.0322946;  
fShowPar[3][10]=0;  
fShowPar[3][11]=0;  
fShowPar[4][0]=-5.435;  
fShowPar[4][1]=2.77518;  
fShowPar[4][2]=-0.516744;  
fShowPar[4][3]=0.0478535;  
fShowPar[4][4]=-0.0021897;  
fShowPar[4][5]=3.95587e-05;  
fShowPar[4][6]=0.554435;  
fShowPar[4][7]=0.00387074;  
fShowPar[4][8]=0;  
fShowPar[4][9]=0;  
fShowPar[4][10]=0;  
fShowPar[4][11]=0;  
fShowPar[5][0]=157.947;  
fShowPar[5][1]=-67.7084;  
fShowPar[5][2]=12.8579;  
fShowPar[5][3]=-1.19931;  
fShowPar[5][4]=0.0549216;  
fShowPar[5][5]=-0.000990001;  
fShowPar[5][6]=17.5042;  
fShowPar[5][7]=-0.0520582;  
fShowPar[5][8]=0;  
fShowPar[5][9]=0;  
fShowPar[5][10]=0;  
fShowPar[5][11]=0;  
fRMSPar[0][0]=0.246633;  
fRMSPar[0][1]=-0.00985166;  
fRMSPar[0][2]=0.000989706;  
fRMSPar[0][3]=-0.000346406;  
fRMSPar[0][4]=3.32312e-05;  
fRMSPar[0][5]=-9.33818e-07;  
fRMSPar[0][6]=0.132772;  
fRMSPar[0][7]=-0.000672923;  
fRMSPar[0][8]=0;  
fRMSPar[0][9]=0;  
fRMSPar[0][10]=0;  
fRMSPar[0][11]=0;  
fRMSPar[1][0]=0.216101;  
fRMSPar[1][1]=-0.0622254;  
fRMSPar[1][2]=0.00948224;  
fRMSPar[1][3]=-0.000709574;  
fRMSPar[1][4]=2.21932e-05;  
fRMSPar[1][5]=-1.59413e-07;  
fRMSPar[1][6]=0.0391432;  
fRMSPar[1][7]=-0.000951411;  
fRMSPar[1][8]=0;  
fRMSPar[1][9]=0;  
fRMSPar[1][10]=0;  
fRMSPar[1][11]=0;  
fRMSPar[2][0]=-0.0291626;  
fRMSPar[2][1]=-0.251015;  
fRMSPar[2][2]=10.1247;  
fRMSPar[2][3]=-5.67474;  
fRMSPar[2][4]=1.04623;  
fRMSPar[2][5]=-0.091684;  
fRMSPar[2][6]=0.00386578;  
fRMSPar[2][7]=-6.31645e-05;  
fRMSPar[2][8]=-1.34825;  
fRMSPar[2][9]=0.00542219;  
fRMSPar[2][10]=0;  
fRMSPar[2][11]=0;  
fRMSPar[3][0]=-1.28838;  
fRMSPar[3][1]=0.33319;  
fRMSPar[3][2]=0.000595198;  
fRMSPar[3][3]=4;  
fRMSPar[3][4]=0;  
fRMSPar[3][5]=0;  
fRMSPar[3][6]=0;  
fRMSPar[3][7]=0;  
fRMSPar[3][8]=0;  
fRMSPar[3][9]=0;  
fRMSPar[3][10]=0;  
fRMSPar[3][11]=0;  
fRMSPar[4][0]=-12.0199;  
fRMSPar[4][1]=2.72188;  
fRMSPar[4][2]=-0.0298989;  
fRMSPar[4][3]=5.91284;  
fRMSPar[4][4]=0;  
fRMSPar[4][5]=0;  
fRMSPar[4][6]=0;  
fRMSPar[4][7]=0;  
fRMSPar[4][8]=0;  
fRMSPar[4][9]=0;  
fRMSPar[4][10]=0;  
fRMSPar[4][11]=0;  
fRMSPar[5][0]=0.100441;  
fRMSPar[5][1]=-0.0409774;  
fRMSPar[5][2]=0.00823801;  
fRMSPar[5][3]=-0.000846907;  
fRMSPar[5][4]=4.32519e-05;  
fRMSPar[5][5]=-8.68752e-07;  
fRMSPar[5][6]=0.0133544;  
fRMSPar[5][7]=-0.000143683;  
fRMSPar[5][8]=0;  
fRMSPar[5][9]=0;  
fRMSPar[5][10]=0;  
fRMSPar[5][11]=0;  
fRMSPar[6][0]=0.641487;  
fRMSPar[6][1]=-0.0849921;  
fRMSPar[6][2]=0.0125019;  
fRMSPar[6][3]=-0.000747369;  
fRMSPar[6][4]=2.11309e-05;  
fRMSPar[6][5]=-2.31237e-07;  
fRMSPar[6][6]=0.538055;  
fRMSPar[6][7]=0.00447083;  
fRMSPar[6][8]=0;  
fRMSPar[6][9]=0;  
fRMSPar[6][10]=0;  
fRMSPar[6][11]=0; 

// fShowPar2[0][0]=0.479859;  
// fShowPar2[0][1]=-0.0920152;  
// fShowPar2[0][2]=0.358612;  
// fShowPar2[0][3]=-0.1168;  
// fShowPar2[0][4]=0.0189296;  
// fShowPar2[0][5]=-0.00158467;  
// fShowPar2[0][6]=6.79042e-05;  
// fShowPar2[0][7]=-1.17286e-06;  
// fShowPar2[0][8]=0.0561908;  
// fShowPar2[0][9]=0.000627101;  
// fShowPar2[0][10]=0;  
// fShowPar2[0][11]=0;  
// fShowPar2[1][0]=2.36952;  
// fShowPar2[1][1]=-0.231937;  
// fShowPar2[1][2]=6.58528;  
// fShowPar2[1][3]=-2.44095;  
// fShowPar2[1][4]=0.439007;  
// fShowPar2[1][5]=-0.0387652;  
// fShowPar2[1][6]=0.00168637;  
// fShowPar2[1][7]=-2.88692e-05;  
// fShowPar2[1][8]=1.25146;  
// fShowPar2[1][9]=0.00873236;  
// fShowPar2[1][10]=0;  
// fShowPar2[1][11]=0;  
// fShowPar2[2][0]=0.289025;  
// fShowPar2[2][1]=0.0883726;  
// fShowPar2[2][2]=1.83895;  
// fShowPar2[2][3]=-0.638557;  
// fShowPar2[2][4]=0.0984676;  
// fShowPar2[2][5]=-0.007603;  
// fShowPar2[2][6]=0.000290524;  
// fShowPar2[2][7]=-4.37979e-06;  
// fShowPar2[2][8]=0.170229;  
// fShowPar2[2][9]=-0.00216952;  
// fShowPar2[2][10]=0;  
// fShowPar2[2][11]=0;  
// fShowPar2[3][0]=3.33402;  
// fShowPar2[3][1]=0.209536;  
// fShowPar2[3][2]=6.22094;  
// fShowPar2[3][3]=-0.955564;  
// fShowPar2[3][4]=0.0738855;  
// fShowPar2[3][5]=-0.00088069;  
// fShowPar2[3][6]=-0.000135048;  
// fShowPar2[3][7]=4.49456e-06;  
// fShowPar2[3][8]=2.36585;  
// fShowPar2[3][9]=-0.0157217;  
// fShowPar2[3][10]=0;  
// fShowPar2[3][11]=0;  
// fShowPar2[4][0]=-0.428616;  
// fShowPar2[4][1]=0.267414;  
// fShowPar2[4][2]=-0.0253594;  
// fShowPar2[4][3]=0.000856145;  
// fShowPar2[4][4]=7.43369e-06;  
// fShowPar2[4][5]=-7.10447e-07;  
// fShowPar2[4][6]=0.567386;  
// fShowPar2[4][7]=0.0024651;  
// fShowPar2[4][8]=0;  
// fShowPar2[4][9]=0;  
// fShowPar2[4][10]=0;  
// fShowPar2[4][11]=0;  
// fShowPar2[5][0]=40.5297;  
// fShowPar2[5][1]=-10.9934;  
// fShowPar2[5][2]=1.92447;  
// fShowPar2[5][3]=-0.168767;  
// fShowPar2[5][4]=0.00730085;  
// fShowPar2[5][5]=-0.000124215;  
// fShowPar2[5][6]=14.7445;  
// fShowPar2[5][7]=-0.0332239;  
// fShowPar2[5][8]=0;  
// fShowPar2[5][9]=0;  
// fShowPar2[5][10]=0;  
// fShowPar2[5][11]=0;  

fShowPar2[0][0]=0.109076;  
fShowPar2[0][1]=-0.0020524;  
fShowPar2[0][2]=4.83446e-05;  
fShowPar2[0][3]=-3.58957e-07;  
fShowPar2[0][4]=0;  
fShowPar2[0][5]=0;  
fShowPar2[0][6]=0;  
fShowPar2[0][7]=0;  
fShowPar2[0][8]=0;  
fShowPar2[0][9]=0;  
fShowPar2[0][10]=0;  
fShowPar2[0][11]=0;  
fShowPar2[1][0]=1.6985;  
fShowPar2[1][1]=-0.00639118;  
fShowPar2[1][2]=0.000153674;  
fShowPar2[1][3]=-1.17523e-06;  
fShowPar2[1][4]=0;  
fShowPar2[1][5]=0;  
fShowPar2[1][6]=0;  
fShowPar2[1][7]=0;  
fShowPar2[1][8]=0;  
fShowPar2[1][9]=0;  
fShowPar2[1][10]=0;  
fShowPar2[1][11]=0;  
fShowPar2[2][0]=0.190027;  
fShowPar2[2][1]=-0.00752877;  
fShowPar2[2][2]=0.000176111;  
fShowPar2[2][3]=-1.28594e-06;  
fShowPar2[2][4]=0;  
fShowPar2[2][5]=0;  
fShowPar2[2][6]=0;  
fShowPar2[2][7]=0;  
fShowPar2[2][8]=0;  
fShowPar2[2][9]=0;  
fShowPar2[2][10]=0;  
fShowPar2[2][11]=0;  
fShowPar2[3][0]=2.42983;  
fShowPar2[3][1]=-0.0417858;  
fShowPar2[3][2]=0.000893255;  
fShowPar2[3][3]=-6.32293e-06;  
fShowPar2[3][4]=0;  
fShowPar2[3][5]=0;  
fShowPar2[3][6]=0;  
fShowPar2[3][7]=0;  
fShowPar2[3][8]=0;  
fShowPar2[3][9]=0;  
fShowPar2[3][10]=0;  
fShowPar2[3][11]=0;  
fShowPar2[4][0]=0.516242;  
fShowPar2[4][1]=0.010746;  
fShowPar2[4][2]=-0.000251936;  
fShowPar2[4][3]=1.84155e-06;  
fShowPar2[4][4]=0;  
fShowPar2[4][5]=0;  
fShowPar2[4][6]=0;  
fShowPar2[4][7]=0;  
fShowPar2[4][8]=0;  
fShowPar2[4][9]=0;  
fShowPar2[4][10]=0;  
fShowPar2[4][11]=0;  
fShowPar2[5][0]=15.0943;  
fShowPar2[5][1]=-0.0981804;  
fShowPar2[5][2]=0.00261719;  
fShowPar2[5][3]=-1.90878e-05;  
fShowPar2[5][4]=0;  
fShowPar2[5][5]=0;  
fShowPar2[5][6]=0;  
fShowPar2[5][7]=0;  
fShowPar2[5][8]=0;  
fShowPar2[5][9]=0;  
fShowPar2[5][10]=0;  
fShowPar2[5][11]=0;  


// fShowPar2[0][0]=0.237327;  
// fShowPar2[0][1]=-0.0391738;  
// fShowPar2[0][2]=0.151717;  
// fShowPar2[0][3]=-0.028838;  
// fShowPar2[0][4]=0.00152317;  
// fShowPar2[0][5]=0.000174666;  
// fShowPar2[0][6]=-2.13266e-05;  
// fShowPar2[0][7]=6.09993e-07;  
// fShowPar2[0][8]=0.0334828;  
// fShowPar2[0][9]=1.45783e-05;  
// fShowPar2[0][10]=0;  
// fShowPar2[0][11]=0;  
// fShowPar2[1][0]=1.88738;  
// fShowPar2[1][1]=-0.168394;  
// fShowPar2[1][2]=5.09688;  
// fShowPar2[1][3]=-1.79623;  
// fShowPar2[1][4]=0.301995;  
// fShowPar2[1][5]=-0.02442;  
// fShowPar2[1][6]=0.000947292;  
// fShowPar2[1][7]=-1.40306e-05;  
// fShowPar2[1][8]=0.900342;  
// fShowPar2[1][9]=0.00529645;  
// fShowPar2[1][10]=0;  
// fShowPar2[1][11]=0;  
// fShowPar2[2][0]=0.497704;  
// fShowPar2[2][1]=0.0488179;  
// fShowPar2[2][2]=11.1603;  
// fShowPar2[2][3]=-5.49755;  
// fShowPar2[2][4]=1.08773;  
// fShowPar2[2][5]=-0.105889;  
// fShowPar2[2][6]=0.00506454;  
// fShowPar2[2][7]=-9.52494e-05;  
// fShowPar2[2][8]=0.172732;  
// fShowPar2[2][9]=-0.00188611;  
// fShowPar2[2][10]=0;  
// fShowPar2[2][11]=0;  
// fShowPar2[3][0]=3.78975;  
// fShowPar2[3][1]=0.153299;  
// fShowPar2[3][2]=42.086;  
// fShowPar2[3][3]=-19.3466;  
// fShowPar2[3][4]=3.74833;  
// fShowPar2[3][5]=-0.358407;  
// fShowPar2[3][6]=0.0168595;  
// fShowPar2[3][7]=-0.000312217;  
// fShowPar2[3][8]=2.33153;  
// fShowPar2[3][9]=-0.0174806;  
// fShowPar2[3][10]=0;  
// fShowPar2[3][11]=0;  
// fShowPar2[4][0]=-7.73067;  
// fShowPar2[4][1]=4.09208;  
// fShowPar2[4][2]=-0.804509;  
// fShowPar2[4][3]=0.0782523;  
// fShowPar2[4][4]=-0.00374862;  
// fShowPar2[4][5]=7.06855e-05;  
// fShowPar2[4][6]=0.58884;  
// fShowPar2[4][7]=0.0026765;  
// fShowPar2[4][8]=0;  
// fShowPar2[4][9]=0;  
// fShowPar2[4][10]=0;  
// fShowPar2[4][11]=0;  
// fShowPar2[5][0]=261.972;  
// fShowPar2[5][1]=-124.819;  
// fShowPar2[5][2]=24.9781;  
// fShowPar2[5][3]=-2.4494;  
// fShowPar2[5][4]=0.117721;  
// fShowPar2[5][5]=-0.00222159;  
// fShowPar2[5][6]=16.2872;  
// fShowPar2[5][7]=-0.00862412;  
// fShowPar2[5][8]=0;  
// fShowPar2[5][9]=0;  
// fShowPar2[5][10]=0;  
// fShowPar2[5][11]=0;  



}

// ///////////////////////////////////////////////////////////////////////////

MpcSectorRecV2::~MpcSectorRecV2()
{
  //  delete [] fHVect;
  // delete fModules;
  //delete fClusters;
}

// ///////////////////////////////////////////////////////////////////////////

mpcClusterContent *MpcSectorRecV2::FillPeakArea(MpcPeakarea& pp, MpcCluster& cluster)
{
//cout << "In MpcSectorRecV2::FillPeakArea()" << endl;
  MpcModule hmax = pp.GetMaxTower();
  int ndead = pp.GetNDead();
  float qual = ndead ? 1.0 : -ndead;

//   float rmax = (hmax.amp > 0) ? 
//     hmax.amp / cluster.GetTowerEnergy(hmax.ich) : 0;

//  EmcModule himp = pp.GetImpactTower();

//   float rimp = (himp.amp > 0) ? 
//     himp.amp / cluster.GetTowerEnergy(himp.ich) : 0;

  float e,ecorr,ecore,ecorecorr,e9;
  float xcg,ycg,xcgm,ycgm, xlcg,ylcg;
  float xgl,ygl,zgl, xm,ym,xl,yl;
  float xx,xy,yy;
  float chi2,chi2core;
  int ndfcore;
  
  float de,dx,dy,dz;
  float logxx,logyy,logxy;
  
  bool split = 0;
  float chi2_split = 9999;
  float se1,sx1,sy1,se2,sx2,sy2;
  se1=sx1=sy1=se2=sx2=sy2=0;

  //4 sets of coordinates
  //xgl,ygl log weighted positions w/ angle corrections
  //xcg, ycg log weighted positions
  // xlcg, ylcg linear weighted positions
  // xl, yl linear weighted positions w/ angle corrections
  //xcgm,ycgm positions determined by shower shape
  //xm,ym positions determined by shower shape w/ angle corrections
  

  pp.GetCharV2(&e, &ecorr, &ecore, &ecorecorr, &e9,
	       &xcg, &ycg, &xcgm, &ycgm,&xlcg,&ylcg,
	       &xgl, &ygl, &zgl, &xm, &ym,&xl,&yl,
	       &xx, &xy, &yy, &chi2,&chi2core,&ndfcore, &de, &dx, &dy, &dz,&logxx,&logxy,&logyy,
	       split,chi2_split,se1,sx1,sy1,se2,sx2,sy2);
  
  
 
  
  //float e9 = pp.GetE9(hmax.ich);

  int nh = pp.GetNofHits();
  if ( nh>HVECTSIZE )
    {
      cout << PHWHERE << " Nhits " << nh
           << " greater than max number of towers = 220" << endl;
      assert(nh<=HVECTSIZE);	// more hits than number of towers!
    }

  pp.GetHits(fHVect, nh);

  float e_max_cl = cluster.GetTowerEnergy(hmax.ich);

  // Principal axis dispersion (eigenvalues).
  float padisp[2];
  float pahelp = (xx + yy) * (xx + yy) - 4.0 * (xx * yy - xy * xy);
  pahelp = sqrt(abs(pahelp));
  padisp[0] = (xx + yy + pahelp) / 2.0;
  padisp[1] = (xx + yy - pahelp) / 2.0;

  //  cout << "padisp 0, 1, xx, yy, xy: " << padisp[0] << ", " << padisp[1] << ", " << xx << ", " << yy << ", " << xy << " is it an ellipse: " << (4*xy*xy-4*xx*yy) << endl;

  if(xx < 0.00001) xx = 0.000001;
  if(yy < 0.00001) yy = 0.000001;
  if(logxx < 0.00001) logxx = 0.000001;
  if(logyy < 0.00001) logyy = 0.000001;

/*
  float vx = fVx-xgl;
  float vy = fVy-ygl;
  float vz = fVz-zgl;
*/

//  float lactual = sqrt( vx*vx + vy*vy + vz*vz );
//  float lnominal = sqrt(xgl * xgl + ygl * ygl + zgl * zgl);
//  float dd = lactual - lnominal;

//  float etof = -9999.;
  float etofmin = -9999.;
  float etofmax = -9999.;
//  float dtof = -9999.;
  float tof = -9999.;
  float tofcorr = -9999.;
  float tofmin = -9999.;
  float tofmax = -9999.;
  float tofmincorr = -9999.;
  float tofmaxcorr = -9999.;
  float tofdisp = 0;

/*
  ToF_Process(fHVect, nh,
              dd, hmax,
              &tof, &etof, &tofcorr, &dtof,
              &tofmin, &etofmin, &tofmincorr,
              &tofmax, &etofmax, &tofmaxcorr,
              tofdisp);
*/

  size_t id = fClusterOut->size();

  mpcClusterContent* clus = fClusterOut->addCluster(id);
  
  clus->set_split(split);
  if(split){
    cout << "split_ph1: " << se1 << ", " << sx1 << ", " << sy1 << endl;
    cout << "split_ph2: " << se2 << ", " << sx2 << ", " << sy2 << endl;
    
    
    clus->set_yx1(sy1,sx1);
    clus->set_yx2(sy2,sx2);
    clus->set_e12(se1,se2);
    clus->set_chi2_split(chi2_split);

    cout << "x1,y,clus: " << clus->x1() << ", " << clus->y1() << endl;
  }

  clus->set_multiplicity(nh);

  clus->set_id(id);
  clus->set_arm(fArm);
  clus->set_xyz(xgl,ygl,zgl);
  clus->set_dxyz(dx,dy,dz);
  clus->set_e(e);
  clus->set_e9(e9);

  clus->set_etofmin(etofmin);
  clus->set_ecore(ecorecorr);
  clus->set_ecent(e_max_cl);
  clus->set_chi2(chi2);
  clus->set_chi2core(chi2core);
  clus->set_ndfcore(ndfcore);
  clus->set_tof(tof);
  clus->set_tofcorr(tofcorr);
  clus->set_tofdisp(tofdisp);
  //                  clus->set_dtof(nPeakArea, dtof);
  clus->set_quality(qual);
  clus->set_pid(0);
  clus->set_prob_photon(pp.GetCL());
  //                  clus->set_prob_neuhad(nPeakArea, 0);
  float phi = ( xgl == 0.0 && ygl == 0.0 ? 0.0 : atan2(ygl,xgl) );
  float theta = ( xgl == 0.0 && ygl == 0.0 && zgl == 0.0 ? 0.0 :
                  atan2(sqrt(xgl*xgl+ygl*ygl),zgl) );
  clus->set_theta(theta);
  clus->set_phi(phi);
  int iy = hmax.ich / fNx;
  int iz = hmax.ich - iy * fNx;
  clus->set_ipos(iz,iy);	// cent arm (iz,iy)->(ix,iy) mpc
  clus->set_tofmin(tofmin);

  clus->set_tofcorrmin(tofmincorr);
  clus->set_tofmax(tofmax);
  clus->set_etofmax(etofmax);
  clus->set_tofcorrmax(tofmaxcorr);
  //                  clus->set_tofmean(0);
  float dispy = yy;
  float dispx = xx;
  clus->set_disp(dispy,dispx);
  float logdispy = logyy;
  float logdispx = logxx;
  clus->set_logdisp(logdispy,logdispx);
  
  clus->set_padisp(padisp[1],padisp[0]);

  if ( clus->has_yz_cg() ) //note this function always returns true
    {
      clus->set_yx_logcg(ycg,xcg);
      clus->set_yx_cgmin(ycgm,xcgm);
      clus->set_yx_min(ym,xm);
      clus->set_yx_lincg(ylcg,xlcg);
      clus->set_yx_lin(yl,xl);

      float corrdispy=-9999;
      float corrdispx=-9999;
      float corrlogdispy=-9999;
      float corrlogdispx=-9999;

//      float zModSize = fSectorGeometries[is]->Tower_xSize;
//      float yModSize = fSectorGeometries[is]->Tower_ySize;

      const float xModSize = 2.26;
      const float yModSize = 2.26;
      
      computeCorrectedDispersionV2(ylcg,xlcg, dispy,dispx, yModSize,xModSize, corrdispy,corrdispx);
      computeCorrectedDispersionV2(ycg,xcg, logdispy,logdispx, yModSize,xModSize, corrlogdispy,corrlogdispx);

      clus->set_corrdisp(corrdispy,corrdispx);
      clus->set_corrlogdisp(corrlogdispy,corrlogdispx);
    }

  for (int ih = 0; ih < nh; ++ih)
    {
      float esum = 0;
      if (fHVect[ih].amp <= 0)
        {
          clus->set_towerid(ih,-1);
          clus->set_partesum(ih,0);
	  
        }
      else
        {
	  int ach = fHVect[ih].ich;
	  int iy = ach / fNx;
	  int ix = ach - iy * fNx;
          
	  int towerid = mpcmap->getFeeCh(ix,iy,fArm);
          clus->set_towerid(ih, towerid);
          esum += fHVect[ih].amp;
          clus->set_partesum(ih, esum);
        }
    }




/*
  float esum = 0;
  for (int ih = 0; ih < nh; ++ih)
    {
      if (fHVect[ih].amp <= 0)
        {
          clus->set_towerid(ih,-1);
          clus->set_partesum(ih,0);

        }
      else
        {
          int ich = fHVect[ih].ich;
          int iy = ich / Nx[is];
          int iz = ich - iy * Nx[is];
          int swkey = iz + iy * 100 + 10000 * sector
            + 100000 * arm;
          int towerid = EmcIndexer::TowerID(swkey);
          clus->set_towerid(ih, towerid);
          esum += fHVect[ih].amp;
          clus->set_partesum(ih, esum);
        }
    }
*/

  clus->set_maps(hmax.deadmap,hmax.warnmap);
  clus->set_rawtdc(hmax.tac);

  //clus->print();

  return clus;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRecV2::Gamma(int nh, MpcModule* phit, float* pchi, float* pchi0,
			float* pe1, float* px1, float* py1, float* pe2,
			float* px2, float* py2, int &ndf)
{
  // Tests for 1 or 2 photon hypothesis by minimizing the chi2.
  // If the energy of one of two showers is less then fgMinShowerEnergy 
  // they are merged
  //
  // Returns two shower parameters (pe1,px1,py1,pe2,px2,py2). 
  // If one shower hypothesis accepted -> *pe2=0
  // *pchi  - chi2 after splitting
  // *pchi0 - chi2 before splitting
  // ndf contains number of degrees of freedom (not equal to number of cluster
  // modules for PbGl).

  float e1, x1, y1, e2, x2, y2;
  float chi, chi0, chi00, chisq0, chisave;
  float chir, chil, chiu, chid;
  int dof;
  float x0, y0, d2, xm2;
  float stepx, stepy, parx, pary;
  const float dxy=0.06;
  const float stepmin=0.01;
  const float zTG=100;
  const float xmcut=0.0015; // (GeV), for overlapping showers separation

  *pe1=0;
  *px1=0;
  *py1=0;
  *pe2=0;
  *px2=0;
  *py2=0;
  if( nh <= 0 ) return;
  
  //  cout << "phit amp0 is: " << phit[0].amp << "\n";
  Mom1V2(nh,phit,&e1,&x1,&y1);
  *pe1=e1;     
  if( e1 <= 0 ) return;
  
  SetProfileParametersV2(0, e1,x1,y1);

  chisave = *pchi;
  chi = *pchi;
  // ClusterChisq parameter list changed MV 28.01.00
  chi0 = ClusterChisqV2(nh, phit, e1, x1, y1, ndf);

  chisq0 = chi0;
  dof = ndf; // nh->ndf MV 28.01.00
  
  // ndf=0 means the cluster's chi2 cannot be found; in this case chi0=0.
  if( dof < 1 ) dof=1;
  chi = chisq0/dof;
  x0 = x1;
  y0 = y1;
  
  int n_iter = 0;
  for(;;){
    n_iter++;
    if(n_iter%100 == 0) cout << "ngamma iterations: " << n_iter << endl;

    chir = ClusterChisqV2(nh, phit, e1, x0+dxy, y0, ndf);
    chil = ClusterChisqV2(nh, phit, e1, x0-dxy, y0, ndf);
    chiu = ClusterChisqV2(nh, phit, e1, x0, y0+dxy, ndf);
    chid = ClusterChisqV2(nh, phit, e1, x0, y0-dxy, ndf);
    
    if( (chi0 > chir) || (chi0 > chil) ) {
      stepx = dxy;
      if( chir > chil ) stepx = -stepx;
    }
    else {
      stepx = 0;
      parx = chir+chil-2*chi0;
      if( parx > 0 ) stepx = -dxy*(chir-chil)/2/parx;
    }
    
    if( (chi0 > chiu) || (chi0 > chid) ) {
      stepy = dxy;
      if( chiu > chid ) stepy = -stepy;
    }
    else {
      stepy = 0;
      pary = chiu+chid-2*chi0;
      if( pary > 0 ) stepy = -dxy*(chiu-chid)/2/pary;
    }
    if( (MpcCluster::ABS(stepx) < stepmin) && (MpcCluster::ABS(stepy) < stepmin) ) break;
    chi00 = ClusterChisqV2(nh, phit, e1, x0+stepx, y0+stepy, ndf);
    
    if( chi00 >= chi0 ) break;
    chi0 = chi00;
    x0 += stepx;
    y0 += stepy;
  }
  if( chi0 < chisq0 ) {
    x1 = x0;
    y1 = y0;
    chi = chi0/dof;
  }
  
  *pchi0 = chi;
  *pchi = chi;
  *px1 = x1;
  *py1 = y1;
  
  if( e1 <= fgMinShowerEnergy ) return;
  
  if( chi > chisave ) {
    TwoGamma(nh,phit,&chi,&e1,&x1,&y1,&e2,&x2,&y2);
    if( e2 > 0 ) {
      d2 = ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))/zTG/zTG;
      xm2 = e1*e2*d2;
      if( xm2 > 0 ) xm2 = sqrt(xm2);
      if( xm2 > xmcut && e1 > fgMinShowerEnergy && e2 > fgMinShowerEnergy) {
	*pe1 = e1;
	*px1 = x1;
	*py1 = y1;
	*pe2 = e2;
	*px2 = x2;
	*py2 = y2;
	*pchi = chi;
      }
    }	
  }
}


// ///////////////////////////////////////////////////////////////////////////


void MpcSectorRecV2::Gammacore(int nh, MpcModule* phit, float* pchi, float* pchi0,
			       float* pe1, float* px1, float* py1, float* pe2,
			       float* px2, float* py2, int &ndf,
			       bool &split, float &chi2_split, float &se1, float &sx1, float &sy1,
			       float &se2, float &sx2, float &sy2)
{ 
  // Tests for 1 or 2 photon hypothesis by minimizing the chi2.
  // If the energy of one of two showers is less then fgMinShowerEnergy 
  // they are merged
  //
  // Returns two shower parameters (pe1,px1,py1,pe2,px2,py2). 
  // If one shower hypothesis accepted -> *pe2=0
  // *pchi  - chi2 after splitting
  // *pchi0 - chi2 before splitting
  // ndf contains number of degrees of freedom (not equal to number of cluster
  // modules for PbGl).
  split = 0;
  
  float e1, x1, y1;//, e2, x2, y2;
  float chi, chi0, chi00, chisq0, chisave;
  float chir, chil, chiu, chid;
  int ndfr, ndfl,ndfu,ndfd;
  int dof;
  float x0, y0;//, d2, xm2;
  float stepx, stepy, parx, pary;
  const float dxy=0.06;
  const float stepmin=0.01;
  //  const float zTG=100;
  //const float xmcut=0.0015; // (GeV), for overlapping showers separation

  *pe1=0;
  *px1=0;
  *py1=0;
  *pe2=0;
  *px2=0;
  *py2=0;
  if( nh <= 0 ) return;
  
  //  cout << "phit amp0 is: " << phit[0].amp << "\n";
  Mom1V2(nh,phit,&e1,&x1,&y1);
  *pe1=e1;     
  if( e1 <= 0 ) return;
  
  SetProfileParametersV2(0, e1,x1,y1);

  chisave = *pchi;
  chi = *pchi;
  // ClusterChisq parameter list changed MV 28.01.00
  chi0 = ClusterChisqcoreV2(nh, phit, e1, x1, y1, ndf);

  chisq0 = chi0;
  dof = ndf; // nh->ndf MV 28.01.00
  
  // ndf=0 means the cluster's chi2 cannot be found; in this case chi0=0.
  if( dof < 1 ) dof=1;
  chi = chisq0/dof;
  chi0/=dof;
  chisq0 = chi;
  x0 = x1;
  y0 = y1;

  int n_itr =0;
  for(;;){
    if(n_itr > 100) break;
    n_itr++;
    chir = ClusterChisqcoreV2(nh, phit, e1, x0+dxy, y0, ndfr);
    if(ndfr) chir/=ndfr;
    chil = ClusterChisqcoreV2(nh, phit, e1, x0-dxy, y0, ndfl);
    if(ndfl)chil/=ndfl;
    chiu = ClusterChisqcoreV2(nh, phit, e1, x0, y0+dxy, ndfu);
    if(ndfu) chiu/=ndfu;
    chid = ClusterChisqcoreV2(nh, phit, e1, x0, y0-dxy, ndfd);
    if(ndfd)chid/=ndfd;
    
    if( (chi0 > chir) || (chi0 > chil) ) {
      stepx = dxy;
      if( chir > chil ) stepx = -stepx;
    }
    else {
      stepx = 0;
      parx = chir+chil-2*chi0;
      if( parx > 0 ) stepx = -dxy*(chir-chil)/2/parx;
    }
    
    if( (chi0 > chiu) || (chi0 > chid) ) {
      stepy = dxy;
      if( chiu > chid ) stepy = -stepy;
    }
    else {
      stepy = 0;
      pary = chiu+chid-2*chi0;
      if( pary > 0 ) stepy = -dxy*(chiu-chid)/2/pary;
    }
    if( (MpcCluster::ABS(stepx) < stepmin) && (MpcCluster::ABS(stepy) < stepmin) ) break;
    chi00 = ClusterChisqcoreV2(nh, phit, e1, x0+stepx, y0+stepy, ndf);
    if(ndf) chi00/=ndf;
    
    if( chi00 >= chi0 ) break;
    chi0 = chi00;
    x0 += stepx;
    y0 += stepy;
  }
  //  if(n_itr > 100) cout << "did not find minimization after 100 evts" << n_itr << endl;
  if( chi0 < chisq0 ) {
    x1 = x0;
    y1 = y0;
    //    chi = chi0/dof;
    chi = chi0;
  }
  
  *pchi0 = chi;
  *pchi = chi;
  *px1 = x1;
  *py1 = y1;
  
  if( e1 <= fgMinShowerEnergy ) return;
  
 //  if( chi > chisave ) {
//     TwoGamma(nh,phit,&chi,&e1,&x1,&y1,&e2,&x2,&y2);
//     if( e2 > 0 ) {
//       d2 = ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))/zTG/zTG;
//       xm2 = e1*e2*d2;
//       if( xm2 > 0 ) xm2 = sqrt(xm2);
//       if( xm2 > xmcut && e1 > fgMinShowerEnergy && e2 > fgMinShowerEnergy) {
// 	*pe1 = e1;
// 	*px1 = x1;
// 	*py1 = y1;
// 	*pe2 = e2;
// 	*px2 = x2;
// 	*py2 = y2;
// 	*pchi = chi;
//       }
//     }	
//   }

  if( chi > chisave && e1 > 17 && split_clus[fArm]) { //only try splitting for high energy clusters
    chi2_split = 9999;
    split = 1;
    //    float se1,sx1,sy1,se2,sx2,sy2;
    TwoGammaV2(nh,phit,&chi2_split,&se1,&sx1,&sy1,&se2,&sx2,&sy2);
    //    *pe1 = e1;
    //*px1 = x1;
    //*py1 = y1;
    //*pe2 = e2;
    //*px2 = x2;
    //*py2 = y2;
    //*pchi = chi;
    //    cout << "Original energy, position e, x, y: " << e1 << ", " << x1 << ", " << y1 << endl;
    //cout << "Splitting yielded: e,x,y 1: " << se1 << ", " << sx1 << ", " << sy1 << "2: " << se2 << ", " << sx2 << ", " << sy2 << endl;
    //cout << "chi, chi2: " << chi << ", " << chi2gamma << endl;
  }
  
  
}


// ///////////////////////////////////////////////////////////////////////////


void MpcSectorRecV2::Mom1V2(int nh, MpcModule* phit, float* pe, float* px,
			  float* py)
{
  float e, x, y,xx,yy,xy;
  if( moment_type > 2 || moment_type < 1)
    Mom1(nh,phit,&e,&x,&y);
  else if( moment_type == 1)
    MomentsLinear(nh,phit,&e,&x,&y,&xx,&yy,&xy);
  else if( moment_type == 2)
    MomentsLog(nh,phit,&e,&x,&y,&xx,&yy,&xy,4.0);
  else
    assert(true==false && "REACHED IMPOSSIBLE STATE, variables not initialized");
  *pe = e;
  *px = x;
  *py = y;

  //  cout << "in gamma mom1 funct e,x,y: " << e << ", " << x << ", " << y << "\n";
}
  



void MpcSectorRecV2::SetChi2Limit(int limit)
{
  // Sets the limit for PeakArea splitting onto 2 EMShowers:
  // limit=0 -> For the Conf Level: 0%
  // limit=1 -> For the Conf Level: 1% for GEANT, 2%-5% for TestBeam
  // limit=2 -> For the Conf Level: 2% for GEANT, 4%-7% for TestBeam
  
  int i;
  
  switch ( limit ) {

  case 0:
    for( i=0; i<50; i++ ) fgChi2Level[i]=9999.;
    break;
  case 1:
    for( i=0; i<50; i++ ) fgChi2Level[i]=fgChi2Level1[i];
    break;
  case 2:
    for( i=0; i<50; i++ ) fgChi2Level[i]=fgChi2Level2[i];
    break;
  default:
    for( i=0; i<50; i++ ) fgChi2Level[i]=fgChi2Level1[i];
    break;

  }
}

void MpcSectorRecV2::CorrectEnergy(float Energy, float x, float y, 
				   float* Ecorr)
{
  // Corrects the EM Shower Energy for attenuation in fibers and 
  // long energy leakage
  //
  // (x,y) - impact position (cm) in Sector frame

  float sinT;
  float att, leak, corr;
  //  const float leakPar = 0.0033; // parameter from fit
  //const float attPar = 120; // Attenuation in module (cm)
  // const float X0 = 2; // radiation length (cm)

  *Ecorr = Energy;
  if( Energy < 0.01 ) return;

  GetImpactAngle(x, y, &sinT); // sinT not used so far
  //  leak = 2-sqrt(1+leakPar*log(1+Energy)*log(1+Energy));
  leak = 1;
  //  att = exp(log(Energy)*X0/attPar);
  att = 1;
  corr = leak*att;
  
  *Ecorr = Energy/corr;
}

/////////////////////////////////////////////////////////////////

void MpcSectorRecV2::CorrectECore(float Ecore, float x, float y, float* Ecorr)
{
  // Corrects the EM Shower Core Energy for attenuation in fibers, 
  // long energy leakage and angle dependance
  //
  // (x,y) - impact position (cm) in Sector frame
  float ec2;
  float ec, corr;
  float sinT;
  //variables for correction to ecore based on simulation

  float ep0, ec_final;
  const float par1 = 0.918;
  const float par2 = 1.35;
  const float par3 = 0.003;
    
  
  *Ecorr = Ecore;
  if( Ecore < 0.01 ) return;
  
  GetImpactAngle(x, y, &sinT );
  corr = par1 * ( 1 - par2*sinT*sinT*sinT*sinT*(1 - par3*log(Ecore)) );
  //  corr = corr*0.953; //from simulations

  //this is basically dividing by 0.918
  ec = Ecore/corr;
  
  //FYI this next function actually doesn't do anything
  CorrectEnergy( ec, x, y, &ec2); //this is for emcal...need different for mpc
  
  const float p0 = 0.9759;
  const float p1 = 0.03201;
  const float p2 = 0.09286;
  ep0 = ec2/0.98;
  
  float ratio = p0+p1*exp(-p2*ep0);
  if(ratio < 0.9 || ratio > 1.1){ 
    
    static int nwarns = 0;
    nwarns++;
    if(nwarns <=5){
      std::cout << ratio << " warning...ratio is out of expected bounds\n" << PHWHERE << endl;
    }
    ratio = 1.0;
  }
  ec_final = ec2/ratio;
  
  
  *Ecorr = ec_final;
}

/////////////////////////////////////////////////////////////////////

void MpcSectorRecV2::CorrectPosition(float Energy, float x, float y,
				     float* pxc, float* pyc, bool callSetPar)
{
  // Corrects the Shower Center of Gravity for the systematic error due to 
  // the limited tower size and angle shift
  //
  // Everything here is in cm. 
  // (x,y) - CG position, (*pxc,*pyc) - corrected position

  float xShift, yShift, xZero, yZero, bx, by;
  float t, x0, y0;
  int ix0, iy0;
  int signx, signy;

  if ( Energy<0. )
    {
      cout << "MpcSectorRecV2::CorrectPosition, ERROR, energy is " << Energy << endl;
    }

  SetProfileParameters( 0, Energy, x/GetModSizex(), y/GetModSizey() );
  if( fSinTx > 0 ) signx =  1;
  else 	   signx = -1;
  if( fSinTy > 0 ) signy =  1;
  else 	   signy = -1;
  t = 1.93+0.383*log(Energy);
  xShift = t*fSinTx;
  yShift = t*fSinTy;
  xZero=xShift-(0.417*MpcCluster::ABS(fSinTx)+1.500*fSinTx*fSinTx)*signx;
  yZero=yShift-(0.417*MpcCluster::ABS(fSinTy)+1.500*fSinTy*fSinTy)*signy;
  t = 0.98 + 0.98*sqrt(Energy);
  bx = 0.16 + t*fSinTx*fSinTx;
  by = 0.16 + t*fSinTy*fSinTy;
  
  x0 = x/GetModSizex();
  x0 = x0 - xShift + xZero;
  ix0 = MpcCluster::lowint(x0 + 0.5);
  if( MpcCluster::ABS(x0-ix0) <= 0.5 ) {
    x0 = (ix0-xZero)+bx*asinh( 2.*(x0-ix0)*sinh(0.5/bx) );
    *pxc = x0*GetModSizex();
  }
  else {
    *pxc =  x - xShift*GetModSizex();
    printf("????? Something wrong in CorrectPosition of EMCalClusterChi2: x=%f  dx=%f\n", x, x0-ix0);
  }

  y0 = y/GetModSizey();
  y0 = y0 - yShift + yZero;
  iy0 = MpcCluster::lowint(y0 + 0.5);

  if( MpcCluster::ABS(y0-iy0) <= 0.5 ) {

    y0 = (iy0-yZero)+by*asinh( 2.*(y0-iy0)*sinh(0.5/by) );
    *pyc = y0*GetModSizey();

  }
  else {

    *pyc = y - yShift*GetModSizey();
    printf("????? Something wrong in CorrectPosition of EMCalClusterChi2: y=%f  dy=%f\n", y, y0-iy0);
  }
  
  //cout << PHWHERE << " in CorrectPositionEnd " << x << "\t" << y << "\t" << *pxc << "\t" << *pyc << endl;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRecV2::CalculateErrors( float e, float x, float y, float* pde,
				   float* pdx, float* pdy, float* pdz)
{
  // Returns the errors for the reconstructed energy and position 
  // (in the hypothesis of EM shower)
  // Should be called only just after CorrectPosition !!!

  float de, dy, dz;
//float  dxg, dyg, dzg;
  static float ae = 0.076, be = 0.022;  	// de/E = a/sqrt(E)&b
  static float a = 0.57, b = 0.155, d = 1.6;	// dx = a/sqrt(E)+b (cm)
  static float dx = 0.1;  // (cm)
  
  de = sqrt( ae*ae*e + be*be*e*e );
  dz = a/sqrt(e) + b;
  dy = dz;
  dz = sqrt( dz*dz + d*d*fSinTx*fSinTx );
  dy = sqrt( dy*dy + d*d*fSinTy*fSinTy );
  
//  SectorToGlobalErr( dx, dy, dz, &dxg, &dyg, &dzg );
  
  *pde = de;
  *pdx = dx;
  *pdy = dy;
  *pdz = dz;
/*
  *pdx = dxg;
  *pdy = dyg;
  *pdz = dzg;
*/

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRecV2::SetProfileParametersV2(int sec, float Energy, float x,
					float y )
{
  if(sec >= 0){  //sec < 0 => we don't set energy dependent parameters
    //shower shape parameters
    for(int ipar=0;ipar<6;ipar++){
      fShow[ipar] = EvalShowPar(ipar,Energy);
    }
    //rms parameters
    for(int ipar=0;ipar<7;ipar++){
      fRMS[ipar] = EvalRMSPar(ipar,Energy);
    }
    
  }
  return;
}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRecV2::SetProfileParametersV2_2(int sec, float Energy, float x,
					float y )
{
  if(sec >= 0){  //sec < 0 => we don't set energy dependent parameters
    //shower shape parameters
    for(int ipar=0;ipar<6;ipar++){
      fShow2[ipar] = EvalShowPar2(ipar,Energy); //used for energy sharing in clusters
    }
    //rms parameters
    //    for(int ipar=0;ipar<7;ipar++){
    //fRMS[ipar] = EvalRMSPar(ipar,Energy);
    //}
    
  }
  return;
}

// ///////////////////////////////////////////////////////////////////////////

float MpcSectorRecV2::PredictEnergyV2(float x, float y, float Energy)
{
  float ret = 0;
  //  SetProfileParametersV2(0,Energy,0,0);
  float r = sqrt(x*x+y*y);
  if(Energy < 5)
    ret = fShow[0]*exp(-fShow[1]*r)+fShow[2]*exp(-fShow[3]*r*r*r);
  else
    ret = fShow[0]*exp(-fShow[1]*r)+fShow[2]*exp(-fShow[3]*r*r*r)+fShow[4]*exp(-fShow[5]*r*r*r*r*r);
  return ret;
}

// ///////////////////////////////////////////////////////////////////////////////

float MpcSectorRecV2::PredictEnergyV2_2(float x, float y, float Energy)
{
  float ret = 0;
  //  SetProfileParametersV2(0,Energy,0,0);
  float r = sqrt(x*x+y*y);
//   if(Energy < 5)
//     ret = fShow2[0]*exp(-fShow2[1]*r)+fShow2[2]*exp(-fShow2[3]*r*r*r);
//   else
//     ret = fShow2[0]*exp(-fShow2[1]*r)+fShow2[2]*exp(-fShow2[3]*r*r*r)+fShow2[4]*exp(-fShow2[5]*r*r*r*r*r);
  
  ret = fShow2[0]*exp(-fShow2[1]*r)+fShow2[2]*exp(-fShow2[3]*r*r*r)+fShow2[4]*exp(-fShow2[5]*r*r*r*r*r);
  
  return ret;
}

// ///////////////////////////////////////////////////////////////////////////////

float MpcSectorRecV2::PredictRMS(float x, float y, float Energy, float efrac){
  float ret = 0;
  float r = sqrt(x*x+y*y);
  //  SetProfileParametersV2(0,Energy,0,0);

  
  if(r < 1.2)
    ret = fRMS[0]+fRMS[1]*(efrac-0.4)+fRMS[2]*pow((efrac-0.4),2)+
      fRMS[3]*pow((efrac-0.4),3)+fRMS[4]*pow((efrac-0.4),4);
  else{
    ret = fRMS[5]*exp(-fRMS[6]*r);
  }
  return ret;
}
// ////////////////////////////////////////////////////////////////////////////

float MpcSectorRecV2::EvalRMSPar(int ipar, float e){
  float par = 0;
  if(e > 30) e = 30;
  if(e < 1) e = 1;
  
  if(ipar == 3 || ipar ==4){
    //hockey stick shape -- 2 lines connected pt e = p[3]
    float y1= fRMSPar[ipar][0]+fRMSPar[ipar][1]*e;
    float b2 = (fRMSPar[ipar][1]-fRMSPar[ipar][2])*fRMSPar[ipar][3]+fRMSPar[ipar][0];
    float y2 = b2+fRMSPar[ipar][2]*e;
    if(e < fRMSPar[ipar][3]) return y1;
    else return y2;
  }
  else if(ipar == 0 || ipar == 1 || ipar == 5 || ipar == 6){
    //pol 5, e < e_break, else pol1
    float e_break = (ipar != 6)?15.:25.;
    
    if(e < e_break){
      par = fRMSPar[ipar][0]+fRMSPar[ipar][1]*e+fRMSPar[ipar][2]*e*e+fRMSPar[ipar][3]*e*e*e
	+fRMSPar[ipar][4]*e*e*e*e+fRMSPar[ipar][5]*e*e*e*e*e;
    }
    else  par = fRMSPar[ipar][6]+fRMSPar[ipar][7]*e;
    return par;
    
  }

  else if(ipar == 2){
    //pol1 e < 5, pol5 e < 15, else pol1
    if(e < 5)
      par = fRMSPar[ipar][0]+fRMSPar[ipar][1]*e;
    else if(e < 15){
      par = fRMSPar[ipar][2]+fRMSPar[ipar][3]*e+fRMSPar[ipar][4]*e*e+fRMSPar[ipar][5]*e*e*e
	+fRMSPar[ipar][6]*e*e*e*e+fRMSPar[ipar][7]*e*e*e*e*e;
    }
    else  par = fRMSPar[ipar][8]+fRMSPar[ipar][9]*e;
    return par;
    
  }
  return par;


}

// ///////////////////////////////////////////////////////////////////

float MpcSectorRecV2::EvalShowPar(int ipar, float e){
  float par = 0;
  if(e > 30) e = 30;
  if(e < 1) e = 1;
  if(ipar == 0){
    //piecewise pol3, e < 5, pol5, e < 15, pol1 e >= 15
    if(e < 5) par = fShowPar[ipar][0]+fShowPar[ipar][1]*e+fShowPar[ipar][2]*e*e+fShowPar[ipar][3]*e*e*e;
    else if(e < 15) par = fShowPar[ipar][4]+fShowPar[ipar][5]*e+fShowPar[ipar][6]*e*e+fShowPar[ipar][7]*e*e*e
                         +fShowPar[ipar][8]*e*e*e*e+fShowPar[ipar][9]*e*e*e*e*e;
    else  par = fShowPar[ipar][10]+fShowPar[ipar][11]*e;
    return par;
  }
  else if(ipar == 1){
    //piecewise pol1, e < 5, pol1
    if(e < 5) par = fShowPar[ipar][0]+fShowPar[ipar][1]*e;
    else par = fShowPar[ipar][2]+fShowPar[ipar][3]*e;
    return par;
  }
  else if(ipar == 2 || ipar == 3){
    //piecewise pol1, e < 5, pol5, e < 15, pol1 e > 15                                                                                                                                                              
    if(e < 5) par = fShowPar[ipar][0]+fShowPar[ipar][1]*e;
    else if(e < 15) par = fShowPar[ipar][2]+fShowPar[ipar][3]*e+fShowPar[ipar][4]*e*e+fShowPar[ipar][5]*e*e*e
                         +fShowPar[ipar][6]*e*e*e*e+fShowPar[ipar][7]*e*e*e*e*e;
    else par = fShowPar[ipar][8]+fShowPar[ipar][9]*e;
    return par;
  }
  else if(ipar == 4 || ipar == 5){
    //0, e < 5, pol5, e < 15, pol1 e > 15          
    if(e < 5) par = 0;
    else if(e < 15) par = fShowPar[ipar][0]+fShowPar[ipar][1]*e+fShowPar[ipar][2]*e*e+fShowPar[ipar][3]*e*e*e
                         +fShowPar[ipar][4]*e*e*e*e+fShowPar[ipar][5]*e*e*e*e*e;
    else par = fShowPar[ipar][6]+fShowPar[ipar][7]*e;
    return par;
  }
  
  return par;
  
}


// ///////////////////////////////////////////////////////////////////////////

float MpcSectorRecV2::EvalShowPar2(int ipar, float e){
  float par = 0;
  if(e > 60) e = 60;
  par = fShowPar2[ipar][0]+fShowPar2[ipar][1]*e+fShowPar2[ipar][2]*e*e+fShowPar2[ipar][3]*e*e*e;
  return par;
  
//   if(e > 30) e = 30;
//   if(e < 1) e = 1;

//   if(ipar >=0 && ipar <= 3){
//     //piecewise pol1, e < 5, pol5, e < 15, pol1 e > 15                                                                                                                                                              
//     if(e < 5) par = fShowPar2[ipar][0]+fShowPar2[ipar][1]*e;
//     else if(e < 15) par = fShowPar2[ipar][2]+fShowPar2[ipar][3]*e+fShowPar2[ipar][4]*e*e+fShowPar2[ipar][5]*e*e*e
//                          +fShowPar2[ipar][6]*e*e*e*e+fShowPar2[ipar][7]*e*e*e*e*e;
//     else par = fShowPar2[ipar][8]+fShowPar2[ipar][9]*e;
//     return par;
//   }
//   else if(ipar == 4 || ipar == 5){
//     //0, e < 5, pol5, e < 15, pol1 e > 15          
//     if(e < 5) par = 0;
//     else if(e < 15) par = fShowPar2[ipar][0]+fShowPar2[ipar][1]*e+fShowPar2[ipar][2]*e*e+fShowPar2[ipar][3]*e*e*e
//                          +fShowPar2[ipar][4]*e*e*e*e+fShowPar2[ipar][5]*e*e*e*e*e;
//     else par = fShowPar2[ipar][6]+fShowPar2[ipar][7]*e;
//     return par;
//   }
  
//  return par;
  
}


// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRecV2::TwoGamma(int nh, MpcModule* phit, float* pchi, float* pe1,
			   float* px1, float* py1, float* pe2, float* px2,
			   float* py2)
{

  float e0, x0, y0, xx, yy, yx;
  float dxy, rsg2, rsq;
  float dxc, dyc, r, epsc;
  int ix, iy, ixy, in, iter, dof;
  double step;
  float cosi, chisq2, u;
  //float step, cosi, chisq2, u;
  float e1c, x1c, y1c, e2c, x2c, y2c;
  float eps0 = 0.0;
  float eps1, eps2, chisqc, ex;
  float dx1, dy1, dx2, dy2, a0, d;
  float dchi, dchi0, dd, dchida, a1, a2;
  float gr = 0.0;
  float grec, grxc, gryc, grc, gx1, gx2, gy1, gy2;
  float gre = 0.0;
  float grx = 0.0;
  float gry = 0.0;
  float scal;
  float dx0 = 0.0;
  float dy0 = 0.0;
  
  const float epsmax=0.9999;
  const float stpmin=0.025;
  const float delch=2;
  
  //  Momenta(nh,phit,&e0,&x0,&y0,&xx,&yy,&yx);
  Momenta(nh,phit,&e0,&x0,&y0,&xx,&yy,&yx);
  *pe2 = 0;
  *px2 = 0;
  *py2 = 0;
  if( nh <= 0 ) return;
  //  choosing of the starting point
  dxy = xx-yy;
  rsg2 = dxy*dxy + 4*yx*yx;
  if( rsg2 < 1e-20 ) rsg2 = 1e-20;
  rsq = sqrt(rsg2);
  dxc = -sqrt((rsq+dxy)*2);
  dyc =  sqrt((rsq-dxy)*2);
  if( yx >= 0 ) dyc = -dyc;
  r = sqrt(dxc*dxc + dyc*dyc);
  epsc = 0;
  for( in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    u = (ix-x0)*dxc/r + (iy-y0)*dyc/r;
    epsc -= phit[in].amp * u * MpcCluster::ABS(u);
  }
  epsc /= (e0*rsq);
  if( epsc >  0.8 ) epsc = 0.8;
  if( epsc < -0.8 ) epsc =-0.8;
  dxc /= sqrt(1-epsc*epsc);
  dyc /= sqrt(1-epsc*epsc);
  //  Start of iterations
  step = 0.1;
  cosi = 0;
  chisq2 = 1.e35;
  for( iter=0; iter<100; iter++)
    {
      c3to5(e0,x0,y0,epsc,dxc,dyc,&e1c,&x1c,&y1c,&e2c,&x2c,&y2c);
      eps1 = (1+epsc)/2;
      eps2 = (1-epsc)/2;
      chisqc = 0;
      for( in=0; in<nh; in++ ) {
	ex = phit[in].amp;
	ixy = phit[in].ich;
	iy = ixy/fNx;
	ix = ixy - iy*fNx;
	dx1 = x1c - ix;
	dy1 = y1c - iy;
	dx2 = x2c - ix;
	dy2 = y2c - iy;
	a0 = e1c*PredictEnergy(dx1, dy1, e1c) + e2c*PredictEnergy(dx2, dy2, e2c);
	d = fgEpar00*fgEpar00 + e0*( fgEpar1*a0/e0 + fgEpar2*a0*a0/e0/e0 +fgEpar3*a0*a0*a0/e0/e0/e0 ) + e0*sqrt(e0)*fgEpar4*a0/e0*(1-a0/e0)*fSin4T + e0*e0*fgEpar0*fgEpar0;
	chisqc += (a0-ex)*(a0-ex)/d;
      }
      if( chisqc >= chisq2 ) {
	if( iter > 0 ) {
	  dchi = chisqc-chisq2;
	  dchi0 = gr*step;
	  step /= (2*sqrt(1+dchi/dchi0));
	}
	step /= 2;
      }
      else {
	// Calculation of gradient
	grec = 0;
	grxc = 0;
	gryc = 0;
	for( in=0; in<nh; in++ ) {
	  ex = phit[in].amp;
	  ixy = phit[in].ich;
	  iy = ixy/fNx;
	  ix = ixy - iy*fNx;
	  dx1 = x1c - ix;
	  dy1 = y1c - iy;
	  dx2 = x2c - ix;
	  dy2 = y2c - iy;
	  a1 = e1c*PredictEnergy(dx1,dy1,e1c);
	  a2 = e2c*PredictEnergy(dx2,dy2,e2c);
	  a0 = a1 + a2;
	  d = fgEpar00*fgEpar00 + e0*( fgEpar1*a0/e0 + fgEpar2*a0*a0/e0/e0 +fgEpar3*a0*a0*a0/e0/e0/e0 ) + e0*sqrt(e0)*fgEpar4*a0/e0*(1-a0/e0)*fSin4T + e0*e0*fgEpar0*fgEpar0;
	  dd = (a0-ex)/d;
	  dchida = dd*( 2 - dd*(fgEpar1 + 2*fgEpar2*a0/e0 + 3*fgEpar3*a0*a0/e0/e0 + e0*sqrt(e0)*fgEpar4*fSin4T*(1-2*a0/e0) + 2*fgEpar0*fgEpar0*a0) );
	  gx1 = ( e1c*PredictEnergy(x1c+0.05-ix,dy1,e1c) - a1 )*20;
	  gx2 = ( e2c*PredictEnergy(x2c+0.05-ix,dy2,e2c) - a2 )*20;
	  gy1 = ( e1c*PredictEnergy(dx1, y1c+0.05-iy,e1c) - a1 )*20;
	  gy2 = ( e2c*PredictEnergy(dx2, y2c+0.05-iy,e2c) - a2 )*20;
	  grec += (dchida*((a1/e1c-a2/e2c)*e0 - (gx1+gx2)*dxc -(gy1+gy2)*dyc)/2);
	  grxc += (dchida*(gx1*eps2-gx2*eps1));
	  gryc += (dchida*(gy1*eps2-gy2*eps1));
	}
	grc = sqrt(grec*grec + grxc*grxc + gryc*gryc);
	if( grc < 1e-10 ) grc = 1e-10;
	if( iter > 0 ) {
	  cosi = (gre*grec + grx*grxc + gry*gryc ) / (gr*grc);
	  scal = MpcCluster::ABS(gr/grc - cosi);
	  if( scal < 0.1 ) scal = 0.1;
	  step /= scal;
	}
	chisq2 = chisqc;
	eps0 = epsc;
	dx0 = dxc;
	dy0 = dyc;
	gre = grec;
	grx = grxc;
	gry = gryc;
	gr = grc;
      }
      epsc = eps0 - step*gre/gr;
      while( MpcCluster::ABS(epsc) >= epsmax ) {
	step /= 2;
	epsc = eps0 - step*gre/gr;

        // break if step becomes too small
        if ( step<1e-29 ) break;
      }
      dxc = dx0 - step*grx/gr;
      dyc = dy0 - step*gry/gr;
      if( step*gr < stpmin ) break;
    }
  if( (*pchi)*nh-chisq2 < delch ) return;
  dof = nh;
  if( dof < 1 ) dof = 1;
  *pchi = chisq2/dof;
  c3to5(e0,x0,y0,eps0,dx0,dy0,pe1,px1,py1,pe2,px2,py2);

}



//just use principle axis of dispersion
void MpcSectorRecV2::TwoGammaV2(int nh, MpcModule* phit, float* pchi, float* pe1,
			   float* px1, float* py1, float* pe2, float* px2,
			   float* py2)
{

  float e0, x0, y0, xx, yy, yx;
  float e1,e2,x1,y1,x2,y2;
  //  float de = 1;
  float dx = 0.3;
  float dy = 0.3;
  float e1c, x1c, y1c, e2c, x2c, y2c;
  float chi2c;
  chi2c = 9999;
  x1c = 9999;
  x2c = 9999;
  y1c = 9999;
  y2c = 9999;
  e2c = 0;
  e1c = 0;
  int ndf = 0;
  
  MomentsLinear(nh,phit,&e0,&x0,&y0,&xx,&yy,&yx);
  if(xx < 0.01 || yy < 0.01) return;
  
  *pe2 = 0;
  *px2 = 0;
  *py2 = 0;
  if( nh <= 0 ) return;
  //  choosing of the starting point
  //get eigenvalues and vectors of dispersion
  
  float pa[2];
  float x_v[2];
  float y_v[2];
  pa[0] = 1/2.*(xx+yy+sqrt(pow(xx-yy,2)+4*yx*yx));  //larger
  pa[1] = 1/2.*(xx+yy-sqrt(pow(xx-yy,2)+4*yx*yx));  //smaller
  if(fabs(yx)<0.00001){
    //matrix is already diagonalized...just use 1,0 for x, 0,1 for y
    if(xx > yy){
      x_v[0] = 1.;
      y_v[0] = 0.;
      x_v[1] = 0.;
      y_v[1] = 1.;
    }
    else{
      x_v[0] = 0.;
      y_v[0] = 1.;
      x_v[1] = 1.;
      y_v[1] = 0.;
    }
  }
  else{
    
    for(int i=0;i<2;i++){
      float delta1 = pa[i]-xx;
      float delta2 = pa[i]-yy;
      if(delta1 >= delta2){
	y_v[i] = 1.;
	x_v[i] = delta2/yx;
      }
      else if(delta1 < delta2){
	x_v[i] = 1.;
	y_v[i] = delta1/yx;
      }
      
      float norm = sqrt(x_v[i]*x_v[i]+y_v[i]*y_v[i]);
      x_v[i] = x_v[i]/norm;
      y_v[i] = y_v[i]/norm;
      
      //    cout << "eigenvalue, x, y: " << i << ": " << pa[i] << ", " 
      // << x[i] << ", " << y[i] << endl;
    }
  }
  
  //cout << "xx,yy,xy eigenvalue1, eigenvalue2: " << xx << ", " << yy << ", " << yx << ", "  << pa[0] << ", " << pa[1] << endl;
  //cout << "eigenvector1: " << x_v[0] << ", " << y_v[0] << endl;
  //cout << "eigenvector2: " << x_v[1] << ", " << y_v[1] << endl;
 
  //now we have eigen values and eigenvectors
  //put the two clusters along the major axis of the pa dispersion
  // vary energy in 1 GeV steps
  //Emin = 2
  //vary position in 0.1 unit steps
  //start at +/- 0.1 and go out to 1.5 units => 15x15 steps = 225
  //maybe this is too many, we will see???
  //remember we are doing these in tower units, rather than actual x,y
  e1 = 2.;
  e2 = e0-2.;
  int ne = 10;
  float delta_e = (e0-4.)/(float)(ne-1);

  //6*8*8*10 = 4000 iterations...pretty time consuming
  for(int ia=0;ia<6;ia++){
    //we try 4 different angles:
    float theta = 3.14159265/6.0*(float)ia;
    float dir_x = x_v[0]*cos(theta) - y_v[0]*sin(theta);
    float dir_y = y_v[0]*cos(theta) + x_v[0]*sin(theta);
    for(int i1=1;i1<6;i1++){
      x1 = x0+dir_x*(float)(i1)*dx;
      y1 = y0+dir_y*(float)(i1)*dy;
      for(int i2=1;i2<6;i2++){
        if( (float)(i2+i1)*dx > 2 ) continue; //make sure solns are w/in 2
	x2 = x0+-dir_x*(float)(i2)*dx; // move in opposite direction
	y2 = y0+-dir_y*(float)(i2)*dy;
	for(int ie=0;ie<ne;ie++){
	  e1 = 2.+(float)(ie)*delta_e;
	  e2 = e0-e1;
	  float chi = ClusterChisqcore_TwoGammaV2(nh,phit,e1,x1,y1,e2,x2,y2,ndf);
	  if(chi < chi2c){
	    chi2c = chi;
	    x1c = x1;
	    y1c = y1;
	    e1c = e1;
	    x2c = x2;
	    y2c = y2;
	    e2c = e2;
	  }
	  
	}
      }
    }
  }
  

  //not done yet...lets search in a small area around our local minimum
  //search in square w/ our local minima at center...Hopefully by varying one variable at a time, we can come up w/ a better fit
  //we have 49 x 3 x 4 more operations, which is only 600
  //not much compared w/ initial search
  //i could do something more fancy, but it won't save cpu time
  for(int itr=0;itr<3;itr++){
    for(int ix=-3;ix<=3;ix++){
      x1 = x1c+(float)ix*dx/(2.0+(float)itr);
      for(int iy=-3;iy<=3;iy++){
	y1 = y1c+(float)iy*dy/2.0;
	float chi = ClusterChisqcore_TwoGammaV2(nh,phit,e1c,x1,y1,e2c,x2c,y2c,ndf);
	if(chi < chi2c){
	  chi2c = chi;
	  x1c = x1;
	  y1c = y1;
	}
      }
    }
  
    for(int ix=-3;ix<=3;ix++){
      x2 = x2c+(float)ix*dx/2.0;
      for(int iy=-3;iy<=3;iy++){
	y2 = y2c+(float)iy*dy/(2.0+(float)itr);
	float chi = ClusterChisqcore_TwoGammaV2(nh,phit,e1c,x1c,y1c,e2c,x2,y2,ndf);
	if(chi < chi2c){
	  chi2c = chi;
	  x2c = x2;
	  y2c = y2;
	}
      }
    }
    
    ne = 50;
    delta_e = (e0-4.)/(float)(ne-1);
    for(int ie=0;ie<ne;ie++){
      e1 = 2.+(float)(ie)*delta_e;
      e2 = e0-e1;
      float chi = ClusterChisqcore_TwoGammaV2(nh,phit,e1,x1c,y1c,e2,x2c,y2c,ndf);
      if(chi < chi2c){
	e1c = e1;
	e2c = e2;
      }
    } //for ie
    
  }
  
      

  *py1=y1c;
  *px1=x1c;
  *pe1=e1c;
  *py2=y2c;
  *px2=x2c;
  *pe2=e2c;
  *pchi = chi2c;

  
}


float MpcSectorRecV2::ClusterChisqcore_TwoGammaV2(int nh, MpcModule* phit, float e1, float x1,
						  float y1, float e2, float x2, float y2, int &ndf)
{
  
  if(moment_type == 0) return 9999;
  //compute chi2 w.r.t. each cluster w/in 3 tower units
  
  //calculates chisq of towers < 2.8 tower units away
  //only parameterized chisq to 3

  //these are predicted values
  //put these as statically allocated arrays to save on cpu time
 //  float* r1 = new float[nh];
//   float* r2 = new float[nh];
//   float* en1 = new float[nh];
//   float* en2 = new float[nh];
//   float* s1 = new float[nh];
//   float* s2 = new float[nh];

  float chi=0;
  int ixy, ix, iy;
  float et, a, d;
  float dx,dy;
  float dr;
  int ntow  = 0;
  
  SetProfileParametersV2_2(0,e1,x1,y1);
  SetProfileParametersV2(0,e1,x1,y1);
  for( int in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    int feech = mpcmap->getFeeCh(ix,iy,fArm);
    dx = x1-mpcmap->getX(feech)/GetModSizex();
    dy = y1-mpcmap->getY(feech)/GetModSizey();
    
    dr = sqrt(dx*dx+dy*dy);
    r1[in] = dr;
    
    en1[in] = PredictEnergyV2_2(dx, dy, e1); //energy fraction
    s1[in] = PredictRMS(dx,dy,e1,en1[in]);  //rms fraction
  }
  
  SetProfileParametersV2_2(0,e2,x2,y2);
  SetProfileParametersV2(0,e2,x2,y2);
  for( int in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    int feech = mpcmap->getFeeCh(ix,iy,fArm);
    dx = x2-mpcmap->getX(feech)/GetModSizex();
    dy = y2-mpcmap->getY(feech)/GetModSizey();
    
    dr = sqrt(dx*dx+dy*dy);
    r2[in] = dr;
    
    en2[in] = PredictEnergyV2_2(dx, dy, e2); //energy fraction
    s2[in] = PredictRMS(dx,dy,e2,en2[in]);  //rms fraction
  }

  float chi1=0; float chi2=0;
  int ndf1=0; int ndf2=0;
  //here we calculate chi1 and chi2
  for( int in=0; in<nh; in++ ) {

    et = phit[in].amp;
    if(et < 0.01) continue;
    a = (en1[in]*e1+en2[in]*e2); //total energy in tower
    d = sqrt(pow(s1[in]*e1,2)+pow(s2[in]*e2,2));
    if(r1[in] < 3){ chi1+= (et-a)*(et-a)/d/d;ndf1++;}
    //if(r2[in] < 3){ chi2+= (et-a)*(et-a)/d/d;ndf2++;}
    if(r2[in] < 3 || r1[in] < 3){ chi2+= (et-a)*(et-a)/d/d;ndf2++;}
    
    ntow++;
    //    cout << "ih, r,e, et-a,sigma" << in << ", " << sqrt(dx*dx+dy*dy) << ", " << e << ", " << fabs(et-a) << ", " << d << endl;
  }
  //  chi = (chi1+chi2)/(float)(ndf1+ndf2);
  // ndf = (ndf1+ndf2)/2;
  chi = chi2/ndf2;
  ndf = ndf2;
  
 //  delete [] r1;
//   delete [] r2;
//   delete [] en1;
//   delete [] en2;
//   delete [] s1;
//   delete [] s2;
  
  return chi;
  
  
}

// ///////////////////////////////////////////////////////////////////////////



// ///////////////////////////////////////////////////////////////////////////

float MpcSectorRecV2::ClusterChisq(int nh, MpcModule* phit, float e, float x,
				float y, int &ndf)
{

  float chi=0;
  int ixy, ix, iy;
  float et, a, d;
  float dx,dy;
  
  for( int in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    int feech = mpcmap->getFeeCh(ix,iy,fArm);
    if(moment_type == 1 || moment_type == 2){
      dx = x-mpcmap->getX(feech)/GetModSizex();
      dy = y-mpcmap->getY(feech)/GetModSizey();
    }
    else{
      dx = x-ix;
      dy = y-iy;
    }

    et = phit[in].amp;
    a = PredictEnergy(dx, dy, -1);
    d = fgEpar00*fgEpar00 + e*(fgEpar1*a + fgEpar2*a*a + fgEpar3*a*a*a) + 
      e*sqrt(e)*fgEpar4*a*(1-a)*fSin4T + e*e*fgEpar0*fgEpar0;
    a *= e;
    chi += (et-a)*(et-a)/d;
  }

  ndf=nh; // change needed for PbGl MV 28.01.00
  return chi;

}

// ///////////////////////////////////////////////////////////////////////////

float MpcSectorRecV2::ClusterChisqV2(int nh, MpcModule* phit, float e, float x,
				float y, int &ndf)
{

  float chi=0;
  int ixy, ix, iy;
  float et, a, d;
  float dx,dy;


  
  for( int in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    int feech = mpcmap->getFeeCh(ix,iy,fArm);
    if(moment_type == 1 || moment_type == 2){
      dx = x-mpcmap->getX(feech)/GetModSizex();
      dy = y-mpcmap->getY(feech)/GetModSizey();
    }
    else{
      dx = x-ix;
      dy = y-iy;
    }

    et = phit[in].amp;
    a = PredictEnergyV2(dx, dy, e);
    d = PredictRMS(dx,dy,e,a);
    
    if(d <=0){cout << PHWHERE << "bad rms value\n"; 
      cout << "dx,dy,dr,e,efrac,real_efrac, rms: " << dx << ", " << dy << ", " << sqrt(dx*dx+dy*dy) << ", " << e << ", " << a << ", " << et << ", " << d << endl;
      cout << "energy, RMSPar: " << e << ", " << fRMS[0] << ", " << fRMS[1] << ", " << fRMS[2] << ", " << fRMS[3] << ", " << fRMS[4] << ", " << fRMS[5] << ", " << fRMS[6] << endl; 
      cout << "energy, ShowPar: " << e << ", " << fShow[0] << ", " << fShow[1] << ", " << fShow[2] << ", " << fShow[3] << ", " << fShow[4] << ", " << fShow[5] << endl;
      
      d = 0.0000001;
    }
    

      
    a = a*e;
    d = d*e;
    chi += (et-a)*(et-a)/d/d;
    //    cout << "ih, r,e, et-a,sigma" << in << ", " << sqrt(dx*dx+dy*dy) << ", " << e << ", " << fabs(et-a) << ", " << d << endl;
  }

  ndf=nh; // change needed for PbGl MV 28.01.00
  return chi;

}

// ///////////////////////////////////////////////////////////////////////////

float MpcSectorRecV2::ClusterChisqcoreV2(int nh, MpcModule* phit, float e, float x,
				float y, int &ndf)
{

  //calculates chisq of towers < 2.8 tower units away
  //only parameterized chisq to 3

  float chi=0;
  int ixy, ix, iy;
  float et, a, d;
  float dx,dy;
  float dr;
  int ntow  = 0;
  
  for( int in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    int feech = mpcmap->getFeeCh(ix,iy,fArm);
    if(moment_type == 1 || moment_type == 2){
      dx = x-mpcmap->getX(feech)/GetModSizex();
      dy = y-mpcmap->getY(feech)/GetModSizey();
    }
    else{
      dx = x-ix;
      dy = y-iy;
    }

    dr = sqrt(dx*dx+dy*dy);
    if(dr > 3.0) continue;
    

    et = phit[in].amp;
    if(et < 0.01) continue;
    a = PredictEnergyV2(dx, dy, e);
    d = PredictRMS(dx,dy,e,a);
    
    
    if(d <=0){cout << PHWHERE << "bad rms value\n"; 
      cout << "dx,dy,dr,e,efrac,real_efrac, rms: " << dx << ", " << dy << ", " << sqrt(dx*dx+dy*dy) << ", " << e << ", " << a << ", " << et << ", " << d << endl;
      cout << "energy, RMSPar: " << e << ", " << fRMS[0] << ", " << fRMS[1] << ", " << fRMS[2] << ", " << fRMS[3] << ", " << fRMS[4] << ", " << fRMS[5] << ", " << fRMS[6] << endl; 
      cout << "energy, ShowPar: " << e << ", " << fShow[0] << ", " << fShow[1] << ", " << fShow[2] << ", " << fShow[3] << ", " << fShow[4] << ", " << fShow[5] << endl;
      
      d = 0.0000001;
    }
    

    
    a = a*e;
    d = d*e;
    chi += (et-a)*(et-a)/d/d;
    ntow++;
    //    cout << "ih, r,e, et-a,sigma" << in << ", " << sqrt(dx*dx+dy*dy) << ", " << e << ", " << fabs(et-a) << ", " << d << endl;
  }

  ndf=ntow; // change needed for PbGl MV 28.01.00
  return chi;

}

// ///////////////////////////////////////////////////////////////////////////

void  MpcSectorRecV2::computeCorrectedDispersionV2(float ycg, float xcg,
                                   float dispy, float dispx,
                                   float yModSize, float xModSize,
                                   float& corrdispy, float& corrdispx)
{

  //first find tower to which we are closest to
  int ich_begin = (fArm == 0)?0:288;
  int ich_end = (fArm == 0)?288:576;
  float dr_min = 9999;
  int dr_ch = -1;
  for(int ich=ich_begin;ich < ich_end;ich++){
    if(mpcmap->getGridX(ich) < 0) continue;
    float x = mpcmap->getX(ich);
    float y = mpcmap->getY(ich);
    float dx = x-xcg; float dy = y-ycg;
    float dr = sqrt(dx*dx+dy*dy);
    if(dr < dr_min){
      dr_min = dr;
      dr_ch = ich;
    }
  }

  float x_offset = 0;
  float y_offset = 0;
  if(dr_ch < 0) {
    cout << PHWHERE << "something wrong in compute corrected dispersion\n";
  }
  else{
    x_offset = mpcmap->getX(dr_ch)/xModSize;
    y_offset = mpcmap->getY(dr_ch)/yModSize;
  }


  
  float xpos = xcg/xModSize - x_offset;
  float ypos = ycg/yModSize - y_offset;


  corrdispy = dispy/(yModSize*yModSize);
  corrdispx = dispx/(xModSize*xModSize);


  float xposmod = xpos - floor(xpos);
  float yposmod = ypos - floor(ypos);

  corrdispx -= ( xposmod - xposmod*xposmod);
  corrdispy -= ( yposmod - yposmod*yposmod);

  corrdispx*=xModSize*xModSize;
  corrdispy*=yModSize*yModSize;

  if(dispx < 0.00001 || corrdispx < 0.00001) corrdispx = 0.00001;
  if(dispy < 0.00001 || corrdispy < 0.00001) corrdispy = 0.00001;
}

// /////////////////////////////////////////////////



float MpcSectorRecV2::Chi2Limit(int ND)
{
  //  Here the reverse Chi2Correct function is used
  
  float rn, a, b, chi2;
  
  if( ND < 1 ) return 9999.;  // Should we put 0. here?
  
  chi2 = fgChi2Level[MpcCluster::min(ND,50)-1];
  if( chi2 > 100 ) return 9999.; // Why should chi2 ever be >100?
  
  rn = ND;
  b = 0.072*sqrt(rn);
  a = 6.21/(sqrt(rn)+4.7);
  
  return chi2*a/(1.-chi2*b);

}

// ///////////////////////////////////////////////////////////////////////////

float MpcSectorRecV2::Chi2Correct(float Chi2, int ND)
{
  // Chi2 - is reduced Chi2: Chi2/ND !!
  // MV 02.22.2000: Actually the above is not true. The supplied value of Chi2
  // has been already divided by ND. So Chi2 here is only corrected.

  float rn, a, b, c;
  
  if( ND < 1 ) return 9999.; // Should we put 0. here?
  
  rn = ND;
  b = 0.072*sqrt(rn);
  a = 6.21/(sqrt(rn)+4.7);
  c = a + b*Chi2;
  if( c < 1 ) c=1;
  
  return Chi2/c;

}

// ///////////////////////////////////////////////////////////////////////////

void MpcSectorRecV2::SetTowerThreshold(float Thresh)
{
  fgTowerThresh = Thresh;
  fgEpar0 = Thresh*0.07;
  fgEpar00 = MpcCluster::max( (double)Thresh/3, 0.005 );
}

// **********************************************************************

void MpcSectorRecV2::getTowerPos(int ix, int iy, float &x, float & y){
  x = 2.859+5.562*ix+int(ix/12)*0.256;
  y = 2.859+5.562*iy+int(iy/12)*0.156;
}

// **********************************************************************


