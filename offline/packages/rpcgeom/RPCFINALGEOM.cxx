/*!
  \file RPCFINALGEOM.cxx
  \brief widely used utility functions and enumerations
  \author Richard Hollis, UCR (rhollis@ucr.edu)
*/
#include "RPCFINALGEOM.h"
#include <cmath>

using namespace::std;

ClassImp( RPCFINALGEOM )

RPCFINALGEOM::RPCFINALGEOM()
{
  fThisArm = RPCFINALGEOM::South;
  fThisStation = RPCFINALGEOM::Station3;
  fThisOctant = 4;
  fThisHalfOctant = 0;
  fThisRadSeg = RPCFINALGEOM::SegmentA;
  fThisChannel = 0;
}

RPCFINALGEOM::RPCFINALGEOM(RPCFINALGEOM::ArmNumber fArm,
			   RPCFINALGEOM::StationNumber fStation,
			   Int_t fOctant,
			   Int_t fHalfOctant,
			   RPCFINALGEOM::RadialSegment fRadSeg,
			   Int_t fChannel) 
{
  fThisArm = fArm;
  fThisStation = fStation;
  fThisOctant = fOctant;
  fThisHalfOctant = fHalfOctant;
  fThisRadSeg = fRadSeg;
  fThisChannel = fChannel;
}

const Float_t RPCFINALGEOM::RAD_TO_DEG = 180.0/M_PI;
const Float_t RPCFINALGEOM::DEG_TO_RAD = M_PI/180.0;
const int RPCFINALGEOM::OUTOFRANGE = -999;

//****************************************
//- Local Positions
//****************************************
const double RPCFINALGEOM::LocalPosition_StripAngle = 11.25;

const double RPCFINALGEOM::LocalPosition_X_FirstStripOuter[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{0.00,     0.00,     0.00},     //RPC1
   {0.00,     0.00,     0.00},     //RPC2 - does not exist
   {0.00,     0.00,     0.00}};    //RPC3

const double RPCFINALGEOM::LocalPosition_Y_FirstStripOuter[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{0.467,   1.064,   0},      //RPC1
   {0,           0,        0},      //RPC2 - does not exist
   {7.57,     6.31,     8.89}};     //RPC3

const double RPCFINALGEOM::LocalPosition_X_FirstStripInner[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{0,            0,        0},     //RPC1 - inner does not exist
   {0,            0,        0},     //RPC2 - does not exist
   {60.41,    55.35,    58.70}};     //RPC3

const double RPCFINALGEOM::LocalPosition_Y_FirstStripInner[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{0,            0,        0},      //RPC1 - inner does not exist
   {0,            0,        0},      //RPC2 - does not exist
   {18.87,    19.20,    21.89}};     //RPC3


const double RPCFINALGEOM::LocalFramePosition_X[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{38.835+(5.00-6.5),   (5.00-6.5),  0.00},  //RPC1 // 37.72 is actual, +5.00 for the frame size and offset (same as rad 1) shifted to 41.8cm from 0,0
   {0,          0,     0},  //RPC2
   {229.9, 117.88,    10.67}}; //RPC3 // Tuned April-June 2011

const double RPCFINALGEOM::LocalFramePosition_Y[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{  0.10,  0.10,  0.00},  //RPC1
   {     0,     0,     0},  //RPC2
   { 2.183, 2.183, 2.183}}; //RPC3 // Tuned April-June 2011

const double RPCFINALGEOM::LocalFramePosition_Z[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{  0.00,  0.00,  0.00},  //RPC1 // These need to be the correct numbers
   {  0.00,  0.00,  0.00},  //RPC2
   {  0.00,  0.00,  0.00}}; //RPC3 // These need to be the correct numbers


const double RPCFINALGEOM::FirstStripOuterNumber[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{36, 36, -9999},    //RPC1
   { 0,  0,   0},      //RPC2 - does not exist
   {34, 33,  33}};     //RPC3

const double RPCFINALGEOM::LastStripOuterNumber[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{59, 59, -9999},    //RPC1
   { 0,  0,   0},      //RPC2 - does not exist
   {61, 61,  62}};     //RPC3

const double RPCFINALGEOM::FirstStripInnerNumber[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{-9999, -9999, -9999},    //RPC1
   { 0,  0,   0},      //RPC2 - does not exist
   { 9,  6,   5}};     //RPC3

const double RPCFINALGEOM::LastStripInnerNumber[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{-9999, -9999, -9999},    //RPC1
   { 0,  0,   0},      //RPC2 - does not exist
   {21, 24,  26}};     //RPC3

//This is a large array of the strip lengths
const double RPCFINALGEOM::StripLength[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg][64] =
  {/* RPC STATION 1*/
    {/* Segment A */{ 0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00, 2.394, 7.181,11.969,16.756,
		    21.544,24.409,24.409,24.409,24.409,24.409,24.409,24.409,
		    24.409,24.409,24.409,24.409,24.409,24.409,24.409,22.248,
		    17.825,13.402, 8.979, 3.142,  0.00,  0.00,  0.00,  0.00},
    /* Segment B */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00, 5.453,13.302,21.149,28.999,
		    36.844,39.290,39.290,39.290,39.290,39.290,39.290,39.290,
		    39.290,39.290,39.290,39.290,39.290,39.290,39.290,39.290,
		    35.749,28.499,21.248,13.998,  0.00,  0.00,  0.00,  0.00},
    /* Segment C */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00}},
   {/* RPC STATION 2*/
    /* Segment A */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00},
    /* Segment B */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00},
    /* Segment C */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00}},
   {/* RPC STATION 3*/
    /* Segment A */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
 		      0.00,  0.00, 61.47, 61.47, 61.47, 61.47, 61.47, 61.47,
  		     61.47, 61.47, 61.47, 61.47, 61.47, 61.47, 61.47,  0.00,
 		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
 		      0.00,  0.00,  0.00, 38.80, 55.87, 72.94, 90.00,107.08,
 		    124.15,124.43, 61.57, 61.57, 61.57, 61.57, 61.57, 61.57,
 		     61.57, 61.57, 61.57, 61.57, 61.57, 61.57, 61.57,124.43,
 		    114.70, 98.93, 83.16, 67.39, 51.62, 35.85, 20.08,  0.00, 0.00},
    /* Segment B */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
 	             67.95, 67.95, 67.95, 67.95, 67.95, 67.95, 67.95, 67.95,
 	             67.95, 67.95, 67.95, 67.95, 67.95, 67.95, 67.95, 67.95,
 	             67.95, 67.95, 67.95,  0.00,  0.00,  0.00,  0.00,  0.00,
 		      0.00,  0.00,  0.00, 32.34, 56.85, 81.35,105.85,125.91,
 	             56.43, 56.43, 56.43, 56.43, 56.43, 56.43, 56.43, 56.43,
 	             56.43, 56.43, 56.43, 56.43, 56.43, 56.43, 56.43, 56.43,
 		     56.43, 56.43, 56.43,125.91,107.64, 85.00, 62.37, 39.73,0.00,  0.00  },
    /* Segment C */{  0.00,  0.00,  0.00,  0.00,  0.00, 64.20, 64.20,
	             64.20, 64.20, 64.20, 64.20, 64.20, 64.20, 64.20, 64.20,
	             64.20, 64.20, 64.20, 64.20, 64.20, 64.20, 64.20, 64.20,
		     64.20, 64.20, 64.20, 64.20,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00, 45.57, 77.20,108.82,124.25, 59.85, 59.85,
	             59.85, 59.85, 59.85, 59.85, 59.85, 59.85, 59.85, 59.85,
	             59.85, 59.85, 59.85, 59.85, 59.85, 59.85, 59.85, 59.85,
		     59.85, 59.85, 59.85, 59.85,112.07, 82.85, 53.63, 24.41,0.00}}};

const double RPCFINALGEOM::StripGap = 0.196;//0.2;
//strip widths
const double RPCFINALGEOM::StripWidth_Outer[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{0.73,  1.33,     0},     //RPC1
   {   0,     0,     0},     //RPC2 - does not exist
   {3.13,  4.58,  5.97}};    //RPC3

const double RPCFINALGEOM::StripWidth_Inner[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::MaxNumberOfRadSeg] =
  {{   0,     0,     0},     //RPC1
   {   0,     0,     0},     //RPC2 - does not exist
   {3.13,  4.58,  5.97}};    //RPC3

//Global Positions
//This is the position of the right-angle corner in PHENIX coordinates
const double RPCFINALGEOM::GlobalFramePosition_X[RPCFINALGEOM::NumberOfStations][16] =
  {{ 102.97,  102.97,   72.81,   72.81,    0.00,    0.00,  -72.81,  -72.81,   //RPC1
    -102.97, -102.97,  -72.81,  -72.81,    0.00,    0.00,   72.81,   72.81},  //RPC1
   {   0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,   //RPC2
       0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00},  //RPC2
   { 493.74,  493.74,  349.13,  349.13,    0.00,    0.00, -349.13, -349.13,   //RPC3
    -493.74, -493.74, -349.13, -349.13,    0.00,    0.00,  349.13,  349.13}}; //RPC3

const double RPCFINALGEOM::GlobalFramePosition_Y[RPCFINALGEOM::NumberOfStations][16] =
  {{   0.00,    0.00,   72.81,   72.81,  102.97,  102.97,   72.81,   72.81,   //RPC1
       0.00,    0.00,  -72.81,  -72.81, -102.97, -102.97,  -72.81,  -72.81},  //RPC1
   {   0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,   //RPC2 
       0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00},  //RPC2
   {   0.00,    0.00,  349.13,  349.13,  493.74,  491.74,  349.13,  349.13,   //RPC3
       0.00,    0.00, -349.13, -349.13, -493.74, -493.74, -349.13, -349.13}}; //RPC3

const double RPCFINALGEOM::GlobalFramePosition_Z[RPCFINALGEOM::NumberOfStations][RPCFINALGEOM::NumberOfArms] =
  {{-162.00, 162.00},  //RPC1
   {   0.00,   0.00},  //RPC2
   {-906.30, 906.30}}; //RPC3
	

//_______________________________________________
void RPCFINALGEOM::PRINT(std::ostream& os, const std::string& message){
  const int max_col=80;
  if(!message.size()) {
    os << std::string(max_col,'-') << std::endl;
    return;
  }
  int fill = max_col - message.size() - 2;
  int pre = static_cast<int>(std::floor(fill/2.0));
  int post = fill - pre;
  os << std::string(pre,'-') << " ";
  os << message << " ";
  os << std::string(post,'-') << std::endl;	
}

PHPoint RPCFINALGEOM::getStartPosition(RPCFINALGEOM::ArmNumber fArm,
				       RPCFINALGEOM::StationNumber fStation,
				       Int_t fOctant, Int_t fHalfOctant,
				       RPCFINALGEOM::RadialSegment fRadSeg,
				       Int_t fChannel)
{
  PHPoint point;
  point.setX(getStartPositionX(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setY(getStartPositionY(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setZ(getStartPositionZ(fArm,fStation,fRadSeg));
  
  return point;
}

double RPCFINALGEOM::getStartPositionX(RPCFINALGEOM::ArmNumber fArm,
				       RPCFINALGEOM::StationNumber fStation,
				       Int_t fOctant, Int_t fHalfOctant,
				       RPCFINALGEOM::RadialSegment fRadSeg,
				       Int_t fChannel)
{    
  if(fStation==RPCFINALGEOM::Station2) {
    /*cout << "RPCFINALGEOM: Station does not exist for this GEOMETRY" << endl;*/ return -9999; }

  Double_t fAngle=-fOctant*45.*RPCFINALGEOM::DEG_TO_RAD;

  Float_t fStartX = LocalFramePosition_X[fStation][fRadSeg];
  Float_t fLocPosX = LocalPosition_X_FirstStripInner[fStation][fRadSeg];
  if(fChannel>=32) { fLocPosX = LocalPosition_X_FirstStripOuter[fStation][fRadSeg]; }
  fStartX += fLocPosX;

  Float_t fStartY = LocalFramePosition_Y[fStation][fRadSeg];
  Float_t fLocPosY = LocalPosition_Y_FirstStripInner[fStation][fRadSeg]+
    (StripGap+StripWidth_Inner[fStation][fRadSeg])*
    (fChannel-FirstStripInnerNumber[fStation][fRadSeg]);
  if(fChannel>=32) {
    fLocPosY = LocalPosition_Y_FirstStripOuter[fStation][fRadSeg]+
      (StripGap+StripWidth_Outer[fStation][fRadSeg])*
      (fChannel-FirstStripOuterNumber[fStation][fRadSeg]);}
  fStartY += fLocPosY;
  
  Float_t fStart = GlobalFramePosition_X[fStation][fOctant*2+fHalfOctant];
  if(fHalfOctant==0) { fStart += -fStartX*cos(fAngle)-fStartY*sin(fAngle); }
  else               { fStart += -fStartX*cos(fAngle)+fStartY*sin(fAngle); }
 
  return fStart;
}

double RPCFINALGEOM::getStartPositionY(RPCFINALGEOM::ArmNumber fArm,
				       RPCFINALGEOM::StationNumber fStation,
				       Int_t fOctant, Int_t fHalfOctant,
				       RPCFINALGEOM::RadialSegment fRadSeg,
				       Int_t fChannel)
{  
  if(fStation==RPCFINALGEOM::Station2) {
    /*cout << "RPCFINALGEOM: Station does not exist" << endl;*/ return -9999; }

  Double_t fAngle=-fOctant*45.*RPCFINALGEOM::DEG_TO_RAD;

  Float_t fStartX = LocalFramePosition_X[fStation][fRadSeg];
  Float_t fLocPosX = LocalPosition_X_FirstStripInner[fStation][fRadSeg];
  if(fChannel>=32) { fLocPosX = LocalPosition_X_FirstStripOuter[fStation][fRadSeg]; }
  fStartX += fLocPosX;

  Float_t fStartY = LocalFramePosition_Y[fStation][fRadSeg];
  Float_t fLocPosY = LocalPosition_Y_FirstStripInner[fStation][fRadSeg]+
    (StripGap+StripWidth_Inner[fStation][fRadSeg])*
    (fChannel-FirstStripInnerNumber[fStation][fRadSeg]);
  if(fChannel>=32) {
    fLocPosY = LocalPosition_Y_FirstStripOuter[fStation][fRadSeg]+
      (StripGap+StripWidth_Outer[fStation][fRadSeg])*
      (fChannel-FirstStripOuterNumber[fStation][fRadSeg]);}
  fStartY += fLocPosY;
  
  Float_t fStart = GlobalFramePosition_Y[fStation][fOctant*2+fHalfOctant];
  if(fHalfOctant==0) { fStart += fStartX*sin(fAngle)-fStartY*cos(fAngle); }
  else               { fStart += fStartX*sin(fAngle)+fStartY*cos(fAngle); }
  
  return fStart;
}

double RPCFINALGEOM::getStartPositionZ(RPCFINALGEOM::ArmNumber fArm,
				       RPCFINALGEOM::StationNumber fStation,
				       RPCFINALGEOM::RadialSegment fRadSeg)
{
  Float_t fStart = LocalFramePosition_Z[fStation][fRadSeg];
  fStart += GlobalFramePosition_Z[fStation][fArm];
  
  return fStart;
}

PHPoint RPCFINALGEOM::getEndPosition(RPCFINALGEOM::ArmNumber fArm,
				     RPCFINALGEOM::StationNumber fStation,
				     Int_t fOctant, Int_t fHalfOctant,
				     RPCFINALGEOM::RadialSegment fRadSeg,
				     Int_t fChannel)
{
  PHPoint point;
  point.setX(getEndPositionX(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setY(getEndPositionY(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setZ(getEndPositionZ(fArm,fStation,fRadSeg));
  
  return point;
}

double RPCFINALGEOM::getEndPositionX(RPCFINALGEOM::ArmNumber fArm,
				     RPCFINALGEOM::StationNumber fStation,
				     Int_t fOctant, Int_t fHalfOctant,
				     RPCFINALGEOM::RadialSegment fRadSeg,
				     Int_t fChannel)
{    
  if(fStation==RPCFINALGEOM::Station2) {
    /*cout << "RPCFINALGEOM: Station does not exist for this PROTOTYPE" << endl;*/ return -9999; }

  Float_t fStart = getStartPositionX(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel);

  Float_t fThisLocalPosition_StripAngle = (fOctant*45)-LocalPosition_StripAngle;
  if(fHalfOctant==1) { fThisLocalPosition_StripAngle = (fOctant*45)+LocalPosition_StripAngle;; }
  
  return fStart-StripLength[fStation][fRadSeg][fChannel]*cos(DEG_TO_RAD*fThisLocalPosition_StripAngle);
}

double RPCFINALGEOM::getEndPositionY(RPCFINALGEOM::ArmNumber fArm,
				     RPCFINALGEOM::StationNumber fStation,
				     Int_t fOctant, Int_t fHalfOctant,
				     RPCFINALGEOM::RadialSegment fRadSeg,
				     Int_t fChannel)
{
  if(fStation==RPCFINALGEOM::Station2) {
    /*cout << "RPCFINALGEOM: Station does not exist" << endl;*/ return -9999; }

  Float_t fStart = getStartPositionY(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel);
  
  Float_t fThisLocalPosition_StripAngle = (fOctant*45)-LocalPosition_StripAngle;
  if(fHalfOctant==1) { fThisLocalPosition_StripAngle = (fOctant*45)+LocalPosition_StripAngle;; }
  
  return fStart-StripLength[fStation][fRadSeg][fChannel]*sin(DEG_TO_RAD*fThisLocalPosition_StripAngle);
}

double RPCFINALGEOM::getEndPositionZ(RPCFINALGEOM::ArmNumber fArm,
				     RPCFINALGEOM::StationNumber fStation,
				     RPCFINALGEOM::RadialSegment fRadSeg)
{
  return getStartPositionZ(fArm,fStation,fRadSeg);
}

PHPoint RPCFINALGEOM::GetBegin()
{
  PHPoint point;
  point.setX(getStartPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setY(getStartPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setZ(getStartPositionZ(fThisArm,fThisStation,fThisRadSeg));
  
  return point;
}

PHPoint RPCFINALGEOM::GetEnd()
{
  PHPoint point;
  point.setX(getEndPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setY(getEndPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setZ(getEndPositionZ(fThisArm,fThisStation,fThisRadSeg));
  
  return point;
}

PHPoint RPCFINALGEOM::GetMid()
{
  PHPoint point;
  point.setX(0.5*(getStartPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)+getEndPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)));
  point.setY(0.5*(getStartPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)+getEndPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)));
  point.setZ(0.5*(getStartPositionZ(fThisArm,fThisStation,fThisRadSeg)+getEndPositionZ(fThisArm,fThisStation,fThisRadSeg)));
  
  return point;
}

Bool_t RPCFINALGEOM::checkStrip()
{
  if(fThisArm >= int(RPCFINALGEOM::NumberOfArms))            { return false; }
  if(fThisStation >= int(RPCFINALGEOM::NumberOfStations))    { return false; }
  if(fThisOctant >= 8)                                       { return false; }
  if(fThisHalfOctant >= 2)                                   { return false; }
  if(fThisStation == RPCFINALGEOM::Station1) {
    if(fThisRadSeg  >= int(RPCFINALGEOM::NumberOfRadSegSt1)) { return false;} }
  if(fThisStation == RPCFINALGEOM::Station2) {
    if(fThisRadSeg  >= int(RPCFINALGEOM::NumberOfRadSegSt2)) { return false;} }
  if(fThisStation == RPCFINALGEOM::Station3) {
    if(fThisRadSeg  >= int(RPCFINALGEOM::NumberOfRadSegSt3)) { return false;} }
  
  //cout << fThisOctant << " " << fThisHalfOctant << " " << fThisRadSeg << " " << fThisChannel << " ";
  if(fThisChannel<32) {
    if(fThisChannel < FirstStripInnerNumber[fThisStation][fThisRadSeg] ||
       fThisChannel > LastStripInnerNumber[fThisStation][fThisRadSeg]) {
      //cout << endl;
      return false;} }
  else {
    if(fThisChannel < FirstStripOuterNumber[fThisStation][fThisRadSeg] ||
       fThisChannel > LastStripOuterNumber[fThisStation][fThisRadSeg]) {
      //cout << endl;
      return false;} }
  //cout << "is okay" << endl;
  return true;
}
