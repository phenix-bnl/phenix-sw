/*!
  \file RPCFULLGEOM.cxx
  \brief widely used utility functions and enumerations
  \author Richard Hollis, UCR (rhollis@ucr.edu)
*/
#include "RPCFULLGEOM.h"
#include <cmath>
using namespace::std;

ClassImp( RPCFULLGEOM )

const Float_t RPCFULLGEOM::RAD_TO_DEG = 180.0/M_PI;
const Float_t RPCFULLGEOM::DEG_TO_RAD = M_PI/180.0;
const int RPCFULLGEOM::OUTOFRANGE = -999;

RPCFULLGEOM::RPCFULLGEOM()
{
  fThisArm = RPCFULLGEOM::South;
  fThisStation = RPCFULLGEOM::Station3;
  fThisOctant = 4;
  fThisHalfOctant = 0;
  fThisRadSeg = RPCFULLGEOM::SegmentA;
  fThisChannel = 0;
}

RPCFULLGEOM::RPCFULLGEOM(RPCFULLGEOM::ArmNumber fArm,
			   RPCFULLGEOM::StationNumber fStation,
			   Int_t fOctant,
			   Int_t fHalfOctant,
			   RPCFULLGEOM::RadialSegment fRadSeg,
			   Int_t fChannel) 
{
  fThisArm = fArm;
  fThisStation = fStation;
  fThisOctant = fOctant;
  fThisHalfOctant = fHalfOctant;
  fThisRadSeg = fRadSeg;
  fThisChannel = fChannel;
}

//****************************************
//- Local Positions
//****************************************
const double RPCFULLGEOM::LocalPosition_StripAngle = 11.25;

const double RPCFULLGEOM::LocalPosition_X_FirstStripOuter[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{0.00,     0.00,     0.00},     //RPC1
   {0.00,     0.00,     0.00},     //RPC2 - does not exist
   {0.00,     0.00,     0.00}};    //RPC3

const double RPCFULLGEOM::LocalPosition_Y_FirstStripOuter[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{0.70,     0.25,        0},      //RPC1
   {0,           0,        0},      //RPC2 - does not exist
   {7.57,     6.31,     8.89}};     //RPC3

const double RPCFULLGEOM::LocalPosition_X_FirstStripInner[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{16.53,    21.36,        0},     //RPC1
   {0,            0,        0},     //RPC2 - does not exist
   {60.41,    55.35,    58.70}};     //RPC3

const double RPCFULLGEOM::LocalPosition_Y_FirstStripInner[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{2.48,      3.20,        0},      //RPC1
   {0,            0,        0},      //RPC2 - does not exist
   {18.87,    19.20,    21.89}};     //RPC3


const double RPCFULLGEOM::LocalFramePosition_X[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{37.72+(5.00),   (5.00),  0.00},  //RPC1 // 37.72 is actual, +5.00 for the frame size and offset (same as rad 1)
   {0,          0,     0},  //RPC2
   {229.9, 117.88,    10.67}}; //RPC3 // Tuned April-June 2011

const double RPCFULLGEOM::LocalFramePosition_Y[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{  0.10,  0.10,  0.00},  //RPC1
   {     0,     0,     0},  //RPC2
   { 2.183, 2.183, 2.183}}; //RPC3 // Tuned April-June 2011

const double RPCFULLGEOM::LocalFramePosition_Z[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{  0.00,  0.00,  0.00},  //RPC1 // These need to be the correct numbers
   {  0.00,  0.00,  0.00},  //RPC2
   {  0.00,  0.00,  0.00}}; //RPC3 // These need to be the correct numbers


const double RPCFULLGEOM::FirstStripOuterNumber[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{41, 33, -9999},    //RPC1
   { 0,  0,   0},      //RPC2 - does not exist
   {34, 33,  33}};     //RPC3

const double RPCFULLGEOM::LastStripOuterNumber[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{56, 63, -9999},    //RPC1
   { 0,  0,   0},      //RPC2 - does not exist
   {61, 61,  62}};     //RPC3

const double RPCFULLGEOM::FirstStripInnerNumber[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{11,  4, -9999},    //RPC1
   { 0,  0,   0},      //RPC2 - does not exist
   { 9,  6,   5}};     //RPC3

const double RPCFULLGEOM::LastStripInnerNumber[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{22, 28, -9999},    //RPC1
   { 0,  0,   0},      //RPC2 - does not exist
   {21, 24,  26}};     //RPC3

//This is a large array of the strip lengths
const double RPCFULLGEOM::StripLength[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg][64] =
  {/* RPC STATION 1*/
    {/* Segment A */{ 0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  7.35,  7.35,  7.35,  7.35,  7.35,
		      7.35,  7.35,  7.35,  7.35,  7.35,  7.35,  7.35,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  3.09, 10.37, 17.69, 24.21, 16.65, 16.65, 16.65,
		     16.65, 16.65, 16.65, 16.65, 16.65, 21.96, 15.78,  9.02,
		      2.58,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00},
    /* Segment B */{  0.00,  0.00,  0.00,  0.00, 15.78, 16.48, 16.48, 16.48,
		     16.48, 16.48, 16.48, 16.48, 16.48, 16.48, 16.48, 16.48,
		     16.48, 16.48, 16.48, 16.48, 16.48, 16.48, 16.48, 16.48,
		     16.48, 16.48, 16.48, 16.48, 16.48,  0.00,  0.00,  0.00,
		      0.00,  0.71,  7.07, 13.35, 19.62, 25.90, 32.16, 21.67,
		     21.67, 21.67, 21.67, 21.67, 21.67, 21.67, 21.67, 21.67,
		     21.67, 21.67, 21.67, 21.67, 21.67, 21.67, 21.67, 21.67,
		     21.67, 21.67, 34.83, 29.04, 23.23, 17.43, 11.64,  5.84},
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

const double RPCFULLGEOM::StripGap = 0.196;//0.2;
//strip widths
const double RPCFULLGEOM::StripWidth_Outer[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{1.22,  1.02,     0},     //RPC1
   {   0,     0,     0},     //RPC2 - does not exist
   {3.13,  4.58,  5.97}};    //RPC3

const double RPCFULLGEOM::StripWidth_Inner[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::MaxNumberOfRadSeg] =
  {{0.80,  0.72,     0},     //RPC1
   {   0,     0,     0},     //RPC2 - does not exist
   {3.13,  4.58,  5.97}};    //RPC3

//Global Positions
//This is the position of the right-angle corner in PHENIX coordinates
const double RPCFULLGEOM::GlobalFramePosition_X[RPCFULLGEOM::NumberOfStations][16] =
  {{  97.97,   97.97,   69.28,   69.28,    0.00,    0.00,  -69.28,  -69.28,   //RPC1
     -97.97,  -97.97,  -69.28,  -69.28,    0.00,    0.00,   69.28,   69.28},  //RPC1
   {   0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,   //RPC2
       0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00},  //RPC2
   { 493.74,  493.74,  349.13,  349.13,    0.00,    0.00, -349.13, -349.13,   //RPC3
    -493.74, -493.74, -349.13, -349.13,    0.00,    0.00,  349.13,  349.13}}; //RPC3

const double RPCFULLGEOM::GlobalFramePosition_Y[RPCFULLGEOM::NumberOfStations][16] =
  {{   0.00,    0.00,   69.28,   69.28,   97.97,   97.97,   69.28,   69.28,   //RPC1
       0.00,    0.00,  -69.28,  -69.28,  -97.97,  -97.97,  -69.28,  -69.28},  //RPC1
   {   0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,   //RPC2 
       0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00},  //RPC2
   {   0.00,    0.00,  349.13,  349.13,  493.74,  491.74,  349.13,  349.13,   //RPC3
       0.00,    0.00, -349.13, -349.13, -493.74, -493.74, -349.13, -349.13}}; //RPC3

const double RPCFULLGEOM::GlobalFramePosition_Z[RPCFULLGEOM::NumberOfStations][RPCFULLGEOM::NumberOfArms] =
  {{-159.00, 159.00},  //RPC1
   {   0.00,   0.00},  //RPC2
   {-906.30, 906.30}}; //RPC3
	

//_______________________________________________
void RPCFULLGEOM::PRINT(std::ostream& os, const std::string& message){
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

PHPoint RPCFULLGEOM::getStartPosition(RPCFULLGEOM::ArmNumber fArm,
				       RPCFULLGEOM::StationNumber fStation,
				       Int_t fOctant, Int_t fHalfOctant,
				       RPCFULLGEOM::RadialSegment fRadSeg,
				       Int_t fChannel)
{
  PHPoint point;
  point.setX(getStartPositionX(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setY(getStartPositionY(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setZ(getStartPositionZ(fArm,fStation,fRadSeg));
  
  return point;
}

double RPCFULLGEOM::getStartPositionX(RPCFULLGEOM::ArmNumber fArm,
				       RPCFULLGEOM::StationNumber fStation,
				       Int_t fOctant, Int_t fHalfOctant,
				       RPCFULLGEOM::RadialSegment fRadSeg,
				       Int_t fChannel)
{    
  if(fStation==RPCFULLGEOM::Station2) {
    /*cout << "RPCFULLGEOM: Station does not exist for this GEOMETRY" << endl;*/ return -9999; }

  Double_t fAngle=-fOctant*45.*RPCFULLGEOM::DEG_TO_RAD;

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

double RPCFULLGEOM::getStartPositionY(RPCFULLGEOM::ArmNumber fArm,
				       RPCFULLGEOM::StationNumber fStation,
				       Int_t fOctant, Int_t fHalfOctant,
				       RPCFULLGEOM::RadialSegment fRadSeg,
				       Int_t fChannel)
{  
  if(fStation==RPCFULLGEOM::Station2) {
    /*cout << "RPCFULLGEOM: Station does not exist" << endl;*/ return -9999; }

  Double_t fAngle=-fOctant*45.*RPCFULLGEOM::DEG_TO_RAD;

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

double RPCFULLGEOM::getStartPositionZ(RPCFULLGEOM::ArmNumber fArm,
				       RPCFULLGEOM::StationNumber fStation,
				       RPCFULLGEOM::RadialSegment fRadSeg)
{
  Float_t fStart = LocalFramePosition_Z[fStation][fRadSeg];
  fStart += GlobalFramePosition_Z[fStation][fArm];
  
  return fStart;
}

PHPoint RPCFULLGEOM::getEndPosition(RPCFULLGEOM::ArmNumber fArm,
				     RPCFULLGEOM::StationNumber fStation,
				     Int_t fOctant, Int_t fHalfOctant,
				     RPCFULLGEOM::RadialSegment fRadSeg,
				     Int_t fChannel)
{
  PHPoint point;
  point.setX(getEndPositionX(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setY(getEndPositionY(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setZ(getEndPositionZ(fArm,fStation,fRadSeg));
  
  return point;
}

double RPCFULLGEOM::getEndPositionX(RPCFULLGEOM::ArmNumber fArm,
				     RPCFULLGEOM::StationNumber fStation,
				     Int_t fOctant, Int_t fHalfOctant,
				     RPCFULLGEOM::RadialSegment fRadSeg,
				     Int_t fChannel)
{    
  if(fStation==RPCFULLGEOM::Station2) {
    /*cout << "RPCFULLGEOM: Station does not exist for this PROTOTYPE" << endl;*/ return -9999; }

  Float_t fStart = getStartPositionX(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel);

  Float_t fThisLocalPosition_StripAngle = (fOctant*45)-LocalPosition_StripAngle;
  if(fHalfOctant==1) { fThisLocalPosition_StripAngle = (fOctant*45)+LocalPosition_StripAngle;; }
  
  return fStart-StripLength[fStation][fRadSeg][fChannel]*cos(DEG_TO_RAD*fThisLocalPosition_StripAngle);
}

double RPCFULLGEOM::getEndPositionY(RPCFULLGEOM::ArmNumber fArm,
				     RPCFULLGEOM::StationNumber fStation,
				     Int_t fOctant, Int_t fHalfOctant,
				     RPCFULLGEOM::RadialSegment fRadSeg,
				     Int_t fChannel)
{
  if(fStation==RPCFULLGEOM::Station2) {
    /*cout << "RPCFULLGEOM: Station does not exist" << endl;*/ return -9999; }

  Float_t fStart = getStartPositionY(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel);
  
  Float_t fThisLocalPosition_StripAngle = (fOctant*45)-LocalPosition_StripAngle;
  if(fHalfOctant==1) { fThisLocalPosition_StripAngle = (fOctant*45)+LocalPosition_StripAngle;; }
  
  return fStart-StripLength[fStation][fRadSeg][fChannel]*sin(DEG_TO_RAD*fThisLocalPosition_StripAngle);
}

double RPCFULLGEOM::getEndPositionZ(RPCFULLGEOM::ArmNumber fArm,
				     RPCFULLGEOM::StationNumber fStation,
				     RPCFULLGEOM::RadialSegment fRadSeg)
{
  return getStartPositionZ(fArm,fStation,fRadSeg);
}

PHPoint RPCFULLGEOM::GetBegin()
{
  PHPoint point;
  point.setX(getStartPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setY(getStartPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setZ(getStartPositionZ(fThisArm,fThisStation,fThisRadSeg));
  
  return point;
}

PHPoint RPCFULLGEOM::GetEnd()
{
  PHPoint point;
  point.setX(getEndPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setY(getEndPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setZ(getEndPositionZ(fThisArm,fThisStation,fThisRadSeg));
  
  return point;
}

PHPoint RPCFULLGEOM::GetMid()
{
  PHPoint point;
  point.setX(0.5*(getStartPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)+getEndPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)));
  point.setY(0.5*(getStartPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)+getEndPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)));
  point.setZ(0.5*(getStartPositionZ(fThisArm,fThisStation,fThisRadSeg)+getEndPositionZ(fThisArm,fThisStation,fThisRadSeg)));
  
  return point;
}

Bool_t RPCFULLGEOM::checkStrip()
{
  if(fThisArm >= int(RPCFULLGEOM::NumberOfArms))            { return false; }
  if(fThisStation >= int(RPCFULLGEOM::NumberOfStations))    { return false; }
  if(fThisOctant >= 8)                                       { return false; }
  if(fThisHalfOctant >= 2)                                   { return false; }
  if(fThisStation == RPCFULLGEOM::Station1) {
    if(fThisRadSeg  >= int(RPCFULLGEOM::NumberOfRadSegSt1)) { return false;} }
  if(fThisStation == RPCFULLGEOM::Station2) {
    if(fThisRadSeg  >= int(RPCFULLGEOM::NumberOfRadSegSt2)) { return false;} }
  if(fThisStation == RPCFULLGEOM::Station3) {
    if(fThisRadSeg  >= int(RPCFULLGEOM::NumberOfRadSegSt3)) { return false;} }
  
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
