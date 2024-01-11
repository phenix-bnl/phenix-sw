/*!
  \file RPCPROTOGEOM.cxx
  \brief widely used utility functions and enumerations
  \author Richard Hollis, UCR (rhollis@ucr.edu)
*/
#include "RPCPROTOGEOM.h"

#include <cmath>
using namespace::std;

ClassImp( RPCPROTOGEOM )

const Float_t RPCPROTOGEOM::RAD_TO_DEG = 180.0/M_PI;
const Float_t RPCPROTOGEOM::DEG_TO_RAD = M_PI/180.0;
const int RPCPROTOGEOM::OUTOFRANGE = -999;

RPCPROTOGEOM::RPCPROTOGEOM()
{
  fThisArm = RPCPROTOGEOM::South;
  fThisStation = RPCPROTOGEOM::Station3;
  fThisOctant = 4;
  fThisHalfOctant = 0;
  fThisRadSeg = RPCPROTOGEOM::SegmentA;
  fThisChannel = 0;
}

RPCPROTOGEOM::RPCPROTOGEOM(RPCPROTOGEOM::ArmNumber fArm,
			   RPCPROTOGEOM::StationNumber fStation,
			   Int_t fOctant,
			   Int_t fHalfOctant,
			   RPCPROTOGEOM::RadialSegment fRadSeg,
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
const double RPCPROTOGEOM::LocalPosition_StripAngle = 11.25;

const double RPCPROTOGEOM::LocalPosition_X_FirstStripOuter[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{0.00,     0.00,     0.00},     //RPC1
   {0.00,     0.00,     0.00},      //RPC2 AS RPC3
   {0.00,     0.00,     0.00}};     //RPC3

const double RPCPROTOGEOM::LocalPosition_Y_FirstStripOuter[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{0,           0,        0},      //RPC1
   {5.17,     7.34,     8.40},      //RPC2 AS RPC3
   {5.17,     7.34,     8.40}};     //RPC3

const double RPCPROTOGEOM::LocalPosition_X_FirstStripInner[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{0.4,      0.65,     -9999},     //RPC1
   {56.04,    59.36,    59.14},     //RPC2 AS RPC3
   {56.04,    59.36,    59.14}};     //RPC3

const double RPCPROTOGEOM::LocalPosition_Y_FirstStripInner[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{0,           0,        0},      //RPC1
   {17.16,    15.93,    16.46},      //RPC2 AS RPC3
   {17.16,    15.93,    16.46}};     //RPC3


const double RPCPROTOGEOM::LocalFramePosition_X[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{              0.00,         0.00,  0.00},   //RPC1
   {(222),  (117),  5.00},   //RPC2
   {(222),  (117),  5.00}};  //RPC3

const double RPCPROTOGEOM::LocalFramePosition_Y[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{  0.00,  0.00,  0.00},  //RPC1
   {  5.00,  5.00,  5.00},  //RPC2
   {  5.00,  5.00,  5.00}}; //RPC3

const double RPCPROTOGEOM::LocalFramePosition_Z[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{  0.00,  0.00,  0.00},  //RPC1
   {  0.00,  0.00,  0.00},  //RPC2
   {  0.00,  0.00,  0.00}}; //RPC3


const double RPCPROTOGEOM::FirstStripOuterNumber[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{0,   0, -9999},    //RPC1
   {63, 63,  63},      //RPC2 AS RPC3
   {63, 63,  63}};     //RPC3

const double RPCPROTOGEOM::LastStripOuterNumber[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{ 0,  0, -9999},    //RPC1
   {35, 34,  35},      //RPC2 AS RPC3
   {35, 34,  35}};     //RPC3

const double RPCPROTOGEOM::FirstStripInnerNumber[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{ 0,  0, -9999},    //RPC1
   {8,  6,   5},      //RPC2 AS RPC3
   {8,  6,   5}};     //RPC3

const double RPCPROTOGEOM::LastStripInnerNumber[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{ 0,  0, -9999},    //RPC1
   {25, 26,  27},      //RPC2 AS RPC3
   {25, 26,  27}};     //RPC3

//This is a large array of the strip lengths
const double RPCPROTOGEOM::StripLength[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg][64] =
  {/* RPC STATION 1*/
    {/* Segment A */{ 0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00, 62.66, 62.66, 62.66, 62.66, 62.66, 62.66,
		     62.66, 62.66, 62.66, 62.66, 62.66, 62.66, 62.66,  0.00,
		      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00, 21.12, 36.89, 52.66, 68.43, 84.20,
		    121.54,124.43,124.43, 61.57, 61.57, 61.57, 61.57, 61.57,
		     61.57, 61.57, 61.57, 61.57, 61.57, 61.57, 61.57, 61.57,
		    123.43,123.43,121.54, 90.52, 73.45, 56.38, 39.32,  0.00},
    /* Segment B */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00, 69.28,
		     69.28, 69.28, 69.28, 69.28, 69.28, 69.28, 69.28, 69.28,
		     69.28, 69.28, 69.28, 69.28, 69.28, 69.28, 69.28, 69.28,
		     69.28, 69.28,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00, 40.20, 62.84, 85.48,122.47,125.91,
		     56.43, 56.43, 56.43, 56.43, 56.43, 56.43, 56.43, 56.43,
		     56.43, 56.43, 56.43, 56.43, 56.43, 56.43, 56.43, 56.43,
		     56.43, 56.43, 56.43,122.47,104.77, 80.27, 55.77, 31.27},
    /* Segment C */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00, 64.20, 64.20,
		     64.20, 64.20, 64.20, 64.20, 64.20, 64.20, 64.20, 64.20,
		     64.20, 64.20, 64.20, 64.20, 64.20, 64.20, 64.20, 64.20,
		     64.20, 64.20, 64.20, 64.20,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00, 52.07, 82.29,110.51, 59.85, 59.85,
		     59.85, 59.85, 59.85, 59.85, 59.85, 59.85, 59.85, 59.85,
		     59.85, 59.85, 59.85, 59.85, 59.85, 59.85, 59.85, 59.85,
		     59.85, 59.85, 59.85, 59.85,124.25,109.82, 78.19, 46.57}},
   {/* RPC STATION 2*/
    /* Segment A */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		     50.87, 50.87, 50.87, 50.87, 50.87, 50.87, 50.87, 50.87,
		     50.87, 50.87, 50.87, 50.87, 50.87, 50.87, 50.87, 50.87,
		     50.87, 50.87,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  8.64, 26.87, 45.11, 63.34, 81.57,
		     56.93, 56.93, 56.93, 56.93, 56.93, 56.93, 56.93, 56.93,
		     56.93, 56.93, 56.93, 56.93, 56.93, 56.93, 56.93, 56.93,
		     56.93, 56.93,105.82,105.44, 85.70, 65.97, 46.23, 26.50},
    /* Segment B */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00, 55.90, 55.90,
	             55.90, 55.90, 55.90, 55.90, 55.90, 55.90, 55.90, 55.90,
	             55.90, 55.90, 55.90, 55.90, 55.90, 55.90, 55.90, 55.90,
	             55.90, 55.90, 55.90,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00, 11.74, 36.18, 60.61, 85.05,109.50, 58.23,
	             58.23, 58.23, 58.23, 58.23, 58.23, 58.23, 58.23, 58.23,
	             58.23, 58.23, 58.23, 58.23, 58.23, 58.23, 58.23, 58.23,
	             58.23, 58.23, 58.23, 58.23,116.97, 90.05, 64.07, 37.62},
    /* Segment C */{  0.00,  0.00,  0.00,  0.00,  0.00, 62.38, 62.87, 62.87,
	             62.87, 62.87, 62.87, 62.87, 62.87, 62.87, 62.87, 62.87,
	             62.87, 62.87, 62.87, 62.87, 62.87, 62.87, 62.87, 62.87,
		     62.87, 62.87, 62.87, 62.87,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00, 15.15, 46.41, 77.66, 60.10, 60.10,
	             60.10, 60.10, 60.10, 60.10, 60.10, 60.10, 60.10, 60.10,
	             60.10, 60.10, 60.10, 60.10, 60.10, 60.10, 60.10, 60.10,
	             60.10, 60.10, 60.10, 60.10, 60.10,110.72, 76.89, 43.06}},
   {/* RPC STATION 3*/
    /* Segment A */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		     50.87, 50.87, 50.87, 50.87, 50.87, 50.87, 50.87, 50.87,
		     50.87, 50.87, 50.87, 50.87, 50.87, 50.87, 50.87, 50.87,
		     50.87, 50.87,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00,  8.64, 26.87, 45.11, 63.34, 81.57,
		     56.93, 56.93, 56.93, 56.93, 56.93, 56.93, 56.93, 56.93,
		     56.93, 56.93, 56.93, 56.93, 56.93, 56.93, 56.93, 56.93,
		     56.93, 56.93,105.82,105.44, 85.70, 65.97, 46.23, 26.50},
    /* Segment B */{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00, 55.90, 55.90,
	             55.90, 55.90, 55.90, 55.90, 55.90, 55.90, 55.90, 55.90,
	             55.90, 55.90, 55.90, 55.90, 55.90, 55.90, 55.90, 55.90,
	             55.90, 55.90, 55.90,  0.00,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00, 11.74, 36.18, 60.61, 85.05,109.50, 58.23,
	             58.23, 58.23, 58.23, 58.23, 58.23, 58.23, 58.23, 58.23,
	             58.23, 58.23, 58.23, 58.23, 58.23, 58.23, 58.23, 58.23,
		     58.23, 58.23, 58.23, 58.23,116.97, 90.05, 64.07, 37.62},
    /* Segment C */{  0.00,  0.00,  0.00,  0.00,  0.00, 62.38, 62.87, 62.87,
	             62.87, 62.87, 62.87, 62.87, 62.87, 62.87, 62.87, 62.87,
	             62.87, 62.87, 62.87, 62.87, 62.87, 62.87, 62.87, 62.87,
		     62.87, 62.87, 62.87, 62.87,  0.00,  0.00,  0.00,  0.00,
		      0.00,  0.00,  0.00, 15.15, 46.41, 77.66, 60.10, 60.10,
	             60.10, 60.10, 60.10, 60.10, 60.10, 60.10, 60.10, 60.10,
	             60.10, 60.10, 60.10, 60.10, 60.10, 60.10, 60.10, 60.10,
		     60.10, 60.10, 60.10, 60.10, 60.10,110.72, 76.89, 43.06}}};

const double RPCPROTOGEOM::StripGap = 0.2;
//strip widths
const double RPCPROTOGEOM::StripWidth_Outer[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{3   ,  3   ,  3},        //RPC1
   {3.65,  4.96,  6.40},     //RPC2 AS RPC3
   {3.65,  4.96,  6.40}};    //RPC3

const double RPCPROTOGEOM::StripWidth_Inner[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::MaxNumberOfRadSeg] =
  {{3   ,  3   ,  3},        //RPC1
   {3.65,  4.96,  6.40},     //RPC2 AS RPC3
   {3.65,  4.96,  6.40}};    //RPC3

//Global Positions
//This is the position of the right-angle corner in PHENIX coordinates
const double RPCPROTOGEOM::GlobalFramePosition_X[RPCPROTOGEOM::NumberOfStations][16] =
  {{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,   //RPC1
      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00},  //RPC1
   {  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,   //RPC2
    -450.85,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00},  //RPC2
   {  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,   //RPC3
    -491.03,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00}}; //RPC3

const double RPCPROTOGEOM::GlobalFramePosition_Y[RPCPROTOGEOM::NumberOfStations][16] =
  {{  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,   //RPC1
      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00},  //RPC1
   {  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,   //RPC2 
      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00},  //RPC2
   {  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,   //RPC3
      0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00}}; //RPC3

const double RPCPROTOGEOM::GlobalFramePosition_Z[RPCPROTOGEOM::NumberOfStations][RPCPROTOGEOM::NumberOfArms] =
  {{0.00, 0.00},    //RPC1
   {-690.19, 0.00}, //RPC2
   {-906.30,0.00}}; //RPC3
	

//_______________________________________________
void RPCPROTOGEOM::PRINT(std::ostream& os, const std::string& message){
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

PHPoint RPCPROTOGEOM::getStartPosition(RPCPROTOGEOM::ArmNumber fArm,
				       RPCPROTOGEOM::StationNumber fStation,
				       Int_t fOctant, Int_t fHalfOctant,
				       RPCPROTOGEOM::RadialSegment fRadSeg,
				       Int_t fChannel)
{
  PHPoint point;
  point.setX(getStartPositionX(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setY(getStartPositionY(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setZ(getStartPositionZ(fArm,fStation,fRadSeg));
  
  return point;
}

double RPCPROTOGEOM::getStartPositionX(RPCPROTOGEOM::ArmNumber fArm,
				       RPCPROTOGEOM::StationNumber fStation,
				       Int_t fOctant, Int_t fHalfOctant,
				       RPCPROTOGEOM::RadialSegment fRadSeg,
				       Int_t fChannel)
{
  //-------------------------------------------------------------
  //- For now only input what's in the tunnel
  //-------------------------------------------------------------
  
  if(fArm!=RPCPROTOGEOM::South) {
    /*cout << "RPCPROTOGEOM: Arm does not exist for this PROTOTYPE" << endl;*/ return -9999.; }
  
  if(fStation==RPCPROTOGEOM::Station1) {
    /*cout << "RPCPROTOGEOM: Station does not exist for this PROTOTYPE" << endl;*/ return -9999; }

  Float_t fStart = LocalFramePosition_X[fStation][fRadSeg];
  fStart += GlobalFramePosition_X[fStation][fOctant*2+fHalfOctant];
  
  if(fChannel>=32) {
    return fStart+LocalPosition_X_FirstStripOuter[fStation][fRadSeg]; }
  else {
    return fStart+LocalPosition_X_FirstStripInner[fStation][fRadSeg]; }
}

double RPCPROTOGEOM::getStartPositionY(RPCPROTOGEOM::ArmNumber fArm,
				       RPCPROTOGEOM::StationNumber fStation,
				       Int_t fOctant, Int_t fHalfOctant,
				       RPCPROTOGEOM::RadialSegment fRadSeg,
				       Int_t fChannel)
{
  //-------------------------------------------------------------
  //- For now only input what's in the tunnel
  //-------------------------------------------------------------
  
  if(fArm!=RPCPROTOGEOM::South) {
    /*cout << "RPCPROTOGEOM: Arm does not exist" << endl;*/ return -9999.; }
  
  if(fStation==RPCPROTOGEOM::Station1) {
    /*cout << "RPCPROTOGEOM: Station does not exist" << endl;*/ return -9999; }

  Float_t fStart = LocalFramePosition_Y[fStation][fRadSeg];
  fStart += GlobalFramePosition_Y[fStation][fOctant*2+fHalfOctant];

  if(fChannel>=32) {
    return fStart+LocalPosition_Y_FirstStripOuter[fStation][fRadSeg]+
      (StripGap+StripWidth_Outer[fStation][fRadSeg])*
      (FirstStripOuterNumber[fStation][fRadSeg]-fChannel); }
  else {
    return fStart+LocalPosition_Y_FirstStripInner[fStation][fRadSeg]+
      (StripGap+StripWidth_Inner[fStation][fRadSeg])*
      (fChannel-FirstStripInnerNumber[fStation][fRadSeg]); }
}

double RPCPROTOGEOM::getStartPositionZ(RPCPROTOGEOM::ArmNumber fArm,
				       RPCPROTOGEOM::StationNumber fStation,
				       RPCPROTOGEOM::RadialSegment fRadSeg)
{
  Float_t fStart = LocalFramePosition_Z[fStation][fRadSeg];
  fStart += GlobalFramePosition_Z[fStation][fArm];
  
  return fStart;
}

PHPoint RPCPROTOGEOM::getEndPosition(RPCPROTOGEOM::ArmNumber fArm,
				     RPCPROTOGEOM::StationNumber fStation,
				     Int_t fOctant, Int_t fHalfOctant,
				     RPCPROTOGEOM::RadialSegment fRadSeg,
				     Int_t fChannel)
{
  PHPoint point;
  point.setX(getEndPositionX(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setY(getEndPositionY(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel));
  point.setZ(getEndPositionZ(fArm,fStation,fRadSeg));
  
  return point;
}

double RPCPROTOGEOM::getEndPositionX(RPCPROTOGEOM::ArmNumber fArm,
				     RPCPROTOGEOM::StationNumber fStation,
				     Int_t fOctant, Int_t fHalfOctant,
				     RPCPROTOGEOM::RadialSegment fRadSeg,
				     Int_t fChannel)
{
  //-------------------------------------------------------------
  //- For now only input what's in the tunnel
  //-------------------------------------------------------------
  
  if(fArm!=RPCPROTOGEOM::South) {
    /*cout << "RPCPROTOGEOM: Arm does not exist for this PROTOTYPE" << endl;*/ return -9999.; }
  
  if(fStation==RPCPROTOGEOM::Station1) {
    /*cout << "RPCPROTOGEOM: Station does not exist for this PROTOTYPE" << endl;*/ return -9999; }

  Float_t fStart = getStartPositionX(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel);
  return fStart+StripLength[fStation][fRadSeg][fChannel]*cos(DEG_TO_RAD*LocalPosition_StripAngle);
}

double RPCPROTOGEOM::getEndPositionY(RPCPROTOGEOM::ArmNumber fArm,
				     RPCPROTOGEOM::StationNumber fStation,
				     Int_t fOctant, Int_t fHalfOctant,
				     RPCPROTOGEOM::RadialSegment fRadSeg,
				     Int_t fChannel)
{
  //-------------------------------------------------------------
  //- For now only input what's in the tunnel
  //-------------------------------------------------------------
  
  if(fArm!=RPCPROTOGEOM::South) {
    /*cout << "RPCPROTOGEOM: Arm does not exist" << endl;*/ return -9999.; }
  
  if(fStation==RPCPROTOGEOM::Station1) {
    /*cout << "RPCPROTOGEOM: Station does not exist" << endl;*/ return -9999; }

  Float_t fStart = getStartPositionY(fArm,fStation,fOctant,fHalfOctant,fRadSeg,fChannel);
  return fStart-StripLength[fStation][fRadSeg][fChannel]*sin(DEG_TO_RAD*LocalPosition_StripAngle);
}

double RPCPROTOGEOM::getEndPositionZ(RPCPROTOGEOM::ArmNumber fArm,
				     RPCPROTOGEOM::StationNumber fStation,
				     RPCPROTOGEOM::RadialSegment fRadSeg)
{
  return getStartPositionZ(fArm,fStation,fRadSeg);
}

PHPoint RPCPROTOGEOM::GetBegin()
{
  PHPoint point;
  point.setX(getStartPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setY(getStartPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setZ(getStartPositionZ(fThisArm,fThisStation,fThisRadSeg));
  
  return point;
}

PHPoint RPCPROTOGEOM::GetEnd()
{
  PHPoint point;
  point.setX(getEndPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setY(getEndPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel));
  point.setZ(getEndPositionZ(fThisArm,fThisStation,fThisRadSeg));
  
  return point;
}

PHPoint RPCPROTOGEOM::GetMid()
{
  PHPoint point;
  point.setX(0.5*(getStartPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)+getEndPositionX(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)));
  point.setY(0.5*(getStartPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)+getEndPositionY(fThisArm,fThisStation,fThisOctant,fThisHalfOctant,fThisRadSeg,fThisChannel)));
  point.setZ(0.5*(getStartPositionZ(fThisArm,fThisStation,fThisRadSeg)+getEndPositionZ(fThisArm,fThisStation,fThisRadSeg)));
  
  return point;
}

Bool_t RPCPROTOGEOM::checkStrip()
{
  //Global Cuts, true all the time
  if(fThisArm >= int(RPCPROTOGEOM::NumberOfArms))            { return false; }
  if(fThisStation >= int(RPCPROTOGEOM::NumberOfStations))    { return false; }
  if(fThisOctant >= 8)                                       { return false; }
  if(fThisHalfOctant >= 2)                                   { return false; }
  if(fThisStation == RPCPROTOGEOM::Station1) {
    if(fThisRadSeg  >= int(RPCPROTOGEOM::NumberOfRadSegSt1)) { return false;} }
  if(fThisStation == RPCPROTOGEOM::Station2) {
    if(fThisRadSeg  >= int(RPCPROTOGEOM::NumberOfRadSegSt2)) { return false;} }
  if(fThisStation == RPCPROTOGEOM::Station3) {
    if(fThisRadSeg  >= int(RPCPROTOGEOM::NumberOfRadSegSt3)) { return false;} }

  //Dataset specific, true for the proto
  if(fThisArm==North)        { return false; }
  if(fThisStation==Station1) { return false; }
  if(fThisOctant!=4)         { return false; }
  if(fThisHalfOctant==1)     { return false; }
  
  if(fThisChannel<32) {
    if(fThisChannel < FirstStripInnerNumber[fThisStation][fThisRadSeg] ||
       fThisChannel > LastStripInnerNumber[fThisStation][fThisRadSeg]) {
      return false;} }
  else {
    if(fThisChannel > FirstStripOuterNumber[fThisStation][fThisRadSeg] ||
       fThisChannel < LastStripOuterNumber[fThisStation][fThisRadSeg]) {
      return false;} }
  return true;
}
