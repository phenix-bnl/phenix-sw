#ifndef _RPCFINALGEOM_h_
#define _RPCFINALGEOM_h_

/*!
	\file RPCFINALGEOM.h
	\brief description of the full, final design, geometry
	\author Richard Hollis
*/

#include <PHPoint.h>

#include<TObject.h>

#include<iostream>
#include<string>

//! widely used utility functions and enumerations
class RPCFINALGEOM : public TObject
{
 public:
  
	//! module verbosity level
	enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};
 
	//! out of range number
        static const int OUTOFRANGE;

	//! convert rads into degrees
	static const Float_t RAD_TO_DEG;
	
	//! convert degrees into radians
	static const Float_t DEG_TO_RAD;
	
	//! number of arms - ONLY 1 in Run9 - kept for consistency
	static const int NumberOfArms = 2;
	
	//! number of stations - ONLY 2 in Run9 - kept for constistency
	static const int NumberOfStations = 3;
	
	//!number of radial segments RPC 1 - NO STATION 1
	static const int NumberOfRadSegSt1 = 2;

	//!number of radial segments RPC 2 - Run9 - STATION 2 is a STATION 3
	static const int NumberOfRadSegSt2 = 3;

	//!number of radial segments RPC 3
	static const int NumberOfRadSegSt3 = 3;

	//!maximum number of radial segments
	static const int MaxNumberOfRadSeg = 3;
	
	//! arm enumeration
	enum ArmNumber {South=0,North}; 
 
	//! station enumeration
	enum StationNumber {Station1=0,Station2,Station3};

	//! half-octant enumeration
	enum HalfOctantNumber {Right=0,Left};

	//! radial segment enumeration
	enum RadialSegment {SegmentA=0, SegmentB, SegmentC};
	
	//actual geometry

	//local geometrical description
	//-- This describes one half octant
	//-- Note that the half octants are mirror images of themselves
	//-- Octants are rotationally symmetric (not needed here, but later)
	
	//- Strip Angle = 11.25
	static const double LocalPosition_StripAngle;
	
	//- Local Positions: First Strip Positions, everything else is calculated from these
	static const double LocalPosition_X_FirstStripOuter[NumberOfStations][MaxNumberOfRadSeg];
	static const double LocalPosition_Y_FirstStripOuter[NumberOfStations][MaxNumberOfRadSeg];
	static const double LocalPosition_X_FirstStripInner[NumberOfStations][MaxNumberOfRadSeg];
	static const double LocalPosition_Y_FirstStripInner[NumberOfStations][MaxNumberOfRadSeg];

	//- Frame position describes the X,Y,Z position relative to the outer corner
	//- This is the right-angled corner
	static const double LocalFramePosition_X[NumberOfStations][MaxNumberOfRadSeg];
	static const double LocalFramePosition_Y[NumberOfStations][MaxNumberOfRadSeg];
	static const double LocalFramePosition_Z[NumberOfStations][MaxNumberOfRadSeg];

	//These are the strip numbers which are valid
	static const double FirstStripOuterNumber[NumberOfStations][MaxNumberOfRadSeg];
	static const double LastStripOuterNumber[NumberOfStations][MaxNumberOfRadSeg];
	static const double FirstStripInnerNumber[NumberOfStations][MaxNumberOfRadSeg];
	static const double LastStripInnerNumber[NumberOfStations][MaxNumberOfRadSeg];
	
	//This is a large array of the strip lengths
	static const double StripLength[NumberOfStations][MaxNumberOfRadSeg][64];
	static const double StripGap;
	
	//strip widths
	static const double StripWidth_Outer[NumberOfStations][MaxNumberOfRadSeg];
	static const double StripWidth_Inner[NumberOfStations][MaxNumberOfRadSeg];

	//Global Positions
	//This is the position of the right-angle corner in PHENIX coordinates
        static const double GlobalFramePosition_X[NumberOfStations][16];
        static const double GlobalFramePosition_Y[NumberOfStations][16];
        static const double GlobalFramePosition_Z[NumberOfStations][NumberOfArms];


  //constructor
  RPCFINALGEOM();
  RPCFINALGEOM(RPCFINALGEOM::ArmNumber fArm,
	       RPCFINALGEOM::StationNumber fStation,
	       Int_t fOctant,
	       Int_t fHalfOctant,
	       RPCFINALGEOM::RadialSegment fRadSeg,
	       Int_t fChannel);

  //destructor
  virtual ~RPCFINALGEOM(){}


	
	//! print a message to cout stream
	static void TRACE( const std::string& message)
	  { std::cout << "TRACE: " << message << std::endl; }	
	
	//! print a message and a value to cout stream
	static void TRACE( const std::string& message, const float& val )
	  { std::cout << "TRACE: " << message << "\t" << val << std::endl; }
	
	//! print a message (formated) to a stream
	static void PRINT(std::ostream& os = std::cout, const std::string& message = "");

	PHPoint getStartPosition(RPCFINALGEOM::ArmNumber fArm,
				 RPCFINALGEOM::StationNumber fStation,
				 Int_t fOctant, Int_t fHalfOctant,
				 RPCFINALGEOM::RadialSegment fRadSeg,
				 Int_t fChannel);
	double getStartPositionX(RPCFINALGEOM::ArmNumber fArm,
				 RPCFINALGEOM::StationNumber fStation,
				 Int_t fOctant, Int_t fHalfOctant,
				 RPCFINALGEOM::RadialSegment fRadSeg,
				 Int_t fChannel);
	double getStartPositionY(RPCFINALGEOM::ArmNumber fArm,
				 RPCFINALGEOM::StationNumber fStation,
				 Int_t fOctant, Int_t fHalfOctant,
				 RPCFINALGEOM::RadialSegment fRadSeg,
				 Int_t fChannel);
	double getStartPositionZ(RPCFINALGEOM::ArmNumber fArm,
				 RPCFINALGEOM::StationNumber fStation,
				 RPCFINALGEOM::RadialSegment fRadSeg);
	PHPoint getEndPosition(RPCFINALGEOM::ArmNumber fArm,
			       RPCFINALGEOM::StationNumber fStation,
			       Int_t fOctant, Int_t fHalfOctant,
			       RPCFINALGEOM::RadialSegment fRadSeg,
			       Int_t fChannel);
	double getEndPositionX(RPCFINALGEOM::ArmNumber fArm,
			       RPCFINALGEOM::StationNumber fStation,
			       Int_t fOctant, Int_t fHalfOctant,
			       RPCFINALGEOM::RadialSegment fRadSeg,
			       Int_t fChannel);
	double getEndPositionY(RPCFINALGEOM::ArmNumber fArm,
			       RPCFINALGEOM::StationNumber fStation,
			       Int_t fOctant, Int_t fHalfOctant,
			       RPCFINALGEOM::RadialSegment fRadSeg,
			       Int_t fChannel);
	double getEndPositionZ(RPCFINALGEOM::ArmNumber fArm,
			       RPCFINALGEOM::StationNumber fStation,
			       RPCFINALGEOM::RadialSegment fRadSeg);
	
	RPCFINALGEOM::ArmNumber fThisArm;
	RPCFINALGEOM::StationNumber fThisStation;
	Int_t fThisOctant;
	Int_t fThisHalfOctant;
	RPCFINALGEOM::RadialSegment fThisRadSeg;
	Int_t fThisChannel;
	
	PHPoint GetBegin();
	PHPoint GetEnd();
	PHPoint GetMid();

	Bool_t checkStrip();

#ifndef __CINT__
	
	//! square a number of a given type
	template < typename T >
	  static T SQUARE(const T& x)
	  { return x*x; } 
	
	//! return sign of a number: 0; +1 or -1
	template < typename T >
	  static int SIGN( const T& value ) 
	  {
	    if( !value ) return 0;
	    return (value > 0) ? 1:-1;
	  }
	
#endif
	
	ClassDef( RPCFINALGEOM,1 );
	
};
#endif
