// $Id: FVTXGEOM.h,v 1.16 2011/02/08 23:10:53 youzy Exp $

#ifndef _FVTXGEOM_h_
#define _FVTXGEOM_h_

/*!
	\file FVTXGEOM.h
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
	\version $Revision: 1.16 $
	\date $Date: 2011/02/08 23:10:53 $
*/

#include <string>
#include <iostream>
#include <TObject.h>

//! widely used utility functions and enumerations
class FVTXGEOM : public TObject
{
	public:

  #ifndef __CINT__
	//! shortcut for r window pair
	typedef std::pair<Float_t, Float_t> Window;
  #endif
  
	//! module verbosity level
	enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};
 
	//! convert rads into degrees
	static const Float_t RAD_TO_DEG;
	
	//! convert degrees into radians
	static const Float_t DEG_TO_RAD;
	
	//! number of arms
	enum { NumberOfArms = 2 };

        //! number of cages
        enum { NumberOfCages = 2 };
	
	//! number of stations
	enum { NumberOfStations = 4 };
	
	//! number of sectors
	enum { NumberOfSectors = 24 };
	
	//! number of columns
	enum { NumberOfColumns = 2 };

        //! number of strips   
        enum { NumberOfStripsSt1 = 640 };   
        enum { NumberOfStripsSt2 = 1664 }; 

        //! Number of wedges
        enum { NumberOfWedges = 48 };

        //! Number of "half" wedges
        enum { NumberOfHalfWedges = 96 };

	//! arm enumeration
	enum ArmNumber { South, North };

        //! cage enumeration
        enum CageNumber {Cage1, Cage2};
 
	//! station enumeration
	enum StationNumber {Station1,Station2,Station3,Station4};

	//! column enumeration
	enum ColumnNumber{ Column1, Column2 };

	//! print a message to cout stream
	static void TRACE( const std::string& message)
	{ std::cout << "TRACE: " << message << std::endl; }	
 
	//! print a message (formated) to a stream
	static void PRINT(std::ostream& os = std::cout, const std::string& message = "");

        //! translate arm, cage, station, sector, column to a corresponding half-wedge id
        static int get_halfwedge_id(const int arm, const int cage, const int station, const int sector, const int column);
	
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

    private:

        static int _config;
    
    ClassDef( FVTXGEOM,1 );
	
};
#endif
