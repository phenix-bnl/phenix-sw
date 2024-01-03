// $Id: PHGlobal_Muon.h,v 1.6 2011/04/04 18:07:13 bbannier Exp $
#ifndef PHGLOBAL_MUON_H
#define PHGLOBAL_MUON_H

/*!
	\file		PHGlobal_Muon.h
	\brief	 Muon specific global variables
	\author	Hugo Pereira
	\version $Revision: 1.6 $
	\date		$Date: 2011/04/04 18:07:13 $
*/

#include "PHObject.h"
#include "phool.h"
#include <iostream>

//! muon specific global variables
/*!	Lightweight object to store muon global variables in nanoDST */
class PHGlobal_Muon: public PHObject
{

 public:

	//! array size enumeration
	enum
	{

			//! number of arms
			MAX_ARM = 2,

			//! number of mutr stations/arm
			MAX_STATION = 3,

			//! number of muid planes/arm
			MAX_PLANE = 5
		};

	//! destructor
	virtual ~PHGlobal_Muon()
		{}

	//! object reset
	virtual void Reset();

	//! returns
	virtual int isValid() const;

	//! identify object
	virtual void identify(std::ostream& os = std::cout) const;

	//! number of mutr hits per arm/station
	virtual int get_nMutrHits( const int& /*arm*/, const int& /*station*/) const
		{
			warning("get_nMutrHits");
			return -9999;
		}

	//! number of mutr hits per arm
	virtual int get_nMutrHits( const int& arm ) const
		{
			int out( 0 );
			for ( unsigned int station = 0; station < MAX_STATION; station++ )
			out += get_nMutrHits( arm, station );
			return out;
		}

	//! number of mutr hits per arm/station
	virtual void set_nMutrHits(const int& /*val*/, const int& /*arm*/, const int& /*station*/)
		{
			warning("set_nMutrHits");
			return ;
		}

	//! number of muid hits per arm/plane
	virtual int get_nMuidHits(const int& /*arm*/, const int& /*plane*/) const
		{
			warning("get_nMuidHits");
			return -9999;
		}

	//! total number of muid hits per arm
	virtual int get_nMuidHits(const int& arm) const
		{
			int out( 0 );
			for ( unsigned int plane = 0; plane < MAX_PLANE; plane++ )
			out += get_nMuidHits( arm, plane );
			return out;
		}

	//! number of muid hits per arm/plane
	virtual void set_nMuidHits(const int& /*val*/, const int& /*arm*/, const int& /*plane*/)
		{
			warning("set_nMuidHits");
			return ;
		}

	//! disable warning log
	void ShutUp(const int i = 1);

	protected:

	//! constructor
	PHGlobal_Muon() {}

	private:

		//! warning message for unimplemented methods
		void warning(const char* field) const;

		ClassDef(PHGlobal_Muon, 1)
};


#endif






