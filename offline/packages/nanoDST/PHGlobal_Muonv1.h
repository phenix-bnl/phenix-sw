// $Id: PHGlobal_Muonv1.h,v 1.3 2012/09/12 07:23:12 bbannier Exp $
#ifndef PHGLOBAL_MUONV1_H
#define PHGLOBAL_MUONV1_H

/*!
  \file    PHGlobal_Muonv1.h
  \brief   Muon specific global variables
  \author  Hugo Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2012/09/12 07:23:12 $
*/

#include <iostream>
#include <vector>

#include "PHGlobal_Muon.h"

class PHGlobal_Muonv1: public PHGlobal_Muon 
{

public:
	
	//! constructor
  PHGlobal_Muonv1():
		_mutr_hits( MAX_ARM*MAX_STATION, -9999 ),
		_muid_hits( MAX_ARM*MAX_PLANE, -9999 )
	{}

	//! destructor
  virtual ~PHGlobal_Muonv1() 
	{}

	//! cloning 
  PHGlobal_Muonv1* clone() const 
	{ return new PHGlobal_Muonv1(*this); }
	
	//! reset method
  void Reset()
	{
  	_mutr_hits.assign( MAX_ARM*MAX_STATION, -9999 );
		_muid_hits.assign( MAX_ARM*MAX_PLANE, -9999 );
	}
	
	//! return 1 if valid
  int isValid() const;

  //! dump version
  void identify(std::ostream& os = std::cout) const;

	//! number of mutr hits/station
	int get_nMutrHits( const int& arm, const int& station ) const
	{ return _mutr_hits[_get_station_id( arm, station )]; }

	//! number of mutr hits/plane
  void set_nMutrHits(const int& val, const int& arm, const int& station)          
	{
		_mutr_hits[_get_station_id( arm, station )]=val; 
		return;
	}

	//! number of muid hits/plane
  int get_nMuidHits(const int& arm, const int& plane) const 
	{ return _muid_hits[_get_plane_id( arm, plane )]; }

	//! number of muid hits/plane
  void set_nMuidHits(const int& val, const int& arm, const int& plane)          
	{
		_muid_hits[_get_plane_id( arm, plane )]=val; 
		return;
	}


protected:
					    
	//! get unique id for muid plane
	static int _get_plane_id( const int& arm, const int& plane )
	{	return arm*MAX_PLANE + plane; }
	
	//! get unique id for mutr station
	static int _get_station_id( const int& arm, const int& station )
	{ return arm*MAX_STATION + station; }

	//! mutr hits/station
	std::vector<int> _mutr_hits;
	
	//! muid hits/plane
	std::vector<int> _muid_hits;

  ClassDef(PHGlobal_Muonv1,1)
};


#endif






