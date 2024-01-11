// $Id: TMui1DRoadO_v1.h,v 1.1 2006/04/22 01:58:27 hpereira Exp $

#ifndef _TMui1DRoadO_v1_h_
#define _TMui1DRoadO_v1_h_

/*!
	\file TMui1DRoadO_v1.h
	\brief Interface Object Class : TMui1DRoadO
	\author Jason Newby
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:58:27 $
*/

#include "TMui1DRoadO.h"
#include "MUIOO.h"

class TMui1DRoadO_v1 : public TMui1DRoadO {

public:

  TMui1DRoadO_v1();  

  virtual ~TMui1DRoadO_v1()
	{;}

  TMui1DRoadO_v1(const Key&,
		 UShort_t arm,
                 UShort_t panel,
                 UShort_t orientation,
		 UShort_t index);  


  TMui1DRoadO_v1(const TMui1DRoadO*);  

  TMui1DRoadO_v1(const TMui1DRoadO&);

  TMui1DRoadO_v1(const Key&, const TMui1DRoadO*);

  TMutFitPar get_fit_par() const 
	{return _fit_par;}

  PHPoint get_gap0_point() const 
	{ return PHPoint(_fit_par.get_x(),
						  _fit_par.get_y(),
						  _fit_par.get_z());}
  
  void set_fit_par(const TMutFitPar& fit_par) 
	{_fit_par = fit_par;}
  
  void set_depth( UShort_t depth) 
	{ _depth=depth;}

  void set_nhit( UShort_t nhit) 
	{ _nhit=nhit;}

  void set_max_hit_plane( UShort_t maxhit ) 
	{ _max_hit_plane=maxhit;}
  
  void set_road_quality( Float_t quality) 
	{ _road_quality=quality;}

  void set_ghost_flag( UShort_t ghostflag) 
	{ _ghost_flag=ghostflag;}

  void set_gapbit( UShort_t gapbit) 
	{ _gapbit=gapbit;}

  void set_freedom( UShort_t freedom) 
	{ _freedom=freedom;}

  void set_fitweight( UShort_t fitplane, Double_t fitweight)
	{
    BOUNDS_CHECK(fitplane,FITWEIGHT_SIZE);
    _fitweight[fitplane] = fitweight;
  }

  UShort_t get_depth() const 
	{ return _depth;}

  UShort_t get_nhit() const 
	{ return _nhit;}

  int get_numfired() const ;

  int get_numskipped() const;

  UShort_t get_max_hit_plane() const 
	{ return _max_hit_plane;}

  Float_t get_road_quality() const 
	{ return _road_quality;}

  UShort_t get_freedom() const 
	{ return _freedom;}

  Double_t get_fitweight(UShort_t fitplane) const
  {
    BOUNDS_CHECK(fitplane,FITWEIGHT_SIZE);
    return _fitweight[fitplane];
  }
  
  UShort_t get_ghost_flag() const 
	{return _ghost_flag;}

  UShort_t get_gapbit() const 
	{return _gapbit;}

  void set_arm( UShort_t arm) 
	{ _arm=arm;}

  void set_panel( UShort_t panel) 
	{ _panel=panel;}

  void set_orientation( UShort_t orientation) 
	{ _orientation=orientation;}
  
  void set_index( UShort_t index) 
	{ _index=index;}

  UShort_t get_arm() const 
	{return _arm;}

  UShort_t get_panel() const 
	{return _panel;}

  UShort_t get_orientation() const 
	{return _orientation;}
  
  UShort_t get_index() const 
	{return _index;}

  void print(std::ostream& os = std::cout) const 
	{
    MUIOO::PRINT(os,GetName());
    os << " arm: " << _arm << std::endl;
    os << " panel: " << _panel << std::endl;
    os << " orientation: " << _orientation << std::endl;
    os << " index: " << _index << std::endl;
    os << " depth: " << _depth << std::endl;
    os << " quality: " << _road_quality << std::endl;
    os << " freedom: " << _freedom << std::endl;
    os << " ghostflag: " << _ghost_flag << std::endl;
    os << " gapbit: " << _gapbit << std::endl;
    os << " nhit: " << _nhit << std::endl;
    os << " ref position: " << _fit_par.get_x() << " " <<
      _fit_par.get_y() << " " <<
      _fit_par.get_z() << " " << std::endl;
    os << " ref direction: " << _fit_par.get_dxdz() << " " <<
      _fit_par.get_dydz() << " "
      << std::endl;
MUIOO::PRINT(os,"**");
  }
  
private:
  
  UShort_t _arm;
  UShort_t _panel;
  UShort_t _orientation;
  UShort_t _index;
  
  TMutFitPar _fit_par;
  
  UShort_t _depth;
  UShort_t _nhit;
  UShort_t _max_hit_plane;
  Float_t _road_quality;
  UShort_t _freedom;
  UShort_t _ghost_flag;
  UShort_t _gapbit;
  enum { FITWEIGHT_SIZE=6};
  double _fitweight[FITWEIGHT_SIZE];
  
  ClassDef(TMui1DRoadO_v1,1)
};
#endif
