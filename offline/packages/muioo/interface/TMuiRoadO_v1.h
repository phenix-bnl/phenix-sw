// $Id: TMuiRoadO_v1.h,v 1.1 2006/04/22 01:58:29 hpereira Exp $
// Interface Object Class : TMuiRoadO
// Author: Jason Newby
// Data: 02/12/03
// Description: Class for muon identifier road

#ifndef _TMUIROADO_V1_H_
#define _TMUIROADO_V1_H_

#include "TMuiRoadO.h"
#include <PHException.h>
#include <MUIOO.h>


class TMuiRoadO_v1 : public TMuiRoadO {

public:

  TMuiRoadO_v1();  

  virtual ~TMuiRoadO_v1(){;}

  TMuiRoadO_v1(const Key&,
		 UShort_t arm,
		 UShort_t index);  


  TMuiRoadO_v1(const TMuiRoadO*);  

  TMuiRoadO_v1(const TMuiRoadO&);  

  TMutFitPar get_fit_par() const {return _fit_par;}
  const TMutFitPar* get_const_fitpar() const {return &_fit_par;}


  PHPoint get_gap0_point() const;
  
  void set_fit_par(const TMutFitPar& fit_par) {_fit_par = fit_par;}
  
  void set_depth( UShort_t depth) { _depth=depth;}

  void set_nhit( UShort_t nhit) { _nhit=nhit;}

  void set_max_hit_plane( UShort_t maxhit ) { _max_hit_plane=maxhit;}
  
  void set_road_quality( Float_t quality) { _road_quality=quality;}

  void set_ghost_flag( UShort_t ghostflag) { _ghost_flag=ghostflag;}

  void set_gapbit( UShort_t gapbit) { _gapbit=gapbit;}

  void set_group( UShort_t group) { _group=group;}

  void set_golden( UShort_t golden) { _golden=golden;}
  
  void set_freedom( UShort_t freedom) { _freedom=freedom;}

  UShort_t get_depth() const { return _depth;}

  UShort_t get_nhit() const { return _nhit;}

  UShort_t get_max_hit_plane() const { return _max_hit_plane;}

  Float_t get_road_quality() const { return _road_quality;}

  UShort_t get_freedom() const { return _freedom;}

  UShort_t get_ghost_flag() const {return _ghost_flag;}

  UShort_t get_gapbit() const {return _gapbit;}

  UShort_t get_group() const {return _group;}

  UShort_t get_golden() const {return _golden;}

  void set_arm( UShort_t arm) { _arm=arm;}

  void set_index( UShort_t index) { _index=index;}

  UShort_t get_arm() const {return _arm;}

  UShort_t get_index() const {return _index;}

  void print(std::ostream& os = std::cout) const;
  
private:
  
  UShort_t _arm;
  UShort_t _index;
  
  TMutFitPar _fit_par;
  
  UShort_t _depth;
  UShort_t _nhit;
  UShort_t _max_hit_plane;
  Float_t _road_quality;
  UShort_t _freedom;
  UShort_t _ghost_flag;
  UShort_t _gapbit;
  UShort_t _group;
  UShort_t _golden;
 
  ClassDef(TMuiRoadO_v1,1)
};

#endif 
