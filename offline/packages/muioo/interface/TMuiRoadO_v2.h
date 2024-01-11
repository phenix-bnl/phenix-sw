// $Id: TMuiRoadO_v2.h,v 1.1 2006/04/22 01:58:29 hpereira Exp $
// Interface Object Class : TMuiRoadO
// Author: Jason Newby
// Data: 02/12/03
// Description: Class for muon identifier road

#ifndef _TMUIROADO_V2_H_
#define _TMUIROADO_V2_H_

#include "TMuiRoadO.h"
#include <PHException.h>
#include <MUIOO.h>


class TMuiRoadO_v2 : public TMuiRoadO {

public:

  TMuiRoadO_v2();  

  virtual ~TMuiRoadO_v2(){;}

  TMuiRoadO_v2(const Key&,
		 UShort_t arm,
		 UShort_t index);  


  TMuiRoadO_v2(const TMuiRoadO*);  

  TMuiRoadO_v2(const TMuiRoadO&);  

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

  void set_pass2_gapbit( UShort_t value) { _pass2_gapbit=value;}

  void set_pass2_depth( UShort_t value) { _pass2_depth=value;}

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

  UShort_t get_pass2_gapbit() const 
  {return _pass2_gapbit;}

  UShort_t get_pass2_depth() const 
  {return _pass2_depth;}

  UShort_t get_group() const {return _group;}

  UShort_t get_golden() const {return _golden;}

  void set_arm( UShort_t arm) { _arm=arm;}

  void set_index( UShort_t index) { _index=index;}

  UShort_t get_arm() const {return _arm;}

  UShort_t get_index() const {return _index;}

  void print(std::ostream& os = std::cout) const;
  
private:
  
  //! road arm
  UShort_t _arm;
  
  //! road index
  UShort_t _index;
  
  //! road fit parameters
  TMutFitPar _fit_par;
  
  //! road depth
  UShort_t _depth;
  
  //! number of hits
  UShort_t _nhit;
  
  //! max number of hits/plane
  UShort_t _max_hit_plane;
  
  //! road chi_square
  Float_t _road_quality;
  
  //! road number of degrees of freedom
  UShort_t _freedom;
  
  //! road ghost flag
  UShort_t _ghost_flag;
  
  //! mMuiRoadFinder1 bit pattern
  UShort_t _gapbit;
  
  /*! \brief
    mMuiRoadFinderPass2 hit pattern.
    may have some clusters missing due to the fit rejection
  */
  UShort_t _pass2_gapbit;
  
  /*! \brief
    mMuiRoadFinderPass2 depth.
    must be less/equal to depth due to fit rejection
  */
  UShort_t _pass2_depth;
  
  //! group index
  UShort_t _group;
  
  //! set to 1 if road is flagged the best one in the group
  UShort_t _golden;
 
  ClassDef(TMuiRoadO_v2,1)
};

#endif 
