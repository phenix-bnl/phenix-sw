// $Id: TMuiRoadO.h,v 1.1 2006/04/22 01:58:29 hpereira Exp $
// Interface Object Class : TMuiRoadO
// Author: Jason Newby
// Data: 02/12/03
// Description: Class for muon identifier road

#ifndef _TMUIROADO_H_
#define _TMUIROADO_H_

// CINT compatible headers//
//
#include<PHKey.hh>
#include<TMutFitPar.hh>
#include<MUIOO.h>
#include<PHPoint.h>

/*! @ingroup interface */
//!  The MUID Road Interface Object 

/*! 
  <b>The MUID Road Interface Object </b><br>

  The MUID road object presents an interface to a TMutFitPar object and
  some data members of old framework mui road object.

  The feature information of a mui road, such as direction (theta, phi),
  gap0 position (x, y), is stored in TMutFitPar. Others informations, such
  as road quality, are stored as seperated data member. 
  
  The purpose for having this interface object is the translate old frame 
  work MuId road into a new framework object.
  <p>
*/

class TMuiRoadO : public PHKey {

public:

  //! @name Constructors/Destructors
  //@{  
  
  /*! Default constructor */
  TMuiRoadO(){;}  
  
  /*! Default destructor */  
  virtual ~TMuiRoadO(){;}
  
  /*! Default constructor */  
  TMuiRoadO(const Key& key) :
    PHKey(key){;}
  
  /*! Construct with key and location */
  TMuiRoadO(const Key&,
	      UShort_t arm,
	      UShort_t index){;}
  
  //@}  

  //! @name Fit Parameters Interface
  //@{  

  /*! Read only reference to TMutFitPar */  
  virtual TMutFitPar get_fit_par() const { return TMutFitPar();}

  /*! const TMutFitPar */
  virtual const TMutFitPar* get_const_fitpar() const { return 0;}

  /*! TMutFitPar (track pars in geometric units) */
  virtual void set_fit_par(const TMutFitPar& fit_par) {}

  //@}

  //! @name Functional Interface
  //@{
  
  /*! 
    Returns a PHPoint corresponding the intersection of the MUID road 
    and gap 0 of the MUID.
  */
  virtual PHPoint get_gap0_point() const { return PHPoint(); } 

  /*! Depth */
  virtual void set_depth( UShort_t depth) {}

  /*! Number of hits */
  virtual void set_nhit( UShort_t nhit) {}

  /*! Maximum hits per plane */
  virtual void set_max_hit_plane( UShort_t maxhit ) {}

  /*! Road quality flag */
  virtual void set_road_quality( Float_t quality) {}

  /*! Ghost flag */
  virtual void set_ghost_flag( UShort_t ghostflag) {}

  /*! gap bit */
  virtual void set_gapbit( UShort_t gapbit) {}
  
  /*! second pass road finder gap bit */
  virtual void set_pass2_gapbit( UShort_t value) 
  {}
  
  /*! second pass road finder depth */
  virtual void set_pass2_depth( UShort_t value) 
  {}

  /*! group id */
  virtual void set_group( UShort_t group) {}

  /*! golden flag */
  virtual void set_golden( UShort_t golden) {}

  /*! Freedom ? */
  virtual void set_freedom( UShort_t freedom) {}

  /*! Depth */
  virtual UShort_t get_depth() const { return 0;}

  /*! Number of hits */
  virtual UShort_t get_nhit() const {return 0;}

  /*! Maximum hits per plane */
  virtual UShort_t get_max_hit_plane() const {return 0;}

  /*! Road quality flag */
  virtual Float_t get_road_quality() const {return 0;}

  /*! Freedom ? */
  virtual UShort_t get_freedom() const {return 0;}

  /*! Ghost flag */
  virtual UShort_t get_ghost_flag() const {return 0;}

  /*! Gap bit */
  virtual UShort_t get_gapbit() const {return 0;}

  /*! second pass road finder Gap bit */
  virtual UShort_t get_pass2_gapbit() const 
  {return 0;}

  /*! second pass road finder Depth */
  virtual UShort_t get_pass2_depth() const 
  { return 0;}

  /*! Group ID */
  virtual UShort_t get_group() const {return 0;}

  /*! Golden Flag */
  virtual UShort_t get_golden() const {return 0;}

  //@}

  //! @name Locators
  //@{
  /*! Arm [0,1] */
  virtual void set_arm( UShort_t arm) {}

  /*! Index */
  virtual void set_index( UShort_t index) {}

  /*! Arm [0,1] */
  virtual UShort_t get_arm() const {return 0;}

  /*! Index */
  virtual UShort_t get_index() const {return 0;}

  //@}

  //! @name Dumpers
  //@{  

  virtual void print(std::ostream& os = std::cout) const {}

  //@}

  ClassDef(TMuiRoadO,1)

};

#endif /* _TMUIROADO_H_ */
	      








