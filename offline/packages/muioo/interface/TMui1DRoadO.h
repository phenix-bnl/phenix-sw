// $Id: TMui1DRoadO.h,v 1.1 2006/04/22 01:58:27 hpereira Exp $

#ifndef _TMui1DRoadO_h_
#define _TMui1DRoadO_h_

/*!
	\file TMui1DRoadO.h
	\brief Interface Object Class : TMui1DRoadO
	\author Jason Newby
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:58:27 $
*/

// CINT compatible headers//
//
#include<TDataType.h>
#include<PHKey.hh>
#include<TMutFitPar.hh>
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

class TMui1DRoadO : public PHKey {

public:

  //! @name Constructors/Destructors
  //@{  
  
  /*! Default constructor */
  TMui1DRoadO()
	{}  
  
  /*! Default destructor */  
  virtual ~TMui1DRoadO()
	{}
  
  /*! Default constructor */  
  TMui1DRoadO(const Key& key) :
    PHKey(key)
	{}
  
  /*! Construct with key and location */
  TMui1DRoadO(const Key&,
	      UShort_t arm,
              UShort_t panel,
              UShort_t orientation,
	      UShort_t index)
	{}  
  //@}  

  //! @name Fit Parameters Interface
  //@{  

  /*! Read only reference to TMutFitPar */  
  virtual TMutFitPar get_fit_par() const 
	{ return TMutFitPar();}

  /*! TMutFitPar (track pars in geometric units) */
  virtual void set_fit_par(const TMutFitPar& fit_par) 
	{}

  //@}

  //! @name Functional Interface
  //@{
  
  /*! 
    Returns a PHPoint corresponding the intersection of the MUID road 
    and gap 0 of the MUID.
  */
  virtual PHPoint get_gap0_point() const 
	{ return PHPoint(); } 

  /*! Depth */
  virtual void set_depth( UShort_t depth) 
	{}

  /*! Number of hits */
  virtual void set_nhit( UShort_t nhit) 
	{}

  /*! Maximum hits per plane */
  virtual void set_max_hit_plane( UShort_t maxhit ) 
	{}

  /*! Road quality flag */
  virtual void set_road_quality( Float_t quality) 
	{}

  /*! Ghost flag */
  virtual void set_ghost_flag( UShort_t ghostflag) 
	{}

  /*! gap bit */
  virtual void set_gapbit( UShort_t gapbit) 
	{}

  /*! Freedom ? */
  virtual void set_freedom( UShort_t freedom) 
	{}

  /*! Fit Weight */
  virtual void set_fitweight( UShort_t fitplane, Double_t fitweight) 
	{}

  /*! Depth */
  virtual UShort_t get_depth() const 
	{ return 0;}

  /*! Number of hits */
  virtual UShort_t get_nhit() const 
	{return 0;}

  /*! Number of fired gaps */
  virtual int get_numfired() const
	{return 0;}

  /*! Number of skipped gaps */
  virtual int get_numskipped() const 
	{return 0;}

  /*! Maximum hits per plane */
  virtual UShort_t get_max_hit_plane() const 
	{return 0;}

  /*! Road quality flag */
  virtual Float_t get_road_quality() const 
	{return 0;}

  /*! Freedom ? */
  virtual UShort_t get_freedom() const 
	{return 0;}

  /*! Fit Weight */
  virtual Double_t get_fitweight(UShort_t fitplane) const 
	{return 0.0;}

  /*! Ghost flag */
  virtual UShort_t get_ghost_flag() const 
	{return 0;}

  /*! Gap bit */
  virtual UShort_t get_gapbit() const 
	{return 0;}

  //@}

  //! @name Locators
  //@{
  /*! Arm [0,1] */
  virtual void set_arm( UShort_t arm) 
	{}

  /*! Panel [0,5] */
  virtual void set_panel( UShort_t panel) 
	{}

  /*! Orientation[0,1] */
  virtual void set_orientation( UShort_t orientation) 
	{}
  
  /*! Index */
  virtual void set_index( UShort_t index) {}

  /*! Arm [0,1] */
  virtual UShort_t get_arm() const 
	{return 0;}

  /*! Panel [0,5] */
  virtual UShort_t get_panel() const 
	{return 0;}

  /*! Orientation[0,1] */
  virtual UShort_t get_orientation() const 
	{return 0;}
  
  /*! Index */
  virtual UShort_t get_index() const 
	{return 0;}

  //@}

  //! @name Dumpers
  //@{  

  virtual void print(std::ostream& os = std::cout) const 
	{}

  //@}

  ClassDef(TMui1DRoadO,1)

};

#endif
	      








