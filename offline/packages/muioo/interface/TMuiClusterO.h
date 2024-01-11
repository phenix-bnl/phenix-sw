// $Id: TMuiClusterO.h,v 1.1 2006/04/22 01:58:27 hpereira Exp $
// Interface Object Class : TMuiClusterO
// Author: Jason Newby
// Data: 02/12/03
// Description: Class for muon identifier road

#ifndef _TMUICLUSTERO_H_
#define _TMUICLUSTERO_H_

// CINT compatible headers//
//
#include<PHKey.hh>
#include<PHPoint.h>
#include<PHLine.h>

#include "MUIOO.h"

/*! @ingroup interface */
//!  The MUID Cluster Interface Object 

/*! 
  <b>The MUID Cluster Interface Object </b><br>

  The MUID Cluster object presents an interface to
  some data members of old framework mui Cluster object.

  The purpose for having this interface object is the translate old frame 
  work Cluster road into a new framework object.
  <p>
*/

class TMuiClusterO : public PHKey {

public:

  //! @name Constructors/Destructors
  //@{  
  
  /*! Default constructor */
  TMuiClusterO(){;}  
  
  /*! Default destructor */  
  virtual ~TMuiClusterO(){;}
  
  /*! Default constructor */  
  TMuiClusterO(const Key& key) :
    PHKey(key){;}
  
  /*! Construct with key and location */
  TMuiClusterO(const Key&,
	       UShort_t arm,
	       UShort_t plane,
	       UShort_t panel,
	       UShort_t orientation,
	       UShort_t index){;}
  
  //! @name Functional Interface
  //@{

  /*! size */
  virtual void set_size(UShort_t size) 
	{}
	
  /*! CentroidPos */
  virtual void set_centroidpos(const PHPoint& point) 
	{}
	
  /*! CentroidPos */
  virtual void set_centroidsigma(const PHPoint& point) 
	{}
  
  /*! size */
  virtual UShort_t get_size() const 
	{return 0;}
  
	/*! CentroidPos */
  virtual PHPoint get_centroidpos() const 
	{return PHPoint();}
  
	/*! 
		CentroidSigma, in local coordinates. 
		For horizontal tubes, x is the tube length, y is the error perp to the tube
		and z is the tube width / sqrt(12). For vertical tubes x is the error perp to the tube, 
		y is the tube length and z the tube width / sqrt(12)
	*/
  virtual PHPoint get_centroidsigma() const 
	{return PHPoint();}

	/*! returns cluster mean z */
  virtual double get_mean_z() const
	{ return 0.5*(get_coord_begin().getZ() + get_coord_end().getZ() ); }
	
	/*! returns distance to origin perp to the cluster */
	virtual double get_w_absolute() const;

  /*! Coordinate error, along w */
  virtual double get_error() const;
	
  /*! Set coordinate */
  virtual void set_coord(const PHLine& line) 
	{}
	
  /*! Set end point */
  virtual void set_coord_end(const PHPoint& point) 
	{}
	
  /*! Set begin point */
  virtual void set_coord_begin(const PHPoint& point) 
	{}

  /*! Return coordinate */
  virtual PHLine get_coord() const
	{return PHLine();}

  /*! Return end point */
  virtual PHPoint get_coord_end() const 
	{ return PHPoint();}
	
  /*! Return begin point */
  virtual PHPoint get_coord_begin() const 
	{ return PHPoint();}

  //@}

  //! @name Locators
  //@{
  /*! Arm [0,1] */
  virtual void set_arm( UShort_t arm) 
	{}
	
  /*! Plane [0,4] */
  virtual void set_plane( UShort_t plane) 
	{}
	
  /*! Panel [0,5] */
  virtual void set_panel( UShort_t panel) 
	{}
	
  /*! Orientation[0,1] */
  virtual void set_orientation( UShort_t orientation) 
	{}

  /*! Arm [0,1] */
  virtual UShort_t get_arm() const 
	{return 0;}
	
  /*! Plane [0,4] */
  virtual UShort_t get_plane() const 
	{return 0;}
	
  /*! Panel [0,5] */
  virtual UShort_t get_panel() const 
	{return 0;}
	
  /*! Orientation[0,1] */
  virtual UShort_t get_orientation() const 
	{return 0;}
	
	#ifndef __CINT__  
  /*! Returns a boost tuple of (arm, plane, panel, orientation) */
  virtual MUIOO::panel_orient_locator get_location() const 
	{ return boost::make_tuple( get_arm(), get_plane(), get_panel(), get_orientation() ); }
	#endif  
  
	/*! Index */
  virtual UShort_t get_index() const 
	{return 0;}

  //@}

  //! @name Dumpers
  //@{  

  virtual void print(std::ostream& os = std::cout) const 
	{}

  //@}

  ClassDef(TMuiClusterO,1)

};

#endif /* _TMUICLUSTERO_H_ */
