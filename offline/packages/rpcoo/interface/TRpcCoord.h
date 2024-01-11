// $Id: TRpcCoord.h,v 1.3 2009/08/31 19:27:26 hpereira Exp $

#ifndef _TRpcCoord_h_
#define _TRpcCoord_h_


/*!
	\file TRpcCoord.h
	\brief Rpc coordinate interface object
	\author H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2009/08/31 19:27:26 $
*/

#include<PHKey.hh>
#include<PHPoint.h>
#include<PHException.h>

/*! @ingroup interface */
//! Rpc coordinate interface object.
/*!
	Rpc coordinate interface object.
	Coordinate position is accessed either via (theta,phi,z) or (x,y,z)
	Errors are set on x and y using a covariance matrix can be accessed
*/
class TRpcCoord : public PHKey
{    
	
	public:  

  //! Default constructor 
  TRpcCoord()
	{}

  //! Construct with key and location 
  TRpcCoord(const Key& key) : PHKey(key) 
	{}

  //! Virtual destructor 
  virtual ~TRpcCoord()
	{}

  //! @name Locators
  //@{  

  //! Arm [0,1] 
  virtual UShort_t  get_arm() const 
	{return 0;}
  
  //! Station [0,2] 
  virtual UShort_t  get_station() const 
	{return 0;}	
  	 
  //! Octant [0,7] 
  virtual UShort_t get_octant() const 
        {return 0;}
  
  //! Half Octant [0,1] 
  virtual UShort_t get_half_octant() const 
	{return 0;}
  
  //! Radial Segment (see RPCGEOM.h)
  virtual UShort_t get_rseg() const 
	{return 0;}

  //! index 
  virtual UShort_t  get_index() const 
	{return 0;}	
	
  //! Arm [0,1] 
  virtual void  set_arm(UShort_t arm)
	{}

  //! Station [0,2] 
  virtual void  set_station(UShort_t station)
	{}   
	  
  //! Octant [0,7] 
  virtual void set_octant(UShort_t octant) 
	{}
	
  //! Half Octant [0,1]
  virtual void set_half_octant(UShort_t half_octant) 
	{}
	
  //! Radial Segment (see RPCGEOM.h)
  virtual void set_rseg(UShort_t rseg) 
	{}

  //! index
  virtual void  set_index(UShort_t index)
	{}   
  //@}  
	            	
  //! @name Functional Interface
  //@{    
  //! Peak strip number 
  virtual UShort_t get_peak_strip() const 
  {return 0;}
   
  //! Peak charge associated with this centroid fit 
  virtual Float_t get_q_peak() const 
  {return 0;}
  
  //! Total charge associated with this centroid fit 
  virtual Float_t get_q_tot() const 
  {return 0;}
   
  //! Error on total charge 
  virtual Float_t get_q_tot_error() const 
  {return 0;}
  
  //! time associated with this centroid fit 
  virtual Float_t get_t() const 
  {return 0;}
   
  //! Error on centroid time 
  virtual Float_t get_t_error() const 
  {return 0; }

	//! position along x
	virtual Float_t get_x() const 
	{ return 0; }

	//! position along y
	virtual Float_t get_y() const 
	{ return 0; }

	//! position along z
	virtual Float_t get_z() const 
	{ return 0; }

	//! (x,y,z) position
	virtual PHPoint get_coord() const
	{ return PHPoint( get_x(), get_y(), get_z() ); }
	
	//! error on xy
	virtual Float_t get_covar( UShort_t i, UShort_t j ) const
	{ return 0; }
	
	//! position along theta
	virtual Float_t get_theta() const 
	{ return 0; }

	//! position along phi
	virtual Float_t get_phi() const 
	{ return 0; }

	//! error along theta
	virtual Float_t get_theta_err() const 
	{ return 0; }

	//! error along phi
	virtual Float_t get_phi_err() const 
	{ return 0; }
  
  //! Peak strip number 
  virtual void set_peak_strip(UShort_t strip) 
  {}
  
  //! Peak charge associated with this centroid fit 
  virtual void set_q_peak(Float_t q_peak) 
  {}
  
  //! Total charge associated with this centroid fit 
  virtual void set_q_tot(Float_t q_tot) 
  {}
  
  //! error on total charge 
  virtual void set_q_tot_error( Float_t value )
  {}
    
  //! time associated with this centroid fit 
  virtual void set_t(Float_t t) 
  {}
  
  //! error on time associated with this centroid fit 
  virtual void set_t_error( Float_t value )
  {}

	//! position along theta
	virtual void set_x( Float_t value ) 
	{}
	
	//! position along phi
	virtual void set_y( Float_t value ) 
	{}
	
	//! position along z
	virtual void set_z( Float_t value )
	{}
	
	//! position
	virtual void set_coord( const PHPoint& point )
	{ 
		set_x( point.getX() );
		set_y( point.getY() );
		set_z( point.getZ() );
	}
	
	//! error on xy
	virtual void set_covar( UShort_t i, UShort_t j, Float_t )
	{}
	
	//@}

  //! @name Dumpers
  //@{    
  //! Print data members to ostream os, defaults to std::cout 
  virtual void print(std::ostream& os = std::cout) const 
	{} 
	//@}

	//! covariance matrix size
  enum { COVAR_ROW=2, COVAR_SIZE=4 };

  ClassDef(TRpcCoord,1)
};

#endif
