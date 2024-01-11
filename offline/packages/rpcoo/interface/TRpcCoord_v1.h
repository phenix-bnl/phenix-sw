// $Id: TRpcCoord_v1.h,v 1.2 2008/08/28 00:50:19 kempel Exp $

#ifndef _TRpcCoord_v1_h_
#define _TRpcCoord_v1_h_


/*!
	\file TRpcCoord_v1.h
	\brief Rpc coordinate interface object
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:50:19 $
*/

#include "TRpcCoord.h"
#include "RPCOO.h"

#include <cmath>

/*! @ingroup interface */
//! Rpc coordinate interface object.
/*!
	Rpc coordinate interface object.
	Coordinate position is accessed either via (theta,phi,z) or (x,y,z)
	Errors are set on x and y using a covariance matrix can be accessed
*/
class TRpcCoord_v1 : public TRpcCoord
{    
	
	public:  

  //! constructor 
  TRpcCoord_v1();

  //! constructor
  TRpcCoord_v1(const Key& key, UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t index );

  //! constructor
  TRpcCoord_v1(const TRpcCoord& ref );

  //! constructor
  TRpcCoord_v1(const TRpcCoord* ptr );

  //! Virtual destructor 
  virtual ~TRpcCoord_v1()
	{}

  //! @name Locators
  //@{  

  //! Arm [0,1] 
  virtual UShort_t  get_arm() const 
	{return _arm;}
  
  //! Station [0,2] 
  virtual UShort_t  get_station() const 
	{return _station;}	
  
	
  /*! Octant [0,7] */
  virtual UShort_t	get_octant() const 
	{return _octant;}
	
  /*! Half Octant [0,1] */
  virtual UShort_t	get_half_octant() const 
	{return _halfoctant;}
	
  /*! Radial Segment (see RPCGEOM.h) */
  virtual UShort_t	get_rseg() const 
	{return _rseg;}

  //! index 
  virtual UShort_t  get_index() const 
	{return _index;}	
	
  //! Arm [0,1] 
  virtual void  set_arm(UShort_t arm)
	{ _arm = arm; }

  //! Station [0,2] 
  virtual void  set_station(UShort_t station)
	{ _station = station; }   

  /*! Octant [0,7] */
  virtual void set_octant(UShort_t octant)
	{ _octant = octant; }	

  /*! Half Octant [0,1] */
  virtual void set_half_octant(UShort_t halfoctant)
	{ _halfoctant = halfoctant; }		

  /*! Radial Segment (see RPCGEOM.h) */
  virtual void set_rseg(UShort_t rseg)
	{ _rseg = rseg; }

  //! index
  virtual void  set_index(UShort_t index)
	{ _index = index; }   
  //@}  
	            	
  //! @name Functional Interface
  //@{    
  //! Peak strip number 
  virtual UShort_t get_peak_strip() const 
  {return _peak_strip;}
   
  //! Peak charge associated with this centroid fit 
  virtual Float_t get_q_peak() const 
  {return _q_peak;}
  
  //! Total charge associated with this centroid fit 
  virtual Float_t get_q_tot() const 
  {return _q_tot;}
   
  //! Error on total charge 
  virtual Float_t get_q_tot_error() const 
  {return _q_tot_error;}
  
  //! time associated with this centroid fit 
  virtual Float_t get_t() const 
  {return _t;}
   
  //! Error on centroid time 
  virtual Float_t get_t_error() const 
  {return _t_error; }

	//! position along x
	virtual Float_t get_x() const 
	{ return _x; }

	//! position along y
	virtual Float_t get_y() const 
	{ return _y; }

	//! position along z
	virtual Float_t get_z() const 
	{ return _z; }
	
	//! error on xy
	virtual Float_t get_covar( UShort_t i, UShort_t j ) const
	{ 
	  UShort_t index = i*COVAR_ROW + j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    return _covar[index];
	}
	
	//! position along theta
	virtual Float_t get_theta() const
	{ return std::atan2( Float_t(sqrt( RPCOO::SQUARE(_x) + RPCOO::SQUARE(_y))), _z ); }


	//! position along phi
	virtual Float_t get_phi() const
	{ return std::atan2( _y, _x ); }

	//! error along theta
	virtual Float_t get_theta_err() const 
	{ return 0; }
	
	//! error along phi
	virtual Float_t get_phi_err() const
	{ return 0; }
  
  //! Peak strip number 
  virtual void set_peak_strip(UShort_t strip) 
  { _peak_strip = strip; }
  
  //! Peak charge associated with this centroid fit 
  virtual void set_q_peak(Float_t q_peak) 
  { _q_peak = q_peak; }
  
  //! Total charge associated with this centroid fit 
  virtual void set_q_tot(Float_t q_tot) 
  { _q_tot = q_tot; }
  
  //! error on total charge 
  virtual void set_q_tot_error( Float_t value )
  { _q_tot_error = value; }
    
  //! time associated with this centroid fit 
  virtual void set_t(Float_t t) 
  { _t = t; }
  
  //! error on time associated with this centroid fit 
  virtual void set_t_error( Float_t value )
  { _t_error = value; }

	//! position along theta
	virtual void set_x( Float_t x ) 
	{ _x = x; }
	
	//! position along phi
	virtual void set_y( Float_t y ) 
	{ _y = y; }
	
	//! position along z
	virtual void set_z( Float_t z )
	{ _z = z; }
	
	//! error on xy
	virtual void set_covar( UShort_t i, UShort_t j, Float_t value )
	{
		UShort_t index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    _covar[index] = value;
	}
	
	//@}

  //! @name Dumpers
  //@{    
  //! Print data members to ostream os, defaults to std::cout 
  virtual void print(std::ostream& os = std::cout) const;
	//@}

	//! covariance matrix size
  enum { COVAR_ROW=2, COVAR_SIZE=4 };
	private:
	
	//! arm
	UShort_t _arm;
	
	//! station
	UShort_t _station;
	
	//! octant
	UShort_t _octant;

	//! half octant
	UShort_t _halfoctant;

	//! radial segment
	UShort_t _rseg;

	//! index
	UShort_t _index;		
			
	//! peak strip
	UShort_t _peak_strip;
	
	//! peak charge
	Float_t _q_peak;
	
	//! total charge
	Float_t _q_tot;
	
	//! error on total charge
	Float_t _q_tot_error;
	
	//! time
	Float_t _t;
	
	//! error on time
	Float_t _t_error;
	
	//! postion along x
	Float_t _x;
	
	//! position along y
	Float_t _y;
	
	//! position along z
	Float_t _z;
	
	//! covariance matrix
  Float_t _covar[COVAR_SIZE];

  ClassDef(TRpcCoord_v1,1)
};

#endif
