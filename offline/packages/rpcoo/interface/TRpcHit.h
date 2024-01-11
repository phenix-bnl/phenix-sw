// $Id: TRpcHit.h,v 1.5 2014/01/26 17:43:23 bbannier Exp $

#ifndef _TRpcHit_h_
#define _TRpcHit_h_

/*!
	\file TRpcHit.h
	\brief Rpc raw hit interface object
	\author H. Pereira Da Costa
	\version $Revision: 1.5 $
	\date		$Date: 2014/01/26 17:43:23 $
*/


#include <PHKey.hh>

/*! @ingroup interface */
//! Rpc raw hit interface object
class TRpcHit : public PHKey
{		
	
	public:	

	//! @name Constructors/Destructors
	//@{		
			
	//! Default constructor 
	TRpcHit()
	{}

	//! Construct with key and location 
	TRpcHit(const Key& key) : PHKey(key) 
	{}

	//! Virtual destructor 
	virtual ~TRpcHit()
	{}

	//@}

	//! @name locator
	//@{		
	
	//! Arm [0,1] 
	virtual UShort_t get_arm() const 
	{return 0;}
	
	//! Station [0,2] 
	virtual UShort_t get_station() const 
	{return 0;}	
	
	//! Octant [0,7] 
	virtual UShort_t get_octant() const
	{return 0;}							 

	//! Half octant [0,1] 
	virtual UShort_t get_half_octant() const
	{return 0;}	

	//! Radial Segment (see RPCGEOM.h) 
	virtual UShort_t get_rseg() const
	{return 0;}	

	//! strip 
	virtual UShort_t get_strip() const 
	{return 0;}	
			
	//! Arm [0,1] 
	virtual void set_arm(UShort_t arm)
	{}

	//! Station [0,2] 
	virtual void set_station(UShort_t station)
	{}							 	
	
	//! Octant [0,7] 
	virtual void set_octant(UShort_t octant)
	{}							 

	//! Half octant [0,1] 
	virtual void set_half_octant(UShort_t halfoctant)
	{}		

	//! Radial Segment (see RPCGEOM.h) 
	virtual void set_rseg(UShort_t rseg)
	{}

	//! strip
        virtual void set_strip(UShort_t strip) {}

        //@}

	//! @name functional interface
	//@{	
	
	//! charge
	virtual Float_t get_q() const
	{ return 0; }
	
	//! error on charge 
	virtual Float_t get_q_error() const
	{ return 0; }
	
	//! time
	virtual Float_t get_t() const
	{ return 0; }
	
	//! error on time
	virtual Float_t get_t_error() const
	{ return 0; }

	//! charge 
	virtual void set_q(Float_t q)
	{}							 	

	//! error on charge 
	virtual void set_q_error(Float_t q_error)
	{}							 	
	
	//! time
	virtual void set_t( Float_t t )
	{}
	
	//! error on time
	virtual void set_t_error( Float_t t_error )
	{}

	//@}

	//! @name hit Status
	//@{	
	
	enum Status {
	  CALIBRATED = 0,
	  RAWTDC,
	  DEAD
	};  

	//! Get the status word 
	virtual ULong_t get_status() const 
	{ return 0;}
	
	//! Get the status word 
	virtual void set_status( TRpcHit::Status status )
	{}
	
	//! Clear the status word 
	virtual void clear_status() 
	{}
				
	//@}
			
	//! Print data members to ostream os, defaults to std::cout 
	virtual void print(std::ostream& os = std::cout) const 
	{} 

	//! ROOT dictionary
	ClassDef(TRpcHit,1)
};

#endif
