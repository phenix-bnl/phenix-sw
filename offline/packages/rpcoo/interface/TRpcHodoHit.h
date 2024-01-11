#ifndef _TRpcHodoHit_h_
#define _TRpcHodoHit_h_

/*!
	\file TRpcHodoHit.h
	\brief Rpc Hodoscope raw hit interface object
	\author R.S.Hollis
	\version $Revision: 1.2 $
	\date		$Date: 2014/01/26 17:43:23 $
*/


#include <PHKey.hh>

/*! @ingroup interface */
//! Rpc Hodoscope raw hit interface object
class TRpcHodoHit : public PHKey
{		
	
	public:	

	//! @name Constructors/Destructors
	//@{		
			
	//! Default constructor 
	TRpcHodoHit()
	{}

	//! Construct with key and location 
	TRpcHodoHit(const Key& key) : PHKey(key) 
	{}

	//! Virtual destructor 
	virtual ~TRpcHodoHit()
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
	
	//! strip 
	virtual UShort_t get_strip() const 
	{return 0;}	
			
	//! Arm [0,1] 
	virtual void set_arm(UShort_t arm)
	{}

	//! Station [0,2]
        virtual void set_station(UShort_t station) {}

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
	virtual void set_status( TRpcHodoHit::Status status )
	{}
	
	//! Clear the status word 
	virtual void clear_status() 
	{}
				
	//@}
			
	//! Print data members to ostream os, defaults to std::cout 
	virtual void print(std::ostream& os = std::cout) const 
	{} 

	//! ROOT dictionary
	ClassDef(TRpcHodoHit,1)
};

#endif
