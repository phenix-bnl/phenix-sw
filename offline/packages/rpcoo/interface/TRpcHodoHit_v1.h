

#ifndef _TRpcHodoHit_v1_h_
#define _TRpcHodoHit_v1_h_

/*!
	\file TRpcHodoHit_v1.h
	\brief Rpc Hodoscope raw hit interface object
	\author H. Pereira Da Costa
  \version $Revision: 1.1 $
  \date    $Date: 2012/02/13 02:53:10 $
*/


#include <TRpcHodoHit.h>

/*! @ingroup interface */
//! Rpc Hodoscope raw hit interface object
class TRpcHodoHit_v1 : public TRpcHodoHit
{    
	
	public:  

  //! @name Constructors/Destructors
  //@{    
			
  //! Default constructor 
  TRpcHodoHit_v1();
	
  //! Construct with key and location 
  TRpcHodoHit_v1(const Key& key, UShort_t arm, UShort_t station, UShort_t strip);

	//! constructor from base class
  TRpcHodoHit_v1(const TRpcHodoHit* base_ptr);
	
	//! constructor from base class
  TRpcHodoHit_v1(const TRpcHodoHit& base_ref);

  //! Virtual destructor 
  virtual ~TRpcHodoHit_v1()
	{}

	//@}

  //! @name locator
  //@{    
	
  //! Arm [0,1] 
  virtual UShort_t  get_arm() const 
	{return _arm;}
  
  //! Station [0,2] 
  virtual UShort_t  get_station() const 
	{return _station;}	
  
  //! strip 
  virtual UShort_t  get_strip() const 
	{return _strip;}	
			
  //! Arm [0,1] 
  virtual void  set_arm(UShort_t arm)
	{ _arm = arm;}

  //! Station [0,2] 
  virtual void  set_station(UShort_t station)
	{ _station = station; }               	
 
  //! strip 
  virtual void  set_strip( UShort_t strip)
	{ _strip = strip; }	
	
	//@}

  //! @name functional interface
  //@{  
	
	//! charge
	virtual Float_t get_q() const
	{ return _q; }
	
	//! error on charge 
	virtual Float_t get_q_error() const
	{ return _q_error; }		
	
	//! time
	virtual Float_t get_t() const
	{ return _t; }
	
	//! error on time
	virtual Float_t get_t_error() const
	{ return _t_error; }

  //! charge 
  virtual void  set_q(Float_t q)
	{ _q = q; }               	

  //! error on charge 
  virtual void  set_q_error(Float_t error )
	{ _q_error = error; }               	
	
	//! time
	virtual void set_t( Float_t t )
	{ _t = t; }
	
	//! error on time
	virtual void set_t_error( Float_t error )
	{ _t_error = error; }
			
  //! Print data members to ostream os, defaults to std::cout 
  virtual void print(std::ostream& os = std::cout) const;

	//@}
	
  //! @name hit Status
  //@{  
	
  //! Get the status word 
  virtual ULong_t get_status() const 
  { return _status;}
  
  //! Clear the status word 
  virtual void clear_status() 
  { _status = 0; }
				
  //@}
	
	private:
			
	UShort_t _arm;
	UShort_t _station;
	UShort_t _strip;
	
	ULong_t _status;
	
	Float_t _q;
	Float_t _q_error;
	Float_t _t;
	Float_t _t_error;	

	//! ROOT dictionary
  ClassDef(TRpcHodoHit_v1,1)
};

#endif
