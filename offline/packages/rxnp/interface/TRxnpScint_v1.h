// $Id: TRxnpScint_v1.h,v 1.4 2006/12/08 20:21:36 zhangc Exp $

#ifndef _TRxnpScint_v1_h_
#define _TRxnpScint_v1_h_

/*!
	\file TRxnpScint_v1.h
	\brief interface object for each Rxnp Scintilator
	\author C. Zhang
	\version $Revision: 1.4 $
	\date		$Date: 2006/12/08 20:21:36 $
*/


#include "TRxnpScint.h"

/*! @ingroup interface */
//! Rpc raw hit interface object
class TRxnpScint_v1 : public TRxnpScint
{		
	
 public:	
  
  //! @name Constructors/Destructors
  //@{		
  
  //! Default constructor 
  TRxnpScint_v1();
  
  //! Contruct with key and location
  TRxnpScint_v1(const Key& key,
		UShort_t arm,
		UShort_t ring,
		UShort_t scint);

  //! Construct from pointer of base class
  TRxnpScint_v1(const TRxnpScint* base_ptr);

  //! Construct from reference of base class
  TRxnpScint_v1(const TRxnpScint& base_ref);

  //! Virtual destructor 
  virtual ~TRxnpScint_v1()
    {;}
  
  //@}
  
  //! @name locator
  //@{		
  
  //! Arm [0,1] 
  virtual UShort_t get_arm() const 
    {return _arm;}
  
  //! Ring [0,1] 
  virtual UShort_t get_ring() const 
    {return _ring;}	
  
  //! Scintilator [0,11]
  virtual UShort_t get_scint() const 
    {return _scint;}	
			
  //! Arm [0,1] 
  virtual void set_arm(UShort_t arm)
    {_arm = arm;}

  //! Ring [0,1] 
  virtual void set_ring(UShort_t ring)
    {_ring = ring;}							 	
	
  //! scintilator
  virtual void set_scint( UShort_t scint) 
    {_scint = scint;}	
	
  //@}

  //! @name functional interface
  //@{	
 
  // getter 
  //
 
  //! time
  virtual Float_t get_tof() const
    { return _tof; }
	
  //! error on time
  virtual Float_t get_tof_error() const
    { return _tof_error; }

  //! get energy from high gain
  virtual Float_t get_high_e() const
    {return _high_e; }

  //! get error on high gain energy
  virtual Float_t get_high_e_error() const
    { return _high_e_error; }

  //! get energy from low gain
  virtual Float_t get_low_e() const
    {return _low_e; }

  //! get error on low gain energy
  virtual Float_t get_low_e_error() const
    { return _low_e_error; }

  //! get nhits from high gain
  virtual Int_t get_high_nhits() const
    {return _high_nhits; }

  //! get error on high gain nhits
  virtual Int_t get_high_nhits_error() const
    { return _high_nhits_error; }

  //! get nhits from low gain
  virtual Int_t get_low_nhits() const
    {return _low_nhits; }

  //! get error on low gain nhits
  virtual Int_t get_low_nhits_error() const
    { return _low_nhits_error; }

  //setter
  //
  //! time
  virtual void set_tof(Float_t val) 
    { _tof = val; }
	
  //! error on time
  virtual void set_tof_error(Float_t val) 
    { _tof_error = val; }

  //! set energy from high gain
  virtual void set_high_e(Float_t val) 
    { _high_e = val; }

  //! set error on high gain energy
  virtual void set_high_e_error(Float_t val) 
    { _high_e_error = val; }

  //! set energy from low gain
  virtual void set_low_e(Float_t val) 
    { _low_e =val; }

  //! set error on low gain energy
  virtual void set_low_e_error(Float_t val) 
    { _low_e_error = val; }

  //! set nhits from high gain
  virtual void set_high_nhits(Int_t val) 
    { _high_nhits = val; }

  //! set error on high gain nhits
  virtual void set_high_nhits_error(Int_t val) 
    { _high_nhits_error = val; }

  //! set nhits from low gain
  virtual void set_low_nhits(Int_t val) 
    { _low_nhits = val; }

  //! set error on low gain nhits
  virtual void set_low_nhits_error(Int_t val) 
    { _low_nhits_error = val; }

  //@}

  //! @name Scintilator Status and Geometry
  //@{	
	
  //! Get the status word 
  virtual ULong_t get_status() const 
    { return _status;}
  
  //! set the statue word
  virtual void set_status(ULong_t status) 
    { _status = status;}
  
  //! Clear the status word 
  virtual void clear_status() 
    { _status = 0;}
  
  //! Get phi of the geometric center of the scintilator
  virtual Float_t get_phi() const
    {return _phi;}
  
  //! set phi of the  geometric center of the scintilator
  virtual void set_phi(Float_t phi) 
    { _phi = phi;}
  
  //! Get theta of the geometric center of the scintilator
  virtual Float_t get_theta() const
    {return _theta;}
  
  //! set theta of the geometric center of the scintilator
  virtual void set_theta(Float_t theta) 
    { _theta = theta;}
  
  //! Get <eta> of all the tracks which leave a hit in the scintilator
  virtual Float_t get_eta() const;
  
  //@}
  
  //! Print data members to ostream os, defaults to std::cout 
  virtual void print(std::ostream& os = std::cout) const; 
  

 private:

  UShort_t _arm;
  UShort_t _ring;
  UShort_t _scint;
        
  Float_t _tof;
  Float_t _tof_error;
  Float_t _high_e;
  Float_t _high_e_error;
  Float_t _low_e;
  Float_t _low_e_error;
  Int_t   _high_nhits;
  Int_t   _high_nhits_error;
  Int_t   _low_nhits;
  Int_t   _low_nhits_error;

  ULong_t _status;
  
  Float_t _phi;
  Float_t _theta;

  //! ROOT dictionary
  ClassDef(TRxnpScint_v1,1);
};

#endif
