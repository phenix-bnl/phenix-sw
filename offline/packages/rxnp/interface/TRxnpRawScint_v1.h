// $Id: TRxnpRawScint_v1.h,v 1.5 2007/03/08 23:11:20 zhangc Exp $

#ifndef _TRxnpRawScint_v1_h_
#define _TRxnpRawScint_v1_h_

/*!
	\file TRxnpRawScint_v1.h
	\brief interface object for each Rxnp Scintilator
	\author C. Zhang
	\version $Revision: 1.5 $
	\date		$Date: 2007/03/08 23:11:20 $
*/


#include "TRxnpRawScint.h"

/*! @ingroup interface */
//! Rpc raw hit interface object
class TRxnpRawScint_v1 : public TRxnpRawScint
{		
	
 public:	
  
  //! @name Constructors/Destructors
  //@{		
  
  //! Default constructor 
  TRxnpRawScint_v1();
  //! Contruct with key and location
  TRxnpRawScint_v1(const Key& key,
		   UShort_t arm,
		   UShort_t ring,
		   UShort_t scint);

  //! Construct from pointer of base class
  TRxnpRawScint_v1(const TRxnpRawScint* base_ptr);

  //! Construct from reference of base class
  TRxnpRawScint_v1(const TRxnpRawScint& base_ref);

  //! Virtual destructor 
  virtual ~TRxnpRawScint_v1()
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
  // the electronic channel id
  //
  virtual UShort_t get_chanid() const
    { return _chanid;} 
  //! high gain pre
  virtual Int_t get_high_pre() const
    { return _high_pre; }
	
  //! error on  high gain pre
  virtual Int_t get_high_pre_error() const
    { return _high_pre_error; }
	
  //! high gain post
  virtual Int_t get_high_post() const
    { return _high_post; }
	
  //! error on  high gain post
  virtual Int_t get_high_post_error() const
    { return _high_post_error; }
	
  //! low gain pre
  virtual Int_t get_low_pre() const
    { return _low_pre; }
	
  //! error on  low gain pre
  virtual Int_t get_low_pre_error() const
    { return _low_pre_error; }
	
  //! low gain post
  virtual Int_t get_low_post() const
    { return _low_post; }
	
  //! error on  low gain post
  virtual Int_t get_low_post_error() const
    { return _low_post_error; }
	
  //! time
  virtual Int_t get_tdc() const
    { return _tdc; }
	
  //! error on time
  virtual Int_t get_tdc_error() const
    { return _tdc_error; }
  //! get amu cells
  //
  virtual UShort_t get_amu_pre() const
    { return _amu_pre;}
  virtual UShort_t get_amu_post() const
    { return _amu_post;}
  virtual UShort_t get_amu_tdc() const
    { return _amu_tdc;}


  //setter
  //
  // the electronic channel id
  //
  virtual void set_chanid(UShort_t val)
    { _chanid = val;}

  //! high gain pre
  virtual void set_high_pre(Int_t val) 
    { _high_pre = val; }
	
  //! error on  high gain pre
  virtual void set_high_pre_error(Int_t val) 
    { _high_pre_error = val; }
	
  //! high gain post
  virtual void set_high_post(Int_t val) 
    { _high_post = val; }
	
  //! error on  high gain post
  virtual void set_high_post_error(Int_t val) 
    { _high_post_error = val; }
	
  //! low gain pre
  virtual void set_low_pre(Int_t val) 
    { _low_pre = val; }
	
  //! error on  low gain pre
  virtual void set_low_pre_error(Int_t val) 
    { _low_pre_error = val; }
	
  //! low gain post
  virtual void set_low_post(Int_t val) 
    { _low_post = val; }
	
  //! error on  low gain post
  virtual void set_low_post_error(Int_t val) 
    { _low_post_error = val; }
	
  //! time
  virtual void set_tdc(Int_t val) 
    { _tdc = val; }
	
  //! error on time
  virtual void set_tdc_error(Int_t val) 
    { _tdc_error = val; }

  //! set amu cell number
  //
  virtual void set_amu_pre(UShort_t val ) 
    {_amu_pre = val;}
  virtual void set_amu_post(UShort_t val) 
    {_amu_post = val;}
  virtual void set_amu_tdc(UShort_t val) 
    { _amu_tdc = val;}
  
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
        
  UShort_t _chanid;

  Int_t _high_pre;
  Int_t _high_pre_error;
  Int_t _high_post;
  Int_t _high_post_error;
  Int_t _low_pre;
  Int_t _low_pre_error;
  Int_t _low_post;
  Int_t _low_post_error;
  Int_t _tdc;
  Int_t _tdc_error;
  UShort_t _amu_pre;
  UShort_t _amu_post; 
  UShort_t _amu_tdc; 

  ULong_t _status;
  
  Float_t _phi;
  Float_t _theta;

  //! ROOT dictionary
  ClassDef(TRxnpRawScint_v1,1);
};

#endif
