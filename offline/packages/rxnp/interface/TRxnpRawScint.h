// $Id: TRxnpRawScint.h,v 1.6 2014/01/10 11:47:23 bbannier Exp $

#ifndef _TRxnpRawScint_h_
#define _TRxnpRawScint_h_

/*!
	\file TRxnpRawScint.h
	\brief interface object for each Rxnp Scintilator
	\author C. Zhang
	\version $Revision: 1.6 $
	\date		$Date: 2014/01/10 11:47:23 $
*/


#include <PHKey.hh>

/*! @ingroup interface */
//! Rpc raw hit interface object
class TRxnpRawScint : public PHKey
{		
	
 public:	
  
  //! @name Constructors/Destructors
  //@{		
  
  //! Default constructor 
  TRxnpRawScint()
    {}
  
  //! Construct with key and location 
  TRxnpRawScint(const Key& key) : PHKey(key) 
    {}
  
  //! Virtual destructor 
  virtual ~TRxnpRawScint()
    {}
  
  //@}
  
  //! @name locator
  //@{		
  
  //! Arm [0,1] 
  virtual UShort_t get_arm() const 
    {return 0;}
  
  //! Ring [0,1] 
  virtual UShort_t get_ring() const 
    {return 0;}	
  
  //! Scintilator [0,11]
  virtual UShort_t get_scint() const 
    {return 0;}	
			
  //! Arm [0,1] 
  virtual void set_arm(UShort_t arm)
    {}

  //! Ring [0,1] 
  virtual void set_ring(UShort_t ring)
    {}

  //! scintilator
  virtual void set_scint( UShort_t scint) {}

  //@}

  //! @name functional interface
  //@{	
  // getter 
  //
 
  // the electronic channel id
  //
  virtual UShort_t get_chanid() const
    { return 0;}

  //! high gain pre
  virtual Int_t get_high_pre() const
    { return 0; }
	
  //! error on  high gain pre
  virtual Int_t get_high_pre_error() const
    { return 0; }
	
  //! high gain post
  virtual Int_t get_high_post() const
    { return 0; }
	
  //! error on  high gain post
  virtual Int_t get_high_post_error() const
    { return 0; }
	
  //! low gain pre
  virtual Int_t get_low_pre() const
    { return 0; }
	
  //! error on  low gain pre
  virtual Int_t get_low_pre_error() const
    { return 0; }
	
  //! low gain post
  virtual Int_t get_low_post() const
    { return 0; }
	
  //! error on  low gain post
  virtual Int_t get_low_post_error() const
    { return 0; }
	
  //! time
  virtual Int_t get_tdc() const
    { return 0; }
	
  //! error on time
  virtual Int_t get_tdc_error() const
    { return 0; }

  //! get amu cell number
  //
  virtual UShort_t get_amu_pre() const
    { return 0;}
  virtual UShort_t get_amu_post() const
    { return 0;}
  virtual UShort_t get_amu_tdc() const
    { return 0;}
  
  //setter
  //
  // the electronic channel id
  //
  virtual void set_chanid(UShort_t val)
    {;}

  //! high gain pre
  virtual void set_high_pre(Int_t val) 
    { ; }
	
  //! error on  high gain pre
  virtual void set_high_pre_error(Int_t val) 
    { ; }
	
  //! high gain post
  virtual void set_high_post(Int_t val) 
    { ; }
	
  //! error on  high gain post
  virtual void set_high_post_error(Int_t val) 
    { ; }
	
  //! low gain pre
  virtual void set_low_pre(Int_t val) 
    { ; }
	
  //! error on  low gain pre
  virtual void set_low_pre_error(Int_t val) 
    { ; }
	
  //! low gain post
  virtual void set_low_post(Int_t val) 
    { ; }
	
  //! error on  low gain post
  virtual void set_low_post_error(Int_t val) 
    { ; }
	
  //! time
  virtual void set_tdc(Int_t val) 
    { ; }
	
  //! error on time
  virtual void set_tdc_error(Int_t val) 
    { ; }

  //! set amu cell number
  //
  virtual void set_amu_pre(UShort_t val ) 
    {;}
  virtual void set_amu_post(UShort_t val) 
    {;}
  virtual void set_amu_tdc(UShort_t val) 
    { ;}


  //! @name Scintilator Status and Geometry
  //@{	
	
  //! Get the status word 
  virtual ULong_t get_status() const 
    { return 0;}
  
  //! set the statue word
  virtual void set_status(ULong_t status) 
    {;}
  
  //! Clear the status word 
  virtual void clear_status() 
    {}
  
  //! Get phi of the geometric center of the scintilator
  virtual Float_t get_phi() const
    {return 0;}
  
  //! set phi of the  geometric center of the scintilator
  virtual void set_phi(Float_t phi) 
    {;}
  
  //! Get theta of the geometric center of the scintilator
  virtual Float_t get_theta() const
    {return 0;}
  
  //! set theta of the geometric center of the scintilator
  virtual void set_theta(Float_t theta) 
    {;}
  
  //! Get <eta> of all the tracks which leave a hit in the scintilator
  virtual Float_t get_eta() const
    {return 0;}
  
  //@}
  
  //! Print data members to ostream os, defaults to std::cout 
  virtual void print(std::ostream& os = std::cout) const 
    {} 
  
  //! ROOT dictionary
  ClassDef(TRxnpRawScint,1);
};

#endif
