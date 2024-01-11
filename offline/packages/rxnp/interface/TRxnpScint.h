// $Id: TRxnpScint.h,v 1.4 2014/01/10 11:47:23 bbannier Exp $

#ifndef _TRxnpScint_h_
#define _TRxnpScint_h_

/*!
	\file TRxnpScint.h
	\brief interface object for each Rxnp Scintilator
	\author C. Zhang
	\version $Revision: 1.4 $
	\date		$Date: 2014/01/10 11:47:23 $
*/


#include <PHKey.hh>

/*! @ingroup interface */
//! Rpc raw hit interface object
class TRxnpScint : public PHKey
{		
	
 public:	
  
  //! @name Constructors/Destructors
  //@{		
  
  //! Default constructor 
  TRxnpScint()
    {}
  
  //! Construct with key and location 
  TRxnpScint(const Key& key) : PHKey(key) 
    {}
  
  //! Virtual destructor 
  virtual ~TRxnpScint()
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
 
  //! time
  virtual Float_t get_tof() const
    { return 0; }
	
  //! error on time
  virtual Float_t get_tof_error() const
    { return 0; }

  //! get energy from high gain
  virtual Float_t get_high_e() const
    {return 0; }

  //! get error on high gain energy
  virtual Float_t get_high_e_error() const
    { return 0; }

  //! get energy from low gain
  virtual Float_t get_low_e() const
    {return 0; }

  //! get error on low gain energy
  virtual Float_t get_low_e_error() const
    { return 0; }

  //! get nhits from high gain
  virtual Int_t get_high_nhits() const
    {return 0; }

  //! get error on high gain nhits
  virtual Int_t get_high_nhits_error() const
    { return 0; }

  //! get nhits from low gain
  virtual Int_t get_low_nhits() const
    {return 0; }

  //! get error on low gain nhits
  virtual Int_t get_low_nhits_error() const
    { return 0; }

  //setter
  //
  //! time
  virtual void set_tof(Float_t val) 
    { ; }
	
  //! error on time
  virtual void set_tof_error(Float_t val) 
    { ; }

  //! set energy from high gain
  virtual void set_high_e(Float_t val) 
    {; }

  //! set error on high gain energy
  virtual void set_high_e_error(Float_t val) 
    { ; }

  //! set energy from low gain
  virtual void set_low_e(Float_t val) 
    {; }

  //! set error on low gain energy
  virtual void set_low_e_error(Float_t val) 
    { ; }

  //! set nhits from high gain
  virtual void set_high_nhits(Int_t val) 
    {; }

  //! set error on high gain nhits
  virtual void set_high_nhits_error(Int_t val) 
    { ; }

  //! set nhits from low gain
  virtual void set_low_nhits(Int_t val) 
    {; }

  //! set error on low gain nhits
  virtual void set_low_nhits_error(Int_t val) 
    { ; }

  //@}

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
  ClassDef(TRxnpScint,1);
};

#endif
