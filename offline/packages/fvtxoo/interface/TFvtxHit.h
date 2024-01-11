#ifndef __TFvtxHit_h__
#define __TFvtxHit_h__

// $Id: TFvtxHit.h,v 1.10 2013/02/23 05:52:33 jinhuang Exp $

/*!
   \file TFvtxHit.h
   \brief The forward vertex hit object 
   \author Hugo Pereira Da Costa
   \version $Revision: 1.10 $
   \date $Date: 2013/02/23 05:52:33 $
*/

#include<PHKey.hh>

/*! @ingroup interface */
//! The forward vertex hit object 

class TFvtxHit : public PHKey 
{
  
  public:

  //! @name Constructors/Destructor
  //@{		
  //! Default
  TFvtxHit()
  {}

  //! Construct with key
  TFvtxHit(const Key& key) : PHKey(key)
  {}

  //! Default
  virtual ~TFvtxHit() 
  {}
  
  //!@}
  
  //! @name Locators
  //@{		
  //! Get arm number for this hit
  virtual unsigned short get_arm() const 
  {return 0;}
  
  //! Get cage number for this hit
  virtual unsigned short get_cage() const
  {return 0;}

  //! Get station number for this hit
  virtual unsigned short get_station() const 
  {return 0;}
  
  //! Get sector number for this hit
  virtual unsigned short get_sector() const 
  {return 0;}
  
  //! Get column number for this hit
  virtual unsigned short get_column() const
  { return 0; }
  
  //! Get strip associated with this hit
  virtual unsigned short get_strip() const 
  {return 0;}
  
  //! Set arm number for this hit
  virtual void set_arm(const unsigned short& ) 
  {}
  
  //! Set cage number for this hit
  virtual void set_cage(const unsigned short&)
  {}

  //! Set station number for this hit
  virtual void set_station(const unsigned short&) 
  {}
  
  //! Set sector number for this hit
  virtual void set_sector(const unsigned short&) 
  {}
  
  //! set column number for this hit
  virtual void set_column( const unsigned short& )
  {}
  
  //! Set strip associated this hit
  virtual void set_strip( const unsigned short& ) 
  {}
  
  //!@}

  //! @name Functional Interface
  //@{	
  
  //! hit charge
  virtual Float_t get_q() const 
  {return 0;}
  
  //! hit charge error
  virtual Float_t get_error_q() const 
  {return 0;}

  //! adc counts
  virtual unsigned short get_adc() const
  { return 0; }

  //! amu counts
  virtual unsigned short get_amu() const
  { return 0; }
  
  //! hit charge
  virtual void set_q( const Float_t& ) 
  {}
  
  //! hit charge error
  virtual void set_error_q( const Float_t& ) 
  {}
  
  //! adc counts
  virtual void set_adc( const unsigned short& ) 
  {}

  //! amu counts
  virtual void set_amu( const unsigned short& )
  {}
    
  //!@}
  
  //!@name status word
  //@{
  
  enum {

    //! this hit is on a strip masked in dead map
    masked = 1<<1,

    //! this is a missing hit, found with two adjacent clusters sandwiching a dead channel
    missing_hit = 1<<2

  };

  //! set status
  virtual void set_status( const unsigned long& status )
  {}
  
  //! get status
  virtual unsigned long get_status( void ) const
  { return 0; }
  
  //! clear status
  virtual void clear_status( void )
  { set_status( 0 ); } 
  
  //@}
  
  //! @name Dumpers
  //@{		
  //! Set Charge associated with this strip
  virtual void print(std::ostream& os = std::cout) const 
  {}
  //!@}
  
  ClassDef(TFvtxHit,1)
};


#endif /* __TFvtxHit_H__*/
