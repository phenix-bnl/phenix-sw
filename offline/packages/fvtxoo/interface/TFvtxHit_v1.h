#ifndef __TFvtxHit_v1_h__
#define __TFvtxHit_v1_h__

// $Id: TFvtxHit_v1.h,v 1.9 2011/12/01 04:16:20 slash Exp $

/*!
   \file TFvtxHit_v1.h
   \brief The forward vertex hit object 
   \author Hugo Pereira Da Costa
   \version $Revision: 1.9 $
   \date $Date: 2011/12/01 04:16:20 $
*/

#include "TFvtxHit.h"

/*! @ingroup interface */
//! The forward vertex hit object 
class TFvtxHit_v1 : public TFvtxHit 
{
  
  public:

  //! @name Constructors/Destructor
  //@{		
  //! Default
  TFvtxHit_v1();

  //! Construct with key
  TFvtxHit_v1(const Key& key,
    const unsigned short& arm,
    const unsigned short& cage,
    const unsigned short& station,
    const unsigned short& sector,
    const unsigned short& column,
    const unsigned short& strip );
  
  //! Construct from base class
  TFvtxHit_v1(const TFvtxHit& base_ref );
  
  //! Construct from base class
  TFvtxHit_v1(const TFvtxHit* base_pointer );

  //! Default
  virtual ~TFvtxHit_v1() 
  {}
  
  //!@}
  
  //! @name Locators
  //@{		
  //! Get arm number for this hit
  virtual unsigned short get_arm() const 
  {return _arm;}
  
  //! Get cage number for this hit
  virtual unsigned short get_cage() const
  {return _cage;}

  //! Get station number for this hit
  virtual unsigned short get_station() const 
  {return _station;}
  
  //! Get sector number for this hit
  virtual unsigned short get_sector() const 
  {return _sector;}
  
  //! Get column number for this hit
  virtual unsigned short get_column() const
  { return _column; }
  
  //! strip associated with this hit
  virtual unsigned short get_strip() const 
  {return _strip;}
  
  //! Set arm number for this hit
  virtual void set_arm(const unsigned short& arm ) 
  { _arm = arm; }
  
  //! Set cage number for this hit
  virtual void set_cage(const unsigned short& cage )
  { _cage = cage; }

  //! Set station number for this hit
  virtual void set_station(const unsigned short& station ) 
  { _station = station; }
  
  //! Set sector number for this hit
  virtual void set_sector(const unsigned short& sector ) 
  { _sector = sector; }
  
  //! set column for this hit
  virtual void set_column( const unsigned short& column )
  { _column = column; }
  
  //! strip associated with this hit
  virtual void set_strip( const unsigned short& strip ) 
  { _strip = strip; }
  
  //!@}

  //! @name Functional Interface
  //@{	
  
  //! hit charge
  virtual Float_t get_q() const 
  {return _q;}
  
  //! hit charge error
  virtual Float_t get_error_q() const 
  {return _error_q;}

  //! adc counts
  virtual unsigned short get_adc() const
  { return _adc; }
  
  //! hit charge
  virtual void set_q( const Float_t& q ) 
  { _q = q; }
  
  //! hit charge error
  virtual void set_error_q( const Float_t& error_q ) 
  { _error_q = error_q; }
  
  //! adc counts
  virtual void set_adc( const unsigned short& adc ) 
  { _adc = adc; }

  //! amu cell number
  virtual void set_amu( const unsigned short& amu )
  { _amu = amu; }

    
  //!@}
  
  //!@name status word
  //@{
  
  //! set status
  virtual void set_status( const unsigned long& status )
  { _status = status; }
  
  //! get status
  virtual unsigned long get_status( void ) const
  { return _status; }
  
  //@}
  
  //! @name Dumpers
  //@{		
  //! Set Charge associated with this strip
  virtual void print(std::ostream& os = std::cout) const;
  //!@}
  
  private:
  
  //! arm index
  unsigned short _arm;
  
  //! cage index
  unsigned short _cage;

  //! station index
  unsigned short _station;
  
  //! sector index
  unsigned short _sector;
  
  //! plane index
  unsigned short _plane;
  
  //! radial segmentation index
  unsigned short _radius;
  
  //! column index
  unsigned short _column;
  
  //! strip index
  unsigned short _strip;
  
  //! hit charge
  Float_t _q;
  
  //! error on hit charge
  Float_t _error_q;
  
  //! hit adc counts
  unsigned short _adc;

  //! hit amu cell number
  unsigned short _amu;
  
  //! 32 bits status word
  unsigned long _status;    
    
 ClassDef(TFvtxHit_v1,1)
};


#endif
