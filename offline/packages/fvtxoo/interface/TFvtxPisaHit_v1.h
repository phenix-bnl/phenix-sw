#ifndef __TFvtxPisaHit_v1_h__
#define __TFvtxPisaHit_v1_h__

// $Id: TFvtxPisaHit_v1.h,v 1.2 2011/12/01 04:16:20 slash Exp $

/*!
   \file TFvtxPisaHit_v1.h
   \brief Empty interface to store SvxPisaHits into a map and enable standard PHKey association
   \author Hugo Pereira Da Costa
   \version $Revision: 1.2 $
   \date $Date: 2011/12/01 04:16:20 $
*/

#include <SvxSnglPisaHitv1.h>
#include "TFvtxPisaHit.h"

/*! @ingroup interface */
//! Empty interface to store SvxPisaHits into a map and enable standard PHKey association
class TFvtxPisaHit_v1 : public TFvtxPisaHit 
{
  
  public:
  
  //! shortcut to SvxSngPisaHit implementation
  typedef SvxSnglPisaHitv1 value_type;
  
  //! @name Constructors/Destructor
  //@{		
  //! Default
  TFvtxPisaHit_v1();

  //! Construct with key
  TFvtxPisaHit_v1(const Key& key, const unsigned short& index );
 
  //! Construct from base class
  TFvtxPisaHit_v1(const TFvtxPisaHit& base_ref );
  
  //! Construct from base class
  TFvtxPisaHit_v1(const TFvtxPisaHit* base_pointer );

  //! Default
  virtual ~TFvtxPisaHit_v1() 
  {}
  
  //!@}

  //! index
  virtual unsigned short  get_index() const 
  {return _index;}  
  
  //! index 
  virtual void set_index( const unsigned short& index)
  { _index = index; }

  //! @name captured pisa hit
  //@{
  
  //! set pisa hit
  virtual void set_pisa_hit( const SvxSnglPisaHit* hit )
  { _pisa_hit = value_type( hit ); }
  
  //! retrieve pisa hit
  const SvxSnglPisaHit* get_pisa_hit( void ) const
  { return &_pisa_hit; }
  
  //@}
 
  //! @name Dumpers
  //@{		
  //! Set Charge associated with this strip
  virtual void print(std::ostream& os = std::cout) const;
  //!@}
  
  private:
  
  //! hit index
  unsigned short _index;
  
  //! captured pisa hit
  value_type _pisa_hit;
  
  ClassDef(TFvtxPisaHit_v1,1) 

};

#endif
