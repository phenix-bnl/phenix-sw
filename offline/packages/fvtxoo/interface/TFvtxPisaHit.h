#ifndef __TFvtxPisaHit_h__
#define __TFvtxPisaHit_h__

// $Id: TFvtxPisaHit.h,v 1.2 2011/12/01 04:16:20 slash Exp $

/*!
  \file TFvtxPisaHit.h
  \brief empty interface to store SvxPisaHits into a map and enable standard PHKey association
  \author Hugo Pereira Da Costa
  \version $Revision: 1.2 $
  \date $Date: 2011/12/01 04:16:20 $
*/

#include <PHKey.hh> 

// forward declaration of embedded class
class SvxSnglPisaHit;


/*! @ingroup interface */
//! Empty interface to store SvxPisaHits into a map and enable standard PHKey association
class TFvtxPisaHit : public PHKey
{
  
  public:
  
  //! @name Constructors/Destructor
  //@{		
  //! Default
  TFvtxPisaHit()
  {}

  //! Construct with key
  TFvtxPisaHit(const Key& key): 
    PHKey(key)
  {}

  //! Destructor
  virtual ~TFvtxPisaHit() 
  {}

  //! index
  virtual unsigned short get_index() const 
  {return 0;}  
  
  //! index 
  virtual void set_index( const unsigned short& index)
  {}
  
  //! set pisa hit
  virtual void set_pisa_hit( const SvxSnglPisaHit* hit )
  {}
  
  //! retrieve pisa hit
  virtual const SvxSnglPisaHit* get_pisa_hit( void ) const
  { return 0; }
  
  //! @name Dumpers
  //@{		
  //! Set Charge associated with this strip
  virtual void print(std::ostream& os = std::cout) const 
  {}
  //!@}
  
  ClassDef(TFvtxPisaHit,1)
  
};
  
#endif
