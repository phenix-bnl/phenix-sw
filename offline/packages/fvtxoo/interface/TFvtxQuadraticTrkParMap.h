#ifndef __TFVTXQUADRATICTRKPARMAP_H__
#define __TFVTXQUADRATICTRKPARMAP_H__


// BOOST headers
//
//#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TFvtxQuadraticTrkPar_v1.h>
#include<TFvtxKeyGen.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! Interface Object Container for TFvtxQuadraticTrkPar objects. */
class TFvtxQuadraticTrkParMap: public PHMap< PHKey::key_type, TFvtxQuadraticTrkPar, TFvtxQuadraticTrkPar_v1>
{
 public:
  
  //! @name Constructors/Destructors
  //@{    
  
  /*! Default constructor */  
  TFvtxQuadraticTrkParMap();

  /*! Construct with map key */  
  TFvtxQuadraticTrkParMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TFvtxQuadraticTrkParMap() {;}

  //@}

  //! @name Extractors
  //@{    
  iterator insert_new(unsigned short arm); 
  
  // clone a track, insert it in the map
  iterator insert_clone( const TFvtxQuadraticTrkParMap::pointer trk_ptr ); 
  
  /*! Get an iterator to all TFvtxStub in given arm */      
  iterator get(unsigned short arm);

  /*! Get an iterator to all TFvtxStub in given arm */    
  const_iterator get(unsigned short arm) const; 


  //@}
  
  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TFvtxQuadraticTrkPar, TFvtxQuadraticTrkPar_v1>::clear(); }
  //@}
  
 private:
  
  unsigned short get_roll_count() 
    { return _count++%TFvtxKeyGen::get_max_index();}
  
  unsigned short _count;
  

};

#endif
