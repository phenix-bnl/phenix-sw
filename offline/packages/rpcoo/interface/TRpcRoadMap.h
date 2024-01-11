#ifndef _TRpcRoadMap_h_
#define _TRpcRoadMap_h_

/*!
	\file TRpcRoadMap.h
	\brief Container for TRpcRoad
	\author R. S. Hollis (rhollis@ucr.edu)
  \version $Revision: 1.1 $
  \date    today
*/


// BOOST headers
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TRpcRoad_v1.h>
#include<TRpcKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

class TRpcRoadMap : public PHMap< PHKey::key_type, TRpcRoad, TRpcRoad_v1 >  
{
 public:

  //! @name Constructors/Destructors
  //@{    
  
  /*! Default contructor */
  TRpcRoadMap();

  /*! Construct with key */
  TRpcRoadMap(PHKey::map_key_type map_key);

  /*! Virtual destructor */
  virtual ~TRpcRoadMap() {;}

  iterator insert_new(UShort_t arm );

  //! @name Extractors
  //@{    

  /*! Get an iterator to all hits in given arm */
  iterator get(UShort_t arm );
  
  /*! Get a const iterator to all hits in given gap */	       
  const_iterator get(UShort_t arm ) const;
	
  //@}
  //! @name Clear
  //@{
  void clear() { 
		_count=0; 
		PHMap<PHKey::key_type, TRpcRoad, TRpcRoad_v1>::clear(); 
	}
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TRpcKeyGen::get_max_index();}
  UShort_t _count;
  
};

#endif




