//
// Interface Object Container Class : TFvtxCompactCoordMap
// Author: Cesar da Silva 
// Date: 1/04/16
// Description: Container class for FVTX global compact coordinate
//

#ifndef __TFVTXCOMPACTCOORDMAP_H__
#define __TFVTXCOMPACTCOORDMAP_H__
// BOOST headers
//
#include<boost/smart_ptr.hpp>
// PHENIX headers
//
#include<TFvtxCompactCoord_v1.h>
#include<TFvtxKeyGen.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for FvtxR TFvtxCompactCoord objects

/*!   

*/
class TFvtxCompactCoordMap : 
public PHMap< PHKey::key_type, TFvtxCompactCoord, TFvtxCompactCoord_v1 >  
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default Constructor */

  /*! Default contructor */
  TFvtxCompactCoordMap();

  /*! Construct with map key */  
  TFvtxCompactCoordMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor  */  
  virtual ~TFvtxCompactCoordMap() {;}

  //@}

  //! @name Insertors
  //@{    

  /*! 
    Insert new object at given location. Returns an
    iterator to the newly created object.
  */

  iterator insert_new(unsigned short arm, 
                      unsigned short cage,
                      unsigned short station, 
                      unsigned short sector, 
                      unsigned short column);
  /*! 
    Insert new object using column locator. Returns an
    iterator to the newly created object.
  */
  
  //@}


  //! @name Extractors
  //@{    

  /*!
    Returns an iterator to all TFvtxCompactCoord in given column plane.
   */
  iterator get(unsigned short arm, 
               unsigned short cage,
               unsigned short station, 
               unsigned short sector,
               unsigned short column);
  
  /*!
    Returns an const_iterator to all TFvtxCompactCoord in given column plane.
   */
  const_iterator get(unsigned short arm, 
                     unsigned short cage,
                     unsigned short station, 
                     unsigned short sector,
                     unsigned short column) const ;
  /*!
    Returns an iterator to all TFvtxCompactCoord in given sector.
  */  
  iterator get(unsigned short arm, 
               unsigned short cage,
               unsigned short station, 
               unsigned short sector);

  /*!
    Returns an const_iterator to all TFvtxCompactCoord in given sector.
  */  
  const_iterator get(unsigned short arm, 
                     unsigned short cage,
                     unsigned short station, 
                     unsigned short sector) const;
  
  /*!
    Returns an iterator to all TFvtxCompactCoord in given station.
  */  
  iterator get(unsigned short arm, 
               unsigned short cage,
               unsigned short station);

  /*!
    Returns an const_iterator to all TFvtxCompactCoord in given station.
  */  
  const_iterator get(unsigned short arm, 
                     unsigned short cage,
                     unsigned short station) const;

  /*!
    Returns an iterator to all TFvtxCompactCoord in given arm
  */  
  iterator get(unsigned short arm);

  /*!
    Returns an const_iterator to all TFvtxCompactCoord in given station.
  */  
  const_iterator get(unsigned short arm) const;
  
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TFvtxCompactCoord, TFvtxCompactCoord_v1>::clear(); }
  //@}
  
 private:
  
  unsigned short get_roll_count() { return _count++%TFvtxKeyGen::get_max_index();}
  unsigned short _count;
  
};


#endif




