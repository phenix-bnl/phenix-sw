//
// Interface Object Container Class : TFvtxCoordMap
// Author: S.Kelly 
// Date: 2/11/01
// Description: Container class for muon tracker global coordinate
//

#ifndef __TFVTXCOORDMAP_H__
#define __TFVTXCOORDMAP_H__
// BOOST headers
//
#include<boost/smart_ptr.hpp>
// PHENIX headers
//
#include<TFvtxCoord_v1.h>
#include<TFvtxKeyGen.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for FvtxR TFvtxCoord objects

/*!   

*/
class TFvtxCoordMap : 
public PHMap< PHKey::key_type, TFvtxCoord, TFvtxCoord_v1 >  
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default Constructor */

  /*! Default contructor */
  TFvtxCoordMap();

  /*! Construct with map key */  
  TFvtxCoordMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor  */  
  virtual ~TFvtxCoordMap() {;}

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
    Returns an iterator to all TFvtxCoord in given column plane.
   */
  iterator get(unsigned short arm, 
               unsigned short cage,
               unsigned short station, 
               unsigned short sector,
               unsigned short column);
  
  /*!
    Returns an const_iterator to all TFvtxCoord in given column plane.
   */
  const_iterator get(unsigned short arm, 
                     unsigned short cage,
                     unsigned short station, 
                     unsigned short sector,
                     unsigned short column) const ;
  /*!
    Returns an iterator to all TFvtxCoord in given sector.
  */  
  iterator get(unsigned short arm, 
               unsigned short cage,
               unsigned short station, 
               unsigned short sector);

  /*!
    Returns an const_iterator to all TFvtxCoord in given sector.
  */  
  const_iterator get(unsigned short arm, 
                     unsigned short cage,
                     unsigned short station, 
                     unsigned short sector) const;
  
  /*!
    Returns an iterator to all TFvtxCoord in given station.
  */  
  iterator get(unsigned short arm, 
               unsigned short cage,
               unsigned short station);

  /*!
    Returns an const_iterator to all TFvtxCoord in given station.
  */  
  const_iterator get(unsigned short arm, 
                     unsigned short cage,
                     unsigned short station) const;

  /*!
    Returns an iterator to all TFvtxCoord in given arm
  */  
  iterator get(unsigned short arm);

  /*!
    Returns an const_iterator to all TFvtxCoord in given station.
  */  
  const_iterator get(unsigned short arm) const;
  
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TFvtxCoord, TFvtxCoord_v1>::clear(); }
  //@}
  
 private:
  
  unsigned short get_roll_count() { return _count++%TFvtxKeyGen::get_max_index();}
  unsigned short _count;
  
};


#endif




