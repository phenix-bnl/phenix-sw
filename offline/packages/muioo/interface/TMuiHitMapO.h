// $Id: TMuiHitMapO.h,v 1.2 2007/07/03 23:10:21 hpereira Exp $

/*!
   \file    TMuiHitMapO.h
   \brief   Container for MUID TMuiHitO objects
   \author  Jason Newby/Chun Zhang/David Silvermyr/Hugo Pereira
   \version $Revision: 1.2 $
   \date    $Date: 2007/07/03 23:10:21 $
*/

#ifndef __TMUIHITMAPO_H__
#define __TMUIHITMAPO_H__

// BOOST header
#include<boost/smart_ptr.hpp>
#include<boost/array.hpp>

// PHENIX headers
#include<TMuiHitO_v1.h>
#include<TMuiKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>
#include<MUIOO.h>

/*! \ingroup container */
//! Container for MUID TMuiHitO objects
/*! TMuiHitMapO - IOC for TMuiHitO objects */
class TMuiHitMapO : public PHMap<PHKey::key_type, TMuiHitO, TMuiHitO_v1 >
{

 public:
  
  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TMuiHitMapO()
  { _counts.assign(0); }

  /*! Construct with key */  
  TMuiHitMapO(PHKey::map_key_type map_key):
		PHMap<PHKey::key_type, TMuiHitO, TMuiHitO_v1>(map_key)
  { _counts.assign(0); }

  /*! Virtual destructor */  
  virtual ~TMuiHitMapO() 
  {}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TMuiHitO into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orientation,
    const UShort_t& twopack); 

  //! Insert a clone of an existing TMuiHitO into another map 
  iterator insert_clone(const TMuiHitMapO::pointer hit_ptr);
  
  //@}

  //! @name Extractors
  //@{    
  
  /*! Get an iterator to all roads in given arm */
  iterator get(const UShort_t& arm);  
  
  /*! Get a const iterator to all roads in given arm */
  const_iterator get(const UShort_t& arm) const; 

  /*! Get an iterator to all roads in given plane */
  iterator get(
    const UShort_t& arm,
    const UShort_t& plane
    );

  /*! Get a const iterator to all roads in given plane */
  const_iterator get(
    const UShort_t& arm,
    const UShort_t& plane
    ) const;

  /*! Get an iterator to all roads in given panel_orientation */
  iterator get(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orientation
    );

  /*! Get a const iterator to all roads in given panel_orientation */
  const_iterator get(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orientation
    ) const;

  /*! Get an iterator to all roads in given twopack*/
  iterator get(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orientation,
    const UShort_t& twopack
    );

  /*! Get a const iterator to all roads in given twopack */
  const_iterator get(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orientation,
    const UShort_t& twopack
    ) const;

  //@}

  //! @name Clear
  //@{
  void clear() 
  { 
    _counts.assign(0); 
    PHMap<PHKey::key_type, TMuiHitO, TMuiHitO_v1 >::clear(); 
  }
  //@}

 private:
  
  //! rolling count for (arm, plane, panel, orientation, twopack)
  UShort_t get_roll_count( 
    const UShort_t& arm, 
    const UShort_t& plane, 
    const UShort_t& panel, 
    const UShort_t& orientation,
    const UShort_t& twopack) 
  { 
    UShort_t index = twopack + MUIOO::MAX_TWOPACK_PANEL*(
      orientation + MUIOO::MAX_ORIENTATION*(
      panel + MUIOO::MAX_PANEL*( 
      plane + MUIOO::MAX_PLANE*arm ) ) );
    
    return _counts[index]++%TMuiKeyGen::get_max_index();
  }
  
  //! rolling count for (arm, plane, panel, orientation, twopack)
  boost::array< UShort_t, 
    MUIOO::MAX_ARM*
    MUIOO::MAX_PLANE*
    MUIOO::MAX_PANEL*
    MUIOO::MAX_ORIENTATION*
    MUIOO::MAX_TWOPACK_PANEL> _counts;
  
};

#endif







