//
// Interface Object Container Class : TMuiClusterMapO
// Author: Jason Newby
// Date: 02/12/03
// Description: Cotainer class for mui hit
//

#ifndef __TMUICLUSTERMAPO_H__
#define __TMUICLUSTERMAPO_H__

// BOOST header
//
#include<boost/smart_ptr.hpp>
// PHENIX headers
//
#include<TMuiClusterO_v2.h>
#include<TMuiKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUID TMuiClusterO objects
/*!
  TMuiClusterMapO - IOC for TMuiClusterO objects
*/

class TMuiClusterMapO :
public PHMap<PHKey::key_type, TMuiClusterO, TMuiClusterO_v2 >
{

 public:

  //! @name Constructors/Destructors
  //@{

  /*! Default contructor */
  TMuiClusterMapO(): 
    _count(0) 
  {}

  /*! Construct with key */
  TMuiClusterMapO( const PHKey::map_key_type& map_key): 
    PHMap<PHKey::key_type, TMuiClusterO, TMuiClusterO_v2>(map_key), 
    _count(0)
  {}

  /*! Virtual destructor */
  virtual ~TMuiClusterMapO() {;}

  //! @name Insertors
  //@{
  /*!
    Insert an new TMuiClusterO into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orientation);

  //@}

  //! @name Extractors
  //@{

  /*! Get an iterator to all clusters in given arm */
  iterator get( const UShort_t& arm);

  /*! Get a const iterator to all clusters in given arm */
  const_iterator get( const UShort_t& arm) const;

  /*! Get an iterator to all roads in given panel */
  iterator get(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel
  );

  /*! Get a const iterator to all roads in given panel */
  const_iterator get(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel
  ) const;

  /*! Get an iterator to all clusters in given panel_orientation */
  iterator get(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orientation
  );

  /*! Get a const iterator to all clusters in given panel_orientation */
  const_iterator get(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orientation
  ) const;

  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMuiClusterO, TMuiClusterO_v2 >::clear(); }
  //@}

 private:

  UShort_t get_roll_count() { return _count++%TMuiKeyGen::get_max_index();}
  UShort_t _count;

};

#endif
