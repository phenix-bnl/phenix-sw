#ifndef __TMUTVTXMAP_H__
#define __TMUTVTXMAP_H__

// $Id: TMutVtxMap.h,v 1.7 2011/12/29 20:19:31 slash Exp $

/*!
   \file TMutVtxMap.h
   \brief vertex container
   \author S. Kelly
   \version $Revision: 1.7 $
   \date $Date: 2011/12/29 20:19:31 $
*/

// BOOST headers
#include <boost/smart_ptr.hpp>

// PHENIX headers
#include "TMutVtx_v2.hh"
#include <TMutKeyGen.h>
#include <TMutMapIO.h>
#include <PHMap.h>
#include <PHMapIterator.h>
#include <PHConstMapIterator.h>
#include <PHKey.hh>

/*! \ingroup container */
//! Container for MUTR TMutVtx objects
/*!

  TMutMCTrkMap is the container for TMutVtx interface objects.  As is
  the case for all MUTOO interface object containers, the actual
  objects that are stored in this container, ie. the value_type in STL
  speak, is a shared_ptr to a TMutVtx. Shared pointers are
  described in the <a href=index.html> FAQ </a>.
*/
class TMutVtxMap : public PHMap< PHKey::key_type, TMutVtx, TMutVtx_v2 >
{

public:

  //! @name Constructors/Destructors
  //@{

  /*! Default contructor */
  TMutVtxMap();

  /*! Default contructor */
  TMutVtxMap(PHKey::map_key_type map_key);

  /*! Virtual destructor */
  virtual ~TMutVtxMap() {}

  //@}

  //! @name Insertors
  //@{
  /*!
    Insert an new TMutVtx into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(UShort_t arm);
  //@}

  //! @name Extractors
  //@{
  /*! Get an iterator to all TMutVtx in given arm */
  iterator get(UShort_t arm);
  /*! Get an iterator to all TMutVtx in given arm */
  const_iterator get(UShort_t arm) const;
  //@}

  //! @name Clear
  //@{
  void clear()
  { _count=0; PHMap<PHKey::key_type, TMutVtx, TMutVtx_v2>::clear(); }

  //@}

 private:

  UShort_t get_roll_count() { return _count++%TMutKeyGen::get_max_index();}
  UShort_t _count;

};


#endif
