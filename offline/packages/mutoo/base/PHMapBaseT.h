// $Id: PHMapBaseT.h,v 1.6 2009/12/06 20:19:37 hpereira Exp $

#ifndef __PHMAPBASET_H__
#define __PHMAPBASET_H__

/*!
  \file PHMapBaseT.h
  \brief Base class for template dependent PHMap methods
  \author S. Kelly
  \version $Revision: 1.6 $
  \date $Date: 2009/12/06 20:19:37 $
*/

#include<boost/smart_ptr.hpp>

#include "PHKey.hh"
#include "PHMapBase.h"
#include "PHMapManager.h"

/*! \ingroup classes */

//! Interface for template parameter dependent methods
/*!
  Provides the interface requirements for map methods that are dependent
  upon template parameters names for pointer and reference types.
*/
template<typename Key, typename Value>
class PHMapBaseT : public PHMapBase
{

 public:

  /*! constructor */
  PHMapBaseT(PHKey::map_key_type map_key) : PHMapBase(map_key)
  { PHMapManager::register_map( this ); }

  /*! constructor */
  PHMapBaseT()
  { PHMapManager::register_map( this ); }

  private:

  // give PHKey access to private_find
  friend class PHKey;

  typedef boost::shared_ptr<Value> value_type;

  virtual value_type private_find(const Key&) = 0;

};

#endif




