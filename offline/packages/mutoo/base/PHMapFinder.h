#ifndef __PHMAPFINDER_H__
#define __PHMAPFINDER_H__

// $Id: PHMapFinder.h,v 1.2 2009/12/06 19:03:21 hpereira Exp $

#include "PHMapFinderBase.h"

/*!
\file PHMapFinder.h
\brief map finder
\author Sean Kelly, Hugo Pereira
\version $Revision: 1.2 $
\date $Date: 2009/12/06 19:03:21 $
*/

//! MapFinder locates the map with given key and executes any_cast
template <typename T> class MapFinder
{

  public:

  static T get(PHMapBase::key_type key)
  {
    /*
    cast the any pointer to T
    or throw if the cast failed
    */
    boost::any any_ptr = MapFinderBase().get(key);
    if(!any_ptr.empty()) {
      try {
        T ptr = boost::any_cast<T>(any_ptr);
        return ptr;
      } catch(...) {
        return 0;
      }
    } else return T();
  }
};

#endif
