#ifndef __PHNONSTATICMAPFINDER_H__
#define __PHNONSTATICMAPFINDER_H__

// $Id: PHNonStaticMapFinder.h,v 1.2 2009/12/06 19:04:50 hpereira Exp $
#include "PHMapFinderBase.h"
#include <iostream>

/*!
\file PHMapFinder.h
\brief map finder
\author Sean Kelly, Hugo Pereira
\version $Revision: 1.2 $
\date $Date: 2009/12/06 19:04:50 $
*/

//! MapFinder locates the map with given key and executes any_cast
template <typename T>
  class NonStaticMapFinder
{

  public:

  T get(PHMapBase::key_type key)
  {
    /*
    cast the any pointer to T
    or throw if the cast failed
    */
    boost::any any_ptr = MapFinderBase().get(key);
    if(!any_ptr.empty())
    {

      try {

        T ptr = boost::any_cast<T>(any_ptr);
        return ptr;

      } catch(...) { return 0; }

    } else return T();

  }
};

#endif
