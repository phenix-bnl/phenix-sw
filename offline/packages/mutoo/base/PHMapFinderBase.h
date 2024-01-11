#ifndef __PHMAPFINDERBASE_H__
#define __PHMAPFINDERBASE_H__

// $Id: PHMapFinderBase.h,v 1.1 2009/08/31 19:20:04 hpereira Exp $

/*!
\file PHMapFinder.h
\brief map finder
\author Sean Kelly, Hugo Pereira
\version $Revision: 1.1 $
\date $Date: 2009/08/31 19:20:04 $
*/

#include <boost/any.hpp>
#include "PHMapBase.h"

//! MapFinder locates the map with given key and executes any_cast 
class MapFinderBase 
{
  
  public:
  
  boost::any get( PHMapBase::key_type ) const;

};

#endif
