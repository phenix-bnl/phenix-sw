// $Id: PHMapFinderBase.cxx,v 1.1 2009/08/31 19:20:04 hpereira Exp $

/*!
\file PHMapFinderBase.cxx
\brief map finder
\author Sean Kelly, Hugo Pereira
\version $Revision: 1.1 $
\date $Date: 2009/08/31 19:20:04 $
*/

#include "PHMapFinderBase.h"
#include "PHMapManager.h"

boost::any MapFinderBase::get( PHMapBase::key_type key ) const
{ return PHMapManager::get( key ); }
