// $Id: FvtxSector.cxx,v 1.8 2013/11/05 00:04:47 jinhuang Exp $

/*!
	\file FvtxSector.cxx
	\brief Forward vertex Sector geometry
	Initialize and provide access to FVTX planes
	\author Hugo Pereira da costa
	\version $Revision: 1.8 $
	\date $Date: 2013/11/05 00:04:47 $
*/

#include <iostream>
#include <stdexcept>
#include <vector>

#include "FvtxGeom.h"
#include "FvtxSector.h"

using namespace std;


//_____________________________________________________________
bool FvtxSector::contains( const PHPoint& point ) const
{
  PHPoint local = global_to_local(point);

  return _shape.Contains(local);
}

