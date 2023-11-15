// $Id: PdbFvtxAlignment.cc,v 1.1 2013/02/05 20:31:55 jinhuang Exp $                                                                                             

/*!
 * \file PdbFvtxAlignment.cc
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/02/05 20:31:55 $
 */

#include "TGeoManager.h"
#include <iostream>

#include "PdbFvtxAlignment.hh"

using namespace std;

PdbFvtxAlignment::PdbFvtxAlignment() :
    _fGeom(NULL)
{
  cout << "dbFvtxAlignment::PdbFvtxAlignment - Init and clearing gGeoManager for possible streaming operations" << endl;
  gGeoManager = NULL;
}

PdbFvtxAlignment::~PdbFvtxAlignment()
{
//  if (_fGeom)
//    {
//      //  preserve the gGeoManager
//      TGeoManager * g_tmp = gGeoManager;
//      gGeoManager = NULL;
//      delete _fGeom;
//      // recover it
//      gGeoManager = g_tmp;
//    }
}

void
PdbFvtxAlignment::SetGeometry(TGeoManager * g)
{
  if (!g)
    {
      cout << "dbFvtxAlignment::SetGeometry - Error - Invalid input" << endl;
      return;
    }

//  if (_fGeom)
//    {
//      cout << "dbFvtxAlignment::SetGeometry - Clean up the old geometry"
//          << endl;
//      delete _fGeom;
//    }

////  preserve the gGeoManager
//  TGeoManager * g_tmp = gGeoManager;
//  gGeoManager = NULL;
//
//  _fGeom = static_cast<TGeoManager *>(g->Clone("PdbFvtxAlignment_fGeom"));
//
//  // recover it
//  gGeoManager = g_tmp;

  _fGeom = g;

}

TGeoManager *
PdbFvtxAlignment::GetGeometry()
{
  return _fGeom;
}

void
PdbFvtxAlignment::print() const
{
  cout << "PdbFvtxAlignment::print - ";
  if (!_fGeom)
    {
      cout << "geometry object has not been initialized yet.";
    }
  else
    {
      cout << "geometry set " << _fGeom->GetName() << " with title "
          << _fGeom->GetTitle();
    }
  cout << endl;
}
