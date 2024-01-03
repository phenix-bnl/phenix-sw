//-----------------------------------------------------------------------------
//  $Header: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/database/pdbcal/base/PdbApplication.cc,v 1.6 2013/02/01 04:32:04 pinkenbu Exp $
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbApplication
//
//  Author: Matthias Messer
//-----------------------------------------------------------------------------

#include <PdbApplication.hh>

#include <iostream>

std::auto_ptr<PdbApplication> PdbApplication::__instance;

PdbApplication *PdbApplication::instance()
{
  if ( not __instance.get() )
    {
      std::cerr << __FILE__ << "  " << __LINE__ << " No instance of  PdbApplication available" << std::endl;
      return NULL;
    }

  return __instance.get();
}

