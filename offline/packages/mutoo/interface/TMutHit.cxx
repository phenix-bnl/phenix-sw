// $Id: TMutHit.cxx,v 1.12 2010/06/10 22:45:30 hpereira Exp $

/*!
   \file TMutHit.cxx
   \brief Class for Muon Tracker hits
   \author S. Kelly
   \version $Revision: 1.12 $
   \date $Date: 2010/06/10 22:45:30 $
*/

#include"TMutHit.hh"

#include <iostream>

#include <MutCalib.h>
#include <MutStrip.h>
#include <MUTOO_FEM.h>
#include <TMutGeo.h>

ClassImp(TMutHit)

//
PHClassId::id_type TMutHit::_class_id( 0 );

//__________________________________________________
MutStrip* TMutHit::get_strip_geom( void ) const
{ return TMutGeo::get_strip_geom( get_location(), get_strip() ); }
