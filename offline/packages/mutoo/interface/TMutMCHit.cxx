// $Id: TMutMCHit.cxx,v 1.7 2007/05/31 12:02:01 hpereira Exp $

/*!
   \file TMutMCHit.cxx
   \brief Class for Muon Tracker Monte Carlo hit object
   \author S. Kelly
   \version $Revision: 1.7 $
   \date $Date: 2007/05/31 12:02:01 $
*/

#include <iostream>

#include "TMutMCHit.hh"
ClassImp(TMutMCHit)

using namespace std;

//
PHClassId::id_type TMutMCHit::_class_id( 0 );
