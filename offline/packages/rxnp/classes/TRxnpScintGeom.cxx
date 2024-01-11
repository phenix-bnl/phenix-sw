// $Id: TRxnpScintGeom.cxx,v 1.2 2007/04/12 05:23:45 phnxrxp Exp $
//////////////////////////////////////////////////////////////////
/*
        \file TRxnpScintGeom.h
        \brief Container class to geom info for each scintilator
        \author Chun Zhang
        \version $Revision: 1.2 $
        \date    $Date: 2007/04/12 05:23:45 $
*/
//////////////////////////////////////////////////////////////////

#include <math.h>

// RXNP include
#include "TRxnpScintGeom.h"

// function implementation of TRxnpScintGeom
//
float TRxnpScintGeom::get_theta() const
{
  if(_ring == 0)
    // take average of the survey positions. Note middle is shared
    //
    return (_theta[INNER_RIGHT]+_theta[INNER_LEFT]+_theta[MIDDLE_LEFT]+_theta[MIDDLE_RIGHT])/4.0;
  else
    // take average of the survey positions. Note middle is shared
    //
    return (_theta[OUTER_RIGHT]+_theta[OUTER_LEFT]+_theta[MIDDLE_LEFT]+_theta[MIDDLE_RIGHT])/4.0;
}
// function implementation of TRxnpScintGeom
//
float TRxnpScintGeom::get_phi() const
{
  if(_ring == 0) {
    // take average of the survey positions. Note middle is shared
    //
    float x=(cos(_phi[INNER_RIGHT])+cos(_phi[INNER_LEFT])
            +cos(_phi[MIDDLE_LEFT])+cos(_phi[MIDDLE_RIGHT]))/4.0;
    float y=(sin(_phi[INNER_RIGHT])+sin(_phi[INNER_LEFT])
            +sin(_phi[MIDDLE_LEFT])+sin(_phi[MIDDLE_RIGHT]))/4.0;
    return atan2(y,x);
  } else {
    // take average of the survey positions. Note middle is shared
    //
    float x=(cos(_phi[OUTER_RIGHT])+cos(_phi[OUTER_LEFT])
            +cos(_phi[MIDDLE_LEFT])+cos(_phi[MIDDLE_RIGHT]))/4.0;
    float y=(sin(_phi[OUTER_RIGHT])+sin(_phi[OUTER_LEFT])
            +sin(_phi[MIDDLE_LEFT])+sin(_phi[MIDDLE_RIGHT]))/4.0;
    return atan2(y,x);
  }
}
// function implementation of TRxnpScintGeom
//
float TRxnpScintGeom::get_z() const
{
  if(_ring == 0)
    // take average of the survey positions. Note middle is shared
    //
    return (_z[INNER_RIGHT]+_z[INNER_LEFT]+_z[MIDDLE_LEFT]+_z[MIDDLE_RIGHT])/4.0;
  else
    // take average of the survey positions. Note middle is shared
    //
    return (_z[OUTER_RIGHT]+_z[OUTER_LEFT]+_z[MIDDLE_LEFT]+_z[MIDDLE_RIGHT])/4.0;
}

