// $Id: TMuiGeo.h,v 1.2 2007/02/13 13:45:56 hpereira Exp $

#ifndef _TMuiGeo_h_
#define _TMuiGeo_h_

/*
  \file TMuiGeo.h
  \brief utility class to interface with the muigeom library
  \author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2007/02/13 13:45:56 $
*/

#include <TObject.h>
#include <PHPoint.h>
#include <TDataType.h>
#include <TMuiChannelId.hh>
#include "MUIOO.h"

//! \brief utility class to interface with the muigeom library
class TMuiGeo: public TObject 
{
  public:
  
  #ifndef __CINT__
  //! retrieve panel tube angle wrt x axis
  static double get_panel_angle( const MUIOO::panel_orient_locator & location )
  { return 	get_panel_angle( location.get<0>(), location.get<1>(), location.get<2>(),location.get<3>() ); }
  
  //! returns true if an point is in a given panel
  static bool is_in_panel( const PHPoint& point, const MUIOO::panel_locator& location )
  { return is_in_panel( point, location.get<0>(), location.get<1>(), location.get<2>() ); }
  #endif
          
  //! retrieve panel tube angle wrt x axis
  static double get_panel_angle (
    UShort_t arm, 
    UShort_t plane, 
    UShort_t panel, 
    UShort_t orientation ); 
  
  //! returns true if an point is in a given panel
  static bool is_in_panel( 
    const PHPoint& point, 
    UShort_t arm,  
    UShort_t plane,  
    UShort_t panel ); 
  
  //! retrieve HV group from twopack location
  static UShort_t get_hv_chain( 
    UShort_t arm,
    UShort_t plane, 
    UShort_t panel, 
    UShort_t orientation, 
    UShort_t twopack ) 
  { return TMuiChannelId(arm,plane,panel,(EOrient_t)orientation,twopack).get_HV_chain_group(); }
  
};
#endif
