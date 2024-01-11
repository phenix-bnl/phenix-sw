// $Id: muid_panel.C,v 1.1 2007/10/02 19:39:32 mwysocki Exp $

/*
	\file muid_panel.C
	\brief converts new framework muid hit pattern into old framework hit pattern
*/


#include <MUIOO.h>
#include <PHMuoTracksOut.h>
#include <PHPoint.h>
#include <TMuiGeo.h>
#include <iostream>

#include "Tools.h"

using namespace std;

//______________________________________________________
int Tools::get_muid_panel( int imu, int iroad, PHMuoTracksOut* muo )
{
	return get_muid_panel(
		muo->get_muIDOO_gap0(0, iroad, imu),
  	muo->get_muIDOO_gap0(1, iroad, imu),
  	muo->get_muIDOO_gap0(2, iroad, imu) );
}

//______________________________________________________
int Tools::get_muid_panel( Float_t x, Float_t y, Float_t z )
{
	
	// decide arm from sign of z
	MUIOO::ArmNumber arm = (z<0) ? MUIOO::South:MUIOO::North;
	int out( -1 );
	
	// loop over plane, panels
	for( int plane = 0; plane < MUIOO::MAX_PLANE; plane++ )
	for( int panel = 0; panel < MUIOO::MAX_PANEL; panel++ )
	if( TMuiGeo::is_in_panel( PHPoint( x, y, z ), arm, plane, panel ) )
	{
		cout 
				<< "Tools::get_muid_panel - point:("
				<< x << ","
				<< y << ","
				<< z << ")"
				<< " plane=" << plane 
				<< " panel=" << panel << endl; 
		out = panel;
	}
	
	if( out < 0 )
		cout << "Tools::get_muid_panel - point:("
				<< x << ","
				<< y << ","
				<< z << ") - no match" << endl;
	return out;
	
}
