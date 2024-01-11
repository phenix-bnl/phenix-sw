// $Id: TMuiAlign.cxx,v 1.7 2009/08/22 13:58:52 hpereira Exp $

/*!
  \file    TMuiAlign.cxx
  \brief   Statically scoped class for muon identifier alignment corrections from file
  \author  Hugo Pereira
  \version $Revision: 1.7 $
  \date    $Date: 2009/08/22 13:58:52 $
*/


#include <PHException.h>
#include <fstream>

#include "MuiGeomClasses.hh"
#include "MUIGEOM.h"
#include "TMuiAlign.h"

using namespace std;

//________________________________________________________
const string TMuiAlign::MUID_PANEL = "muid_panel";
TMuiAlign::panel_parameter_set TMuiAlign::_panel_parameters;

//________________________________________________________
void TMuiAlign::init( const string& filename )
{
 	// open file
 	ifstream in( filename.c_str() );
 	if( !in ) 
 	{
 		cout << "TMuiAlign::init - unable to open file " << filename << endl;
 		return;
 	}
	cout << "TMuiAlign::init - misalignments loaded, file " << filename << endl;
	
 	char line[512];
 	while( !(in.rdstate() & ios::failbit ) )
 	{
 		in.getline( line, 512, '\n' );
 		if( !strlen( line ) ) continue;
		if( strncmp( line, "//", 2 ) == 0 ) continue;
  		
 		istringstream line_stream( line );
  		
 		// read tag to decide if parameter set is from muid panel
 		string tag;
 		line_stream >> tag;
 		if( line_stream.rdstate() & ios::failbit ) continue;
  		
 		if( tag == MUID_PANEL ) 
 		{  			
 			PanelParameters par;
 			line_stream >> par;
	   		if( line_stream.rdstate() & ios::failbit ) continue;
   		
   			// check if alignment parameters already exists
			panel_parameter_set::iterator panel_iter = _panel_parameters.find( par );
   			if( panel_iter != _panel_parameters.end() ) {
    		
				par._delta_x += panel_iter->_delta_x;
				par._delta_y += panel_iter->_delta_y;
				par._delta_z += panel_iter->_delta_z;
				par._delta_phi += panel_iter->_delta_phi;
				_panel_parameters.erase( par );
				_panel_parameters.insert( par );

			} else _panel_parameters.insert( par );
		}		
 	}
};

//________________________________________________________
void TMuiAlign::update_geometry()
{  
  	// loop over panel parameters
  	for( panel_parameter_set::iterator iter = _panel_parameters.begin(); iter != _panel_parameters.end(); iter++ )
  	{
  		
  		// get panel id matching this parameter set
  		TMuiChannelId pPanelId( iter->_arm, iter->_plane, iter->_panel );
  		TMuiPanelGeo* panel = TMuiGeometry::Geom()->getPanel(pPanelId);
  		if( !panel ) {
			ostringstream what;
			what << "["
  				<< iter->_arm << ","
    				<< iter->_plane << ","
    				<< iter->_panel << "] invalid";
			throw runtime_error( DESCRIPTION( what.str() ) );
		}
			 			

		/* 
			The center of the Rotation is the center of the panel and not the center of the beam (0,0).
			However the rotation angle delta_phi is a misalignment correction that has its center in (0,0).
			One has to take it in consideration to move correctly the panel
		*/		
  		
		// rotate
	  	// translation
    		PHVector translation( iter->_delta_x, iter->_delta_y, iter->_delta_z );
    		panel->Translate( translation );
  	
    		// rotation
    		double cos_phi( cos( iter->_delta_phi ) );
    		double sin_phi( sin( iter->_delta_phi ) );
  
		// rotate panel
    		PHMatrix rotation(
    				PHVector( cos_phi, -sin_phi, 0 ),
    				PHVector( sin_phi, cos_phi, 0 ),
    				PHVector(0, 0, 1 ) );
    		panel->Rotate( rotation );
	
	}
 };

//________________________________________________________
TMuiAlign::PanelParameters TMuiAlign::get_panel_parameters( int arm, int plane, int panel )
{
	PanelParameters tmp( arm, plane, panel );
	panel_parameter_set::iterator iter( _panel_parameters.find( tmp ) );
	return (iter == _panel_parameters.end() ) ? tmp:*iter;
}
	
//________________________________________________________
void TMuiAlign::print_parameters( ostream& out )
{

	// do nothing if no parameters were loaded
	if( !_panel_parameters.size() ) return;
		
	MUIGEOM::PRINT( out, "TMuiAlign::print_parameters" );

	// loop over panel parameters
	for( panel_parameter_set::iterator iter = _panel_parameters.begin(); iter != _panel_parameters.end(); iter++ )
	out << MUID_PANEL << " " << (*iter) << endl;
	
	MUIGEOM::PRINT( out, "**" );
}
