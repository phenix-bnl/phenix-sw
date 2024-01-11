// $Id: TFvtxMCHit3D.cxx,v 1.5 2011/02/08 23:12:17 youzy Exp $

/*!
	 \file TFvtxMCHit3D.cxx
	 \brief 3D object for forward vertex mc_hit
	 \author Hugo Pereira
	 \version $Revision: 1.5 $
	 \date $Date: 2011/02/08 23:12:17 $
*/

#include <FvtxGeom.h>
#include <FVTXOO.h>

#include <cmath>
#include <sstream>
#include <FvtxGeom.h>

#include "TFvtxMCHit3D.h"

using namespace std;
	 
//_______________________________________________________
void TFvtxMCHit3D::_make_nodes( void )
{
	
	if( !_is_valid ) {
		cout << "TFvtxMCHit3D::make_nodes - invalid object.\n"; 
		return;
	}
		
	if( _parent ) _parent->cd();
	
	// get column associated to MC hit
	FvtxColumn* column =
			FvtxGeom::get_arm( _mc_hit.get()->get_arm() )
                        ->get_cage( _mc_hit.get()->get_cage() )
			->get_station( _mc_hit.get()->get_station() )
			->get_sector( _mc_hit.get()->get_sector() )
			->get_column( _mc_hit.get()->get_column() );
	
	// get strips associated to MC hit
	for( unsigned int i=0; i < _mc_hit.get()->get_n_strip(); i++ )
	{
		FvtxStrip* strip( column->get_strip( _mc_hit.get()->get_strip(i)->get_strip() ) );
		_make_segment( 
			strip->get_position_begin(), 
			strip->get_position_end(), 
			strip->get_width() );
	}

	if( _parent ) _parent->cd();
	
}
