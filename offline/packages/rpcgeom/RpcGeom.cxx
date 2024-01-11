
#include "RpcGeom.h"
#include "RpcStationIOArray.h"
#include "RpcStation.h"

#include "PHCompositeNode.h"
#include <getClass.h>

#include <iostream>
#include <sstream>
#include <stdexcept>

using namespace std;
		
//___________________________________________________________
/* 
   geometry description.
   this part needs to be rewritten if a new geometry is implemented.
   As long as only the numbers (and not the structure) change,
   there is no need to use new version of the stations.
   If the version are changed (most likely RpcStation), then RpcStation classes
   needs to be evolved, and the create_arm method needs to be changed
   accordingly
*/

//! current station implementation version
typedef RpcStation Station_ValueImp;
	
RpcArm* RpcGeom::_south_arm( 0 );
RpcArm* RpcGeom::_north_arm( 0 );
	
//___________________________________________________________

void RpcGeom::create_arms_v1( void )
{
	
  {
    cout << "RpcGeom::initialize - initializing south arm geometry" << endl;
    _south_arm = new RpcArm( RPCGEOM::South );
		
    // segmentation for station 1
    Station_ValueImp* station1_S = new Station_ValueImp( RPCGEOM::South, RPCGEOM::Station1 );
    station1_S->set_z( RPCGEOM::station_z[RPCGEOM::South][RPCGEOM::Station1] );
    station1_S->set_Rsegmentation( RPCGEOM::NumberofRBoundsSt1 );
    station1_S->create_strips();
    _south_arm->set_station1( station1_S );
		
    // segmentation for station 2
    Station_ValueImp* station2_S = new Station_ValueImp( RPCGEOM::South, RPCGEOM::Station2 );
    station2_S->set_z( RPCGEOM::station_z[RPCGEOM::South][RPCGEOM::Station2] );
    station2_S->set_Rsegmentation(RPCGEOM::NumberofRBoundsSt2 );
    station2_S->create_strips();	
    _south_arm->set_station2( station2_S );
	
    // segmentation for station 3
    Station_ValueImp* station3_S = new Station_ValueImp( RPCGEOM::South, RPCGEOM::Station3 );
    station3_S->set_z( RPCGEOM::station_z[RPCGEOM::South][RPCGEOM::Station3] );
    station3_S->set_Rsegmentation(RPCGEOM::NumberofRBoundsSt3);
    station3_S->create_strips();	
    _south_arm->set_station3( station3_S );
  }
		
  {	
    cout << "RpcGeom::initialize - initializing north arm geometry" << endl;
    _north_arm = new RpcArm( RPCGEOM::North );
		
    // segmentation for station 1
    Station_ValueImp* station1_N = new Station_ValueImp( RPCGEOM::North, RPCGEOM::Station1 );
    station1_N->set_z(RPCGEOM::station_z[RPCGEOM::North][RPCGEOM::Station1]);
    station1_N->set_Rsegmentation(RPCGEOM::NumberofRBoundsSt1);
    station1_N->create_strips();	
    _north_arm->set_station1( station1_N );
		
    // segmentation for station 2
    Station_ValueImp* station2_N = new Station_ValueImp( RPCGEOM::North, RPCGEOM::Station2 );
    station2_N->set_z(RPCGEOM::station_z[RPCGEOM::North][RPCGEOM::Station2]);
    station2_N->set_Rsegmentation(RPCGEOM::NumberofRBoundsSt2);
    station2_N->create_strips();	
    _north_arm->set_station2( station2_N );
	
    // segmentation for station 2
    Station_ValueImp* station3_N = new Station_ValueImp( RPCGEOM::North, RPCGEOM::Station3 );
    station3_N->set_z(RPCGEOM::station_z[RPCGEOM::North][RPCGEOM::Station3]);
    station3_N->set_Rsegmentation(RPCGEOM::NumberofRBoundsSt3);
    station3_N->create_strips();	
    _north_arm->set_station3( station3_N );
  }
		
}
						
//___________________________________________________________
/* 
   IO interface.
   This part of the code should not need to be changed when implementing
   new versions of the geometry.
*/
RpcArm* RpcGeom::get_arm( unsigned int arm )
{
	
  switch( arm )
    { 
    case RPCGEOM::North: 
      return north_arm();
      break;
    case RPCGEOM::South:
      return south_arm();
      break;
    default:
      {
	ostringstream what;
	what << "RpcGeom::get_arm - invalid index: " << arm;
	throw runtime_error( what.str() );
      }
      break;
    }
	
}

//___________________________________________________________
RpcArm* RpcGeom::north_arm( void )
{
  if( !_north_arm ) create_arms();
  return _north_arm;
}

//___________________________________________________________
RpcArm* RpcGeom::south_arm( void )
{
  if( !_south_arm ) create_arms();
  return _south_arm;
}
	
//___________________________________________________________
void RpcGeom::read_arms( PHCompositeNode* top_node )
{
  try {
    // check if arm are already initialized
    if( _north_arm ) throw runtime_error( "RpcGeom::read_arms - North already initialized" );
    if( _south_arm ) throw runtime_error( "RpcGeom::read_arms - South already initialized" );
				
    //! retrieve stationType1 node
    RpcStationIOArray* array = findNode::getClass<RpcStationIOArray>(top_node, node_name().c_str()); 

    if( !array ){
      ostringstream what; 
      what << "RpcGeom::read_arms - cannot find node " << node_name();
      throw runtime_error( what.str() );
    }
			
    // create arms
    _south_arm = new RpcArm( RPCGEOM::South );
    _north_arm = new RpcArm( RPCGEOM::North );
		
    // Get the TClonesArray of interface objects from the PHIOArray
    TClonesArray* clones_array = array->get_array();

    // fill stations
    for(int i = 0; i<clones_array->GetEntries();++i)
      {
	RpcStation *station( static_cast<RpcStation*>( clones_array->At(i) ) );
	if( !station ) throw runtime_error( "RpcGeom::read_arms - read invalid station" );			
	cout 
	  << "RpcGeom::read_arms - read station"
	  << "[" << station->get_arm() << "," << station->get_index() << "]" 
	  << " - " << station->get_n_strips() << " strips"
	  << endl;
			
	RpcArm* arm = 0;
	switch( station->get_arm() )
	  {
	  case RPCGEOM::South:
	    arm = _south_arm;
	    break;
			  
	  case RPCGEOM::North:
	    arm = _north_arm;
	    break;
			  
	  default:
	    throw runtime_error( "RpcGeom::read_arms - invalid arm" );						
	  }
			
	switch( station->get_index() )
	  {
	  case RPCGEOM::Station1:
	    arm->set_station1( station );
	    break;
					
	  case RPCGEOM::Station2:
	    arm->set_station2( station );
	    break;
					
	  case RPCGEOM::Station3:
	    arm->set_station3( station );
	    break;
				
	  default:
	    throw runtime_error( "RpcGeom::read_arms - invalid station" );
	  }
			
      }
	      
		
  } catch(std::exception& e){ cout << e.what() << endl; } 
				
}
	
//___________________________________________________________
void RpcGeom::write_arms( PHCompositeNode* top_node )
{
	
  cout << "RpcGeom::write_arms" << endl;
	
  TClonesArray* clones_array = new TClonesArray( Station_ValueImp().GetName(), 6 );
  new((*clones_array)[0])Station_ValueImp( *static_cast<Station_ValueImp*>( north_arm()->station1() ) );
  new((*clones_array)[1])Station_ValueImp( *static_cast<Station_ValueImp*>( north_arm()->station2() ) );
  new((*clones_array)[2])Station_ValueImp( *static_cast<Station_ValueImp*>( north_arm()->station3() ) );
  new((*clones_array)[3])Station_ValueImp( *static_cast<Station_ValueImp*>( south_arm()->station1() ) );
  new((*clones_array)[4])Station_ValueImp( *static_cast<Station_ValueImp*>( south_arm()->station2() ) );
  new((*clones_array)[5])Station_ValueImp( *static_cast<Station_ValueImp*>( south_arm()->station3() ) );
  
  RpcStationIOArray *array = new RpcStationIOArray( clones_array );
  PHIODataNode<PHObject>* node = new PHIODataNode<PHObject>( array, node_name().c_str() );		 
  top_node->addNode( node );
	
  cout << "RpcGeom::write_arms - done." << endl;
	
}
