
#include "RpcGeom_v1.h"
#include "RpcStationIOArray.h"
#include "RpcStation_v1.h"

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
typedef RpcStation_v1 Station_ValueImp;
	
RpcArm_v1* RpcGeom_v1::_south_arm( 0 );
RpcArm_v1* RpcGeom_v1::_north_arm( 0 );
	
//___________________________________________________________

void RpcGeom_v1::create_arms_v1( void )
{
  
  {
    cout << "RpcGeom_v1::initialize - initializing south arm geometry" << endl;
    _south_arm = new RpcArm_v1( RPCFULLGEOM::South );
		
    // segmentation for station 1
    Station_ValueImp* station1_S = new Station_ValueImp( RPCFULLGEOM::South, RPCFULLGEOM::Station1 );
    station1_S->set_z(RPCFULLGEOM::GlobalFramePosition_Z[RPCFULLGEOM::Station1][RPCFULLGEOM::South]);
    //station1_S->set_Rsegmentation( RPCFULLGEOM::NumberofRBoundsSt1 );
    station1_S->create_strips();
    _south_arm->set_station1( station1_S );
		
    // segmentation for station 2
    Station_ValueImp* station2_S = new Station_ValueImp( RPCFULLGEOM::South, RPCFULLGEOM::Station2 );
    station2_S->set_z(RPCFULLGEOM::GlobalFramePosition_Z[RPCFULLGEOM::Station2][RPCFULLGEOM::South]);
    //station2_S->set_Rsegmentation(RPCFULLGEOM::NumberofRBoundsSt2 );
    station2_S->create_strips();	
    _south_arm->set_station2( station2_S );
    
    // segmentation for station 3
    Station_ValueImp* station3_S = new Station_ValueImp( RPCFULLGEOM::South, RPCFULLGEOM::Station3 );
    station3_S->set_z(RPCFULLGEOM::GlobalFramePosition_Z[RPCFULLGEOM::Station3][RPCFULLGEOM::South]);
    //station3_S->set_Rsegmentation(RPCFULLGEOM::NumberofRBoundsSt3);
    station3_S->create_strips();	
    _south_arm->set_station3( station3_S );
  }
		
  {	
    cout << "RpcGeom_v1::initialize - initializing north arm geometry" << endl;
    _north_arm = new RpcArm_v1( RPCFULLGEOM::North );
		
    // segmentation for station 1
    Station_ValueImp* station1_N = new Station_ValueImp( RPCFULLGEOM::North, RPCFULLGEOM::Station1 );
    station1_N->set_z(RPCFULLGEOM::GlobalFramePosition_Z[RPCFULLGEOM::Station1][RPCFULLGEOM::North]);
    //station1_N->set_Rsegmentation(RPCFULLGEOM::NumberofRBoundsSt1);
    station1_N->create_strips();
    _north_arm->set_station1( station1_N );
		
    // segmentation for station 2
    Station_ValueImp* station2_N = new Station_ValueImp( RPCFULLGEOM::North, RPCFULLGEOM::Station2 );
    station2_N->set_z(RPCFULLGEOM::GlobalFramePosition_Z[RPCFULLGEOM::Station2][RPCFULLGEOM::North]);
    //station2_N->set_Rsegmentation(RPCFULLGEOM::NumberofRBoundsSt2);
    station2_N->create_strips();
    _north_arm->set_station2( station2_N );
	
    // segmentation for station 2
    Station_ValueImp* station3_N = new Station_ValueImp( RPCFULLGEOM::North, RPCFULLGEOM::Station3 );
    station3_N->set_z(RPCFULLGEOM::GlobalFramePosition_Z[RPCFULLGEOM::Station3][RPCFULLGEOM::North]);
    //station3_N->set_Rsegmentation(RPCFULLGEOM::NumberofRBoundsSt3);
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
RpcArm_v1* RpcGeom_v1::get_arm( unsigned int arm )
{
  switch( arm )
    { 
    case RPCFULLGEOM::North: 
      return north_arm();
      break;
    case RPCFULLGEOM::South:
      return south_arm();
      break;
    default:
      {
	ostringstream what;
	what << "RpcGeom_v1::get_arm - invalid index: " << arm;
	throw runtime_error( what.str() );
      }
      break;
    }
	
}

//___________________________________________________________
RpcArm_v1* RpcGeom_v1::north_arm( void )
{
  if( !_north_arm ) create_arms();
  return _north_arm;
}

//___________________________________________________________
RpcArm_v1* RpcGeom_v1::south_arm( void )
{
  if( !_south_arm ) create_arms();
  return _south_arm;
}
	
//___________________________________________________________
void RpcGeom_v1::read_arms( PHCompositeNode* top_node )
{
  try {
    // check if arms are already initialized
    if( _north_arm ) throw runtime_error( "RpcGeom_v1::read_arms - North already initialized" );
    if( _south_arm ) throw runtime_error( "RpcGeom_v1::read_arms - South already initialized" );
				
    //! retrieve stationType1 node
    RpcStationIOArray* array = findNode::getClass<RpcStationIOArray>(top_node, node_name().c_str()); 

    if( !array ){
      ostringstream what; 
      what << "RpcGeom::read_arms - cannot find node " << node_name();
      throw runtime_error( what.str() );
    }
			
    // create arms
    _south_arm = new RpcArm_v1( RPCFULLGEOM::South );
    _north_arm = new RpcArm_v1( RPCFULLGEOM::North );
		
    // Get the TClonesArray of interface objects from the PHIOArray
    TClonesArray* clones_array = array->get_array();

    // fill stations
    for(int i = 0; i<clones_array->GetEntries();++i)
      {
	RpcStation_v1 *station( static_cast<RpcStation_v1*>( clones_array->At(i) ) );
	if( !station ) throw runtime_error( "RpcGeom_v1::read_arms - read invalid station" );			
	cout 
	  << "RpcGeom_v1::read_arms - read station"
	  << "[" << station->get_arm() << "," << station->get_index() << "]" 
	  << " - " << station->get_n_strips() << " strips"
	  << endl;
			
	RpcArm_v1* arm = 0;
	switch( station->get_arm() )
	  {
	  case RPCFULLGEOM::South:
	    arm = _south_arm;
	    break;
			  
	  case RPCFULLGEOM::North:
	    arm = _north_arm;
	    break;
			  
	  default:
	    throw runtime_error( "RpcGeom_v1::read_arms - invalid arm" );						
	  }
			
	switch( station->get_index() )
	  {
	  case RPCFULLGEOM::Station1:
	    arm->set_station1( station );
	    break;
					
	  case RPCFULLGEOM::Station2:
	    arm->set_station2( station );
	    break;
					
	  case RPCFULLGEOM::Station3:
	    arm->set_station3( station );
	    break;
				
	  default:
	    throw runtime_error( "RpcGeom_v1::read_arms - invalid station" );
	  }
			
      }
	      
		
  } catch(std::exception& e){ cout << e.what() << endl; } 
				
}
	
//___________________________________________________________
void RpcGeom_v1::write_arms( PHCompositeNode* top_node )
{
	
  cout << "RpcGeom_v1::write_arms" << endl;
	
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
	
  cout << "RpcGeom_v1::write_arms - done." << endl;
	
}
