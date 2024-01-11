// $Id: L2PrimitiveMatch.C,v 1.3 2013/01/17 22:06:01 phnxbld Exp $

/*! 
  \file L2PrimitiveMatch.C
  \brief matches the offline muid roads to the level2 primitives. 
  check level2 pairs from offline pairs against level2 requirements
*/

#include <iostream>
#include <Lvl2OutArray.h>
#include <TMutNode.h>
#include <list>

#include <MUTOO.h>
#include <PHMuoTracksOut.h>
#include <PHCompositeNode.h>
#include "Tools.h"
 
using namespace std;

Lvl2OutArray* Tools::_lvl2_out_array = 0;
int Tools::_verbosity = 0;

//__________________________________________________________________________________________
//! get lvl2 muid valid primitive informations ( theta and phi ) associated to a given road
Tools::L2MuidDataList Tools::GetMuidPrimitives( int imu, int iroad, PHMuoTracksOut* muo )
{

  // prepare output list		
  L2MuidDataList out;
  
  // check level2 
  if( !_lvl2_out_array ) return out;
  
  // load level2 muid roads from output array
  _lvl2_out_array->GetPrimitive("L2MuiTracks");
  
  // get the track arm (south=0, north=1) and corresponding roads
  //  int arm = (muo->get_pz(0,imu)<0)?0:1;
//   L2MuiTracksRBP l2_mui_roads(arm);
//   if( !l2_mui_roads.wasRead() ) {
//     cout << "Tools::GetMuidPrimitives - L2 muitracks was not read: iarm = " << arm << endl;
//     return out;
//   }
  
  // check number of primitives vs trigger decision
//   if( l2_mui_roads->tracks.size() < 2 && L2MuidDecision( arm ) )
//   { cout << "Tools::GetMuidPrimitives - something wrong: no Muid track primitive found for arm " << arm << " whereas the trigger decision is true" << endl; }
  
  // get offline road informations
  double offline_theta = atan2( sqrt( MUTOO::SQUARE(muo->get_muIDOO_gap0(0,iroad,imu)) 
    + MUTOO::SQUARE(muo->get_muIDOO_gap0(1,iroad,imu)) ),
    fabs(muo->get_muIDOO_gap0( 2, iroad, imu )) );
  
  double offline_phi = atan2( muo->get_muIDOO_gap0(1,iroad,imu) ,
    muo->get_muIDOO_gap0(0,iroad,imu) );
  
  if ( _verbosity )
    cout 
    << "Tools::GetMuidPrimitives - New track found with theta and phi" 
    << offline_theta << " " << offline_phi << endl;
  
  // loop over l2 roads
//   for( int l2_index =0; l2_index < (int) l2_mui_roads->tracks.size(); l2_index++) 
//   {
    
//     /* 
//       single road level2 muid requirements:
//       road must have at least 8 hits 
//       road must be deep (depth>=4)
//       road angle wrt beam must be larger than 12 deg
//     */
//     if (! (l2_mui_roads->tracks[l2_index].slope > tan(12.0 * MUTOO::DEG_TO_RAD) ) ) continue;
    
//     if (! (l2_mui_roads->tracks[l2_index].hitsum >= 8)) continue;
//     if (! (l2_mui_roads->tracks[l2_index].depth >= 4)) continue; 
    
//     // get road angles
//     double l2_theta = atan2( sqrt( MUTOO::SQUARE( l2_mui_roads->tracks[l2_index].direction[0])+ MUTOO::SQUARE(l2_mui_roads->tracks[l2_index].direction[1]) ) , 
//       fabs(l2_mui_roads->tracks[l2_index].direction[2]) );
    
//     double l2_phi = atan2( l2_mui_roads->tracks[l2_index].direction[1],
//       l2_mui_roads->tracks[l2_index].direction[0] );
    
//     // check if theta angles match
//     double delta_theta( offline_theta - l2_theta );
//     if( fabs( delta_theta ) >= MWGL2::_delta_theta_cut ) continue;				
    
//     // check if phi angles match
//     double delta_phi( offline_phi - l2_phi );
//     if( delta_phi > M_PI ) delta_phi -= 2*M_PI;
//     if( delta_phi < -M_PI ) delta_phi += 2*M_PI;
//     if( fabs( delta_phi ) >= MWGL2::_delta_phi_cut ) continue;
//     if ( _verbosity ) 
//     {
//       cout 
//         << "Tools::GetMuidPrimitives - l2 slope " << l2_mui_roads->tracks[l2_index].slope 
//         << " hitsum "	<< l2_mui_roads->tracks[l2_index].hitsum
//         << " depth "	 << l2_mui_roads->tracks[l2_index].depth
//         << " l2theta " << l2_theta << " l2phi " << l2_phi << endl;
//     }
//     // primitive is accepted. Add to output
//     out.push_back( MWGL2::L2MuidData( l2_theta, l2_phi ) );
    
//   }
  return out;			
}
    
//__________________________________________________________________________________________
//! returns true if one pair amongst all combinations matching level2 muid triggers
bool Tools::Find_accepted_l2_pair( const L2MuidDataList& l2_road0, const L2MuidDataList& l2_road1 )
{	
		
  // loop over lists
  for( L2MuidDataList::const_iterator iter0 = l2_road0.begin(); iter0 != l2_road0.end(); iter0++ )
  {
    for( L2MuidDataList::const_iterator iter1 = l2_road1.begin(); iter1 != l2_road1.end(); iter1++ )
    {
      
      // get slopes from theta/phi
      double dxdz0 = tan( iter0->_theta ) * sin( iter0->_phi );
      double dydz0 = tan( iter0->_theta ) * cos( iter0->_phi );
      double dxdz1 = tan( iter1->_theta ) * sin( iter1->_phi );
      double dydz1 = tan( iter1->_theta ) * cos( iter1->_phi );
      
      // Opening angle > 19.2												
      double cost = 
        (dxdz0*dxdz1 + dydz0*dydz1 + 1)/
        (sqrt( MUTOO::SQUARE( dxdz0 ) + MUTOO::SQUARE( dydz0 ) + 1 )*
        sqrt( MUTOO::SQUARE( dxdz1 ) + MUTOO::SQUARE( dydz1 ) + 1 ) );
        
      double angle = MUTOO::RAD_TO_DEG*acos(cost);
      if( angle > 19.2) return true;		
        
    }	
  }	
  return false;
    
}

//__________________________________________________________________________________________
// try load level2 primitives from top_node
void Tools::LoadL2Primitives( PHCompositeNode *top_node )
{

  static int count = 0; 
  static string node_name = "Lvl2OutArrayCal";
  static string alternative_node_name = "Lvl2OutArray";
 
  try 
  {
    
    // try load Lvl2OutArray from  Lvl2OutArrayCal node 
    _lvl2_out_array = TMutNode<Lvl2OutArray>::find_io_node( top_node, node_name );
    if( count <= 10 ) cout << "Tools::LoadL2Primitives - primitives loaded from node " << node_name << endl;
  
  } catch( exception &e ) {
    
    // print error message if not found (for first 10 events)
    if( count <= 10 ) 
    {
      cout << "Tools::LoadL2Primitives - unable to load primitives from node " << node_name << endl;
      cout << "Tools::LoadL2Primitives - trying node Lvl2OutArray" << endl;
    }
    
    try {
      
      // try load Lvl2OutArray from  Lvl2OutArray node 
      _lvl2_out_array = TMutNode<Lvl2OutArray>::find_io_node( top_node, alternative_node_name);
      if( count <= 10 ) cout << "Tools::LoadL2Primitives - primitives loaded from node " << alternative_node_name << endl;
      
    } catch( exception &e ) {
      
      // print error message if not found (for first 10 events)
      if( count <= 10 ) { 
        cout << e.what() << endl; 
        cout << "Tools::LoadL2Primitives - unable to load primitives from node " << alternative_node_name << endl;
      }
      
      _lvl2_out_array = 0;      
    }

  }
 
  // disable message
  if( count == 10 ) cout << "Tools::LoadL2Primitives - message disabled" << endl;
  count++;
   
}
//__________________________________________________________________________________________
// returns true if road has at least one associated valid level2 muid primitive
bool Tools::L2MuidRoadOK( int imu, int iroad, PHMuoTracksOut* muo )
{ return GetMuidPrimitives( imu, iroad, muo ).size(); }

//__________________________________________________________________________________________
// return true if one pair of roads have level2 primitives matching the criterions
bool Tools::L2MuidPairOK( int idimu, PHMuoTracksOut* muo )
{
	
  // get dimuon associated track indices
  int imu0 = muo->get_ditrkIndex(0,idimu);
  int imu1 = muo->get_ditrkIndex(1,idimu);
  
  bool accepted( false );
  
  // loop over first track associated roads
  for( int iroad0 = 0; iroad0 < 3 && !accepted; iroad0++ )
  {
    
    // check road
    if( !muo->get_muIDOOhits( iroad0, imu0 ) ) continue;
    
    // get associated primitives
    L2MuidDataList l2_road_0( GetMuidPrimitives( imu0, iroad0, muo ) );
    if( !l2_road_0.size() ) continue;
    
    // loop over second track associated roads
    for( int iroad1=0; iroad1<3 && !accepted; iroad1 ++ )
    {
      // check road
      if( !muo->get_muIDOOhits( iroad1, imu1 ) ) continue;
      
      // get associated primitives
      L2MuidDataList l2_road_1( GetMuidPrimitives( imu1, iroad1, muo ) );
      if( !l2_road_1.size() ) continue;
      
      // check all combinations of primitives
      accepted = Find_accepted_l2_pair( l2_road_0, l2_road_1 );
    }	
  } 
  
  return accepted;
}
