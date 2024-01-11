// $Id: MuonArmSelect.cxx,v 1.4 2011/07/14 22:27:09 pinkenbu Exp $

/*!
  \file    MuonArmSelect.cxx  
  \ingroup supermodules
  \brief   remove all hits from map for arm wich do not match the module selection
  \author  Hugo Pereira
  \version $Revision: 1.4 $
  \date    $Date: 2011/07/14 22:27:09 $
*/

// local
#include "MuonArmSelect.h"
#include "MuonUtil.h"

// extern
//INCLUDECHECKER: Removed this line: #include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>

// MUTOO
#include <TMutHitMap.h>
#include <TMuiHitMapO.h>
#include <PHTimer.h>

using namespace std;

//__________________________________________________________________________
MuonArmSelect::MuonArmSelect( const char* name, MuonArmSelect::MODE mode ) : 
  SubsysReco( name ),
  _mode( mode ),
  _timer( PHTimeServer::get()->insert_new( name ) )
{
  
  _mut_hits_tot.assign( 0 );
  _mui_hits_tot.assign( 0 );
  _mut_hits_rejected.assign( 0 );
  _mui_hits_rejected.assign( 0 );

}

//__________________________________________________________________________
int MuonArmSelect::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();  
  
  // do nothing if mode is NONE
  if( _mode == NONE )
  {
    cout << "MuonArmSelect::process_event [" << ThisName << "] - mode is NONE. Doing nothing." << endl;
    return EVENT_OK;
  }
    
  // level2 decision array (used only for LEVEL2 mode)
  boost::array< bool, MUTOO::NumberOfArms > l2_accept;
  l2_accept.assign( false );
  if( _mode == LEVEL2 ) {
  
    // retrieve muid decisions
    l2_accept[ MUTOO::South ] = MuonUtil::get_l2_trigger_decision( top_node, "L2MuidDimuonSouthTrigger" );
    l2_accept[ MUTOO::North ] = MuonUtil::get_l2_trigger_decision( top_node, "L2MuidDimuonNorthTrigger" );
  
  }
  
  // try load muid hit_map
  try {
    
    // retrieve the TMuiHitMapO
    TMuiHitMapO *mui_hit_map = TMutNode<TMuiHitMapO>::find_node( top_node, "TMuiHitMapO" );
    _mui_hits_tot[ MUTOO::South ] += mui_hit_map->get( MUTOO::South ).count(); 
    _mui_hits_tot[ MUTOO::North ] += mui_hit_map->get( MUTOO::North ).count(); 

    TMuiHitMapO::iterator mui_hit_iter = mui_hit_map->range();
    while( TMuiHitMapO::pointer mui_hit_ptr = mui_hit_iter.next() )
    {
      
      // decide if hit is to be rejected
      bool accepted( true );
      switch (_mode )
      {
        
        case LEVEL2:
        if( !l2_accept[mui_hit_ptr->get()->get_arm()] ) accepted = false;
        break;
      
        case SOUTH:
        if( mui_hit_ptr->get()->get_arm() != MUTOO::South ) accepted = false;
        break;
        
        case NORTH:
        if( mui_hit_ptr->get()->get_arm() != MUTOO::North ) accepted = false;
        break;
        
        default: break;
        
      }
    
      // reject hit if required
      if( !accepted ) {
        _mui_hits_rejected[ mui_hit_ptr->get()->get_arm() ] ++;
        mui_hit_map->erase(mui_hit_ptr->get()->get_key());
      }  
    
    }
    
  } catch( exception& e ) { cout << e.what() << endl;}
  
  // try load mutr hit_map
  try {
    
    // retrieve the TMuiHitMapO
    TMutHitMap* mut_hit_map = TMutNode<TMutHitMap>::find_node( top_node, "TMutHitMap" );
    _mut_hits_tot[ MUTOO::South ] += mut_hit_map->get( MUTOO::South ).count(); 
    _mut_hits_tot[ MUTOO::North ] += mut_hit_map->get( MUTOO::North ).count(); 
    
    TMutHitMap::iterator mut_hit_iter = mut_hit_map->range();
    while( TMutHitMap::pointer mut_hit_ptr = mut_hit_iter.next() )
    {
      
      // decide if hit is to be rejected
      bool accepted( true );
      switch (_mode )
      {
        
        case LEVEL2:
        if( !l2_accept[mut_hit_ptr->get()->get_arm()] ) accepted = false;
        break;
      
        case SOUTH:
        if( mut_hit_ptr->get()->get_arm() != MUTOO::South ) accepted = false;
        break;
        
        case NORTH:
        if( mut_hit_ptr->get()->get_arm() != MUTOO::North ) accepted = false;
        break;
        
        default: break;
        
      }
    
      // reject hit if required
      if( !accepted ) {
        _mut_hits_rejected[ mut_hit_ptr->get()->get_arm() ] ++;
        mut_hit_map->erase(mut_hit_ptr->get()->get_key());
      }  
    
    }
    
  } catch( exception& e ) { cout << e.what() << endl;}
  
  _timer.get()->stop();
  return EVENT_OK;
}
  
//__________________________________________________________________________
int MuonArmSelect::End(PHCompositeNode* top_node) 
{
// 	_timer.get()->print_stat();
  MUTOO::PRINT(cout, ThisName );
  cout << "mui hits total south: " << _mui_hits_tot[MUTOO::South] << " rejected: " << _mui_hits_rejected[MUTOO::South] << endl;
  cout << "mut hits total south: " << _mut_hits_tot[MUTOO::South] << " rejected: " << _mut_hits_rejected[MUTOO::South] << endl;
  cout << "mui hits total north: " << _mui_hits_tot[MUTOO::North] << " rejected: " << _mui_hits_rejected[MUTOO::North] << endl;
  cout << "mut hits total north: " << _mut_hits_tot[MUTOO::North] << " rejected: " << _mut_hits_rejected[MUTOO::North] << endl;
  MUTOO::PRINT(cout,"**");  
  return 0;
}

