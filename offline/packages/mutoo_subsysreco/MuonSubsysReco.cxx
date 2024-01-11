// $Id: MuonSubsysReco.cxx,v 1.21 2014/04/16 15:52:56 brooks Exp $

/*!
  \file MuonSubsysReco.cxx
  \ingroup supermodules 
  \brief muon implementation of fun4all subsysreco
  \author H. Pereira Da Costa
  \version $Revision: 1.21 $
  \date $Date: 2014/04/16 15:52:56 $
*/

#include "MuonSubsysReco.h"
#include <TMutExtVtx.h>
#include <PHMapManager.h>
#include <iostream>

using namespace std;

//__________________________________________________
set<unsigned int> MuonSubsysReco::_rows;
map<unsigned int, string> MuonSubsysReco::_row_map;
unsigned int MuonSubsysReco::_min_row = 0;
unsigned int MuonSubsysReco::_max_row = 0;
MuonSubsysReco::Status MuonSubsysReco::_map_status = MuonSubsysReco::CLEARED;

//__________________________________________________
MuonSubsysReco::MuonSubsysReco( const char* name ):
  SubsysReco( name ),
  _initialized( false ),
  _row( 0 )
{}

//__________________________________________________
MuonSubsysReco::~MuonSubsysReco( void )
{
  
  // clear from rows
  _rows.erase( _row ); 
  _update_min_max();

}

//__________________________________________________
int MuonSubsysReco::Init( PHCompositeNode* )
{
  if( _initialized ) return 0;
  _initialized = true;
  _row = _get_row( ThisName );
  
  if( verbosity > 1 ) cout << "MuonSubsysReco::Init - " << ThisName << " has row: " << _row << endl;
  return 0;
}

//__________________________________________________
int MuonSubsysReco::ResetEvent( PHCompositeNode* )
{
  
  // clear map if needed
  // this makes valgrind happy.
  clear_maps_if_needed();
  return 0;
  
}

//__________________________________________________
void MuonSubsysReco::print_rows( void )
{
  if( _rows.empty() ) return;
  
  MUTOO::PRINT( cout, "MuonSubsysReco::print_rows" );
  for( map< unsigned int, string>::const_iterator iter = _row_map.begin(); iter != _row_map.end(); iter++ )
  { cout << "row: " << iter->first << " module name: " << iter->second << endl; }
  MUTOO::PRINT( cout, "**" );
  
}

//__________________________________________________
void MuonSubsysReco::clear_maps_if_needed( void )
{ 
  
  // check initialization
  if( !_initialized )
  {
    cout << "MuonSubsysReco::clear_maps_if_needed - module " << ThisName << endl;
    cout << "MuonSubsysReco::clear_maps_if_needed - the method is called while the module 'row' was not initialized." << endl;
    cout << "MuonSubsysReco::clear_maps_if_needed - this probably means that the base class Init method was not called." << endl;
    cout << "MuonSubsysReco::clear_maps_if_needed - this is a serious issue since it might mess up the map contents." << endl;
    cout << "MuonSubsysReco::clear_maps_if_needed - fix the code or remove the module." << endl;
    exit(1);
  }
  
  if( is_last() ) 
  {
  
    if( verbosity > 1 )
    { cout << "MuonSubsysReco::clear_maps_if_needed (" << ThisName << ")" << endl; }
    
//     static bool first = true;
//     // check if write_maps has been called
//     // the message is not emmited at first call because
//     // maps are never written prior to first event. 
//     if( !( _map_status == WRITTEN || first ) )
//     {
//       cout 
//         << "MuonSubsysReco::clear_maps_if_needed (" << ThisName << ") - "
//         << "WARNING: maps have not been written since last call" << endl;
//     } 
//     first = false;
    
    PHMapManager::enable_clear();
    PHMapManager::clear();
    PHMapManager::disable_clear();
    _map_status = CLEARED;
    
  }
  
  
}
  
//__________________________________________________
void MuonSubsysReco::write_maps_if_needed( void )
{ 
 
  // check initialization
  if( !_initialized )
  {
    cout << "MuonSubsysReco::write_maps_if_needed - module " << ThisName <<  endl;
    cout << "MuonSubsysReco::write_maps_if_needed - the method is called while the module 'row' was not initialized." << endl;
    cout << "MuonSubsysReco::write_maps_if_needed - this probably means that the base class Init method was not called." << endl;
    cout << "MuonSubsysReco::write_maps_if_needed - this is a serious issue since it might mess up the map contents." << endl;
    cout << "MuonSubsysReco::write_maps_if_needed - fix the code or remove the module." << endl;
    exit(1);
  }
 
  if( is_last() ) 
  {
    if( verbosity > 1 )
    { cout << "MuonSubsysReco::write_maps_if_needed (" << ThisName << ")" << endl; }
    
    //! check if write_maps has been called
    if( _map_status != CLEARED )
    {
      cout 
        << "MuonSubsysReco::write_maps_if_needed (" << ThisName << ") - "
        << "WARNING: maps have not been cleared since last call." << endl;
    }
      
    PHMapManager::write(); 
    _map_status = WRITTEN;
  }

}

//__________________________________________________
void MuonSubsysReco::load_vertex_if_needed( PHCompositeNode* top_node )
{ 

  // check initialization
  if( !_initialized )
  {
    cout << "MuonSubsysReco::clear_maps_if_needed - module " << ThisName << endl;
    cout << "MuonSubsysReco::clear_maps_if_needed - the method is called while the module 'row' was not initialized." << endl;
    cout << "MuonSubsysReco::clear_maps_if_needed - this probably means that the base class Init method was not called." << endl;
    cout << "MuonSubsysReco::clear_maps_if_needed - this is a serious issue since it might mess up the map contents." << endl;
    cout << "MuonSubsysReco::clear_maps_if_needed - fix the code or remove the module." << endl;
    exit(1);
  }
  
  if( is_first() ) 
    {
      TMutExtVtx::get().load_vtx( top_node ); 
    }
}

//_________________________________________________________________________________
unsigned int MuonSubsysReco::_get_row( const std::string& name )
{
  if( _rows.empty() ) 
  {
    _rows.insert( 0 );
    _row_map.insert( make_pair( 0, name ) );
    return 0;
  } 

  // insert new integer which is 1 higher than the max in the set
  unsigned int new_row( *std::max_element( _rows.begin(), _rows.end() )+1 );
  _rows.insert( new_row );
  _row_map.insert( make_pair( new_row, name ) );
  _update_min_max();
    
  return new_row;

}

//_________________________________________________________________________________
void MuonSubsysReco::_update_min_max( void )
{
  if( _rows.empty() ) _min_row = _max_row = 0;
  else {
    _min_row = *(_rows.begin());
    _max_row = *(_rows.rbegin());
  }
}
