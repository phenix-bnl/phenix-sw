
//////////////////////////////////////////////////////////////
/*!
  \file MuonResources.cxx
  \brief muon code resources evaluation
  \author  Hugo Pereira
  \version $Revision: 1.7 $
*/
//////////////////////////////////////////////////////////////

#include <MUTOO.h>
#include <PHMapBase.h>
#include <PHTFileServer.h>
#include <TMutNode.h>
#include <TTree.h>

#include "MuonResources.h"

using namespace std;

//___________________________________________________________
MuonResources::MuonResources( const char* name, const char* filename):
  SubsysReco( name ),
  _initialized( false ),
  _event_sampling( 100 ),
  _resources( getpid() ),
  _timer( PHTimeServer::get()->insert_new(name) ),
  _file_name( filename ),
  _event( 0 ),
  _start_time( 0 )
{
  MUTOO::PRINT( cout, "MuonResources::MuonResources" );
  return ;
}


//______________________________________________________
void MuonResources::initialize(void)
{
  if( _initialized ) return;
  
  MUTOO::PRINT( cout, "MuonResources::initialize" );

  // create TFile
  PHTFileServer::get().open( _file_name );

  // create tree
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _resources_tree = new TTree( "resources", "cpu/memory resources" );
  _resources_tree->SetAutoSave( AUTO_SAVE );
  
  _resources_tree->Branch( "evt", &_event, "evt/I", BUFFER_SIZE );
  _resources_tree->Branch( "time", &_time, "time/I", BUFFER_SIZE );
  
  _resources_tree->Branch( "vmSize", &_resources.get_current_resources()._vmSize, "vmSize/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmLock", &_resources.get_current_resources()._vmLock, "vmLock/I", BUFFER_SIZE );
  _resources_tree->Branch( "vmResident", &_resources.get_current_resources()._vmResident, "vmResident/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmData", &_resources.get_current_resources()._vmData, "vmData/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmStack", &_resources.get_current_resources()._vmStack, "vmStack/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmExec", &_resources.get_current_resources()._vmExec, "vmExec/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmLib", &_resources.get_current_resources()._vmLib, "vmLib/I", BUFFER_SIZE );   
  
  _resources_tree->Branch( "vmSize_inc", &_resources.get_diff_resources()._vmSize, "vmSize/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmLock_inc", &_resources.get_diff_resources()._vmLock, "vmLock/I", BUFFER_SIZE );
  _resources_tree->Branch( "vmResident_inc", &_resources.get_diff_resources()._vmResident, "vmResident/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmData_inc", &_resources.get_diff_resources()._vmData, "vmData/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmStack_inc", &_resources.get_diff_resources()._vmStack, "vmStack/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmExec_inc", &_resources.get_diff_resources()._vmExec, "vmExec/I", BUFFER_SIZE );   
  _resources_tree->Branch( "vmLib_inc", &_resources.get_diff_resources()._vmLib, "vmLib/I", BUFFER_SIZE );   
  
  // maps
  for( MapMultiplicity_list::iterator iter = _maps.begin(); iter != _maps.end(); iter++ )
  {
    string name( iter->_name );
    string var_name = name + "/I";
    int* var = &(iter->_diff);
    _resources_tree->Branch( name.c_str(), var, var_name.c_str(), BUFFER_SIZE );
    cout << "registering map " << name << endl;
  }

  // initialize
  _resources.set_verbosity( verbosity >= 1 ? MUTOO::SOME:MUTOO::NONE );
  _resources.update( "initialization" );
  _initialized = true;
  MUTOO::PRINT( cout, "**" );
  return;
}

//______________________________________________________
int MuonResources::Init(PHCompositeNode *topNode)
{
  initialize();
  return 0;
}

//______________________________________________________
void MuonResources::add_map( const std::string& name )
{ _maps.push_back( MapMultiplicity( name ) ); }

//______________________________________________________
int MuonResources::process_event(PHCompositeNode *top_node)
{

  _timer.get()->restart();
  try{

    // check event sampling and update PHResources if needed
    if( (_event_sampling <= 1 ) || (_event%_event_sampling == 0 ) )
    { 
    
      // update resources
      _resources.update(); 
      
      // retrieve map sizes
      for( MapMultiplicity_list::iterator iter = _maps.begin(); iter != _maps.end(); iter++ )
      { iter->update( top_node ); }
    
      // update time
      _time = time(0) - _start_time;

      // fill tree 
      if( _resources_tree ) _resources_tree->Fill();
      
    }

    // increment local event counter
    _event++;

  } catch( exception& e ) { cout << e.what() << endl; }
  _timer.get()->stop();

  return 0;
}

//______________________________________________________
int MuonResources::End(PHCompositeNode* top_node)
{

//   _timer.get()->print_stat();
  PHTFileServer::get().write( _file_name );
  return 0;
  
}

//_____________________________________________________________
bool MuonResources::MapMultiplicity::update( PHCompositeNode* top_node )
{

  if( !top_node ) return false;
  try {
    
    PHMapBase* map_base( TMutNode<PHMapBase>::find_node( top_node, _name ) );
    _current = map_base->get_accumulated_size();
    _diff = _current - _previous;
    _previous = _current;
    return true;
    
  } catch( exception &e ) { cout << e.what() << endl; }
  
  return false;
  
}
