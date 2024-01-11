// $Id: MuonCounter.cxx,v 1.5 2013/10/09 22:43:57 jinhuang Exp $

#include <iostream>
#include <EventHeader.h>
#include <MUTOO.h>
#include <PHCompositeNode.h>
#include <PHTimeServer.h>
#include <PHMapManager.h>
#include <recoConsts.h>

#include "MuonSubsysReco.h"
#include "MuonCounter.h"
#include "MuonUtil.h"

using namespace std;

//_____________________________________________________
MuonCounter::MuonCounter( const char* name ):
  SubsysReco( name ),
  _local_counter( 0 ),
  _event_dump( 1 ),
  _log_scale_dump(false)
{}

//_____________________________________________________
int MuonCounter::process_event(PHCompositeNode *top_node)
{ 

  _local_counter++;
  
  // check against dump
  if (_log_scale_dump)
    {
      const double significand = _local_counter/pow(10,(int)(log10(_local_counter)));

      if (fmod (significand,1) == 0 )
        {
          // do nothing here. Proceed to dump

        }
      else return 0;

    }
  else if( _event_dump <= 0 )
  {
    
    static bool first( true );
    if( first )
    {
      cout << "MuonCounter::process_event - _event_dump is non positive. No event counter is displayed" << endl;
      first = false;
    }
    
    return 0;
    
  } else if( _event_dump > 1 && _local_counter % _event_dump ) return 0;
  
  // retrieve run number
  int run_number = recoConsts::instance()->get_IntFlag("RUNNUMBER", 0);
  
  // retrieve event number
  int event_number( -1 );
  try {
    EventHeader* evt = TMutNode<EventHeader>::find_io_node(top_node,"EventHeader");
    event_number = evt->get_EvtSequence();
  } catch (exception& e) { }   
  
  ostringstream what;
  what << "run=" <<  run_number << " event=" << event_number << " local=" << _local_counter;
  MUTOO::PRINT( cout, what.str().c_str() );
  return 0;

}

//_____________________________________________________
int MuonCounter::End(PHCompositeNode *topNode)
{ 
  
  // print module rows
  MuonSubsysReco::print_rows();
    
  // print map statistics
  PHMapManager::print_stat();
  
  // print server statistics
  PHTimeServer::get()->print_stat();
  return 0;
}
