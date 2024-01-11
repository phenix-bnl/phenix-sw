// $Id: MuonEventNumberFilter.cxx,v 1.2 2011/07/14 22:27:09 pinkenbu Exp $

/*!
  \file		MuonEventNumberFilter.cxx
  \ingroup supermodules
  \brief	 muid based offline trigger used to filter data on Deep Deep road pairs
  \author	Sean Kelly/Hugo Pereira
  \version $Revision: 1.2 $
  \date		$Date: 2011/07/14 22:27:09 $
*/

#include <recoConsts.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <EventHeader.h>
#include <TMutNode.h>
#include <fstream>

#include "MuonEventNumberFilter.h"

using namespace std;

//__________________________________________________________________________
MuonEventNumberFilter::MuonEventNumberFilter( const char* name, const ACTION& action ) :
  SubsysReco( name ),
  _action( action ),
  _timer(PHTimeServer::get()->insert_new("MuonEventNumberFilter") ),
  _total_events(0),
  _accepted_events(0)
{ cout << "MuonEventNumberFilter::MuonEventNumberFilter - name: " << name << endl; }

//______________________________________________________
int MuonEventNumberFilter::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "MuonEventNumberFilter::InitRun" );
  cout << "_filename: " << _filename << endl;
  load_events( recoConsts::instance()->get_IntFlag("RUNNUMBER", 0) );
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//__________________________________________________________________________
int MuonEventNumberFilter::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();
  _total_events++;
  try {
    
    if( accept_event( top_node ) )
    { 
      _accepted_events++;
      return EVENT_OK;
    }
    
  } catch( exception &e ) { cout << e.what() << endl; }

  // stop timer
  _timer.get()->stop();

  // take proper action if event is rejected
  switch ( _action ) {
    
    case DISCARD_EVENT:
    return DISCARDEVENT;
    
    case ABORT_EVENT:
    return ABORTEVENT;
    
    case DO_NOTHING:
    return EVENT_OK;
    
    default:
    return EVENT_OK;
   
  }

  // should not reach there
  return EVENT_OK;

}

//__________________________________________________________________________
int MuonEventNumberFilter::End(PHCompositeNode* top_node)
{
//   _timer.get()->print_stat();
  MUTOO::PRINT(cout, ThisName );
  cout << "_total_events: " << _total_events << endl;
  cout << "_accepted_events: " << _accepted_events << endl;
  MUTOO::PRINT(cout,"**");
  return 0;
}

//__________________________________________________________________________
void MuonEventNumberFilter::load_events( int run_number )
{
  
  // clear event list
  _events.clear();
  
  char filename[1024];
  sprintf( filename, _filename.c_str(), run_number );
  ifstream in( filename );
  if( !in ) 
  {  
    cout << "MuonEventNumberFilter::load_events - cannot read file: " << filename << endl;
    cout << "MuonEventNumberFilter::load_events - all events will be rejected." << endl;
    return;
  }
  
  cout << "MuonEventNumberFilter::load_events - loading file: " << filename << endl;
  char line[512];
  while( in.getline( line, 512, '\n' ) )
  {
    istringstream stream( line );
    int event(0);
    stream >> event;
    if( stream.rdstate() & ios::failbit ) 
    {
      cout << "MuonEventNumberFilter::load_events - skipping line " << line << endl;
      continue;
    }
    
    _events.insert( event );
  }
  
  cout << "MuonEventNumberFilter::load_events - loaded " << _events.size() << " events" << endl;
  return;
}

//_______________________________________________________________________
bool MuonEventNumberFilter::accept_event( PHCompositeNode* top_node)
{

  // get event number
  EventHeader* evt = TMutNode<EventHeader>::find_io_node(top_node,"EventHeader");
  int event_number = evt->get_EvtSequence();
  return _events.find( event_number ) != _events.end();

}
