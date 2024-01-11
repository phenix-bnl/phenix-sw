// $Id: FvtxEventFilter.cxx,v 1.2 2013/06/16 00:44:44 keyaaron Exp $

/*!
  \file		MuonTrigFilter.cxx
  \ingroup supermodules
  \brief	 fvtx based offline event filter to select special events for ouput
  \author	 Aaron Key
  \version $Revision: 1.2 $
  \date		$Date: 2013/06/16 00:44:44 $
*/

#include <Fun4AllReturnCodes.h>
#include <PHTimer.h>
#include <TFvtxHitMap.h>

#include "FvtxEventFilter.h"

using namespace std;

//__________________________________________________________________________
FvtxEventFilter::FvtxEventFilter( const char* name, const MODE& mode, const ACTION& action ) :
  SubsysReco( name ),
  _min_fvtx_hits(450),
  _timer(name),
  _mode(mode),
  _action(action),
  _event_accepted(true),
  _nevent(0),
  _nrejected(0),  
  _hit_map(0)
{
  cout << "FvtxEventFilter::FvtxEventFilter - name: " << name << " mode: " << mode << endl;
}

//______________________________________________________
int FvtxEventFilter::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "FvtxEventFilter::InitRun" );
  try { TMutNode<TFvtxHitMap>::find_node( top_node, "TFvtxHitMap" ); } catch( exception &e ) { cout << e.what() << endl; }
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//__________________________________________________________________________
int FvtxEventFilter::process_event(PHCompositeNode *top_node)
{
  ++_nevent;
  _timer.restart();

  // trigger decision
  _event_accepted = true;

  // Offline based event selection
  switch ( _mode ) {

    case HIGH_HIT_MULT:
    if( !is_high_multiplicity( top_node ) )
    { _event_accepted = false; }
    break;

    case NONE:
    static unsigned int count( 0 );
    if( verbosity == 1 ) {
      if( count < 5 ) cout << "FvtxEventFilter::process_event [" << ThisName
        << "] - mode is NONE. Doing nothing." << endl;
      if( count == 5 ) cout << "FvtxEventFilter::process_event ["
        << ThisName << "]	 - mode is NONE - message disabled.\n";
      count++;
    }
    break;

    default: break;
  }

  _timer.stop();

  // return if event is accepted
  if( _event_accepted ) return EVENT_OK;

  // take proper action if event is rejected
  _nrejected ++;
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
int FvtxEventFilter::End(PHCompositeNode* top_node)
{
//   _timer.get()->print_stat();
  MUTOO::PRINT(cout, ThisName );

  cout
    << ThisName
    << " mode: " << _mode
    << " total: " << _nevent
    << " accepted: " << _nevent - _nrejected
    << " rejected: " << _nrejected
    << " rejection: " << _nrejected/static_cast<double>(_nevent)
    << endl;
  MUTOO::PRINT(cout,"**");
  return 0;
}

//__________________________________________________________________________
bool FvtxEventFilter::is_high_multiplicity(PHCompositeNode* top_node) 
{

  try 
    {
    // retrieves map of coords
    _hit_map = TMutNode<TFvtxHitMap>::find_node( top_node, "TFvtxHitMap" );   
    return ( (_hit_map->get(FVTXOO::North).count() > _min_fvtx_hits) ? 
             true : _hit_map->get(FVTXOO::South).count() > _min_fvtx_hits );

    } 
  catch( exception &e ) {cout << e.what() << endl;}

  return false;
}

