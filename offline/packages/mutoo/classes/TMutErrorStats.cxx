// $Id: TMutErrorStats.cxx,v 1.10 2011/12/24 04:48:21 slash Exp $
//////////////////////////////////////////////////////////////////
/*
  \class TMutErrorStat.h
  \author S.Kelly
  \brief Maintains statistics on MUIOO/MUTOO exceptions
  \version $Revision: 1.10 $
  \date		$Date: 2011/12/24 04:48:21 $
*/
//////////////////////////////////////////////////////////////////

#include"PHTFileServer.h"
#include"TMutErrorStats.h"
#include"TMutHitMap.h"
#include"TMutStubMap.h"
#include"TMutTrkMap.h"

#include<TNtuple.h>
#include<list>

using namespace std;

list<TMutErrorStats::Error> TMutErrorStats::_event_errors;
string TMutErrorStats::_filename = "muon_eval.root";
TNtuple* TMutErrorStats::_tuple_event=0;
TNtuple* TMutErrorStats::_tuple_error=0;
bool TMutErrorStats::_initialized = false;

//______________________________________________
bool TMutErrorStats::is_error(unsigned short type)
{
  list<Error>::iterator error_iter = _event_errors.begin();
  for(; error_iter!=_event_errors.end(); ++error_iter)
  if( error_iter->type == type ) return true;
  return false;
}

//______________________________________________
bool TMutErrorStats::is_error() // returns TRUE if there was an error of either kind
{ return (TMutErrorStats::is_error(STUB_BIFURCATE) || TMutErrorStats::is_error(CLONE_TRACK) ); }

//______________________________________________
void TMutErrorStats::set_error(
  ErrorType type, unsigned short arm, unsigned short station, unsigned short octant,
  ULong_t hit_occ, ULong_t stub_occ, ULong_t trk_occ, unsigned short time_ms)
{
  Error error( type, arm, station, octant, hit_occ, stub_occ, trk_occ, time_ms);
  _event_errors.push_back( error );
}

//______________________________________________
void TMutErrorStats::set_event_stats(PHCompositeNode* top_node, unsigned short time_ms)
{
  TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
  TMutStubMap* stub_map = TMutNode<TMutStubMap>::find_node(top_node,"TMutStubMap");
  TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");

  if(_event_errors.size() == 0 )
    for(int arm=0; arm<MUTOO::NumberOfArms; ++arm)
    for(int sta=0; sta<MUTOO::NumberOfStations; ++sta)
    for(int oct=0; oct<MUTOO::NumberOfOctants; ++oct)
  {

    ULong_t hit_occ = hit_map->get(arm,sta,oct).count();
    ULong_t stub_occ = stub_map->get(arm,sta,oct).count();
    ULong_t trk_occ = trk_map->get(arm,oct).count();
    set_error(NO_ERROR,arm,sta,oct,hit_occ,stub_occ,trk_occ,time_ms);

  } else {

    list<Error>::iterator iter = _event_errors.begin();
    for(;iter!=_event_errors.end(); ++iter)
    {
      iter->hit_occ = hit_map->get(iter->arm,iter->station,iter->octant).count();
      iter->stub_occ = stub_map->get(iter->arm,iter->station,iter->octant).count();
      iter->trk_occ = trk_map->get(iter->arm,iter->octant).count();
      iter->time_ms = time_ms;
    }
  }
}

//______________________________________________
bool TMutErrorStats::initialize_ntuple( void )
{
  MUTOO::TRACE( "TMutErrorStats::initialize_ntuple" );
  if( _initialized ) return false;

  PHTFileServer::get().open( _filename, "RECREATE" );
  _tuple_error = new TNtuple("errors","errors","type:arm:station:octant:nhit:nstub:ntrk:time");
  _tuple_event = new TNtuple("event","event","south_err:north_err:time");
  _initialized = true;

  return true;

}

//______________________________________________
void TMutErrorStats::print_event()
{
  MUTOO::PRINT(cout,"TMutErrorStats");
  list<Error>::const_iterator iter = _event_errors.begin();
  for(;iter!=_event_errors.end(); ++iter) iter->print();
  MUTOO::PRINT(cout,"**");
}

//______________________________________________
void TMutErrorStats::write_event()
{

  // check initialization.
  if( !_initialized ) return;

  // Flush tuple every 500 events
  static unsigned short cnt = 0;
  if( !(++cnt%500) ){
    _tuple_event->AutoSave();
    _tuple_error->AutoSave();
    PHTFileServer::get().flush(_filename);
  }

  // Per event ntuple
  {
    list<Error>::const_iterator iter = _event_errors.begin();
    float event_var[3] = {0};
    for(;iter!=_event_errors.end(); ++iter)
    if(iter->type == NO_ERROR) {
      event_var[2] = iter->time_ms;
      _tuple_event->Fill(event_var);
      break;
    } else {
      iter->arm==MUTOO::South ? event_var[0]++ : event_var[1]++;
      event_var[2] = iter->time_ms;
    }
    _tuple_event->Fill(event_var);
  }

  // Per error ntuple
  {
    list<Error>::const_iterator iter = _event_errors.begin();
    float error_var[8] = {0};
    for(;iter!=_event_errors.end(); ++iter){
      error_var[0] = iter->type;
      error_var[1] = iter->arm;
      error_var[2] = iter->station;
      error_var[3] = iter->octant;
      error_var[4] = iter->hit_occ;
      error_var[5] = iter->stub_occ;
      error_var[6] = iter->trk_occ;
      error_var[7] = iter->time_ms;
      _tuple_error->Fill(error_var);
    }
  }
}

//__________________________________________
void TMutErrorStats::finish()
{
  if( !_initialized ) return;
  PHTFileServer::get().write( _filename );
}

