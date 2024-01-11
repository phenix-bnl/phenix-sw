#include<TMuiErrorStats.h>
#include<TMuiRoadMapO.h>
#include<TFile.h>
#include<TNtuple.h>
#include<list>

std::list<TMuiErrorStats::error> TMuiErrorStats::_event_errors;
std::list<TMuiErrorStats::error> TMuiErrorStats::_cummulate_errors;
TFile* TMuiErrorStats::_file=0;
TNtuple* TMuiErrorStats::_tuple_event=0;
TNtuple* TMuiErrorStats::_tuple_error=0;

void
TMuiErrorStats::clear_event()
{
  _event_errors.clear();
  // In the future might want to write summary data to DST object
  // for now we just use and ntuple
  //  _cummulate_errors.splice(_cummulate_errors.end(), _event_errors);
}

unsigned long
TMuiErrorStats::get_n_accumulated_errors()
{
  return _cummulate_errors.size();
}

unsigned long
TMuiErrorStats::get_n_event_errors()
{
  return _event_errors.size();
}

bool
TMuiErrorStats::is_error(UShort_t type) // return TRUE if there was an error of type
                                        // can also be used with type=0 to tell if there was no error
{
  int error_count = 0;
  std::list<error>::iterator error_iter = _event_errors.begin();
  for(; error_iter!=_event_errors.end(); ++error_iter){
    if( error_iter->type == type )
      error_count++;
  }
  return error_count;
}

bool
TMuiErrorStats::is_error() // returns TRUE if there was an error of a known kind
{
  return ( TMuiErrorStats::is_error(ROAD_BIFURCATE) );
}

void
TMuiErrorStats::set_error(ErrorType type, UShort_t arm, 
			  unsigned long road_occ, UShort_t time_ms)
{
  _event_errors.push_back(error(type, arm, road_occ, time_ms));
}

void
TMuiErrorStats::set_event_stats(PHCompositeNode* top_node, UShort_t time_ms)
{
  // TMuiHitMapO* hit_map = TMuiNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");    
  TMuiRoadMapO* road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");    
  if(_event_errors.size() == 0) {
    for(int arm=0; arm<MUIOO::NumberOfArms; ++arm) {
      // unsigned long hit_occ = hit_map->get(arm).count();
      unsigned long road_occ = road_map->get(arm).count();
      set_error(NO_ERROR,arm,road_occ,time_ms);
    }
  } else {
    std::list<error>::iterator iter = _event_errors.begin();
    for(;iter!=_event_errors.end(); ++iter){
      //      iter->hit_occ = hit_map->get(iter->arm).count();
      iter->road_occ = road_map->get(iter->arm).count();  
      iter->time_ms = time_ms;
    } 
  }
}


bool
TMuiErrorStats::initialize_ntuple()
{
  _file = new TFile("muon_error_statistics.root", "RECREATE");
  _tuple_error = new TNtuple("errors","errors","type:arm:ntrk:time");
  _tuple_event = new TNtuple("event","event","south_err:north_err:time");
  return true;
}

void
TMuiErrorStats::print_event()
{
  MUIOO::PRINT(std::cout,"TMuiErrorStats");
  std::list<error>::const_iterator iter = _event_errors.begin();
  for(;iter!=_event_errors.end(); ++iter){
    iter->print();
  }
  MUIOO::PRINT(std::cout,"**");
}

void
TMuiErrorStats::write_event()
{
  static bool init __attribute__ ((unused)) = initialize_ntuple();  

  // Flush tuple every 500 events
  //
  static UShort_t cnt = 0;
  if(++cnt%500==0 && _file){
    _tuple_event->AutoSave();
    _tuple_error->AutoSave();
    _file->Flush();
  }

  // Per event ntuple
  //
  {
    std::list<error>::const_iterator iter = _event_errors.begin();
    float event_var[3] = {0};
    for(;iter!=_event_errors.end(); ++iter){
      if(iter->type == NO_ERROR) {
	event_var[2] = iter->time_ms;
	_tuple_event->Fill(event_var);
	break;
      } else {
	iter->arm==MUIOO::South ? event_var[0]++ : event_var[1]++;
	event_var[2] = iter->time_ms;
      }
    }
    _tuple_event->Fill(event_var);
  }
  // Per error ntuple
  //
  {
    std::list<error>::const_iterator iter = _event_errors.begin();
    float error_var[8] = {0};
    for(;iter!=_event_errors.end(); ++iter){
      error_var[0] = iter->type;
      error_var[1] = iter->arm;
      error_var[2] = iter->road_occ;
      error_var[3] = iter->time_ms;
      _tuple_error->Fill(error_var);
    }
  }
}
void
TMuiErrorStats::finish()
{
  if(_file) _file->Write();
}
void
TMuiErrorStats::print_cummulate()
{
  static std::list<error>::const_iterator iter = _event_errors.begin();
  for(;iter!=_event_errors.end(); ++iter){
    
  }
}

