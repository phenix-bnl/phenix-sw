// $Id: TMutStubFinder.cxx,v 1.59 2017/10/12 16:54:40 shlim Exp $

/*!
  \file    TMutStubFinder.cxx.
  \brief   Encapsulates the stub finding algorithm used by the mMutFindStub
	and mMutFindTrack moduleslog file parser based on xml
  \author  S.Kelly
  \version $Revision: 1.59 $
  \date    $Date: 2017/10/12 16:54:40 $
*/

#include <PHGeometry.h>
#include <gsl/gsl_fit.h>
#include <boost/array.hpp>
#include <cmath>
#include <TROOT.h>

// MUTOO
#include "PHTFileServer.h"
#include "TMutClusMap.h"
#include "TMutErrorStats.h"
#include "TMutGeo.h"
#include "TMutHitMap.h"
#include "TMutMCHitMap.h"
#include "TMutStubFinder.h"
#include "TMutTrackUtil.h"

using namespace std;

// Static data members TMutStubFinder
MUTOO::Verbosity TMutStubFinder::_verbosity = MUTOO::NONE;
MUTOO::Verbosity TMutStubFinder::Stub::_verbosity = MUTOO::NONE;

bool TMutStubFinder::_reverse_algo = false;
unsigned short TMutStubFinder::_max_n_stubs = 5000;
unsigned short TMutStubFinder::_min_coord_1 = 3;
unsigned short TMutStubFinder::_min_coord_2 = 3;
unsigned short TMutStubFinder::_min_point_12 = 0;
unsigned short TMutStubFinder::_min_coord_3 = 2;
unsigned short TMutStubFinder::_min_point_3 = 0;

// evaluation ntuples and filenames
bool TMutStubFinder::_do_evaluation = false;
bool TMutStubFinder::_evaluation = false;
string TMutStubFinder::_evaluation_filename = "muon_eval.root";
TNtuple* TMutStubFinder::_ntuple = 0;

bool TMutStubFinder::Stub::_evaluation = false;
TNtuple* TMutStubFinder::Stub::_ntuple = 0;
boost::array<float,20> TMutStubFinder::Stub::_eval_data;

// Static data members TMutStubFinder::Stub
double TMutStubFinder::Stub::_dca_cut = 4.0;
double TMutStubFinder::Stub::_w_prox_cut = 3.0;
bool TMutStubFinder::Stub::_check_gap_coord = false;

// Statistics
PHTimeServer::timer TMutStubFinder::_timer(PHTimeServer::get()->insert_new("TMutStubFinder") );
double TMutStubFinder::_n_stubs_direct(0);
double TMutStubFinder::_n_stubs_reverse(0);
double TMutStubFinder::_n_stubs_completed(0);

// TMutStubFinder methods

//______________________________________________________________
void TMutStubFinder::print_parameters( ostream& out )
{
  MUTOO::PRINT( out, "TMutStubFinder::print_parameters" );
  out << "_max_n_stubs="  << get_max_n_stubs() << endl;
  out << "_min_coord_1="  << get_min_coord_1() << endl;
  out << "_min_coord_2="  << get_min_coord_2() << endl;
  out << "_min_coord_3="  << get_min_coord_2() << endl;
  out << "_min_point_12=" << get_min_point_12() << endl;
  out << "_min_point_3="  << get_min_point_3() << endl;
  out << endl;

  out << "Stub::_dca_cut="    << Stub::get_dca_cut() << endl;
  out << "Stub::_w_prox_cut=" << Stub::get_w_prox_cut() << endl;
  out << "Stub::_check_gap_coord=" << Stub::get_check_gap_coord() <<endl;

  MUTOO::PRINT( out, "**" );
  return;
}

//______________________________________________________________
void TMutStubFinder::print_summary( ostream& out )
{
  MUTOO::PRINT( out, "TMutStubFinder::print_summary" );
  out << "_n_stubs_direct= " << _n_stubs_direct << endl;
  out << "_n_stubs_reverse= " << _n_stubs_reverse << endl;
  out << "_n_stubs_completed= " << _n_stubs_completed << endl;
  MUTOO::PRINT( out, "**" );
  return;
}
  
//______________________________________________________________
// Find all stubs -- Windows are wide open
void TMutStubFinder::find(TMutCoordMap* coord_map)
{
  // Never trust a raw pointer
  if(!coord_map) throw logic_error(DESCRIPTION("TMutCoordMap pointer is null"));

  for(int arm=0; arm<MUTOO::NumberOfArms;++arm)
  for(int station=0; station<MUTOO::NumberOfStations;++station)
  for(int octant=0; octant<MUTOO::NumberOfOctants;++octant)
  for(int half_octant=0; half_octant<MUTOO::NumberOfHalfOctants;++half_octant)
  find(coord_map, arm, station, octant, half_octant);

}

//______________________________________________________________
// Find Stub in specified half octant in given window
void TMutStubFinder::find(TMutCoordMap* coord_map,
			  unsigned short arm,
			  unsigned short station,
			  unsigned short octant,
			  unsigned short half_octant,
			  bool use_window,
			  const pair<float,float>& theta_window,
			  const pair<float,float>& phi_window)
{

  if( _verbosity >= MUTOO::SOME )
    cout
      << " TMutStubFinder::find: location: " << " arm: " << arm << " station: " << station
      << " octant: " << octant << " half octant: " << half_octant << endl;

  // Never trust a raw pointer
  if(!coord_map) throw logic_error(DESCRIPTION("TMutCoordMap pointer is null"));

  // start timer
  _timer.get()->restart();
  
  // clear local stub list
  _stub_list.clear();
  
  // Local storage for coordinate list
  local_coord_list local_list;

  // Store all coords in this half octant in local list.  If using window only
  // store those coordinates that intersect the window
  if(use_window) {
    local_list = get_coords_in_window(coord_map,
				      arm,
				      station,
				      octant,
				      half_octant,
				      theta_window,
				      phi_window);
  } else {
    TMutCoordMap::iterator coord_iter = coord_map->get(arm,station,octant,half_octant);
    while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){
      local_list.push_back(*coord_ptr);
    }
  }

  // Run the algorithm
  local_coord_list::iterator iter = local_list.begin();
  for(;iter!=local_list.end();++iter)
  {
    
    // try to happen the coordinate to one of the current stub.
    bool was_appended = append_to_stub(&(*iter));
    
    _stub_list.sort( stub_less_ftor() );
    _stub_list.unique( stub_equal_ftor() );
    
    /* 
      if the coordinate was not appended to any stub, 
      create a new one and sort the list
    */
    if( !was_appended )
    { start_new_stub(&(*iter), use_window, theta_window, phi_window); }

    if(_verbosity >= MUTOO::SOME ) cout << "TMutStubFinder::find - _stub_list.size(): " << _stub_list.size() << endl;
    if(_verbosity >= MUTOO::MAX) print_stubs();
    
  }

  // Before running the reverse algorithm we tag the stubs as complete.
  // (this is so we don't try to add coords previously found stubs)
  set_complete_tag();

  // increment counter
  _n_stubs_direct += _stub_list.size();
  
  if(_verbosity >= MUTOO::ALOT) {
    cout << "Printing Stubs [direct]" << endl;
    print_stubs();
  }


  if(_verbosity >= MUTOO::MAX) {
    stub_list::iterator iter1 = _stub_list.begin();
    for(;iter1!=_stub_list.end();++iter1){
      typedef TMutStubFinder::Stub::coord_list stub_coord_list;
      stub_coord_list& coord_list = iter1->get_coord_list();
      stub_coord_list::iterator coord_iter_1 = coord_list.begin();
      cout << "Stub in octant  "  << iter1->get_octant() << endl;
      for(;coord_iter_1!=coord_list.end();++coord_iter_1){
        cout <<
          "w = " << coord_iter_1->get()->get_w_absolute() <<
          " strip = " << coord_iter_1->get()->get_peak_strip() <<
		      " plane = " << 2*coord_iter_1->get()->get_gap() + coord_iter_1->get()->get_cathode()<< endl;
      }
    }
  }
  
  // run reverse algorithm
  if(_reverse_algo) 
  {

    // Reverse the list
    local_list.reverse();
    
    // Run the algorithm again (on the reverse sorted list)
    iter = local_list.begin();
    for(;iter!=local_list.end();++iter){
      
      // Attempt to append coord to existing stub.  If unsuccessful create new stub.
      bool was_appended = append_to_stub(&(*iter));
      _stub_list.sort( stub_less_ftor() );
      _stub_list.unique( stub_equal_ftor() );
      
      /* 
        if the coordinate was not appended to any stub, 
        create a new one and sort the list
      */
      if( !was_appended )
      {
        start_new_stub(&(*iter), use_window, theta_window, phi_window);
      }
      
      if(_verbosity >= MUTOO::MAX) print_stubs();
      
    }
    
    // Set complete on stub found in second pass
    set_complete_tag();
    
    if(_verbosity >= MUTOO::ALOT)
    {
      cout << "Printing Stubs [reverse]" << endl;
      print_stubs();
    }

  }
  
  _n_stubs_reverse += _stub_list.size();
  
  // Removes stubs that are subsets of other stubs
  size_t n_stub = _stub_list.size();
  remove_includes();
 
  // increment counter
  _n_stubs_completed += _stub_list.size();

  // update master list
  update_master_list();
   
  // stop timer
  _timer.get()->stop();
  
  if(_verbosity >= MUTOO::SOME)
  { cout << " number stubs [includes]=" << _stub_list.size() << " removed=" << n_stub - _stub_list.size() << " includes" << endl; }
  
  // print stubs
  if(_verbosity >= MUTOO::ALOT)
  {
    cout << "Printing Stubs [completed]" << endl;
    print_stubs();
  }

}

//______________________________________________________________
void TMutStubFinder::remove_includes()
{

  // first loop over stubs
  for( stub_list::iterator iter1 = _stub_list.begin(); iter1!=_stub_list.end(); ++iter1 ){

    // second loop over stubs
    stub_list::iterator iter2 = iter1;
    ++iter2;
    for(;iter2!=_stub_list.end();++iter2){

      // If stubs have same number of coordinates and passed the unique check
      // then one is not a subset of the other.
      if(iter1->get_n_coord() == iter2->get_n_coord()) continue;

      stub_list::iterator big = (iter1->get_n_coord() > iter2->get_n_coord()) ? iter1 : iter2;
      stub_list::iterator little = (iter1->get_n_coord() > iter2->get_n_coord()) ? iter2 : iter1;

      /*
        if the small stub is already removed,
        then do not recheck
      */
      if( !little->get_status() ) continue;

      // retrieve coord lists for both
      Stub::coord_list& big_coords( big->get_coord_list() );
      Stub::coord_list& little_coords( little->get_coord_list() );

      if( std::includes(
        big_coords.begin(), big_coords.end(),
        little_coords.begin(), little_coords.end(),
        Stub::coord_less_ftor() ) )
      little->set_status( false );

      // dump
      if( _verbosity >= MUTOO::SOME && !little->get_status() ) {
        cout << "TMutStubFinder::remove_includes" << endl;
        cout << *little << *big;
      }
    }
  }

  // Remove marked stubs
  _stub_list.remove_if(stub_bad_status_ftor());

}

//______________________________________________________________
void TMutStubFinder::set_complete_tag()
{

  stub_list::iterator stub_iter = _stub_list.begin();
  stub_iter = _stub_list.begin();
  for(;stub_iter!=_stub_list.end();++stub_iter)
  if(stub_iter->get_station() == MUTOO::Station3){

    if(_verbosity >= MUTOO::MAX)
	  cout << "stub status = " << stub_iter->get_status() << endl;

    if(stub_iter->get_coord_list().size() < _min_coord_3 || stub_iter->get_point_list().size() < _min_point_3)
	  stub_iter->set_status(false);
    else stub_iter->set_complete(true);

  } else if(stub_iter->get_station() == MUTOO::Station2) {

    if(_verbosity >= MUTOO::MAX)
	  cout << "stub status = " << stub_iter->get_status() << endl;

    if(stub_iter->get_coord_list().size() < _min_coord_2 || stub_iter->get_point_list().size() < _min_point_12)
	  stub_iter->set_status(false);
    else stub_iter->set_complete(true);

  } else {

    if(stub_iter->get_coord_list().size() < _min_coord_1 || stub_iter->get_point_list().size() < _min_point_12)
	  stub_iter->set_status(false);
    else stub_iter->set_complete(true);

  }

  _stub_list.remove_if(stub_bad_status_ftor());

}

//______________________________________________________________
void TMutStubFinder::update_master_list( void )
{
  _stub_list.sort( stub_less_ftor() );
  _stub_list.unique( stub_equal_ftor() );
  _master_stub_list.merge( _stub_list, stub_less_ftor() );
  return;
}

//______________________________________________________________
bool TMutStubFinder::append_to_stub(TMutCoordMap::pointer coord_ptr)
{
  // Initialize was_appended = false
  // Loop over Stubs in stub list [
  //   if add_new_coord successfull [
  //     set was_appended = true
  //   ]
  // ]
  // return was_appended

  bool sticky_appended = false;
  stub_list::iterator stub_iter = _stub_list.begin();
  for(;stub_iter!=_stub_list.end();++stub_iter){

    // Punt if this stub has the complete flag set
    if(stub_iter->is_complete()) continue;

    // Attempt to append coord to current stub
    if (_verbosity >= MUTOO::MAX){
      cout << "Attempt to add to coord: " << endl;
      cout << "arm = " << coord_ptr->get()->get_arm() <<
	      " station = " << coord_ptr->get()->get_station() <<
	      " octant = " << coord_ptr->get()->get_octant() <<
	      " half_octant = " << coord_ptr->get()->get_half_octant() <<
	      " gap = " << coord_ptr->get()->get_gap() <<
	      " cathode = " << coord_ptr->get()->get_cathode() <<
	      " peak_strip = " << coord_ptr->get()->get_peak_strip() <<endl;

      typedef TMutStubFinder::Stub::coord_list stub_coord_list;
      stub_coord_list& coord_list = stub_iter->get_coord_list();
      stub_coord_list::iterator coord_iter_1 = coord_list.begin();
      cout << "to stub: " << endl;
      for(;coord_iter_1!=coord_list.end();++coord_iter_1)
	    cout << "arm = " << coord_iter_1->get()->get_arm() <<
	      " station = " << coord_iter_1->get()->get_station() <<
	      " octant = " << coord_iter_1->get()->get_octant() <<
	      " half_octant = " << coord_iter_1->get()->get_half_octant() <<
	      " gap = " << coord_iter_1->get()->get_gap() <<
	      " cathode = " << coord_iter_1->get()->get_cathode() <<
	      " peak_strip = " << coord_iter_1->get()->get_peak_strip() <<endl;

    }

    bool was_appended =  stub_iter->add_coord(coord_ptr);

    if (_verbosity >= MUTOO::MAX && was_appended )
    cout << "Successful Add" << endl;

    sticky_appended |= was_appended;

  }
  
  return sticky_appended;
}

//______________________________________________________________
void TMutStubFinder::abort_new_stub()
{ _stub_list.pop_front(); }

//______________________________________________________________
TMutStubFinder::Stub* TMutStubFinder::start_new_stub(
						     TMutCoordMap::pointer coord_ptr,
						     bool use_window,
						     const pair<float,float>& theta_window,
						     const pair<float,float>& phi_window)
{
  _stub_list.push_front(Stub(this,coord_ptr,use_window,theta_window,phi_window));

  // Return a pointer to the newly created Stub
  return &(*_stub_list.begin());
}


//______________________________________________________________
// TMutStubFinder::Stub methods
bool TMutStubFinder::Stub::add_coord(TMutCoordMap::pointer coord_ptr)
{

  bool was_added=false;
  if(_evaluation) _eval_data.assign(0);

  /*
    If stub already has a coordinate in the other cathode which do not
    match the current coord, abort
  */
  if( _check_gap_coord && !check_gap_coord( coord_ptr ) ) {
    if (_verbosity >= MUTOO::MAX )
	  cout << "TMutStubFinder::Stub::add_coord - check_gap_coord failed" << endl;
    return was_added;
  }

  /*
    If stub has a coord in this plane and has a least 2 coords
    then then call bifurcate
  */
  if(!unique_check(coord_ptr))
  {
    if( _bifurcate == 2*coord_ptr->get()->get_gap() + coord_ptr->get()->get_cathode() + 1) return was_added;
    if(_coords.size()>1) was_added = bifurcate(coord_ptr);
    return was_added;
  }

  // if Stub has point [
  //  project point to z of coord
  //  test theta, phi window of projected point
  //  test projected point - coord DCA
  //  if added update stub parameters
  // ] else [
  //  if can make point with this coord [
  //    test theta, phi window of point
  //    if added update stub parameters
  //  ] else [
  //    test w proximity of coord with existing coord
  //  ]
  // ]

  if(_points.size() != 0){

    PHPoint point = project(coord_ptr->get()->get_mean_z());
    if(test_window(point) &&
       check_dca(coord_ptr,point) &&
       check_w_proximity(coord_ptr)){
      update_stub(coord_ptr);
      was_added = true;
    }

    if (_verbosity >= MUTOO::MAX){
      if (!test_window(point))            cout << "TMutStubFinder::Stub::add_coord - test window failed" << endl;
      if (!check_dca(coord_ptr,point))    cout << "TMutStubFinder::Stub::add_coord - dca failed" << endl;
      if (!check_w_proximity(coord_ptr))  cout << "TMutStubFinder::Stub::add_coord - w prox failed" << endl;
    }

  } else {

    // Check if this coord is non-parallel to existing coord
    if(!check_parallel(coord_ptr)) {

      // Not parallel so calculate the intersection
      PHPoint point = make_point(coord_ptr);

      // If point is in window then add coord and point to stub
      if(test_window(point)){
	      update_stub(coord_ptr);
	      was_added = true;
      } else if (_verbosity >= MUTOO::MAX)
	      cout << "TMutStubFinder::Stub::add_coord - test window failed" << endl;

    } else {

      // Prior and current coord are parallel, check proximity and add
      // coord to stub if w proximity cut is satisfied.
      if (check_w_proximity(coord_ptr)) {
	      update_stub(coord_ptr);
	      was_added = true;
      } else if (_verbosity >= MUTOO::MAX )
	      cout << "TMutStubFinder::Stub::add_coord - w prox failed" << endl;
    }
  }
  if(_coords.size() >= 3) check_slope();

  if( _evaluation && _ntuple ){

    if( _verbosity >= MUTOO::MAX )
    cerr << "TMutStubFinder::Stub::add_coord - filling ntuple" << endl;

    _eval_data[0] = _arm;
    _eval_data[1] = _station;
    _eval_data[2] = coord_ptr->get()->get_gap();
    _eval_data[3] = coord_ptr->get()->get_cathode();
    _eval_data[4] = _coords.size();
    _eval_data[5] = was_added ? 1 : 0;
    _eval_data[8] = MUTOO::RAD_TO_DEG*TMutGeo::get_angle_cathode_anode(coord_ptr->get()->get_arm(),
								       coord_ptr->get()->get_station(),
								       coord_ptr->get()->get_octant(),
								       coord_ptr->get()->get_half_octant(),
								       coord_ptr->get()->get_gap(),
								       coord_ptr->get()->get_cathode());
    _ntuple->Fill(&(_eval_data[0]));
  }

  return was_added;
}

//______________________________________________________________
double TMutStubFinder::Stub::get_dwdz() const {

  // Project tangent onto wz plane and return dwdz
  PHVector w_axis = TMutGeo::get_w_axis(_arm,_station,_octant,_half_octant);
  double wcomp = PHGeometry::dot(
		TMutGeo::get_w_axis(_arm,_station,_octant,_half_octant),
		_fit_par.get_tangent(_arm)
	);

	double zcomp = PHGeometry::dot(
		PHVector(0,0,1),
		_fit_par.get_tangent(_arm)
	);

  return (zcomp!=0) ? wcomp/zcomp : 0;

}

//______________________________________________________________
bool TMutStubFinder::Stub::check_parallel(const TMutCoordMap::pointer coord_ptr) const
{

  // Loop over coords in stub coord list and check for non-parallel coord
  coord_list::const_iterator coord_iter = _coords.begin();
  for(;coord_iter!=_coords.end();++coord_iter)
    if(!TMutGeo::are_parallel(coord_iter->get()->get_location(), coord_ptr->get()->get_location()))
      return false;

  return true;
}


//______________________________________________________________
bool TMutStubFinder::Stub::check_slope() const
{ return true; }

//______________________________________________________________
bool TMutStubFinder::Stub::test_window(const PHPoint& point) const
{

  // If there is no window defined for this stub then check
  // that the point is within the fiducial volume of the 1/2
  // octant
  if(!has_window()) return TMutGeo::in_fiducial(_arm,_station,_octant,_half_octant,point);

  TMutStubFinder::WindowEnum phi_check = TMutStubFinder::check_phi_window(point,_phi_window);
  TMutStubFinder::WindowEnum theta_check = TMutStubFinder::check_theta_window(point,_theta_window);

  if (_verbosity >= MUTOO::MAX){
    cout << "point = " << point.getX() << ", " << point.getY() << ", " << point.getZ() << endl;
    cout << "phi = " << _phi_window.first << ", " << _phi_window.second << endl;
    cout << "theta = " << _theta_window.first << ", " << _theta_window.second << endl;
  }

  return (phi_check == TMutStubFinder::IN_WINDOW && theta_check == TMutStubFinder::IN_WINDOW);

}

//______________________________________________________________
bool TMutStubFinder::Stub::check_dca(const TMutCoordMap::pointer coord_ptr, const PHPoint& point)  const
{

  // Check the distance from the coordinate to the point
  double distance = PHGeometry::distanceLinePoint(coord_ptr->get()->get_coord(),point);

  double point_error = 0;

  coord_list::const_iterator coord_iter = _coords.begin();

  for(;coord_iter!=_coords.end();++coord_iter)
    point_error = max(point_error, static_cast<double> (coord_iter->get()->get_error()) );

  point_error = sqrt(point_error)/_coords.size();

  // cat evaluation data
  if(_evaluation) _eval_data[7] = distance;
  if (_verbosity >= MUTOO::MAX) {
    cout << "distance = " << distance << endl;
    cout
      << "error = " << 3*coord_ptr->get()->get_error() << ", "
      << 3*sqrt(
		    MUTOO::SQUARE(coord_ptr->get()->get_error()) +
		    MUTOO::SQUARE(point_error))
      << endl;
  }

  return (distance < _dca_cut + 3*sqrt(MUTOO::SQUARE(coord_ptr->get()->get_error()) + MUTOO::SQUARE(point_error)));
}

//______________________________________________________________
bool TMutStubFinder::Stub::bifurcate(TMutCoordMap::pointer coord_ptr)
{

  // Sanity check
  if(_finder->_stub_list.size() > _max_n_stubs) {
    ostringstream msg;
    msg << " bifurcate: too many stubs, arm:" << _arm <<  " station: " << _station << " octant: " << _octant;
    TMutErrorStats::set_error(TMutErrorStats::STUB_BIFURCATE, _arm, _station, _octant);
    throw runtime_error(DESCRIPTION(msg.str().c_str()));
  }

  // New stub pointer with null initialization
  TMutStubFinder::Stub* new_stub_ptr = 0;

  // Loop over coord list copy coords from old stub that are not in the
  // same plane as coord_ptr
  coord_list::iterator coord_iter = _coords.begin();
  for(;coord_iter!=_coords.end();++coord_iter)
    if(!(coord_ptr->get()->get_gap() == coord_iter->get()->get_gap() &&

	 coord_ptr->get()->get_cathode() == coord_iter->get()->get_cathode())) {

      // Old coord
      TMutCoordMap::value_type old_coord = *coord_iter;

      // First time start the new stub -- subsequent times update stub with old coord
      if(!new_stub_ptr) new_stub_ptr = _finder->start_new_stub(&old_coord, _has_window, _theta_window, _phi_window);
      else new_stub_ptr->update_stub(&old_coord);

    }

  // If coord_ptr is the first then return here
  if(!new_stub_ptr) {
    _finder->start_new_stub(coord_ptr, _has_window, _theta_window, _phi_window);
    return true;
  }
  new_stub_ptr->set_bifurcate(2*coord_ptr->get()->get_gap() + coord_ptr->get()->get_cathode() + 1);

  // Now we have a stub that is guarenteed *not* to have a coord in the same plane
  // as coord_ptr.  Here we invoke a fancy (obtuse) piece of recursion by calling
  // add_coord using the new stub pointer. This will ensure that exactly the same
  // criteria is applied for adding coordinates to stubs in both the original and
  // new (bifurcated) stub.
  //
  bool was_added = new_stub_ptr->add_coord(coord_ptr);
  if(was_added) return true;
  else {
    _finder->abort_new_stub();
    return false;
  }

}

//______________________________________________________________
bool TMutStubFinder::Stub::check_gap_coord(TMutCoordMap::pointer coord_ptr) const
{
  cout << "TMutStubFinder::Stub::check_gap_coord.\n" << endl;
  coord_list::const_iterator coord_iter = _coords.begin();
  for(;coord_iter!=_coords.end();++coord_iter)
  {
    // check only coords on same station, same gap, different cathode
    if(
      coord_iter->get()->get_station() != coord_ptr->get()->get_station() ||
      coord_iter->get()->get_gap() != coord_ptr->get()->get_gap() ||
      coord_iter->get()->get_cathode() == coord_ptr->get()->get_cathode()
    ) continue;

    // returns true if the two coordinates are associated (form a gap_coordinate)
    return coord_iter->get()->is_associated<TMutCoord>( coord_ptr );

  }

  return true;
}

//______________________________________________________________
bool TMutStubFinder::Stub::check_w_proximity(TMutCoordMap::pointer coord_ptr) const
{

  // Only applies to non-stereo
  if(coord_ptr->get()->get_stereo()) return true;

  // Determine the minimal delta w
  double delta_w = DBL_MAX;
  double coord_error = 0.0;
  coord_list::const_iterator coord_iter = _coords.begin();
  for(;coord_iter!=_coords.end();++coord_iter){

    // Skip over stereo planes
    if(coord_iter->get()->get_stereo()) continue;

    delta_w = min(delta_w, fabs(coord_iter->get()->get_w_absolute() - coord_ptr->get()->get_w_absolute()));
		//Sanghoon
		//In SL7, +/-1e-19 level of fluctuation appears wihch make 0 in SL6 as a non-zero value
		//This fluctuation causes different minimization results
		if (delta_w<1e-3) delta_w = 0;
    //if (delta_w == fabs(coord_iter->get()->get_w_absolute() - coord_ptr->get()->get_w_absolute()))
    if (fabs(delta_w - fabs(coord_iter->get()->get_w_absolute() - coord_ptr->get()->get_w_absolute()))<1e-4)
      coord_error = coord_iter->get()->get_error();

  }

  if(_evaluation) _eval_data[6] = delta_w;
  return (delta_w < _w_prox_cut + sqrt(MUTOO::SQUARE(coord_ptr->get()->get_error()) + MUTOO::SQUARE(coord_error)));

}

//______________________________________________________________
void TMutStubFinder::Stub::update_stub(TMutCoordMap::pointer coord_ptr)
{
  float sigx( 0 ), sigy( 0 );
  float m1, m2;
  PHVector v1, v2;
  PHPoint begin, end;

  // Push coord onto coord list
  _coords.insert(*coord_ptr);

  // Regenerate points list with new coord -- repetitive but
  // makes the code simple.
  _points.clear();

  // Loop over all unique combinations of coords in this stub,
  // attempt to make points out of coord that share a common
  // gap.
  coord_list::const_iterator iter1 = _coords.begin();

  for(;iter1!=_coords.end();++iter1){
    coord_list::const_iterator iter2 = iter1;
    ++iter2;
    for(;iter2!=_coords.end();++iter2)
      if(iter1->get()->get_gap() == iter2->get()->get_gap()){
	      PHPoint point = PHGeometry::closestApproachLineLine(iter1->get()->get_coord(), iter2->get()->get_coord());
        _points.push_back(point);

	      // Calculate x,y error on point:
	      v1 = iter1->get()->get_coord().getDirection();
	      m1 = v1.getY()/v1.getX();

	      v2 = iter2->get()->get_coord().getDirection();
	      m2 = v2.getY()/v2.getX();

	      sigx = MUTOO::SQUARE(1/(m1-m2))*(
          MUTOO::SQUARE(iter1->get()->get_error()) +
          MUTOO::SQUARE(iter2->get()->get_error()));

	      sigy = MUTOO::SQUARE(1/(m1-m2))*(
          MUTOO::SQUARE(m1*iter2->get()->get_error()) +
          MUTOO::SQUARE(m2*iter1->get()->get_error()));

      }
  }

  if(_points.size()==0){

    // If we have 0 points then try to make points from cathodes
    // that don't share a gap but are not parallel.
    bool made_point = false;
    coord_list::const_iterator iter1 = _coords.begin();

    for(;iter1!=_coords.end();++iter1){
      coord_list::const_iterator iter2 = iter1;
      ++iter2;
      for(;iter2!=_coords.end();++iter2){

	      // Check for parallel strips
	      if(TMutGeo::are_parallel(iter1->get()->get_location(), iter2->get()->get_location())) continue;

      	PHPoint point = PHGeometry::closestApproachLineLine(iter1->get()->get_coord(), iter2->get()->get_coord());
      	_fit_par.set_x(point.getX());
      	_fit_par.set_y(point.getY());
      	_fit_par.set_z(point.getZ());
      	_fit_par.set_dxdz(point.getX()/point.getZ());
      	_fit_par.set_dydz(point.getY()/point.getZ());

        // Calculate error on x and y :
      	v1 = iter1->get()->get_coord().getDirection();
	      m1 = v1.getY()/v1.getX();

      	v2 = iter2->get()->get_coord().getDirection();
	      m2 = v2.getY()/v2.getX();

        sigx = MUTOO::SQUARE(1/(m1-m2))*(
					 MUTOO::SQUARE(iter1->get()->get_error()) +
					 MUTOO::SQUARE(iter2->get()->get_error()));

        sigy = MUTOO::SQUARE(1/(m1-m2))*(
					 MUTOO::SQUARE(m1*iter2->get()->get_error()) +
					 MUTOO::SQUARE(m2*iter1->get()->get_error()));

        _fit_par.set_covar(0,0,sigx);
      	_fit_par.set_covar(2,2,sigy);
      	made_point = true;
      }
    }

    // If the above didn't work then just initialize the fit_par with the center of the
    // first strip
    if(!made_point) {
      iter1 = _coords.begin();
      PHPoint mid_point = iter1->get()->get_coord_midpoint();
      begin = iter1->get()->get_coord_begin();
      end = iter1->get()->get_coord_end();

      // One point, take the point with 0 angles
      _fit_par.set_x(mid_point.getX());
      _fit_par.set_y(mid_point.getY());
      _fit_par.set_z(mid_point.getZ());
      _fit_par.set_dxdz(mid_point.getX()/mid_point.getZ());
      _fit_par.set_dydz(mid_point.getY()/mid_point.getZ());
      sigx = MUTOO::SQUARE(begin.getX() - end.getX())/12.0;
      sigy = MUTOO::SQUARE(begin.getY() - end.getY())/12.0;
      _fit_par.set_covar(0,0,sigx);
      _fit_par.set_covar(2,2,sigy);

    }
  } else if(_points.size()==1){

    // One point, take the point with 0 angles
    _fit_par.set_x(_points.begin()->getX());
    _fit_par.set_y(_points.begin()->getY());
    _fit_par.set_z(_points.begin()->getZ());
    _fit_par.set_dxdz(_points.begin()->getX()/_points.begin()->getZ());
    _fit_par.set_dydz(_points.begin()->getY()/_points.begin()->getZ());
    _fit_par.set_covar(0,0,sigx);
    _fit_par.set_covar(2,2,sigy);

  } else if(_points.size()>1) {

    // More that one point do a linear fit
    boost::array<double,3> x = {{0}};
    boost::array<double,3> y = {{0}};
    boost::array<double,3> z = {{0}};

    double z0 = _points[0].getZ();
    size_t n_data = _points.size();

    double z_max = -DBL_MAX;
    double z_min = DBL_MAX;

    for(size_t i=0;i<n_data;++i){
      x.at(i) = _points[i].getX();
      y.at(i) = _points[i].getY();
      z.at(i) = _points[i].getZ() - z0;
      z_min = min(z_min,_points[i].getZ());
      z_max = max(z_max,_points[i].getZ());
    }

    double x0=0,m_x=0, y0=0, m_y=0;
    double cov00=0, cov11=0, cov22=0,chi2=0;

    // Fit x
    gsl_fit_linear(z.begin(),1,x.begin(),1,n_data,&x0,&m_x,&cov00,&cov11,&cov22,&chi2);

    // Fit y
    gsl_fit_linear(z.begin(),1,y.begin(),1,n_data,&y0,&m_y,&cov00,&cov11,&cov22,&chi2);

    // Sanity check (dx/dz dy/dz cut)
    if(fabs(m_x) > 20 || fabs(m_y) > 20) {
      _status = false;
      return;
    }

    _fit_par.set_x(x0);
    _fit_par.set_y(y0);
    _fit_par.set_z(z0);
    if(_points.size()>2){
      _fit_par.set_dxdz(m_x);
      _fit_par.set_dydz(m_y);
    }
    else{
      _fit_par.set_dxdz(x0/z0);
      _fit_par.set_dydz(y0/z0);
    }


    // "begin" is closest to interaction region
    if(_arm == MUTOO::South) {
      _fit_par.set_z_begin(z_max) ;
      _fit_par.set_z_end(z_min);
    } else {
      _fit_par.set_z_begin(z_min) ;
      _fit_par.set_z_end(z_max);
    }
  }
}

//______________________________________________________________
PHPoint TMutStubFinder::Stub::project(double z) const
{ return TMutTrackUtil::linear_track_model(&_fit_par,z); }

//______________________________________________________________
PHPoint TMutStubFinder::Stub::make_point(const TMutCoordMap::pointer coord_ptr) const
{
  double min_z_dist = DBL_MAX;
  coord_list::const_iterator nearest = _coords.end();
  coord_list::const_iterator coord_iter = _coords.begin();
  for(;coord_iter!=_coords.end();++coord_iter){

    // Skip if coord is parallel
    if(TMutGeo::are_parallel(coord_ptr->get()->get_location(), coord_iter->get()->get_location()))
      continue;

    // Capture coord iterator if this is nearest stereo coord
    double dist = fabs(coord_iter->get()->get_mean_z() - coord_ptr->get()->get_mean_z());
    min_z_dist = min(dist,min_z_dist);
    if(dist == min_z_dist) nearest = coord_iter;

  }

  if(nearest == _coords.end()) return PHPoint();

  // Point on nearest closest to coord
  return PHGeometry::closestApproachLineLine(coord_ptr->get()->get_coord(), nearest->get()->get_coord());
}

//______________________________________________________________
bool TMutStubFinder::Stub::unique_check(TMutCoordMap::pointer coord_ptr) const
{

  // Loop over coords and punt if there stub already has a coord in
  // this plane.
  coord_list::const_iterator coord_iter = _coords.begin();
  for(;coord_iter!=_coords.end();++coord_iter)
  if(coord_ptr->get()->get_arm() == coord_iter->get()->get_arm() &&
     coord_ptr->get()->get_station() == coord_iter->get()->get_station() &&
     coord_ptr->get()->get_octant() == coord_iter->get()->get_octant() &&
     coord_ptr->get()->get_half_octant() == coord_iter->get()->get_half_octant() &&
     coord_ptr->get()->get_gap() == coord_iter->get()->get_gap() &&
     coord_ptr->get()->get_cathode() == coord_iter->get()->get_cathode())
    return false;

  return true;
}


//______________________________________________________________
bool TMutStubFinder::Stub::check_theta_window(const TMutStubFinder::stub_window& theta_window) const
{
  double theta = get_theta();
  return (theta > theta_window.first && theta < theta_window.second);
}

//______________________________________________________________
bool TMutStubFinder::Stub::check_phi_window(const TMutStubFinder::stub_window& phi_window) const
{

  // Project onto x and y axis to avoid [0,2PI] boundary shenanigans
  double x1 = cos(phi_window.first);
  double x2 = cos(phi_window.second);
  double y1 = sin(phi_window.first);
  double y2 = sin(phi_window.second);
  double xp = cos(get_phi());
  double yp = sin(get_phi());

  double r1 = sqrt(MUTOO::SQUARE(x1) + MUTOO::SQUARE(y1));
  double r2 = sqrt(MUTOO::SQUARE(x2) + MUTOO::SQUARE(y2));
  double rp = sqrt(MUTOO::SQUARE(xp) + MUTOO::SQUARE(yp));

  double dphi_window = acos((x1*x2 + y1*y2)/(r1*r2));
  double dphi_left = acos((x1*xp + y1*yp)/(r1*rp));
  double dphi_right = acos((x2*xp + y2*yp)/(r2*rp));

  // intermediate value -- has to be withing dphi_window of
  // both edges.
  return (dphi_left < dphi_window && dphi_right < dphi_window);
}

//______________________________________________________________
void TMutStubFinder::Stub::initialize_evaluation()
{
  // check evaluation is requested
  if( !TMutStubFinder::get_do_evaluation() ) return;

  // check if initialization was not already done
  if( _evaluation ) return;

  // retrieve evaluation filename
  string filename( TMutStubFinder::get_evaluation_filename() );
  PHTFileServer::get().open( filename );

  ostringstream what;
  what << "TMutStubFinder::Stub::initialize_evaluation, writing to " << filename;
  MUTOO::TRACE( what.str() );

  // Fields for stub finder algorithm during add coord stage
  _ntuple = new TNtuple("stub_coord","stub_coord","arm:sta:gap:cath:ncoord:added:w_prox:dca:stereo:dwdz:drdz");
  _evaluation = true;

  return;
}

//______________________________________________________________
void TMutStubFinder::Stub::finish_evaluation()
{
  MUTOO::TRACE("TMutStubFinder::Stub::finish_evaluation");
  if( !_evaluation ) return;

  PHTFileServer::get().write( TMutStubFinder::get_evaluation_filename() );
  _evaluation = false;

}

//______________________________________________________________
list<TMutCoordMap::value_type> TMutStubFinder::get_coords_in_window(
  TMutCoordMap* coord_map,
  unsigned short arm,
  unsigned short station,
  unsigned short octant,
  unsigned short half_octant,
  const pair<float,float>& theta_window,
  const pair<float,float>& phi_window)
{

  // Local storage for coordinate list
  local_coord_list local_list;

  // Determine the outer radius of the station
  TMutCoordMap::iterator coord_iter = coord_map->get(arm,station,octant,half_octant);

  if( _verbosity >= MUTOO::MAX ) cout << "arm " << arm << " sta " << station << " oct" << octant << " before " << coord_iter.count();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){

    WindowEnum phi_begin = check_phi_window(coord_ptr->get()->get_coord_begin(), phi_window);
    WindowEnum phi_end = check_phi_window(coord_ptr->get()->get_coord_end(), phi_window);

    // Either the invervals overlap or the coord phi interval spans the phi window.
    bool in_phi_window = false;
    if((phi_begin == IN_WINDOW || phi_end == IN_WINDOW) ||
       (phi_begin == HIGH_WINDOW && phi_end == LOW_WINDOW) ||
       (phi_begin == LOW_WINDOW && phi_end == HIGH_WINDOW)) in_phi_window = true;

    WindowEnum theta_begin = check_theta_window(coord_ptr->get()->get_coord_begin(), theta_window);
    WindowEnum theta_end = check_theta_window(coord_ptr->get()->get_coord_end(), theta_window);

    // Either the invervals overlap or the coord theta interval spans the theta window.
    bool in_theta_window = false;
    if((theta_begin == IN_WINDOW || theta_end == IN_WINDOW) ||
       (theta_begin == HIGH_WINDOW && theta_end == LOW_WINDOW) ||
       (theta_begin == LOW_WINDOW && theta_end == HIGH_WINDOW)) in_theta_window = true;

    if(in_theta_window && in_phi_window) local_list.push_back(*coord_ptr);
  }

  if( _verbosity >= MUTOO::MAX ) cout << " after " << local_list.size() << endl;

  // Not so efficient but safe
  return local_list;
}

//______________________________________________________________
TMutStubFinder::WindowEnum TMutStubFinder::check_phi_window(const PHPoint& point, const TMutStubFinder::stub_window& phi_window)
{
  // Project onto x and y axis to avoid [0,2PI] boundary shenanigans
  double x1 = cos(phi_window.first);
  double x2 = cos(phi_window.second);
  double y1 = sin(phi_window.first);
  double y2 = sin(phi_window.second);
  double xp = point.getX();
  double yp = point.getY();

  double r1 = sqrt(MUTOO::SQUARE(x1) + MUTOO::SQUARE(y1));
  double r2 = sqrt(MUTOO::SQUARE(x2) + MUTOO::SQUARE(y2));
  double rp = sqrt(MUTOO::SQUARE(xp) + MUTOO::SQUARE(yp));

  // Here calculate delta phi of window and delta phi of provided
  // point and left and right window extrema.
  double dphi_window = acos((x1*x2 + y1*y2)/(r1*r2));
  double dphi_left = acos((x1*xp + y1*yp)/(r1*rp));
  double dphi_right = acos((x2*xp + y2*yp)/(r2*rp));

  // We calculate the phi angle between the point an the left and
  // right windows.
  //
  // 1) within delta window of both inidicates inside
  // 2) within delta of left but not right indicates left
  // 3) within delta of right but not left indicates right
  // 4) not within delta of either inicates left or right depending upon
  //    which is closer
  if(dphi_left < dphi_window && dphi_right < dphi_window) return IN_WINDOW;
  else if(dphi_left < dphi_window && dphi_right >= dphi_window) return LOW_WINDOW;
  else if(dphi_right < dphi_window && dphi_left >= dphi_window) return HIGH_WINDOW;
  else return (dphi_left > dphi_right) ? LOW_WINDOW : HIGH_WINDOW;

}

//______________________________________________________________
TMutStubFinder::WindowEnum TMutStubFinder::check_theta_window(const PHPoint& point, const TMutStubFinder::stub_window& theta_window)
{

  double xp = point.getX();
  double yp = point.getY();
  double zp = fabs(point.getZ());
  double rp = sqrt(MUTOO::SQUARE(xp) + MUTOO::SQUARE(yp));
  double theta_p = atan2(rp,zp);

  if(theta_p > theta_window.first && theta_p < theta_window.second) return IN_WINDOW;
  else if(theta_p <= theta_window.first) return LOW_WINDOW;
  else return HIGH_WINDOW;

}

//______________________________________________________________
bool TMutStubFinder::initialize_evaluation( void )
{

  // check evaluation is requested
  if( !get_do_evaluation() ) return false;

  // check evaluation was not already done
  if(_evaluation) return false;

  // retrieve evaluation filename
  string filename( TMutStubFinder::get_evaluation_filename() );
  PHTFileServer::get().open( filename );

  ostringstream what;
  what << "TMutStubFinder::initialize_evaluation, writing to " << filename;
  MUTOO::TRACE( what.str() );


  // Fields for stub evaluation after algorithm is complete
  _ntuple = new TNtuple("stub","stub",
			"arm:sta:oct:hoct:nstub:mc_exist:mc_found:"
			"true_hit:bg_hit:thtrue:phtrue:thwinlo:thwinhi:"
			"phwinlo:phwinhi:gap");

  _evaluation = true;
  return true;
}

//______________________________________________________________
void TMutStubFinder::finish_evaluation()
{
  MUTOO::TRACE("TMutStubFinder::finish_evaluation");
  PHTFileServer::get().write( _evaluation_filename );
  _evaluation=false;
  return;
}

//______________________________________________________________
void TMutStubFinder::evaluate(PHCompositeNode* top_node)
{

  // check evaluation is requested
  if( !get_do_evaluation() ) return;

  // check ntuple
  if( !_ntuple ) return;

  TMutStubFinder::stub_list& stub_list = get_stub_list();
  TMutStubFinder::stub_list::iterator stub_list_iter = stub_list.begin();
  for(;stub_list_iter!=stub_list.end();++stub_list_iter){
    float ntvar[20] = {0};

    ntvar[0] = stub_list_iter->get_arm();
    ntvar[1] = stub_list_iter->get_station();
    ntvar[2] = stub_list_iter->get_octant();
    ntvar[3] = stub_list_iter->get_half_octant();
    ntvar[4] = stub_list_iter->get_coord_list().size();

    ntvar[11] = stub_list_iter->get_theta_window().first;
    ntvar[12] = stub_list_iter->get_theta_window().second;
    ntvar[13] = stub_list_iter->get_phi_window().first;
    ntvar[14] = stub_list_iter->get_phi_window().second;

    typedef TMutStubFinder::Stub::coord_list stub_coord_list;
    stub_coord_list& coord_list = stub_list_iter->get_coord_list();
    stub_coord_list::iterator coord_iter = coord_list.begin();
    for(;coord_iter!=coord_list.end();++coord_iter){

      TMutClusMap::const_key_iterator clus_iter = coord_iter->get()->get_associated<TMutClus>();
      if( clus_iter.at_end() ) continue;

      TMutHitMap::const_key_iterator hit_iter = clus_iter->get()->get_associated<TMutHit>();
      while(TMutHitMap::const_pointer hit_ptr = hit_iter.next()){
	TMutMCHitMap::const_key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TMutMCHit>();
	if( mc_hit_iter.at_end() ) continue;

        ntvar[9] = atan2(sqrt(
          mc_hit_iter->get()->get_x()*mc_hit_iter->get()->get_x() +
          mc_hit_iter->get()->get_y()*mc_hit_iter->get()->get_y()),
			    mc_hit_iter->get()->get_z());
        ntvar[10] = atan2( mc_hit_iter->get()->get_y(), mc_hit_iter->get()->get_x());
        ntvar[15] = mc_hit_iter->get()->get_gap();

        //_ntuple->Fill(ntvar);
	      ++mc_hit_iter;

      } // loop over hits

      ++clus_iter;

    }   // loop over clusters

		_ntuple->Fill(ntvar);

  }
}

