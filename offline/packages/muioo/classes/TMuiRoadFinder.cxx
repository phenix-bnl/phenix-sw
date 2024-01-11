#include<TMuiRoadFinder.h>
#include<TMuiChannelId.hh>
#include<PHGeometry.h>
#include<gsl/gsl_fit.h>
#include<boost/array.hpp>
#include<cmath>
#include<TROOT.h>
#include<TMuiClusterMapO.h>
#include<TMuiHitMapO.h>
#include<TMuiMCHitMapO.h>
#include<TMuiErrorStats.h>

using namespace std;

// Static data members TMuiRoadFinder
MUIOO::Verbosity TMuiRoadFinder::_verbosity = MUIOO::NONE;
MUIOO::Verbosity TMuiRoadFinder::Road::_verbosity = MUIOO::NONE;

bool TMuiRoadFinder::_reverse_algo = false;
// limit for when to punt
UShort_t TMuiRoadFinder::_max_n_roads = 5000;
// cuts for whether roads should be accepted
UShort_t TMuiRoadFinder::_min_cluster = 4;
UShort_t TMuiRoadFinder::_min_point = 2;
// cut for whether duplicate roads should be eliminated
UShort_t TMuiRoadFinder::_max_cluster_share = 4;
// evaluation stuff
// evaluation is turned off by default
bool TMuiRoadFinder::_do_evaluation = false;
bool TMuiRoadFinder::_evaluation = false; 
TNtuple* TMuiRoadFinder::_ntuple = 0; 
TFile* TMuiRoadFinder::_file = 0; 
    
// Static data members TMuiRoadFinder::Road
// "tube" cuts for whether new clusters should be added to road
double TMuiRoadFinder::Road::_dca_cut = 4.0;  
double TMuiRoadFinder::Road::_prox_cut = 4.0;
// different orientations have different z values (offset = 2.0 cm)
// i.e. intersections in x-y from lines are still somewhat distant
double TMuiRoadFinder::Road::_orient_z_dist = 2.0;
double TMuiRoadFinder::Road::_tube_width = 8.35; // cm
// evaluation stuff
bool TMuiRoadFinder::Road::_evaluation = false; 
bool TMuiRoadFinder::Road::_use_slope_cuts = false; 
TFile* TMuiRoadFinder::Road::_file = 0; 
TNtuple* TMuiRoadFinder::Road::_ntuple = 0; 
boost::array<float,20> TMuiRoadFinder::Road::_eval_data;

// TMuiRoadFinder methods
//
//______________________________________________________________
struct TMuiRoadFinder::road_equal_ftor
{
  bool operator()(Road& road1, Road& road2)
  {
    const Road::cluster_list& clusters1( road1.get_cluster_list() );
    const Road::cluster_list& clusters2( road2.get_cluster_list() );
    if( clusters1.size() != clusters2.size() ) return false;
    
    Road::cluster_list::const_iterator iter1( clusters1.begin() );
    Road::cluster_list::const_iterator iter2( clusters2.begin() );
    for( ; iter1 != clusters1.end() && iter2 != clusters2.end(); 
	 iter1++, iter2++ )
      if( iter1->get()->get_key().get_obj_key() != 
	  iter2->get()->get_key().get_obj_key() )
	return false;
    
    return true;
  }
};
//______________________________________________________________
struct TMuiRoadFinder::road_less_ftor
{
  bool operator()(Road& road1, Road& road2)
  {
    const Road::cluster_list& clusters1( road1.get_cluster_list() );
    const Road::cluster_list& clusters2( road2.get_cluster_list() );
    Road::cluster_list::const_iterator iter1( clusters1.begin() );
    Road::cluster_list::const_iterator iter2( clusters2.begin() );
    for( ; iter1 != clusters1.end() && iter2 != clusters2.end(); 
	 iter1++, iter2++ )
      if( iter1->get()->get_key().get_obj_key() != 
	  iter2->get()->get_key().get_obj_key() )
	return( iter1->get()->get_key().get_obj_key() < 
		iter2->get()->get_key().get_obj_key() );
		
    return ( clusters1.size() < clusters2.size() );		
  }
};

//______________________________________________________________
// Find all roads -- Windows are wide open
void
TMuiRoadFinder::find(TMuiClusterMapO* cluster_map)
{
  // Never trust a raw pointer
  if(!cluster_map) throw logic_error(DESCRIPTION("TMuiClusterMapO pointer is null"));

  for(int arm=0; arm<MUIOO::NumberOfArms;++arm){
    find(cluster_map, arm);
  }  
  return;
}
//______________________________________________________________
// Find Road in specified arm in given window
void
TMuiRoadFinder::find(TMuiClusterMapO* cluster_map,
		     UShort_t arm,
		     bool use_window,
		     const road_window& theta_window,
		     const road_window& phi_window) 
{
  if(_verbosity >= MUIOO::SOME) {
    cout << PHWHERE << " TMuiRoadFinder::find: location: " << " arm: " << arm << endl;
  }
  // Never trust a raw pointer
  if(!cluster_map) throw logic_error(DESCRIPTION("TMuiClusterMapO pointer is null"));  

  // Local storage for cluster list 
  typedef list<TMuiClusterMapO::value_type> local_list_type;
  local_list_type local_list;

  // Store all coords in this arm in local list.  If using window only 
  // store those coordinates that intersect the window
  if(use_window) {
    local_list = get_coords_in_window(cluster_map,
                                      arm,
                                      theta_window,
                                      phi_window);
  } 
  else {
    TMuiClusterMapO::iterator cluster_iter = cluster_map->get(arm);
    while(TMuiClusterMapO::pointer cluster_ptr = cluster_iter.next()){
      local_list.push_back(*cluster_ptr);
    }
  }

  // Run the algorithm 
  local_list_type::iterator iter = local_list.begin();
  for(;iter!=local_list.end();++iter){
    // Attempt to append cluster to existing road.  If unsuccessful create new road.
    bool was_appended = append_to_road(&(*iter));
    if(!was_appended){
      start_new_road(&(*iter), use_window, theta_window, phi_window);
      if ( _verbosity >= MUIOO::SOME ) cout << PHWHERE << " Start New Road " << endl;
    } 
    if(_verbosity >= MUIOO::SOME) print_roads();
  }

  // Before running the reverse algorithm we tag the roads as complete.
  // (this is so we don't try to add clusters previously found roads)
  set_complete_tag();
  if(_verbosity >= MUIOO::SOME) print_roads();

  // Removes roads that are subsets of other roads
  //  remove_includes();
  size_t n_road = _road_list.size();
  _road_list.sort(road_less_ftor());
  if(_verbosity >= MUIOO::ALOT) {
    cout << PHWHERE << " Printing Roads - after sorting, but before unique check" 
	 << endl;
    print_roads();
  }
  _road_list.unique(road_equal_ftor());
  if(_verbosity >= MUIOO::SOME) {
    cout << PHWHERE << " Printing Roads " << endl;
    print_roads();
  }

  if(_verbosity >= MUIOO::SOME) {
    cout << PHWHERE << " removed " << n_road - _road_list.size() 
	 << " duplicate roads" << endl;
  }


  // If not in REVERSE mode punt before running reverse algorithm
  if(!_reverse_algo) {
    if( get_do_evaluation() ) { // but do evaluation first though, if you should
      evaluate();
    }
    return;
  }

  // Reverse the list
  local_list.reverse();

  // Run the algorithm again (on the reverse sorted list)
  iter = local_list.begin();
  for(;iter!=local_list.end();++iter){
    // Attempt to append cluster to existing road.  
    // If unsuccessful create new road.
    bool was_appended = append_to_road(&(*iter));
    if(!was_appended){
      start_new_road(&(*iter), use_window, theta_window, phi_window);
    }
    if(_verbosity >= MUIOO::SOME) print_roads();
  }
  
  // Set complete on road found in second pass
  set_complete_tag();    
    
  // Removes roads that are subsets of other roads
  remove_includes();
  
  n_road = _road_list.size();
  _road_list.sort(road_less_ftor());
  _road_list.unique(road_equal_ftor());

  if(_verbosity >= MUIOO::SOME) {
    cout << PHWHERE << " removed " << n_road - _road_list.size() 
	 << " duplicate roads" << endl;
  }
  if(_verbosity >= MUIOO::SOME) {
    cout << PHWHERE << " Printing Roads " << endl;
    print_roads();
  }

  if( get_do_evaluation() ) {
    evaluate();
  }

  return;
}
//______________________________________________________________
void 
TMuiRoadFinder::remove_includes()
{
  road_list::iterator iter1 = _road_list.begin();
  for(;iter1!=_road_list.end();++iter1){
    if (!iter1->get_status()) continue; // already marked for removal
    road_list::iterator iter2 = iter1;
    ++iter2;
    for(;iter2!=_road_list.end();++iter2){
      if (!iter2->get_status()) continue; // already marked for removal


      // If roads have same number of clusters and passed the unique check
      // then one is not a subset of the other.
      if(iter1->get_n_cluster() == iter2->get_n_cluster()) continue;

      road_list::iterator big = (iter1->get_n_cluster() > iter2->get_n_cluster()) ? iter1 : iter2;
      road_list::iterator little = (iter1->get_n_cluster() > iter2->get_n_cluster()) ? iter2 : iter1;

      // Mark identical cluster includes for removal
      if(includes(big->get_cluster_list().begin(),
		       big->get_cluster_list().end(),
		       little->get_cluster_list().begin(),
		       little->get_cluster_list().end())) {
	little->set_status(false);
      }      

      // Count the number of clusters that are shared
      // If it's more than _max_cluster_share, then the little road is deleted
      UShort_t nshare = 0;

      Road::cluster_list::const_iterator big_iter = 
	big->get_cluster_list().begin();
      Road::cluster_list::const_iterator little_iter = 
	little->get_cluster_list().begin();
      for(;big_iter!=big->get_cluster_list().end();++big_iter){
	bool match = false;
	little_iter = little->get_cluster_list().begin();
	for(;little_iter!=little->get_cluster_list().end();++little_iter){
	  if(big_iter->get()->get_key().get_obj_key()  
	     == little_iter->get()->get_key().get_obj_key()) {
	    match = true;
	  }
	}
	if (match) nshare++;
      }
      if (nshare > _max_cluster_share) {    
	little->set_status(false);
      }
    }
  }  
  // Remove marked roads
  _road_list.remove_if(road_bad_status_ftor());    
  return;
}
//______________________________________________________________
void
TMuiRoadFinder::set_complete_tag()
{
  road_list::iterator road_iter = _road_list.begin();
  road_iter = _road_list.begin();
  for(;road_iter!=_road_list.end();++road_iter) {
    if(_verbosity >= MUIOO::ALOT) {
      cout << PHWHERE << " road status = " << road_iter->get_status() << endl;
    }
    if(road_iter->get_cluster_list().size() < _min_cluster ||
       road_iter->get_point_list().size() < _min_point) {
      road_iter->set_status(false);
    } else {
      road_iter->set_complete(true);
    }
  }
  _road_list.remove_if(road_bad_status_ftor());    
  return;
}
//______________________________________________________________
bool 
TMuiRoadFinder::append_to_road(TMuiClusterMapO::pointer cluster_ptr)
{    
  // Initialize was_appended = false
  // Loop over Roads in road list [
  //   if add_new_cluster successfull [
  //     set was_appended = true
  //   ]
  // ]  
  // return was_appended  
  //
  bool sticky_appended = false;
  road_list::iterator road_iter = _road_list.begin();
  for(;road_iter!=_road_list.end();++road_iter){
    
    // Punt if this road has the complete flag set
    if(road_iter->is_complete()) continue;
    
    // Attempt to append cluster to current road
    if (_verbosity >= MUIOO::SOME){
      cout << PHWHERE << " Attempt to add cluster (to road): " << endl;
      cout << "arm = " << cluster_ptr->get()->get_arm() <<
	" plane = " << cluster_ptr->get()->get_plane() <<
	" panel = " << cluster_ptr->get()->get_panel() <<
	" orientation = " << cluster_ptr->get()->get_orientation() <<
	" index = " << cluster_ptr->get()->get_index() <<endl;

      typedef TMuiRoadFinder::Road::cluster_list road_cluster_list;
      road_cluster_list& cluster_list = road_iter->get_cluster_list();
      road_cluster_list::iterator cluster_iter_1 = cluster_list.begin();
      cout << "to road: " << endl;
      for(;cluster_iter_1!=cluster_list.end();++cluster_iter_1)
	cout << PHWHERE  << " arm = " << cluster_iter_1->get()->get_arm() <<
	  " plane = " << cluster_iter_1->get()->get_plane() <<
	  " panel = " << cluster_iter_1->get()->get_panel() <<
	  " orientation = " << cluster_iter_1->get()->get_orientation() <<
	  " index = " << cluster_iter_1->get()->get_index() <<endl;  
    }


    bool was_appended =  road_iter->add_cluster(cluster_ptr);

    if (_verbosity >= MUIOO::SOME && was_appended ) {
      cout << PHWHERE << " Successful Add " << endl;
    }
    sticky_appended = (sticky_appended) ? sticky_appended : was_appended;    
  }

  // Plugging a logic hole here.  If road has been previously
  // bifurcated then 2 roads will share a list of hits when
  // the current plane is removed.  The new hit will be added
  // to both hence we sort and remove duplicates at this stage.
  //
  _road_list.sort(road_less_ftor());
  _road_list.unique(road_equal_ftor());

  return sticky_appended;
}
//______________________________________________________________
void
TMuiRoadFinder::print_roads() const
{
  MUIOO::PRINT(cout,"**");
  for_each(_road_list.begin(), _road_list.end(), road_print_ftor());
  MUIOO::PRINT(cout,"**");
  return;
}
//______________________________________________________________
void
TMuiRoadFinder::abort_new_road()
{
  _road_list.pop_front();      			    
  return;
}
//______________________________________________________________
TMuiRoadFinder::Road*
TMuiRoadFinder::start_new_road(TMuiClusterMapO::pointer cluster_ptr,
			       bool use_window,
			       const road_window& theta_window,
			       const road_window& phi_window)
{
  _road_list.push_front(Road(this, cluster_ptr, use_window,
			     theta_window, phi_window));      			    

  // Return a pointer to the newly created Road
  return &(*_road_list.begin());
}

// TMuiRoadFinder::Road methods
//
//______________________________________________________________
bool 
TMuiRoadFinder::Road::add_cluster(TMuiClusterMapO::pointer cluster_ptr) 
{
  bool was_added=false;
  if(_evaluation) _eval_data.assign(0);

  // If road has a cluster in this plane and orientation and has a least 2 clusters 
  // already then call bifurcate
  if(!unique_check(cluster_ptr)) {
    if( _bifurcate == 2*cluster_ptr->get()->get_plane() + 
	cluster_ptr->get()->get_orientation() ) return was_added;
    if(_clusters.size()>1) {
      was_added = bifurcate(cluster_ptr);
    } 
    return was_added;
  } 
  
  // If we made it until here, it means that this cluster is the first in this plane 
  // and orientation and could add to our information on the road (if it fits in)

  // if Road has point [
  //  project point to z of cluster
  //  test theta, phi window of projected point
  //  test projected point - cluster DCA
  //  if added update road parameters
  // ] else [ 
  //  if can make point with this cluster [
  //    test theta, phi window of point
  //    if added update road parameters
  //  ] else [
  //    test proximity of cluster with existing cluster
  //  ]
  // ]  
  
  if(_points.size() != 0){

    PHPoint point = project(cluster_ptr->get()->get_centroidpos().getZ());
    if(test_window(point) && 
       check_dca(cluster_ptr,point) &&
       check_proximity(cluster_ptr)){
      update_road(cluster_ptr);
      was_added = true;
    }
    if (_verbosity >= MUIOO::SOME){
      if (!test_window(point)) cout << PHWHERE << " test window failed" << endl;
      if (!check_dca(cluster_ptr,point)) cout << PHWHERE  << " dca failed" << endl;
      if (!check_proximity(cluster_ptr)) cout << PHWHERE << " prox failed" << endl;
    }
    
  } 
  else {
    // Check if this cluster is parallel to existing cluster
    if(!check_parallel(cluster_ptr)) {
      // Not parallel (and the right panel/plane) so it will intersect
      // with an existing cluster in this road - update&add
      if (has_window()) {
	PHPoint point = make_point(cluster_ptr);
	// If point is in window then add cluster and point to road
	if(test_window(point)){
	  update_road(cluster_ptr);
	  was_added = true;
	}
	else if (_verbosity >= MUIOO::SOME) {
	  cout << PHWHERE << " test window failed" << endl;
	}
      }
      else {
	update_road(cluster_ptr);
	was_added = true;
      } 
    }
    else {
      // Prior and current cluster are parallel, check proximity and add
      // cluster to road if proximity cut is satisfied.
      if (check_proximity(cluster_ptr)) {
	update_road(cluster_ptr);
	was_added = true;
      }
      else if (_verbosity >= MUIOO::SOME) {
	cout << PHWHERE << " prox failed" << endl;
      }
    }
  }  

  if(_clusters.size() >= 3){
    check_slope();
  }
  if(_evaluation && _ntuple) {
    cerr << "TMuiRoadFinder::Road::add_cluster - filling ntuple" << endl;
    _eval_data[0] = _arm;
    _eval_data[1] = cluster_ptr->get()->get_plane();
    _eval_data[2] = cluster_ptr->get()->get_panel();
    _eval_data[3] = cluster_ptr->get()->get_orientation();
    _eval_data[4] = _clusters.size(); 
    _eval_data[5] = was_added ? 1 : 0; 
    _ntuple->Fill(&(_eval_data[0]));
  }
  return was_added;
}
//______________________________________________________________
bool 
TMuiRoadFinder::Road::check_parallel(const TMuiClusterMapO::pointer cluster_ptr) const 
{  
  // Loop over clusters in road cluster list and check for non-parallel cluster
  // in the same plane, panel
  cluster_list::const_iterator cluster_iter = _clusters.begin();
  for(;cluster_iter!=_clusters.end();++cluster_iter){
    if ( (cluster_iter->get()->get_plane() == cluster_ptr->get()->get_plane()) && 
	 (cluster_iter->get()->get_panel() == cluster_ptr->get()->get_panel()) && 
	 (cluster_iter->get()->get_orientation() != cluster_ptr->get()->get_orientation()) ) {
      return false;
    }
  }
  return true;
}
//______________________________________________________________
bool
TMuiRoadFinder::Road::check_slope() const
{
  return true;
}
//______________________________________________________________
bool 
TMuiRoadFinder::Road::test_window(const PHPoint& point) const
{
  // If there is no window defined for this road then  
  // we can say that the new cluster falls within the window..
  //
  if (!has_window()) {
    return true;
  }
  
  // If we haven't got any points, we don't have a well-defined/centered
  // window yet
  //
  if (_points.size() == 0) {
    return true;
  }

  TMuiRoadFinder::WindowEnum phi_check = 
    TMuiRoadFinder::check_phi_window(point,_phi_window);

  TMuiRoadFinder::WindowEnum theta_check = 
    TMuiRoadFinder::check_theta_window(point,_theta_window);

  if (_verbosity >= MUIOO::SOME){
    cout << PHWHERE << " point = " << point.getX() << ", " << point.getY() << ", " 
	 << point.getZ() << endl;
    cout << " phi = " << _phi_window.first << ", " 
	 << _phi_window.second << endl;
    cout << " theta = " << _theta_window.first << ", " 
	 << _theta_window.second << endl;
  }

  if(_evaluation) {
    _eval_data[6] = phi_check; 
    _eval_data[7] = theta_check; 
    _eval_data[10] = point.getX(); 
    _eval_data[11] = point.getY();
    _eval_data[12] = point.getZ();  
    _eval_data[13] = _phi_window.first;  
    _eval_data[14] = _phi_window.second;  
    _eval_data[15] = _theta_window.first;  
    _eval_data[16] = _theta_window.second;  
  }

  return (phi_check == TMuiRoadFinder::IN_WINDOW && 
	  theta_check == TMuiRoadFinder::IN_WINDOW);
}
//______________________________________________________________
bool 
TMuiRoadFinder::Road::check_dca(const TMuiClusterMapO::pointer cluster_ptr, 
				const PHPoint& point)  const 
{
  // get the line info for the cluster
  PHLine line = cluster_ptr->get()->get_coord();
  double distance = PHGeometry::distanceLinePoint(line, point);
  // normalize distance to something 
  // width is from the narrow dimension of the tube
  if (_tube_width > 0) distance /= _tube_width; 
  else return false;
       
  if(_evaluation) {
    _eval_data[8] = distance; 
  }

  if (_verbosity >= MUIOO::SOME) {
    cout << PHWHERE << " distance = " << distance << endl;
  }
 
  if(distance < _dca_cut) {
    // since the line distance calculation assumes an infinite line, 
    // it is possible to get an acceptable distance even when the line is
    // offset from the point. Use centroidpos, which should not be more
    // than half the length away from any point, for this
    double projdist = PHGeometry::distancePointToPoint(cluster_ptr->get()->get_centroidpos(), point);
    double halflength = 0.5*line.length();
    if (projdist < halflength) {
      return true;
    }
  }
  return false;
}
//______________________________________________________________
bool
TMuiRoadFinder::Road::bifurcate(TMuiClusterMapO::pointer cluster_ptr)  
{
  // Sanity check
  //
  if(_finder->_road_list.size() > _max_n_roads) {
    ostringstream msg;
    msg << " bifurcate: too many roads " << _finder->_road_list.size()
	<< " in arm:" << _arm ;
    TMuiErrorStats::set_error(TMuiErrorStats::ROAD_BIFURCATE, _arm,
			      _finder->_road_list.size());
    throw runtime_error(DESCRIPTION(msg.str().c_str()));
  }

  // New road pointer with null initialization
  TMuiRoadFinder::Road* new_road_ptr = 0;
  
  // Loop over cluster list; copy clusters from old road that are not in the 
  // same plane as cluster_ptr
  cluster_list::iterator cluster_iter = _clusters.begin();
  for(;cluster_iter!=_clusters.end();++cluster_iter){
    if(cluster_ptr->get()->get_plane() != cluster_iter->get()->get_plane()) {      
      // Old cluster
      TMuiClusterMapO::value_type old_cluster = *cluster_iter;

      // First time start the new road -- subsequent times update road with old cluster
      if(!new_road_ptr) {
	new_road_ptr = _finder->start_new_road(&old_cluster, _has_window, _theta_window, _phi_window);
      } 
      else {
	new_road_ptr->update_road(&old_cluster);
      }
    }
  }

  // If cluster_ptr is the first then return here
  if(!new_road_ptr) {
    _finder->start_new_road(cluster_ptr, _has_window, _theta_window, _phi_window);
    return true;
  }
  new_road_ptr->set_bifurcate( 2*cluster_ptr->get()->get_plane() +
			       cluster_ptr->get()->get_orientation() );
  
  // Now we have a road that is guarenteed *not* to have a cluster in the same plane
  // as cluster_ptr.  Here we invoke a fancy (obtuse) piece of recursion by calling
  // add_cluster using the new road pointer. This will ensure that exactly the same
  // criteria is applied for adding clusters to roads in both the original and
  // new (bifurcated) road.
  //
  bool was_added = new_road_ptr->add_cluster(cluster_ptr);
  if(was_added) {
    return true;
  } 
  else {
    _finder->abort_new_road();
    return false;
  }
}

//______________________________________________________________
bool 
TMuiRoadFinder::Road::check_proximity(TMuiClusterMapO::pointer cluster_ptr) const 
{
  // This method is called only when we don't have any points found yet, and
  // not any non-parallell clusters to this new one, in the same plane and panel.
  // If the new one is close enough, we'll add it to our cluster list.
  //
  // Idiot check
  if(!_clusters.size()) throw logic_error(DESCRIPTION("attempted comparison with null entry"));

  PHPoint point1 = cluster_ptr->get()->get_centroidpos();

  // Loop over clusters in road cluster list and get the closest (in Z) parallel cluster
  double distance = DBL_MAX;
  cluster_list::const_iterator best_iter;
  cluster_list::const_iterator cluster_iter = _clusters.begin();
  for(;cluster_iter!=_clusters.end();++cluster_iter){
    if (cluster_iter->get()->get_orientation() == cluster_ptr->get()->get_orientation()) {
      PHPoint point2 = cluster_iter->get()->get_centroidpos();
      double thisdistance = fabs(point2.getZ() - point1.getZ());
      if (thisdistance < distance) {
	distance = thisdistance;
	best_iter = cluster_iter;
      }
    }
  }

  if (distance == DBL_MAX) { 
    // nothing found to compare with
    return false;
  }

  PHPoint point2 = best_iter->get()->get_centroidpos();
  point1.setZ(0);
  point2.setZ(0);
  distance = PHGeometry::distancePointToPoint(point1,point2);

  if (_tube_width > 0) distance /= _tube_width; 
  else return false;

  if(_evaluation) {
    _eval_data[9] = distance; 
  }

  if(distance < _prox_cut) {
    return true;
  }
  return false;
}
//______________________________________________________________
void
TMuiRoadFinder::Road::update_road(TMuiClusterMapO::pointer cluster_ptr) 
{
  // Push cluster onto cluster list
  _clusters.insert(*cluster_ptr);
  
  // Regenerate points list with new cluster -- repetitive but
  // makes the code simple.
  _points.clear();

  // Loop over all unique combinations of clusters in this road,
  // attempt to make points out of cluster that share a common
  // plane.
  cluster_list::const_iterator iter1 = _clusters.begin();
  PHLine line1, line2;
  PHPoint point;
  for(;iter1!=_clusters.end();++iter1){

    line1 = iter1->get()->get_coord();
    cluster_list::const_iterator iter2 = iter1;
    ++iter2;
    
    for(;iter2!=_clusters.end();++iter2){
     
      if(iter1->get()->get_plane() == iter2->get()->get_plane() &&
	 iter1->get()->get_panel() == iter2->get()->get_panel() &&
	 iter1->get()->get_orientation() != iter2->get()->get_orientation() ){

	line2 = iter2->get()->get_coord();

	// check possible intersection of line1 and line2
	double distance = PHGeometry::distanceLineLine(line1, line2);
	
	if (distance <= _orient_z_dist) // we had an intersection
	  {
	    point = PHGeometry::closestApproachLineLine(line1, line2);
	    _points.push_back(point);
	  }
      }
    }
  }

  // update fit parameters
  //  cout << " number of points " << _points.size() << endl;
  if(_points.size()==0){
    // TMutStubFinder here tries to find points by combining hits from 
    // different gaps - we will not attempt this for MUID. [gaps are well separated]
    // If we have 0 points then just initialize the fit_par with the 
    // center of the first cluster
    iter1 = _clusters.begin();
    PHPoint mid_point = iter1->get()->get_centroidpos();
    // One point, take the point with 0 angles (dx/ydz)
    //
    _fit_par.set_x(mid_point.getX());
    _fit_par.set_y(mid_point.getY());
    _fit_par.set_z(mid_point.getZ());
    _fit_par.set_dxdz(0);
    _fit_par.set_dydz(0);    
  } 
  else if(_points.size()==1){
    
    // One point, take the point and assign angles from nominal vertex for dxy/dz
    _fit_par.set_x(_points.begin()->getX());
    _fit_par.set_y(_points.begin()->getY());
    _fit_par.set_z(_points.begin()->getZ());
    _fit_par.set_dxdz(_points.begin()->getX()/_points.begin()->getZ());
    _fit_par.set_dydz(_points.begin()->getY()/_points.begin()->getZ());

    // center the phi/theta windows around the found point
    PHSphPoint spherepoint;
    PHGeometry::cartesianToSpherical(_points[0], spherepoint);
    double half_phi_window_size = 0.5*(_phi_window.second - _phi_window.first);
    double half_theta_window_size = 0.5*(_theta_window.second - _theta_window.first);

    _phi_window.first = static_cast<double>(spherepoint.getPhi())
      - half_phi_window_size; 
    _phi_window.second = static_cast<double>(spherepoint.getPhi())
      + half_phi_window_size; 
    _theta_window.first = static_cast<double>(spherepoint.getTheta())
      - half_theta_window_size;  
    _theta_window.second = static_cast<double>(spherepoint.getTheta())
      + half_theta_window_size; 
  } 
  else if(_points.size()>1) {

    // More than one point, do a linear fit
    // The second argument is the array dimension
    boost::array<double,MUIOO::MAX_PLANE> x = {{0}}; 
    boost::array<double,MUIOO::MAX_PLANE> y = {{0}}; 
    boost::array<double,MUIOO::MAX_PLANE> z = {{0}}; 
      
    double z0 = _points[0].getZ();
    size_t n_data = _points.size();

    // use constants from float.h to really init min/max properly
    double z_max = -DBL_MAX; 
    double z_min = DBL_MAX;

    PHSphPoint spherepoint;
    double phi_sum = 0.0;
    double theta_sum = 0.0;
    double half_phi_window_size = 0.5*(_phi_window.second - _phi_window.first);
    double half_theta_window_size = 0.5*(_theta_window.second - _theta_window.first);

    for(size_t i=0;i<n_data;++i){
      PHGeometry::cartesianToSpherical(_points[i], spherepoint);
      phi_sum += spherepoint.getPhi();
      theta_sum += spherepoint.getTheta();
      x.at(i) = _points[i].getX();
      y.at(i) = _points[i].getY();
      z.at(i) = _points[i].getZ() - z0;
      z_min = min(z_min,_points[i].getZ());
      z_max = max(z_max,_points[i].getZ());
    }

    phi_sum /= n_data;
    theta_sum /= n_data;
    _phi_window.first = phi_sum - half_phi_window_size; 
    _phi_window.second = phi_sum + half_phi_window_size; 
    _theta_window.first = theta_sum - half_theta_window_size;  
    _theta_window.second = theta_sum + half_theta_window_size;  

    double x0=0,m_x=0, y0=0, m_y=0;
    double cov00=0, cov11=0, cov22=0,chi2x=0,chi2y=0;
      
    // Fit x: method from gsl_fit.h
    gsl_fit_linear(z.begin(),1,x.begin(),1,n_data,
		   &x0,&m_x,&cov00,&cov11,&cov22,&chi2x);
      
    // Fit y
    gsl_fit_linear(z.begin(),1,y.begin(),1,n_data,
		   &y0,&m_y,&cov00,&cov11,&cov22,&chi2y);

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

    _fit_par.set_dxdz(m_x);
    _fit_par.set_dydz(m_y);
    
    // "begin" is closest to interaction region
    if(_arm == MUIOO::South) {
      _fit_par.set_z_begin(z_max) ;
      _fit_par.set_z_end(z_min);
    } else {
      _fit_par.set_z_begin(z_min) ;
      _fit_par.set_z_end(z_max);
    }
  }
}
//______________________________________________________________
PHPoint
TMuiRoadFinder::Road::project(double z) const 
{
  return TMutTrackUtil::linear_track_model(&_fit_par,z);
}
//______________________________________________________________
PHPoint
TMuiRoadFinder::Road::make_point(const TMuiClusterMapO::pointer cluster_ptr) const 
{  
  // get the line info for the cluster
  PHLine line1 = cluster_ptr->get()->get_coord();
  // Loop over clusters in road cluster list and check for non-parallel cluster
  // in the same plane, panel; if distance between lines is just distance in z,
  // then we'll call this an overlap/point.
  // Selection should agree with what is done in unique_check!
  cluster_list::const_iterator cluster_iter = _clusters.begin();
  for(;cluster_iter!=_clusters.end();++cluster_iter){
    if ( (cluster_iter->get()->get_plane() == cluster_ptr->get()->get_plane()) && 
	 (cluster_iter->get()->get_panel() == cluster_ptr->get()->get_panel()) && 
	 (cluster_iter->get()->get_orientation() != cluster_ptr->get()->get_orientation()) ) { 
      // there should only be one candidate in the list, so we should have it now
      PHLine line2 = cluster_iter->get()->get_coord();
      // check possible intersection of line1 and line2
      double distance = PHGeometry::distanceLineLine(line1, line2);
	
      if (distance <= _orient_z_dist) // we had an intersection
	{
	  PHPoint point = PHGeometry::closestApproachLineLine(line1, line2);
	  return point;
	}
    }
  }
  return PHPoint();
}
//______________________________________________________________
bool
TMuiRoadFinder::Road::unique_check(TMuiClusterMapO::pointer cluster_ptr) const
{
  // Loop over clusters and punt if the road already has a cluster in
  // this panel, plane and orientation, i.e. allow for hits in neighboring panels too 
  // Selection should agree with what is done in make_point!
  cluster_list::const_iterator cluster_iter = _clusters.begin();
  for(;cluster_iter!=_clusters.end();++cluster_iter){
    
   if(cluster_ptr->get()->get_plane() == cluster_iter->get()->get_plane() &&
      cluster_ptr->get()->get_panel() == cluster_iter->get()->get_panel() && 
      cluster_ptr->get()->get_orientation() == cluster_iter->get()->get_orientation()) {
     return false;
   }
  }
  return true;
}
//______________________________________________________________
bool
TMuiRoadFinder::Road::check_theta_window(const TMuiRoadFinder::road_window& theta_window) const 
{
  double theta = get_theta();
  return (theta > theta_window.first && theta < theta_window.second);
}
//______________________________________________________________
bool
TMuiRoadFinder::Road::check_phi_window(const TMuiRoadFinder::road_window& phi_window) const 
{
  // Project onto x and y axis to avoid [0,2PI] boundary shenanigans
  //
  double x1 = cos(phi_window.first);
  double x2 = cos(phi_window.second);
  double y1 = sin(phi_window.first);
  double y2 = sin(phi_window.second);
  double xp = cos(get_phi());
  double yp = sin(get_phi());

  double r1 = sqrt(MUIOO::SQUARE(x1) + MUIOO::SQUARE(y1));
  double r2 = sqrt(MUIOO::SQUARE(x2) + MUIOO::SQUARE(y2));
  double rp = sqrt(MUIOO::SQUARE(xp) + MUIOO::SQUARE(yp));
  
  double dphi_window = acos((x1*x2 + y1*y2)/(r1*r2));
  double dphi_left = acos((x1*xp + y1*yp)/(r1*rp));
  double dphi_right = acos((x2*xp + y2*yp)/(r2*rp));
  
  // intermediate value -- has to be within dphi_window of
  // both edges.
  return (dphi_left < dphi_window && dphi_right < dphi_window);
}
//______________________________________________________________
void 
TMuiRoadFinder::Road::initialize_evaluation()
{ 
  // check evaluation is requested
  if(_verbosity >= MUIOO::ALOT) cout << PHWHERE << " initialize evaluation " << endl;

  if( !TMuiRoadFinder::get_do_evaluation() ) return;
  if(_verbosity >= MUIOO::ALOT) cout << PHWHERE << " do evaluation " << endl;
  
  // check initialization was not already done
  if(_evaluation) return;
  MUIOO::TRACE("TMuiRoadFinder::Road::initialize_evaluation");
  if(_verbosity >= MUIOO::ALOT) cout << PHWHERE << " evaluation " << endl;

  MUIOO::TRACE("TMuiRoadFinder::Road::initialize_evaluation, creating eval root file");
  _file = new TFile("muioo_eval_ntuple.root", "RECREATE");

  // Fields for road finder algorithm during add cluster stage
  _ntuple = new TNtuple("road_cluster","road_cluster","arm:plane:panel:orientation:ncoord:added:phi_check:theta_check:dca_dist:prox_dist:x:y:z:phi1:phi2:theta1:theta2");
  cout << PHWHERE << " TMuiRoadFinder::Road::initialize_evaluation() ntuple created" << endl; 
  
  _evaluation = true;
  
  return;
}
//______________________________________________________________
void 
TMuiRoadFinder::Road::finish_evaluation()
{  
  MUIOO::TRACE("TMuiRoadFinder::Road::finish_evaluation");
  if( _file ) _file->Write();
  if(_ntuple) {
    delete _ntuple;
    _ntuple = 0;
  }
  _evaluation = false;
  return;
}
// back to TMuiRoadFinder methods
//______________________________________________________________
list<TMuiClusterMapO::value_type>
TMuiRoadFinder::get_coords_in_window(TMuiClusterMapO* cluster_map,
				     UShort_t arm,
				     const road_window& theta_window,
				     const road_window& phi_window)
{
  // Local storage for coordinate list 
  typedef list<TMuiClusterMapO::value_type> local_list_type;
  local_list_type local_list;

  TMuiClusterMapO::iterator cluster_iter = cluster_map->get(arm);
  if( _verbosity >= MUIOO::SOME ) {
    cout << PHWHERE << " arm " << arm 
	 << " before " << cluster_iter.count();  
  }

  while(TMuiClusterMapO::pointer cluster_ptr = cluster_iter.next()){

    WindowEnum phi_begin = 
      check_phi_window(cluster_ptr->get()->get_coord_begin(), phi_window);
    WindowEnum phi_end = 
      check_phi_window(cluster_ptr->get()->get_coord_end(), phi_window);

    // Either the invervals overlap or the coord phi interval spans 
    // the phi window.  
    bool in_phi_window = false;
    if((phi_begin == IN_WINDOW || phi_end == IN_WINDOW) ||
       (phi_begin == HIGH_WINDOW && phi_end == LOW_WINDOW) ||
       (phi_begin == LOW_WINDOW && phi_end == HIGH_WINDOW)) {
      in_phi_window = true;
    }
    
    WindowEnum theta_begin = 
      check_theta_window(cluster_ptr->get()->get_coord_begin(), theta_window);
    WindowEnum theta_end = 
      check_theta_window(cluster_ptr->get()->get_coord_end(), theta_window);

    // Either the invervals overlap or the coord theta interval spans 
    // the theta window.  
    bool in_theta_window = false;
    if((theta_begin == IN_WINDOW || theta_end == IN_WINDOW) ||
       (theta_begin == HIGH_WINDOW && theta_end == LOW_WINDOW) ||
       (theta_begin == LOW_WINDOW && theta_end == HIGH_WINDOW)) {
      in_theta_window = true;    
    }

    if(in_theta_window && in_phi_window) {
      local_list.push_back(*cluster_ptr);
      if ( _verbosity >= MUIOO::SOME ) {
	cout << PHWHERE << " after " << local_list.size() << endl;
      }
    }    
  }
  return local_list;
}
//______________________________________________________________
TMuiRoadFinder::WindowEnum
TMuiRoadFinder::check_phi_window(const PHPoint& point, 
				 const TMuiRoadFinder::road_window&phi_window) 
{
  // Convert to spherical coordinates
  PHSphPoint spherepoint;
  PHGeometry::cartesianToSpherical(point, spherepoint);
 
  // spherepoints phi-coordinate should be in the range between
  // the first and second phi-window values.
  double thisphi = spherepoint.getPhi();
  double dphi_right = phi_window.second - thisphi;
  double dphi_left = thisphi - phi_window.first;
  // i.e. if both dphi_left/right values are positive, we are within the window

  // Some care need to be taken around 2PI
  // If the higher limit is above 2PI, and the point is on the positive side
  // we subtract 2PI, so the 
  // same scale is used as for PHSphPoint
  // The lower value is always less than 2PI
  if (phi_window.second > 2*M_PI && point.getY() > 0) {
    dphi_right -= 2*M_PI;
  }

  //
  // 1) within delta window of both indicates inside
  // 2) within delta of left but not right indicates left
  // 3) within delta of right but not left indicates right
  // 4) not within delta of either indicates left or right depending upon
  //    which is closer
  //
  if(dphi_left >= 0 && dphi_right >= 0){
    return IN_WINDOW;
  } else if(dphi_left < 0) {
    return LOW_WINDOW;
  } else if(dphi_right < 0) {
    return HIGH_WINDOW;
  } else {
    return HIGH_WINDOW; // this should never happen but makes compiler happy
  }
}
//______________________________________________________________
TMuiRoadFinder::WindowEnum
TMuiRoadFinder::check_theta_window(const PHPoint& point, 
				   const TMuiRoadFinder::road_window& 
				   theta_window) 
{ 
  // Convert to spherical coordinates
  PHSphPoint spherepoint;
  PHGeometry::cartesianToSpherical(point, spherepoint);
  double theta_p = spherepoint.getTheta();
  
  if(theta_p >= theta_window.first && theta_p <= theta_window.second){
    return IN_WINDOW;
  } else if(theta_p < theta_window.first) {
    return LOW_WINDOW;
  } else if(theta_p > theta_window.second) {
    return HIGH_WINDOW;
  } else {
    return HIGH_WINDOW; // this should never happen but makes compiler happy
  }
}
//______________________________________________________________
bool
TMuiRoadFinder::initialize_evaluation()
{
  // check evaluation is requested
  if(_verbosity >= MUIOO::ALOT) cout << PHWHERE << " initialize evaluation " << endl;
  // filename (hard wired)
  static const string filename( "muioo_eval_road.root" );

  // check evaluation is requested
  if( !get_do_evaluation() ) return false;
  if(_verbosity >= MUIOO::ALOT) cout << PHWHERE << " do evaluation " << endl;
  
  // check evaluation was not already done
  if(_evaluation) return false;
  if(_verbosity >= MUIOO::ALOT) cout << PHWHERE << " evaluation " << endl;

  MUIOO::TRACE("TMuiRoadFinder::initialize_evaluation, creating eval root file");
  _file = new TFile(filename.c_str(), "RECREATE");

  // Fields for road evaluation after algorithm is complete
  _ntuple = new TNtuple("road","road",
			"arm:iroad:iclust:ihit:mc_found:"
			"true_hit:bg_hit:th_true:phi_true:th_win:ph_win");
  
  _evaluation = true;
  return true;
}
//______________________________________________________________
void 
TMuiRoadFinder::finish_evaluation()
{
  MUIOO::TRACE("TMuiRoadFinder::finish_evaluation");
  if( _file ) _file->Write();
  if( _ntuple ) {
    delete _ntuple;
    _ntuple = 0;
  }

  _evaluation=false;
  return;
}
//______________________________________________________________
void 
TMuiRoadFinder::evaluate()
{
  // NOTE: This method needs some serious work to become useful!!
  // (DS: 20040724)
  // check evaluation is requested
  if( !get_do_evaluation() ) return;  
  // check ntuple
  if( !_ntuple ) return;

  TMuiRoadFinder::road_list& road_list = get_road_list();
  TMuiRoadFinder::road_list::iterator road_list_iter = road_list.begin();
  int iroad = 0;

  if(_verbosity >= MUIOO::ALOT) {
    cout << PHWHERE << " evaluate " 
	 << " nroads " << road_list.size() << endl;
  }

  for(;road_list_iter!=road_list.end();++road_list_iter){
    float ntvar[20] = {0};

    ntvar[0] = road_list_iter->get_arm();
    ntvar[1] = iroad;
    iroad++;
    int iclust = 0;

    typedef TMuiRoadFinder::Road::cluster_list road_cluster_list;
    road_cluster_list& cluster_list = road_list_iter->get_cluster_list();
    road_cluster_list::iterator cluster_iter = cluster_list.begin();    

    if(_verbosity >= MUIOO::ALOT) {
      cout << PHWHERE << " evaluate " 
	   << " iroad " << iroad
	   << " nclusters " << cluster_list.size() << endl;
    }

    for(;cluster_iter!=cluster_list.end();++cluster_iter){
      ntvar[2] = iclust;
      iclust++;
      
      TMuiHitMapO::const_key_iterator hit_iter = cluster_iter->get()->get_associated<TMuiHitO>();
      if(_verbosity >= MUIOO::ALOT) {
	cout << PHWHERE << " evaluate " 
	     << " iclust " << iclust
	     << " nhits " << hit_iter.count() << endl;
      }

      int ihit = 0;
      while(TMuiHitMapO::const_pointer hit_ptr = hit_iter.next()){

	TMuiMCHitMapO::const_key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TMuiMCHitO>();
	if(_verbosity >= MUIOO::ALOT) {
	  cout << PHWHERE << " evaluate " 
	       << " ihit " << ihit
	       << " nmchits " << mc_hit_iter.count() << endl;
	}
	int mc_found = 0;
	if(!mc_hit_iter.at_end()) {
	  ntvar[7] = atan2(sqrt(mc_hit_iter->get()->get_x()*mc_hit_iter->get()->get_x() +
				mc_hit_iter->get()->get_y()*mc_hit_iter->get()->get_y()),
			   mc_hit_iter->get()->get_z());
	  ntvar[8] = atan2( mc_hit_iter->get()->get_y(), mc_hit_iter->get()->get_x());
	  mc_found = 1;
	}
	else {
	  ntvar[7] = ntvar[8] = 0;
	}

	ntvar[3] = ihit;
	ihit++;
	ntvar[4] = mc_found;
	
	_ntuple->Fill(ntvar);
	++mc_hit_iter;
	
      } // loop over hits

    } // loop over clusters
  }
  return;
}



