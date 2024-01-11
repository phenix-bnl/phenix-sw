// $Id: mMuiFindRoad.cxx,v 1.4 2014/11/29 03:22:43 slash Exp $

/*!
	 \file mMuiFindRoad.cxx
	 \brief Associate TMuiCluster's with TMuiRoad
	 \author S. Kelly, C. Zhang
	 \version $Revision: 1.4 $
	 \date $Date: 2014/11/29 03:22:43 $
*/

// MUIOO headers
//
#include <mMuiFindRoad.h>
#include <mMuiFindRoadPar.h>
#include <TMuiChannelId.hh>
#include <PHGeometry.h>

// STL/BOOST/GSL
//
#include <gsl/gsl_fit.h>
#include <iostream>
#include <string>
#include <boost/array.hpp>

using namespace std;

/*! \ingroup modules */
//_____________________________________
mMuiFindRoad::mMuiFindRoad() : 
	_timer( PHTimeServer::get()->insert_new( "mMuiFindRoad" ) )
{
        MUIOO::TRACE("initializing module mMuiFindRoad",MUIOO::ALOT);
}

//_____________________________________
PHBoolean mMuiFindRoad::event(PHCompositeNode* top_node)
{
	
	_timer.get()->restart(); 
	
	try { 

		// Reset IOC pointers
		set_interface_ptrs(top_node);
		_road_map->clear();
		
		// Find Roads using chosen algorithm
		find_roads();
		
		// Do the associations
		//
		associate_clusters();
		
		// Do a final fit, now after everything is settled
		//
		fit_roads();

		// Set road parameters that have not yet been set
		//
		set_road_parameters();

		// Apply road cuts - done in TMuiRoadFinder; not much too add here yet
		//
		//apply_road_cuts();
		
		// Eliminate duplicate roads
		//
		//eliminate_duplicates();
		
	} 
	catch(std::exception& e) {
		
		MUIOO::TRACE(e.what());
		return False;
	}	
	
	// If verbose dump the contents of the cluster map
	//
	_timer.get()->stop();
	if(_mod_par->get_verbosity() >= MUIOO::ALOT) _road_map->print();
	if(_mod_par->get_verbosity() >= MUIOO::SOME) _timer.get()->print();		 
	
	return True;
}

//_____________________________________
void mMuiFindRoad::set_interface_ptrs(PHCompositeNode* top_node){	
	
	// module runtime parameters
	//
	_mod_par = TMutNode<mMuiFindRoadPar>::find_node(top_node,"mMuiFindRoadPar");
	
	// TMuiCluster IOC
	//
	_cluster_map = TMutNode<TMuiClusterMapO>::find_node(top_node,"TMuiClusterMapO");
	// TMuiRoad IOC
	//
	_road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
} 

//_____________________________________
void 
mMuiFindRoad::find_roads()
{
	// The road finding algorithm is encapsulated in TMuiRoadFinder
	//
	TMuiRoadFinder road_finder;
	// 
	// some parameters are hardwired as statics into TMuiRoadFinder
	// use parameter file to set them
	road_finder.set_verbosity(_mod_par->get_verbosity()); 
	road_finder.set_dca_cut(_mod_par->get_coord_dca_cut());
	road_finder.set_prox_cut(_mod_par->get_coord_proximity_cut());
	road_finder.set_min_cluster(_mod_par->get_min_cluster());
	road_finder.set_min_point(_mod_par->get_min_point());
	if (_mod_par->get_mode() == mMuiFindRoadPar::NO_REVERSE) {
		road_finder.set_reversed_algo(false);
	}
	else {
		road_finder.set_reversed_algo(true);
	}
	// window usage
	bool use_window = _mod_par->get_use_window();
	TMuiRoadFinder::road_window phi_window = std::make_pair(0,0);
	TMuiRoadFinder::road_window theta_window = std::make_pair(0,0);
	if (use_window) {
		// set ranges of phi and theta windows
		// the windows are centered by TMuiRoadFinder
		// when starting a road
		phi_window.first -= 0.5 * _mod_par->get_phi_window();
		theta_window.first -= 0.5 * _mod_par->get_theta_window();
		phi_window.second += 0.5 * _mod_par->get_phi_window();
		theta_window.second += 0.5 * _mod_par->get_theta_window();
	}

	if(_mod_par->get_verbosity() == MUIOO::ALOT) {
		static bool dump_done = false; // just dump the info once
		if (!dump_done) {
			cout << PHWHERE << " dump mMuiFindRoadPar info " << endl;
			_mod_par->print();
			cout << PHWHERE << " dump roadfinder info " << endl;
			road_finder.print();
			dump_done = true;

			road_finder.set_do_evaluation(true);
			road_finder.initialize_evaluation();
		}
	}

	// Clear local list
	//
	_roads.clear();
	
	// Loop over all/both arms and envoke the road finder if given
	// arm has hits.
	//
	for(int arm=0; arm<MUIOO::NumberOfArms;++arm) {
		if(_cluster_map->get(arm).count()){
			road_finder.find(_cluster_map, arm,
					 use_window, theta_window, phi_window);
			// Splice all roads found in this section into local list
			//
			_roads.splice(_roads.end(), road_finder.get_road_list());
		}
	}
	
	// Get the list of roads found by the algorithm and instantiate an IOC
	// for those found.
	//
	TMuiRoadFinder::road_list::const_iterator road_list_iter = _roads.begin();
	for(;road_list_iter!=_roads.end();++road_list_iter){
		
		if(_mod_par->get_verbosity() == MUIOO::ALOT) road_list_iter->print();
		
		TMuiRoadMapO::iterator road_iter = _road_map->insert_new(road_list_iter->get_arm());
		// Seed the full fit with road parameters
		//
		road_iter->get()->set_fit_par(road_list_iter->get_fit_par());
	}
}

//_____________________________________
void 
mMuiFindRoad::associate_clusters()
{
	// Loop over TMuiRoad [
	//	Loop over TMuiCluster in same arm
	//		It cluster is close enough to extrapolated
	//		road, then call associate_cluster(..)
	//	]
	// ]

	// Get an iterator to all roads
	//
	TMuiRoadMapO::iterator road_iter = _road_map->range();
	while(TMuiRoadMapO::pointer road_ptr = road_iter.next()){					
		
		UShort_t arm = road_ptr->get()->get_arm();
		
		// Get an iterator to all TMuiCluster in same arm
		//
		TMuiClusterMapO::iterator cluster_iter = _cluster_map->get(arm);
		while(TMuiClusterMapO::pointer cluster_ptr = cluster_iter.next()){								
			// Extract the z, PHLine and road parameters
			//
			PHLine line = cluster_ptr->get()->get_coord();	
			double z_cluster = cluster_ptr->get()->get_centroidpos().getZ();			 
			const TMutFitPar* fit_par = road_ptr->get()->get_const_fitpar();
			
			// Calculate distance form TMuiCluster to extrapolated road
			//
			PHPoint trk_point = TMutTrackUtil::linear_track_model(fit_par,z_cluster);
			double distance = PHGeometry::distanceLinePoint(line,trk_point);
			
			// If the cluster is close enough to the extrapolated Road then associate
			//
			if(distance < _mod_par->get_coord_proximity_cut()){
	associate_cluster(road_ptr,cluster_ptr,trk_point,distance);
			}
		}
	}
}

//_____________________________________
void
mMuiFindRoad::associate_cluster(TMuiRoadMapO::pointer road_ptr,
				TMuiClusterMapO::pointer in_cluster_ptr,
				const PHPoint& trk_point,
				double in_distance)
{
	// Loop over TMuiCluster associated with this road
	//	 if we find a cluster at same locations as input [
	//		if new cluster is closer [
	//			remove old association
	//			associate new cluster
	//			return
	//		] else [
	//			do nothing
	//			return
	//	 ]
	// ]
	//
	// (if we get here no previous association in this location exists)
	// Associate road and cluster
	//
	TMuiClusterMapO::key_iterator cluster_iter = road_ptr->get()->get_associated<TMuiClusterO>();
	while(TMuiClusterMapO::pointer cluster_ptr = cluster_iter.next()){
		
		// Check for existing TMuiCluster at this location and keep the closest one.
		//
		if(cluster_ptr->get()->get_plane() ==	in_cluster_ptr->get()->get_plane()
&& cluster_ptr->get()->get_orientation() ==	in_cluster_ptr->get()->get_orientation()) {
			
			double distance = PHGeometry::distanceLinePoint(cluster_ptr->get()->get_coord(),trk_point);
			// Keep the old one.
			//
			if(distance < in_distance) {
	return;
			} else {
	// Keep the new one, remove the old one.
	//
	PHKey::disassociate(road_ptr,cluster_ptr);
	PHKey::associate(road_ptr, in_cluster_ptr);
	return;
			}
		} 
	}
	// No existing cluster at this location
	//
	PHKey::associate(road_ptr, in_cluster_ptr);
}

//_____________________________________
void
mMuiFindRoad::fit_roads()
{
	// Here we'll do a final fit after all associations 
	// and include all clusters in the fit
	// Get an iterator to all roads
	//
	TMuiRoadMapO::iterator road_iter = _road_map->range();
	while(TMuiRoadMapO::pointer road_ptr = road_iter.next()) {					

		TMutFitPar fit_par = road_ptr->get()->get_fit_par();					

		// Iterator for all clusters associated to this road
		TMuiClusterMapO::const_key_iterator clust_iter = 
			road_ptr->get()->get_associated<TMuiClusterO>();

		// arrays for cluster info
		// The second argument is the array dimension
		// horizontal
		int n_h = 0;
		boost::array<double,MUIOO::MAX_PLANE> hx = {{0}}; 
		boost::array<double,MUIOO::MAX_PLANE> hy = {{0}}; 
		boost::array<double,MUIOO::MAX_PLANE> hz = {{0}}; 
		// vertical
		int n_v = 0;
		boost::array<double,MUIOO::MAX_PLANE> vx = {{0}}; 
		boost::array<double,MUIOO::MAX_PLANE> vy = {{0}}; 
		boost::array<double,MUIOO::MAX_PLANE> vz = {{0}}; 

		double z0 = road_ptr->get()->get_gap0_point().getZ();

		// use constants from float.h to really init min/max properly
		double z_max = -DBL_MAX; 
		double z_min = DBL_MAX;

		while(TMuiClusterMapO::const_pointer clust_ptr = clust_iter.next()) {

			PHPoint cpos = clust_ptr->get()->get_centroidpos();
			z_min = std::min(z_min,cpos.getZ());
			z_max = std::max(z_max,cpos.getZ());
			if (clust_ptr->get()->get_orientation() == kHORIZ) {
	if (n_h >= MUIOO::MAX_PLANE) 
	MUIOO::TRACE(" too many horizontal clusters associated to road");	

	hx.at(n_h) = cpos.getX();
	hy.at(n_h) = cpos.getY();
	hz.at(n_h) = cpos.getZ() - z0;
	n_h++;
			}
			else { // vertical
	if (n_v >= MUIOO::MAX_PLANE) 
	MUIOO::TRACE(" too many vertical clusters associated to road");	
	
	vx.at(n_v) = cpos.getX();
	vy.at(n_v) = cpos.getY();
	vz.at(n_v) = cpos.getZ() - z0;
	n_v++;
			}
		}
			
		double x0=0,m_x=0, y0=0, m_y=0;
		double hcov00=0, hcov11=0, hcov22=0;
		double vcov00=0, vcov11=0, vcov22=0;
		double chi2x=0, chi2y=0;
			
		// Fit x: method from gsl_fit.h
		// use vertical info
		gsl_fit_linear(vz.begin(),1,vx.begin(),1,n_v,
			 &x0,&m_x,&vcov00,&vcov11,&vcov22,&chi2x);
			
		// Fit y
		// use horizontal info
		gsl_fit_linear(hz.begin(),1,hy.begin(),1,n_h,
			 &y0,&m_y,&hcov00,&hcov11,&hcov22,&chi2y);

		// no sanity check made on results - let's just store them
		fit_par.set_x(x0);
		fit_par.set_y(y0);
		fit_par.set_z(z0);
		fit_par.set_dxdz(m_x);
		fit_par.set_dydz(m_y);
		
		// for the covariance part, we don't need to use all the 4x4 space
		// in TMutFitPar, 2*3 is enough; first horizontal and then vertical info
		fit_par.set_covar(kHORIZ, 0, hcov00);
		fit_par.set_covar(kHORIZ, 1, hcov11);
		fit_par.set_covar(kHORIZ, 2, hcov22);
		fit_par.set_covar(kVERT, 0, vcov00);
		fit_par.set_covar(kVERT, 1, vcov11);
		fit_par.set_covar(kVERT, 2, vcov22);

		// chi-sq and degrees of freedom
		double chisq = chi2x + chi2y; 
		size_t ndof = n_h + n_v - 2*2;
		if (ndof > 0) { 
			chisq /= ndof;
			road_ptr->get()->set_road_quality(chisq);
		}
		else road_ptr->get()->set_road_quality(0.0); 

		road_ptr->get()->set_freedom(ndof);
		fit_par.set_chi_square(chisq);

		// "begin" is closest to interaction region
		//
		if(road_ptr->get()->get_arm() == MUIOO::South) {
			fit_par.set_z_begin(z_max) ;
			fit_par.set_z_end(z_min);
		} 
		else {
			fit_par.set_z_begin(z_min) ;
			fit_par.set_z_end(z_max);
		}
		road_ptr->get()->set_fit_par(fit_par);
	}

	return;
}

//_____________________________________
void
mMuiFindRoad::set_road_parameters()
{
	// The parameters that have been set so far is basically 
	// just which clusters belong to it and what the fit parameters
	// are.	freedom and road_quality should be set in fit_roads() 
	// Not set variables are
	// nhit - number of clusters in both orientations
	// depth - last plane hit
	// gapbit - bit coding of which gap/plane and orientation had a hit 
	// max_hit_plane - maximum hits associated per plane?
	// ghost_flag 
	// group
	// golden
	//
	// The last three of these are not too relevant for this roadfinder
	// and we'll leave them as	their init values.
	// The ones with ?, I'm not exactly sure of the meaning of 
	// (previously not set in standard road finder: mMuiRoadFinder1)

	// Get an iterator to all roads
	//
	TMuiRoadMapO::iterator road_iter = _road_map->range();
	while(TMuiRoadMapO::pointer road_ptr = road_iter.next()) {					

		// Iterator for all clusters associated to this road
		TMuiClusterMapO::const_key_iterator clust_iter = 
			road_ptr->get()->get_associated<TMuiClusterO>();

		road_ptr->get()->set_nhit(clust_iter.count());


		UShort_t depth = 0;
		UShort_t gapbit = 0;
		UShort_t hits_plane[MUIOO::MAX_PLANE] = {0};

		while(TMuiClusterMapO::const_pointer clust_ptr = clust_iter.next()) {

			if (depth < clust_ptr->get()->get_plane()) {
	depth = clust_ptr->get()->get_plane();
			}
			gapbit=gapbit | (clust_ptr->get()->get_plane() << 
					 (MUIOO::MAX_PLANE*clust_ptr->get()->get_orientation()));
			hits_plane[clust_ptr->get()->get_plane()]++;			
		}
		road_ptr->get()->set_depth(depth);
		road_ptr->get()->set_gapbit(gapbit);
		UShort_t max_hit_plane = 0;
		for (int plane = 0; plane<MUIOO::MAX_PLANE; plane++) {
			if (max_hit_plane < hits_plane[plane]) {
	max_hit_plane = hits_plane[plane];
			}
		}
		road_ptr->get()->set_max_hit_plane(max_hit_plane);
	}
	return;
}

//_____________________________________
void
mMuiFindRoad::apply_road_cuts()
{	
	// Here we punt on roads that don't have a minimum number of associated
	// TMuiCluster.
	//
	TMuiRoadMapO::iterator road_iter = _road_map->range();
	while(TMuiRoadMapO::pointer road_ptr = road_iter.next()){
		// Minimum hits/cluster cut
		//
		if(road_ptr->get()->get_nhit() < _mod_par->get_min_cluster()) { 
			_road_map->erase(road_ptr->get()->get_key());
		} 
	}
}

//_____________________________________
void
mMuiFindRoad::eliminate_duplicates()
{
	typedef std::vector<TMuiRoadMapO::value_type> cluster_list;
	cluster_list remove_list;
	TMuiRoadMapO::iterator road_iter = _road_map->range();
	while(TMuiRoadMapO::pointer road_ptr = road_iter.next()){
		TMuiRoadMapO::iterator road_iter2 = road_iter;
		++road_iter2;
		while(TMuiRoadMapO::pointer road_ptr2 = road_iter2.next()) {
			if(cluster_list_equal(road_ptr, road_ptr2)) {
	remove_list.push_back(*road_ptr2);
			}
		}
	}
	
	cluster_list::iterator remove_iter = remove_list.begin();
	for(;remove_iter != remove_list.end(); ++remove_iter){
		_road_map->erase(remove_iter->get()->get_key());
	} 
}

//_____________________________________
bool
mMuiFindRoad::cluster_list_equal(const TMuiRoadMapO::pointer road1,
				 const TMuiRoadMapO::pointer road2) 
{
	TMuiClusterMapO::key_iterator road1_iter = road1->get()->get_associated<TMuiClusterO>();	
	TMuiClusterMapO::key_iterator road2_iter = road1->get()->get_associated<TMuiClusterO>();	
	if(road1_iter.count() != road2_iter.count()) return false;
	while(TMuiClusterMapO::pointer road1_ptr = road1_iter.next()){
		bool match = false;
		road2_iter = road2->get()->get_associated<TMuiClusterO>();
		while(TMuiClusterMapO::pointer road2_ptr = road2_iter.next()){
			if(road1_ptr->get()->get_key() == road2_ptr->get()->get_key()) {
	match = true;
			}
		}
		if(!match) return false;
	}
	return true;
}






