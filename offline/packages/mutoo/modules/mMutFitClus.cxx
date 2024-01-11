
// $Id: mMutFitClus.cxx,v 1.53 2011/12/24 04:48:30 slash Exp $

/*!
  \file mMutFitClus.cxx
  \brief fit clusters, create coordinates
  \author S. Kelly M. Brooks
  \version $Revision: 1.53 $
  \date $Date: 2011/12/24 04:48:30 $
*/

// MUTOO headers
#include <MUTOO.h>
#include <PHException.h>
#include <TMutNode.h>
#include <TMutGeo.h>
#include <TMutClusMap.h>
#include <TMutHitMap.h>
#include <TMutClusterFit.h>
#include <TMutClusterFitEval.h>
#include <TMutCoordFill.h>

// PHENIX headers
#include<PHLine.h>
#include<PHVector.h>
#include<PHPoint.h>
#include<PHGeometry.h>

/*! \ingroup modules */

// STL/BOOST
#include <cmath>
#include <iostream>
#include <string>
#include <climits>
#include <boost/iterator_adaptors.hpp>

#include "mMutFitClus.h"
#include "mMutFitClusPar.h"

using namespace std;

//________________________________________________________________
mMutFitClus::mMutFitClus() :
  _mod_par(0),
  _clus_map(0),
  _coord_map(0),
  _use_section( false ),
  _arm(0),
  _station(0),
  _octant(0),
  _timer( PHTimeServer::get()->insert_new( "mMutFitClus"))
{
  MUTOO::TRACE("initializing module mMutFitClus",MUTOO::ALOT);
}

//________________________________________________________________
PHBoolean mMutFitClus::event(
  PHCompositeNode* top_node,
  const unsigned short& arm,
  const unsigned short& station,
  const unsigned short& octant )
{

  // store parameters into members
  _use_section = true;
  _arm = arm;
  _station = station;
  _octant = octant;

  // call standard event method
  PHBoolean out = event( top_node );

  // reset use_section
  _use_section = false;

  return out;

}

//________________________________________________________________
PHBoolean mMutFitClus::event(PHCompositeNode* top_node)
{
  _timer.get()->restart();

  try {

    /*
      In _use_section mode, set_interface_pointer is not called.
      It must be called externally
    */
    if( !_use_section )  set_interface_ptrs(top_node);


    // reinitialize evaluation
    if( TMutClusterFitEval::get_do_evaluation() )
    {  TMutClusterFitEval::reset(); }

    // Fit clusters
    fit_clusters();

    // Derive TMutCoord objects from clusters and
    // append to TMutCoordMap.
    fill_coord_map();

    // reinitialize evaluation
    if( TMutClusterFitEval::get_do_evaluation() )
      { TMutClusterFitEval::fill_evaluation_ntuple( _clus_map ); }

    // some prints
    if(_mod_par->get_verbosity() >= MUTOO::ALOT){
      _clus_map->print();
      _coord_map->print();
    }

  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  _timer.get()->stop();
  return true;
}

//________________________________________________________________
void mMutFitClus::set_interface_ptrs(PHCompositeNode* top_node)
{

  // module runtime parameters
  _mod_par = TMutNode<mMutFitClusPar>::find_node(top_node,"mMutFitClusPar");

  // put module parameters to TMutClusterFit singleton
  TMutClusterFit::set_verbosity( _mod_par->get_verbosity() );
  TMutClusterFit::set_peak_ratio_scale( _mod_par->get_peak_ratio_scale() );
  TMutClusterFit::set_chi_max_good_fit( _mod_par->get_chi_max_good_fit() );
  TMutClusterFit::set_mc_smear_perp( _mod_par->get_mc_smear_perp() );
  TMutClusterFit::set_mc_smear_ster( _mod_par->get_mc_smear_ster() );
  TMutClusterFit::set_max_fit( _mod_par->get_max_fit() );
  TMutClusterFit::set_single_track_fit_type( _mod_par->get_single_track_fit_type() );
  TMutClusterFit::set_multi_track_fit_type( _mod_par->get_multi_track_fit_type() );
  TMutClusterFit::set_multi_track_fit( _mod_par->get_multi_track_fit() );

  // TMutClus IOC
  _clus_map = TMutNode<TMutClusMap>::find_node(top_node,"TMutClusMap");

  // TMutCoord IOC
  _coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");
  _coord_map->clear();

}


//________________________________________________________________
void mMutFitClus::fit_clusters()
{

  // retrieve clusters of interest
	TMutClusMap::iterator clus_iter = (_use_section) ?
    _clus_map->get( _arm, _station, _octant ) :
    _clus_map->range();

  // Loop over clusters
	while(TMutClusMap::pointer clus_ptr = clus_iter.next())
	TMutClusterFit::fit( clus_ptr );

}

//________________________________________________________________
void mMutFitClus::fill_coord_map()
{

  // retrieve clusters of interest
	TMutClusMap::iterator clus_iter = (_use_section) ?
    _clus_map->get( _arm, _station, _octant ) :
    _clus_map->range();

  // Loop over clusters
	while(TMutClusMap::pointer clus_ptr = clus_iter.next())
	TMutCoordFill::create_coords(clus_ptr, _coord_map);
}
