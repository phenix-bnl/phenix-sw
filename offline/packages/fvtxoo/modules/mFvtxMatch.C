// $Id: mFvtxMatch.C,v 1.6 2015/07/01 18:12:23 jinhuang Exp $

/*!
 \file mFvtxMatch.cxx
 \brief associate MC_Hit and MC_Trk information with tracks
 \author D. Winter
 \version $Revision: 1.6 $
 \date $Date: 2015/07/01 18:12:23 $
*/
///////////////////////////////////////////////////////////////////

// MUTOO headers
#include<mFvtxMatch.h>
//#include<mFvtxEvalPar.h>
//#include<TMutNode.h>
#include<TFvtxMCHitMap.h>
#include<TMutMCTrkMap.h>
#include<TFvtxHitMap.h>
#include<TFvtxClusMap.h>
#include<TFvtxCoordMap.h>
//#include<TFvtxTrkEval.h>
#include<TFvtxTrkMap.h>
#include<FVTXOO.h>
#include<PHCylPoint.h>

// STL/BOOST
#include<string>
#include<iostream>
#include <numeric>
#include <boost/bind.hpp>

mFvtxMatch::mFvtxMatch() :
  _dR(0.04),
  _dM(0.001),
  _eval_z(18.0),
  _timer(PHTimeServer::get()->insert_new("mFvtxMatch") )
{
  FVTXOO::TRACE("initializing module mFvtxMatch");
}

//_____________________________________________________________
PHBoolean
mFvtxMatch::event(PHCompositeNode* signal_node, PHCompositeNode* top_node)
{

  _timer.get()->restart();
  try {

    // Reset IOC pointers
    set_interface_ptrs(signal_node, top_node);

    // associate TFvtxMCHits with TFvtxCoord
    associate_mchit();

    // associate TFvtxMCTrk with TFvtxTrk
    associate_trk_to_mctrk();

  } catch(const std::exception& e) {
    FVTXOO::TRACE(e.what());
  }

  // If verbose dump the contents of the eval map
  //
  _timer.get()->stop();

  return True;
}

void
mFvtxMatch::set_interface_ptrs(PHCompositeNode* signal_node, PHCompositeNode* top_node)
{
  // module runtime parameters
  //_mod_par = TMutNode<mFvtxMatchPar>::find_node(top_node,"mFvtxMatchPar");

  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
  _mctrk_map = TMutNode<TMutMCTrkMap>::find_node(signal_node,"TMutMCTrkMap");
}

void
mFvtxMatch::associate_mchit()
{
  // TFvtxMCHits are associated with TFvtxHit objects after running
  // the mFvtxResponse module. It is convienient here to associate
  // TFvtxMCHit's with TFvtxCoord since these are associated with
  // TFvtxTrk objects and are used in the reconstruction.  So here we
  // loop over TFvtxCoord objects -- trace through their association
  // tree until we get to the underlying TFvtxMCHit and make the
  // association between TFvtxMCHit and TFvtxCoord explicit.
  //
  TFvtxCoordMap::iterator coord_iter = _coord_map->range();
  while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next())
    {
      // TFvtxCoord -> TFvtxClus
      TFvtxClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TFvtxClus>();
      while(TFvtxClusMap::pointer clus_ptr = clus_iter.next())
	{
	  // TFvtxClus -> TFvtxHit
	  TFvtxHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TFvtxHit>();
	  while(TFvtxHitMap::pointer hit_ptr = hit_iter.next())
	    {
	      // TFvtxHit->TFvtxMCHit
	      TFvtxMCHitMap::key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TFvtxMCHit>();
	      while(TFvtxMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next())
		PHKey::associate_unique(mc_hit_ptr,coord_ptr);
	    }
	}
    }
}


// Normalizes angle to (pi,-pi]
static double
angle_normalize(const double x)
{
  double ret;

  if ( x > -3 * M_PI && x <= 3 * M_PI )
    {
      ret = x;
      if ( ret > M_PI )        ret -= 2 * M_PI;
      else if ( ret <= -M_PI ) ret += 2 * M_PI;
    }
  else
    {
      const double x_pi_mod = fmod(x + M_PI, 2 * M_PI);
      ret = x_pi_mod > 0 ? x_pi_mod - M_PI : x_pi_mod + M_PI;
    }

  return ret;
}

struct PhiOverlaps {
  bool operator()(double x1, double x2, double y1, double y2)
  {
    double dX1Y1 = angle_normalize(x1-y1);
    double dX1Y2 = angle_normalize(x1-y2);
    double dX2Y1 = angle_normalize(x2-y1);
    double dX2Y2 = angle_normalize(x2-y2);

    // If x1 (start of range) is between y1 & y2, return true
    if ( dX1Y1 >= 0 && dX1Y2 <= 0 ) return true;

    // If x2 (end of range) is between y1 & y2, return true
    else if ( dX2Y1 >= 0 && dX2Y2 <= 0 ) return true;

    // otherwise x1-x2 does not overlap y1-y2
    return false;
  }
};

// Analytically perform a linear fit, using constant weights
//
template<typename T> std::pair<T,T>
linearFit(const std::vector<T>& x, const std::vector<T>& y)
{
  int n = x.size();

  std::vector<T> x2(x.size());
  std::vector<T> xy(x.size());
  std::transform(x.begin(),x.end(),x.begin(),x2.begin(),std::multiplies<T>());
  std::transform(x.begin(),x.end(),y.begin(),xy.begin(),std::multiplies<T>());

  T sumX  = std::accumulate(x.begin(),x.end(),0.0);
  T sumXY = std::accumulate(xy.begin(),xy.end(),0.0);
  T sumY  = std::accumulate(y.begin(),y.end(),0.0);
  T sumX2 = std::accumulate(x2.begin(),x2.end(),0.0);
  T delta = n*sumX2 - sumX*sumX;

  T slope  = 1.0/delta * (n*sumXY - sumX*sumY);
  T offset = 1.0/delta * (sumX2*sumY - sumX*sumXY);

  return std::pair<T,T>(slope,offset);
}

void
mFvtxMatch::associate_trk_to_mctrk()
{
  TMutMCTrkMap::iterator mctrk_iter = _mctrk_map->range();

  while( TMutMCTrkMap::pointer mctrk_ptr = mctrk_iter.next())
    {
      // Assemble a list of the MC hits for this MC track
      //
      std::vector<TFvtxMCHit*> hits;

      TFvtxMCHitMap::key_iterator mchit_iter = mctrk_ptr->get()->get_associated<TFvtxMCHit>();

      // Need at least 2 mc hits for this work
      if ( mchit_iter.count() < 3 ) continue;

      while ( TFvtxMCHitMap::pointer mchit_ptr = mchit_iter.next() )
	{
	  if ( mchit_ptr->get()->get_track_id() == mctrk_ptr->get()->get_track_id() )
	    hits.push_back(mchit_ptr->get());
	}

      // Check the number again, because some of the associated mc hits could
      // have come from the parent or child
      if ( hits.size() < 2 ) continue;

      // Sort the assembled list, but take care to account for which
      // arm (ie. south arm should be sorted in descending Z)
      //
      if ( mctrk_ptr->get()->get_arm() == FVTXOO::North )
	std::sort(hits.begin(),hits.end(),
		  boost::bind(&TFvtxMCHit::get_z,_1) < boost::bind(&TFvtxMCHit::get_z,_2) );
      else
	std::sort(hits.begin(),hits.end(),
		  boost::bind(std::negate<double>(),boost::bind(&TFvtxMCHit::get_z,_1)) <
		  boost::bind(std::negate<double>(),boost::bind(&TFvtxMCHit::get_z,_2))
		  );

      // Get the phi start, end of the first hit in Z
      //

      std::vector<TFvtxCoord*> coords;
      mchit_iter.reset();
      while ( TFvtxMCHitMap::pointer mchit_ptr = mchit_iter.next() )
	{
	  if ( mchit_ptr->get()->get_track_id() != mctrk_ptr->get()->get_track_id() ) continue;

	  TFvtxCoordMap::key_iterator coord_iter = mchit_ptr->get()->get_associated<TFvtxCoord>();
	  while ( TFvtxCoordMap::pointer coord_ptr = coord_iter.next() )
	    {
		coords.push_back(coord_ptr->get());
	    }
	}

      if ( coords.size() < 2 ) continue;

      if ( mctrk_ptr->get()->get_arm() == FVTXOO::North )
	std::sort(coords.begin(),coords.end(),
		  boost::bind(&TFvtxCoord::get_mean_z,_1) < boost::bind(&TFvtxCoord::get_mean_z,_2));
      else
	std::sort(coords.begin(),coords.end(),
		  boost::bind(std::negate<double>(),boost::bind(&TFvtxCoord::get_mean_z,_1)) <
		  boost::bind(std::negate<double>(),boost::bind(&TFvtxCoord::get_mean_z,_2)));

      PHCylPoint pnt0(coords.front()->get_coord_begin());
      PHCylPoint pnt1(coords.front()->get_coord_end());

      double phi_start = pnt0.getPhi();
      double phi_end = pnt1.getPhi();

      // Calculate the slope of the mc track from the begin, end points
      //
      PHCylPoint mid0(coords.front()->get_coord_midpoint());
      PHCylPoint mid1(coords.back()->get_coord_midpoint());

      double r0 = mid0.getR();
      double z0 = mid0.getZ();
      double r1 = mid1.getR();
      double z1 = mid1.getZ();
      double mc_slope = (r1-r0)/(z1-z0);
      double mc_offset = r0 - mc_slope * z0;

      double z = _eval_z * (mctrk_ptr->get()->get_arm() == FVTXOO::North? 1 : -1);
      double mc_r = mc_slope * z + mc_offset;

      // Now examine the reconstructed tracks, selecting those in a
      // dPhi,dR,dm window to associate with the current mc track
      //
      //PhiOverlaps phiPred;
      FVTXOO::AngleOverlap<double> phiPred;
      TFvtxTrkMap::iterator trk_iter = _trk_map->range();
      while ( TFvtxTrkMap::pointer trk_ptr = trk_iter.next() )
        {

          //ghost and bad track rejections
          if (trk_ptr->get()->get_ghost()) continue;
          if (not trk_ptr->get()->get_reco_success()) continue;


	  if ( trk_ptr->get()->get_arm() != mctrk_ptr->get()->get_arm() ) continue;

	  // Assemble a list of the coords for this reco track
	  //
	  std::vector<TFvtxCoord*> coords;
	  std::vector<double> rv;
	  std::vector<double> zv;
	  TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
	  while ( TFvtxCoordMap::pointer coord_ptr = coord_iter.next() )
	    {
	      coords.push_back(coord_ptr->get());
	      PHCylPoint mid(coord_ptr->get()->get_coord_midpoint());
	      rv.push_back(mid.getR());
	      zv.push_back(mid.getZ());
	    }

	  // This might happen if, eg tracks are started with a single coord, and never add any
	  // new ones (possibly in the LANL track finder?).
	  //
	  if ( coords.size() < 2 ) {
	    //std::cout << "mFvtxMatch::associate_trk_to_mctrk: WARNING: bad number of coords = "
	    //<< coords.size() << " -- continuing" << std::endl;;
	    continue;
	  }

	  // Sort the list
	  //
	  if ( trk_ptr->get()->get_arm() == FVTXOO::North )
	    std::sort(coords.begin(),coords.end(),
		      boost::bind(&TFvtxCoord::get_mean_z,_1) < boost::bind(&TFvtxCoord::get_mean_z,_2)
		      );
	  else
	    std::sort(coords.begin(),coords.end(),
		      boost::bind(std::negate<double>(),boost::bind(&TFvtxCoord::get_mean_z,_1)) <
		      boost::bind(std::negate<double>(),boost::bind(&TFvtxCoord::get_mean_z,_2))
		      );

	  PHCylPoint pnt0(coords.front()->get_coord_begin());
	  PHCylPoint pnt1(coords.front()->get_coord_end());

	  double recoPhiStart = pnt0.getPhi();
	  double recoPhiEnd = pnt1.getPhi();

	  // TODO: extract the slope & offset info from the track, not calculate it on the fly.
	  double reco_offset = 0.0;
	  double reco_slope = 0.0;
	  boost::tie(reco_slope,reco_offset) = linearFit(zv,rv);

	  double reco_r = reco_slope * z + reco_offset;
	  double deltaR = mc_r - reco_r;
	  double deltaM = mc_slope - reco_slope;

	  // TODO: apply correction to undo correlation btn dR and dM
	  //
	  if ( phiPred(recoPhiStart,recoPhiEnd,phi_start,phi_end) &&
	       std::abs(deltaR) < _dR &&
	       std::abs(deltaM) < _dM )
	    {
	      PHKey::associate(trk_ptr,mctrk_ptr);
	    }
	}

    }

  return;
}
