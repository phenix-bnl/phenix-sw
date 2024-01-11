// $Id: mFvtxFindTrack.cxx,v 1.17 2014/12/01 16:57:57 slash Exp $

/*!
  \file mFvtxFindTrack.cxx
  \brief Associate TFvtxCoord with TFvtxTrk using monte-carlo information (perfect pattern recognition)
  \author Melynda Brooks
  \version $Revision: 1.17 $
  \date $Date: 2014/12/01 16:57:57 $
*/

#include <TMutNode.h>

#include <TFvtxClusMap.h>
#include <TFvtxCoordMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxMCHitMap.h>
#include <TFvtxPisaHitMap.h>
#include <TFvtxTrkMap.h>
#include <TMutMCTrkMap.h>
#include <TMutExtVtx.h>

#include <PHException.h>
#include <FVTXOO.h>
#include <FvtxGeom.h>
#include <TMutTrackUtil.h>
#include <PHGeometry.h>

// STL/BOOST/GSL
#include <gsl/gsl_fit.h>
#include <gsl/gsl_randist.h>
#include <cmath>
#include <iostream>
#include <string>
#include <boost/array.hpp>

#include "mFvtxFindTrack.h"
#include "mFvtxFindTrackPar.h"

using namespace std;

/*! \ingroup modules */
//______________________________________________________________
mFvtxFindTrack::mFvtxFindTrack():
  _timer( PHTimeServer::get()->insert_new("mFvtxFindTrack") )
{ 

  FVTXOO::TRACE("initializing module mFvtxFindTrack");

  _n_tracks.assign(0);

}

//______________________________________________________________
//! Event method
PHBoolean mFvtxFindTrack::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();

  try {
    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // clear maps
    _trk_map->clear();

    // Promote TFvtxMCHit association from TFvtxHit to TFvtxCoord
//    promote_associations();

    // Load the event vertex position:
    load_ext_vertex( top_node );

    // reset number of tracks
    _n_tracks.assign( 0 );

    // make TFvtxTrk objects from TMutMCCTrk objects and associate
    // TFvtxCoord, TFvtxMCHits using TMutMCTrk/TFvtxMCHit information
    find_tracks();

    set_trk_par();

  } catch(const std::exception& e) {
    FVTXOO::TRACE(e.what());
    return False;
  }

  // If verbose dump the contents of the track map

  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= FVTXOO::ALOT) _trk_map->print();
  if(_mod_par->get_verbosity() >= FVTXOO::SOME) _timer.get()->print();

  return True;
}

void
mFvtxFindTrack::end(PHCompositeNode* topNode)
{
  _timer.get()->print_stat();
}

//______________________________________________________________
//! Reset IOC and external interface pointers
void mFvtxFindTrack::set_interface_ptrs(PHCompositeNode* top_node)
{

  // module runtime parameters
  _mod_par = TMutNode<mFvtxFindTrackPar>::find_node(top_node,"mFvtxFindTrackPar");
  _pisa_hit_map = TMutNode<TFvtxPisaHitMap>::find_node(top_node,"TFvtxPisaHitMap");
  _mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
  _mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node(top_node,"TFvtxMCHitMap");
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
  _clus_map = TMutNode<TFvtxClusMap>::find_node(top_node,"TFvtxClusMap");
  _hit_map = TMutNode<TFvtxHitMap>::find_node(top_node,"TFvtxHitMap");

}

//______________________________________________________________
void mFvtxFindTrack::promote_associations()
{
  // TFvtxMCHits are associated with TFvtxHit objects after running the mFvtxResponse
  // module.  It is convienient here to associate TFvtxMCHit's with TFvtxCoord since
  // these are associated with TFvtxTrk objects and are used in the reconstruction.
  // So here we loop over TFvtxCoord objects -- trace through their association tree
  // until we get to the underlying TFvtxMCHit and make the association between
  // TFvtxMCHit and TFvtxCoord explicit.
  TFvtxCoordMap::iterator coord_iter = _coord_map->range();
  while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next()){

    // TFvtxCoord -> TFvtxClus
    TFvtxClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TFvtxClus>();
    while(TFvtxClusMap::pointer clus_ptr = clus_iter.next()){

      // TFvtxClus -> TFvtxHit
      TFvtxHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TFvtxHit>();
      while(TFvtxHitMap::pointer hit_ptr = hit_iter.next()){

        // TFvtxHit->TFvtxMCHit
        TFvtxMCHitMap::key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TFvtxMCHit>();
        while(TFvtxMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next())
        PHKey::associate_unique(mc_hit_ptr,coord_ptr);

      }
    }
  }
}

//______________________________________________________________
void mFvtxFindTrack::find_tracks()
{

  // Start tracks either using MuTr tracks as seeds, or using Plane 4 coordinates as seeds:
  if (_mod_par->get_mode() == mFvtxFindTrackPar::USE_MUTR) start_tracks_mutr();
  else start_tracks_no_mutr();

  find_coords_in_station(FVTXOO::Station3);
  find_coords_in_station(FVTXOO::Station2);
  find_coords_in_station(FVTXOO::Station1);


  // Do we need to set any track parameters now or just after fitting?
  //  trk_iter->get()->set_trk_par(local_trk);
}

//____________________________________________
// Each hit in Station 4 starts a new track:

void mFvtxFindTrack::start_tracks_no_mutr(){

  for (int iarm = 0; iarm < FVTXOO::MAX_ARM ; iarm++){
  for (int icage = 0; icage < FVTXOO::MAX_CAGE ; icage++){
    for (int isector = 0; isector < FVTXOO::MAX_SECTOR ; isector++){

      TFvtxCoordMap::iterator coord_iter = _coord_map->get(iarm, icage, FVTXOO::Station4, isector);

      while (TFvtxCoordMap::pointer coord_ptr = coord_iter.next()){
 
        // Insert a new track into the track map (if this coordinate is not already used in
        // a track) and associate this coordinate with it.

        if (coord_ptr->get()->get_usedintrack() ) continue;

        TFvtxTrkMap::iterator trk_iter = _trk_map->insert_new( iarm );
        PHKey::associate( trk_iter.current(), coord_ptr );

        if (_mod_par->get_verbosity() > FVTXOO::NONE){
          cout << "Start track " << trk_iter->get()->get_key().get_obj_key() << " in station 4" << endl;
          coord_ptr->get()->print();
        }

        // Tag this coordinate as being used in a track:
        coord_ptr->get()->set_usedintrack();

        // Set a theta and phi search window for the track finding:  get the mid-point
        // of the silicon strip and set window width from mFvtxFindTrackPar

        PHPoint CoordMidPoint = coord_ptr->get()->get_coord_midpoint();

        set_trk_windows(&(*trk_iter), CoordMidPoint);

        // Check to see if there is another coordinate in an overlapping sensor which should be
        // added to this track (and not used to start a second track):
          std::pair<double, double> phi_window = make_pair(
            trk_iter->get()->get_phi_min(FVTXOO::Station4),
            trk_iter->get()->get_phi_max(FVTXOO::Station4));
          std::pair<double, double> theta_window = make_pair(
            trk_iter->get()->get_theta_min(FVTXOO::Station4),
            trk_iter->get()->get_theta_max(FVTXOO::Station4));

          local_coord_list coord_list =
             find_coords_in_window(_coord_map, trk_iter->get()->get_arm(), coord_ptr->get()->get_cage(),
                 FVTXOO::Station4, (int)(coord_ptr->get()->get_sector()), theta_window, phi_window);

          for (local_coord_list::iterator coord_list_iter = coord_list.begin(); 
                       coord_list_iter != coord_list.end(); coord_list_iter++){

/*          if (!trk_iter->get()->has_coord(coord_list_iter->get()->get_station(), 
                                      coord_list_iter->get()->get_sector(),
                                      coord_list_iter->get()->get_plane(),
                                      coord_list_iter->get()->get_radius() )) { */
           if (check_detector_overlap(&(*coord_list_iter), &(*trk_iter)) ){ 

              PHKey::associate(&(*coord_list_iter), &(*trk_iter));

              if (_mod_par->get_verbosity() > FVTXOO::NONE){
                cout << "Track " << trk_iter->get()->get_key().get_obj_key() << endl;
                cout << "Found coord in station 4" << endl;
                coord_list_iter->get()->print();
              }

              coord_list_iter->get()->set_usedintrack();
            }
          }  // Loop over candidate coordinates found

      }  // Loop over coordinates found in Station 4 for this arm, sector

    }  // Loop over sectors in the station
  }  // Loop over cages
  }  // Loop over arms

}

//____________________________________________
void mFvtxFindTrack::start_tracks_mutr(){
}

//____________________________________________
void mFvtxFindTrack::find_coords_in_station(unsigned short station){


  // Create local track list so that if a track is cloned because more than one candidate coordinate
  // is found, we do not go into an infinite loop.

  typedef list<TFvtxTrkMap::value_type> trk_ptr_list;
  trk_ptr_list local_trk_list;

  TFvtxTrkMap::iterator trk_iter = _trk_map->range();
  while(TFvtxTrkMap::pointer trk_ptr = trk_iter.next())
    { local_trk_list.push_back(*trk_ptr); }
 
  for( trk_ptr_list::iterator trk_list_iter = local_trk_list.begin(); trk_list_iter!=local_trk_list.end();++trk_list_iter)
  {
    // Search each of the remaining planes for coordinates to go with this track:

    if (_mod_par->get_verbosity() > FVTXOO::NONE){
      cout << "Search for coordinate for track =" <<  trk_list_iter->get()->get_key().get_obj_key() << endl;
    }

      std::pair<double, double> phi_window = make_pair(
        trk_list_iter->get()->get_phi_min(station),
        trk_list_iter->get()->get_phi_max(station));
      std::pair<double, double> theta_window = make_pair(
        trk_list_iter->get()->get_theta_min(station),
        trk_list_iter->get()->get_theta_max(station));

      if (_mod_par->get_verbosity() > FVTXOO::NONE){
        cout << "theta window = " << trk_list_iter->get()->get_theta_min(station) << ", " <<
                                     trk_list_iter->get()->get_theta_max(station) << endl;
        cout << "phi window = " << trk_list_iter->get()->get_phi_min(station) << ", " <<
                                   trk_list_iter->get()->get_phi_max(station) << endl;
      }

      // Get sector of last hit so we can reduce sectors searched for coordinates;
      int isector = 0;
      int icage = 0;
      TFvtxCoordMap::key_iterator coord_iter = trk_list_iter->get()->get_associated<TFvtxCoord>();
      if (coord_iter.count() != 0) {
        isector = (int)(coord_iter.current()->get()->get_sector());
        icage = (int)(coord_iter.current()->get()->get_cage());
      }

      local_coord_list coord_list = 
         find_coords_in_window(_coord_map, trk_list_iter->get()->get_arm(), icage, station, 
            isector, theta_window, phi_window);

      // Loop over found coords and add them to current track or make a new track if there is already
      // a coordinate from this station, sector on this track
      for (local_coord_list::iterator coord_list_iter = coord_list.begin(); 
                                      coord_list_iter != coord_list.end(); coord_list_iter++){

        if (_mod_par->get_verbosity() > FVTXOO::NONE){
          cout << "Found coord in station " << station <<", " << "Track= " << trk_list_iter->get()->get_key().get_obj_key() << endl;
          coord_list_iter->get()->print();
        }

        if (!trk_list_iter->get()->has_coord(coord_list_iter->get()->get_station()) ||
             check_detector_overlap(&(*coord_list_iter), &(*trk_list_iter)) ){ 

          PHKey::associate(&(*coord_list_iter), &(*trk_list_iter));

          if (_mod_par->get_verbosity() > FVTXOO::NONE){
            cout << "associate coord to track" << endl;
          }

        }
        else {

          if (_mod_par->get_verbosity() > FVTXOO::NONE){
            cout << "Clone track " << trk_list_iter->get()->get_key().get_obj_key() << " in station " << station << endl;
          }

          clone_trk(&(*trk_list_iter), &(*coord_list_iter) );

        }
      }  // Loop over candidate coordinates

  }  // Loop over all candidate tracks

}

//____________________________________________
list<TFvtxCoordMap::value_type>
  mFvtxFindTrack::find_coords_in_window(TFvtxCoordMap* coord_map, unsigned short arm, int cage_in, int station_in, 
    int sector_in, const  pair<float,float>& theta_window, const pair<float,float>& phi_window){

  // Local storage for coordinate list
  local_coord_list local_list;

  unsigned short cage = (unsigned short) cage_in;
  unsigned short station = (unsigned short) station_in;

  // Get candidate coordinates for each sector in this station, plane:
  for (int icage=0; icage<FVTXOO::MAX_CAGE; icage++){
  for (int isector=0; isector<FVTXOO::MAX_SECTOR; isector++){
    if (sector_in > 0 && isector < sector_in - 1) continue;
    if (sector_in < FVTXOO::MAX_SECTOR && isector > sector_in + 1) continue;
    if (sector_in == 0 && !(isector==FVTXOO::MAX_SECTOR || isector==0 || isector==1)) continue;
    if (sector_in == FVTXOO::MAX_SECTOR && !(isector==FVTXOO::MAX_SECTOR || 
                                isector==0 || isector==FVTXOO::MAX_SECTOR-1)) continue;

    unsigned short sector = (unsigned short)isector;
    TFvtxCoordMap::iterator coord_iter = coord_map->get(arm, cage, station, sector);

    while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next()){

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
  }
  }  // Loop over sectors in this station,cage

  // Not so efficient but safe
  return local_list;
}

//______________________________________________________________
mFvtxFindTrack::WindowEnum mFvtxFindTrack::check_phi_window(const PHPoint& point, const mFvtxFindTrack::coord_window& phi_window)
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
  // add protection against dphi~180 degrees getting accepted (always return HIGH_WINDOW if dphi is large)
  if(dphi_left < dphi_window && dphi_right < dphi_window) return IN_WINDOW;
  else if(dphi_left < dphi_window && dphi_right >= dphi_window) return LOW_WINDOW;
  else if(dphi_right < dphi_window && dphi_left >= dphi_window) return HIGH_WINDOW;
  else if (fabs(dphi_left) > M_PI/2 && fabs(dphi_right) > M_PI/2) return HIGH_WINDOW;
  else return (dphi_left > dphi_right) ? LOW_WINDOW : HIGH_WINDOW;

}

//______________________________________________________________
mFvtxFindTrack::WindowEnum mFvtxFindTrack::check_theta_window(const PHPoint& point, const mFvtxFindTrack::coord_window& theta_window)
{ 
  
  double xp = point.getX();
  double yp = point.getY();
//  double zp = fabs(point.getZ());
  double zp = point.getZ();
  double rp = sqrt(MUTOO::SQUARE(xp) + MUTOO::SQUARE(yp));
  double theta_p = atan(rp/zp);
  
  if(theta_p > theta_window.first && theta_p < theta_window.second) return IN_WINDOW;
  else if(theta_p <= theta_window.first) return LOW_WINDOW;
  else return HIGH_WINDOW;

}


//____________________________________________
void
mFvtxFindTrack::clone_trk(TFvtxTrkMap::pointer in_trk_ptr, TFvtxCoordMap::pointer in_coord_ptr)
{

  if ( _n_tracks[ in_trk_ptr->get()->get_arm() ] > _mod_par->get_max_n_tracks() ) {
/*    TFvtxErrorStats::set_error(TFvtxErrorStats::CLONE_TRACK,
            in_trk_ptr->get()->get_arm(),
            in_stub_ptr->get()->get_station(),
            in_trk_ptr->get()->get_octant()); */

    ostringstream what;
    what << "too many tracks in clone_trk ("
   << in_trk_ptr->get()->get_arm() << ")";

    throw runtime_error(DESCRIPTION( what.str() ));

  } else _n_tracks[ in_trk_ptr->get()->get_arm() ]++;

  // New track same octant
  TFvtxTrkMap::iterator trk_iter = _trk_map->insert_new(in_trk_ptr->get()->get_arm());

  if (_mod_par->get_verbosity() > FVTXOO::NONE){
    cout << "Clone track # = " << trk_iter->get()->get_key().get_obj_key() << endl;
  }

  // Input track coords are associated (excluding current station, sector)
  TFvtxCoordMap::key_iterator coord_iter = in_trk_ptr->get()->get_associated<TFvtxCoord>();
  while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next())
    if(!(coord_ptr->get()->get_station() == in_coord_ptr->get()->get_station() &&
         coord_ptr->get()->get_cage() == in_coord_ptr->get()->get_cage() && 
         coord_ptr->get()->get_sector() == in_coord_ptr->get()->get_sector()) )
      PHKey::associate(trk_iter.current(),coord_ptr);

  // Associate the input coord iterator
  PHKey::associate(trk_iter.current(),in_coord_ptr);

  // Pull the windows from the old track
  for(int sta=0; sta<FVTXOO::MAX_STATION; ++sta){
      trk_iter->get()->set_phi_min(sta,in_trk_ptr->get()->get_phi_min(sta));
      trk_iter->get()->set_phi_max(sta,in_trk_ptr->get()->get_phi_max(sta));
      trk_iter->get()->set_theta_min(sta,in_trk_ptr->get()->get_theta_min(sta));
      trk_iter->get()->set_theta_max(sta,in_trk_ptr->get()->get_theta_max(sta));
  }


}

//______________________________________________________________
// Calculate and store the initial track parameters for each track
void mFvtxFindTrack::set_trk_par()
{

  TFvtxTrkMap::iterator trk_iter = _trk_map->range();
  while(TFvtxTrkMap::pointer trk_ptr = trk_iter.next()){

    TMutStraightTrackFit trk_fit;
 
    // The z position here should be set to the event vertex position.  This position is where
    // the track fit parameters are extrapolated to.
    trk_fit.set_z( _vertex_z );

    // Set total momentum guess to large value to effectively give straight-line fit until momentum is known
    double pz_init;           
    pz_init = (trk_ptr->get()->get_arm()==FVTXOO::North ? 1000. : -1000.);     

    double pxpz = 0.0;
    double pypz = 0.0;

    // Retrieve associated coordinates and add to track fit node:
    TFvtxCoordMap::key_iterator coord_iter( trk_ptr->get()->get_associated<TFvtxCoord>() );
    while( TFvtxCoordMap::pointer coord_ptr = coord_iter.next() )
    {
       // Straight-line fitter seems to be returning some poor results occasionally, but
       // does not have an error. For now, calculate an average px/pz, py/pz from individual 
       // coords and use this for the slope below, because poor initial track params
       // mess up the KF fit.
       
       PHPoint CoordMidPoint = coord_ptr->get()->get_coord_midpoint();
       pxpz += CoordMidPoint.getX()/(CoordMidPoint.getZ() - _vertex_z);
       pypz += CoordMidPoint.getY()/(CoordMidPoint.getZ() - _vertex_z);

       int phiflag = 0;     // Store measurement in r direction
       trk_fit.add_node( LocalNode( coord_ptr, trk_fit.get_z(), phiflag ) );
       phiflag = 1;     // Store measurement in phi direction
       trk_fit.add_node( LocalNode( coord_ptr, trk_fit.get_z(), phiflag ) );
    }

    pxpz = pxpz/coord_iter.count();
    pypz = pypz/coord_iter.count();

    // If track fit succeeds, add fit parameters to the track object:
    // **Need to decide how to set charge at this point**
    int charge = 1;

    if (trk_fit.fit()){
//      TMutTrkPar local_trk(trk_fit.get_state()(0,0),          // x at trk_fit.get_z
//                           trk_fit.get_state()(1,0),          // y at trk_fit.get_z
      TMutTrkPar local_trk(0.0,                               // x at trk_fit.get_z
                           0.0,                               // y at trk_fit.get_z
                           _vertex_z,                         // z of fit parameters
                           pz_init*pxpz,  // px at trk_fit.get_z
                           pz_init*pypz,  // py at trk_fit.get_z
//                           pz_init*trk_fit.get_state()(2,0),  // px at trk_fit.get_z
//                           pz_init*trk_fit.get_state()(3,0),  // py at trk_fit.get_z
                           pz_init,                           // pz at trk_fit.get_z
                           charge);                              // charge (no knowledge here)

      trk_ptr->get()->set_trk_par(local_trk);
    }
    else{
    }

  }   // Loop over tracks in the track bank

}

//______________________________________________________________
// Calculate and store the initial track parameters for each track
bool mFvtxFindTrack::check_detector_overlap(TFvtxCoordMap::pointer in_coord_ptr, TFvtxTrkMap::pointer trk_ptr)
{

  if (in_coord_ptr->get()->get_column() == 0 &&
           trk_ptr->get()->has_coord(in_coord_ptr->get()->get_cage(),
				      in_coord_ptr->get()->get_station(), 
                                      in_coord_ptr->get()->get_sector()-1,
                                      1 )) return true;

  if (in_coord_ptr->get()->get_column() == 1 &&
           trk_ptr->get()->has_coord(in_coord_ptr->get()->get_cage(),
                                      in_coord_ptr->get()->get_station(), 
                                      in_coord_ptr->get()->get_sector()+1,
                                      1 )) return true;

  if ( trk_ptr->get()->has_coord(in_coord_ptr->get()->get_cage(),
                                      in_coord_ptr->get()->get_station(), 
                                      in_coord_ptr->get()->get_sector(),
                                      -1*in_coord_ptr->get()->get_column() + 1 )) return true;

  if ( trk_ptr->get()->has_coord(in_coord_ptr->get()->get_cage(),
                                      in_coord_ptr->get()->get_station(), 
                                      in_coord_ptr->get()->get_sector(),
                                      in_coord_ptr->get()->get_column() )) return true;

  return false;
}

//________________________________________________________
mFvtxFindTrack::LocalNode::LocalNode( TFvtxCoordMap::pointer fvtx_coord_ptr, const double &z, const int phiflag )
{

        // retrieve cluster position/error
        PHGslMatrix m( 1, 1 );
        PHGslMatrix cov( 1, 1 );
        PHGslMatrix h( 1, 4 );

        // Get points at ends of strip:
        PHPoint begin = fvtx_coord_ptr->get()->get_coord_begin();
        PHPoint end = fvtx_coord_ptr->get()->get_coord_end();

        // phiflag=1, measuring along length of strip; !phiflag measuring in r direction
        if (!phiflag){
          m( 0, 0 ) = fvtx_coord_ptr->get()->get_w_absolute();
          cov( 0, 0 ) = 1/MUTOO::SQUARE( fvtx_coord_ptr->get()->get_error() );
        }
        else{
          m( 0, 0 ) = 0.0;
          double error = sqrt(
                        FVTXOO::SQUARE(end.getY() - begin.getY()) +
                        FVTXOO::SQUARE(end.getX() - begin.getX()));
          cov( 0, 0 ) = error/sqrt(12.0);
        }

        // retrieve strip angle
        double angle = atan2((end.getY() - begin.getY()), (end.getX() - begin.getX()));

        // if phi flag is set, we are to add the phi portion of the measurement to the coord
        // list (as opposed to the r portion of the measurement).       Phi measurement orientation
        // is 90 degrees from r orientation.

        if (phiflag) angle += M_PI/2;

        // Ensure that the range is -pi/2, pi/2
        //
        angle = (angle < -M_PI_2) ? angle + M_PI : angle;
        angle = (angle > M_PI_2) ? angle - M_PI : angle;

        // transfer matrix
        h( 0, 0 ) = -sin( angle );
        h( 0, 1 ) = cos( angle );
        h( 0, 2 ) = -sin(angle)*(fvtx_coord_ptr->get()->get_coord_midpoint().getZ() - z);
        h( 0, 3 ) = cos(angle)*(fvtx_coord_ptr->get()->get_coord_midpoint().getZ() - z);

        // set measurement to private node
        set_measurement( m, cov );

        // set transfer (projection) matrix
        set_h( h );

}

//______________________________________________________________________
void mFvtxFindTrack::set_trk_windows( TFvtxTrkMap::pointer trk_ptr, PHPoint CoordMidPoint )
{
  // Project theta windows to other stations, using vertex point:
  double plane_x, plane_y, plane_z;
  double plane_theta, plane_phi;
  for (int istation = 0; istation< FVTXOO::MAX_STATION; istation++){
      FvtxStation *station= FvtxGeom::get_arm(trk_ptr->get()->get_arm())->get_cage(0)->get_station( istation );
      plane_z = station->get_z();
      plane_x =  CoordMidPoint.getX() - CoordMidPoint.getX()*(CoordMidPoint.getZ() - plane_z)/
                 (CoordMidPoint.getZ() - _vertex_z);
      plane_y =  CoordMidPoint.getY() - CoordMidPoint.getY()*(CoordMidPoint.getZ() - plane_z)/
                 (CoordMidPoint.getZ() - _vertex_z);

      plane_theta = atan(sqrt( MUTOO::SQUARE(plane_x) + MUTOO::SQUARE(plane_y) )/plane_z); 
      plane_phi = atan2(plane_y, plane_x); 

      double theta_width = atan( FvtxGeom::get_strip_width()*_mod_par->get_theta_window_width()/fabs(plane_z) );

      trk_ptr->get()->set_phi_min(istation, plane_phi - _mod_par->get_phi_window_width() );
      trk_ptr->get()->set_phi_max(istation, plane_phi + _mod_par->get_phi_window_width() );
      trk_ptr->get()->set_theta_min(istation, plane_theta - theta_width );
      trk_ptr->get()->set_theta_max(istation, plane_theta + theta_width );
  }

}
//______________________________________________________________________
void mFvtxFindTrack::load_ext_vertex( PHCompositeNode* top_node )
{

  // reference z is used by default
  _vertex_z = 0.0;

  // try load external vertex
  bool error( false );
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  if( error )
  {

    if( _mod_par->get_verbosity() >= FVTXOO::SOME )
    cerr << "mMutKalFit::load_ext_vertex - wrong external vertex.\n";

  } else {
    _vertex_z = vtx.getZ();
  }

  if (_mod_par->get_verbosity() > FVTXOO::NONE){
    cout << "_vertex_z in mFvtxFindTrack = " << _vertex_z << endl;
  }

  return;

}


