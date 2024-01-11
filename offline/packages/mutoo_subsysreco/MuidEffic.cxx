// $Id: MuidEffic.cxx,v 1.37 2011/07/14 22:27:09 pinkenbu Exp $

/*!
  \file    MuidEffic.cxx
  \ingroup supermodules
  \brief   reads MC and reconstructed muid maps, fills efficiency ntuples
  \author  S. Kelly
  \version $Revision: 1.37 $
  \date    $Date: 2011/07/14 22:27:09 $
*/

#include <boost/array.hpp>

// PHOOL includes
#include<PHCompositeNode.h>
#include<PHGeometry.h>
#include<PHTFileServer.h>
#include<PHTimer.h>
#include<recoConsts.h>

// ROOT includes
#include<TNtuple.h>

// MUIOO includes
#include<TMuiRoadMapO.h>
#include<TMuiClusterMapO.h>

// MUTOO includes
#include<TMutTrackUtil.h>
#include<TMutTrkMap.h>
#include<TMutExtVtx.h>

#include<EventHeader.h>
#include<BbcOut.h>

#include "MuonUtil.h"
#include "MuidEffic.h"

using namespace std;

//_____________________________________________________________
MuidEffic::MuidEffic( const char* name, const char* file) :
  SubsysReco( name ),
  _timer( PHTimeServer::get()->insert_new(name) ),
  _filename( file ? file:"muid_effic_ntuple.root" ),
  _is_sim( false ),
  _muid_only( false )
{ cout << "MuidEffic::MuidEffic - BETA version" << endl; }

//_____________________________________________________________
int MuidEffic::Init(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  if( rc->get_IntFlag( "MUONFUN4SIM", 0 ) )
    {
      _is_sim = true;
    }
  return 0;
}

//_____________________________________________________________
int MuidEffic::process_event(PHCompositeNode *top_node)
{
  static bool init = initialize_ntuple();
  if( !init ) throw runtime_error(DESCRIPTION("MuidEffic::initialize_ntuples failed"));
  
  _timer.get()->restart();
  
  // ntuple data
  boost::array<float, 100> ntuple_data;
  ntuple_data.assign(0);
  
  TMuiRoadMapO* road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
  TMuiRoadMapO::const_iterator road_iter = road_map->range();
  while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next())
    {
      // Require depth 4 road
      if( road_ptr->get()->get_depth() < 4 ) continue;
      
      // reset ntuple data
      ntuple_data.assign(0);
      
      // road info
      PHPoint road_point = road_ptr->get()->get_gap0_point();
      const TMutFitPar* road_fit_par = road_ptr->get()->get_const_fitpar();
      
      // loop over associated tracks and find the best one
      float bestroadchi = 1e9; // a large number
      
      // retrieve associated tracks and loop
      TMutTrkMap::const_key_iterator trk_iter = road_ptr->get()->get_associated<TMutTrk>();
      while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
	{
	  // skip ghost tracks
	  if( trk_ptr->get()->get_ghost() ||
	      !trk_ptr->get()->get_reco_success() ) continue;
	  
	  // get track fit parameters at station 3
	  const TMutTrkPar* st3_trk_par = trk_ptr->get()->get_trk_par_station(2);
	  
	  // extrapolate (linearly) towards the MuID road first plane z position
	  PHPoint trk_point  = TMutTrackUtil::linear_track_model(st3_trk_par, road_point.getZ());
	  
	  // calc distance, chi, etc.
	  float distance = PHGeometry::distancePointToPoint(road_point,trk_point);
	  float r_trk  = sqrt( MUTOO::SQUARE(st3_trk_par->get_dxdz()) +
			       MUTOO::SQUARE(st3_trk_par->get_dydz()) + 1 );
	  
	  float r_muid = sqrt( MUTOO::SQUARE(road_fit_par->get_dxdz()) +
			       MUTOO::SQUARE(road_fit_par->get_dydz()) + 1);
	  
	  const PHVector trk_vect( st3_trk_par->get_dxdz()/r_trk,
				   st3_trk_par->get_dydz()/r_trk,
				   1/r_trk );
	  
	  const PHVector muid_vect( road_fit_par->get_dxdz()/r_muid,
				    road_fit_par->get_dydz()/r_muid,
				    1/r_muid );
	  
	  float delta_theta = acos( trk_vect.dot(muid_vect) );
	  float chi = sqrt( MUTOO::SQUARE(distance/18.0) +
			    MUTOO::SQUARE(delta_theta/0.18) );
	  
	  // if chi better than previous entries, update ntuple var.
	  if( chi < bestroadchi )
	    {
	      // this is the momentum at station 1 inside the MUTR
	      ntuple_data[0] = trk_ptr->get()->get_trk_par()->get_px();
	      ntuple_data[1] = trk_ptr->get()->get_trk_par()->get_py();
	      ntuple_data[2] = trk_ptr->get()->get_trk_par()->get_pz();
	      
	      ntuple_data[3] = trk_ptr->get()->get_chi_square();
	      ntuple_data[4] = trk_ptr->get()->get_charge();
	      
	      ntuple_data[14] = distance;     // track_road_distance
	      ntuple_data[32] = delta_theta;  // track_road_dtheta
	      ntuple_data[33] = chi;          // track_road_chi
	    }
	}
      
      ntuple_data[5] = road_ptr->get()->get_depth();
      ntuple_data[6] = road_ptr->get()->get_gapbit();
      ntuple_data[7] = road_ptr->get()->get_gap0_point().getX();
      ntuple_data[8] = road_ptr->get()->get_gap0_point().getY();
      ntuple_data[9] = road_ptr->get()->get_gap0_point().getZ();
      ntuple_data[10] = road_ptr->get()->get_const_fitpar()->get_dxdz();
      ntuple_data[11] = road_ptr->get()->get_const_fitpar()->get_dydz();
      
      // BBC
      if( !_muid_only )
	{
	  bool error = false;
	  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
	  if( !error ) ntuple_data[12] = vtx.getZ();
	  else ntuple_data[12] = -9999;
	  
	  if( !_is_sim )
	    {
	      BbcOut *bbcout = TMutNode<BbcOut>::find_io_node( top_node, "BbcOut" );
	      if( !bbcout->isValid() )
		{
		  if (verbosity > 0)
		    {
		      cout << PHWHERE << " invalid BBC data" << endl;
		    }
		}
	      else
		{
		  // add more BBC info after other variables
		  ntuple_data[30] = bbcout->get_ChargeSum(0);
		  ntuple_data[31] = bbcout->get_ChargeSum(1);
		}
	    }
	}
      
      // j.nagle - this variable was trigger information in isminbias, but is not used so just set to zero
      if( !_is_sim && !_muid_only ) ntuple_data[13] = 0.0;
      
      recoConsts *rc = recoConsts::instance();
      ntuple_data[15] = rc->get_IntFlag( "RUNNUMBER", 0 );  // run number
      
      // Get the event number
      try
	{
	  EventHeader* evt = TMutNode<EventHeader>::find_io_node(top_node,"EventHeader");
	  ntuple_data[16] = evt->get_EvtSequence();
	}
      catch(exception& e)
	{}
      
      ntuple_data[17] = trk_iter.count();
      
      // Loop over roads and follow associations down to clusters
      TMuiClusterMapO::const_key_iterator clus_iter = road_ptr->get()->get_associated<TMuiClusterO>();
      while(TMuiClusterMapO::const_pointer clus_ptr = clus_iter.next())
	{
	  double target_z = clus_ptr->get()->get_centroidpos().getZ();
	  
	  // Extrapolate road to cluster z
	  PHPoint trk_point = TMutTrackUtil::linear_track_model(road_ptr->get()->get_const_fitpar(),target_z);
	  
	  // Calculate the distance
	  double distance = PHGeometry::distanceLinePoint(clus_ptr->get()->get_coord(),trk_point);
	  
	  // Write residual into ntuple (h0:h1:h2:h3:h4:v0:v1:v2:v3:v4)
	  UShort_t index = get_plane_index(clus_ptr->get()->get_plane(),
					   clus_ptr->get()->get_orientation());
	  ntuple_data[18+index] = distance;
	}
      
      // Arm
      ntuple_data[28] = road_ptr->get()->get_arm();
      ntuple_data[29] = road_iter.count();
      // note: BBC charge info comes later in the array, so this is not the last index
      _ntuple->Fill( &ntuple_data[0] );
    }
  
  _timer.get()->stop();
  return 0;
}

//_____________________________________________________________
int MuidEffic::End(PHCompositeNode* top_node)
{
//   _timer.get()->print_stat();
  PHTFileServer::get().write( _filename );
  return 0;
}

//_____________________________________________________________
bool MuidEffic::initialize_ntuple()
{
  // open TFile
  PHTFileServer::get().open( _filename, "RECREATE" );
  
  // This ntuple is used by the analysis routine to calculated the efficiency
  _ntuple = new TNtuple( "muid", "muid",
			 "px:py:pz:trk_chi:sign:depth:rbitmask:"
			 "rx:ry:rz:rdxdz:rdydz:bbc_z:is_minbias:road_track_distance:run:evt:has_trk:"
			 "h0:h1:h2:h3:h4:v0:v1:v2:v3:v4:"
			 "arm:nroads:bbc_ch_sum_0:bbc_ch_sum_1:road_track_dtheta:road_track_chi" );
  
  return true;
}

//_____________________________________________________________
int MuidEffic::get_plane_index( const int& gap, const int& orientation ) const
{
  static int index_map[nGaps][nOrients];
  static bool init = false;
  if( !init )
    {
      for( int i=0; i<nGaps; i++ )
	for( int j=0; j<nOrients; j++ ) index_map[i][j] = 0;
      
      // Horizontal
      index_map[0][kHoriz] = 0;
      index_map[1][kHoriz] = 1;
      index_map[2][kHoriz] = 2;
      index_map[3][kHoriz] = 3;
      index_map[4][kHoriz] = 4;
      
      // Vertical
      index_map[0][kVert] = 5;
      index_map[1][kVert] = 6;
      index_map[2][kVert] = 7;
      index_map[3][kVert] = 8;
      index_map[4][kVert] = 9;
      
      init = true;
    }
  
  return index_map[gap][orientation];
}
