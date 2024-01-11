// $Id: MWGOOReco.C,v 1.1 2009/07/04 18:33:51 hpereira Exp $

/*!
\file    MWGOOReco.C
\brief   muon nanoDST creation, using new framework input nodes
\author  Hugo Pereira
\version $Revision: 1.1 $
\date    $Date: 2009/07/04 18:33:51 $
*/

#include <Fun4AllReturnCodes.h>
#include <PHIODataNode.h>
#include <PHGeometry.h>
#include <MUTOO.h>

// MUTOO
#include <TMutCoordMap.h>
#include <TMutGapCoordMap.h>
#include <TMutTrackUtil.h>
#include <TMutTrkMap.h>
#include <TMutVtxMap.h>

// MUIOO
#include <TMuiHitMapO.h>
#include <TMuiRoadMapO.h>
#include <TMuiClusterMapO.h>
#include <MuiCommon.hh>
#include <iostream>

// MWG
#include <PHInclusiveNanoCuts.h>
#include <PHMuoTracksOut.h>

#include <MWG.h>
#include <MWGVersion.h>
#include <rcp.h>

#include "MWGOOReco.h"

using namespace std;

//_______________________________________________________________
MWGOOReco::MWGOOReco(PHInclusiveNanoCuts *aCutter):
  _timer(PHTimeServer::get()->insert_new("MWGOORECO") ),
  _cutter( aCutter ),
  _mutoo_node( 0 ),
  _muioo_node( 0 )
{
  ThisName = "MWGOORECO";
  return;
}

//_______________________________________________________________
MWGOOReco::~MWGOOReco()
{ return; }

//_______________________________________________________________
int MWGOOReco::Init(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "MWGOOReco::Init" );
  PHNodeIterator iter(top_node);
  PHCompositeNode *dstNode =  static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));
  
  if( !dstNode ) {
    cout << "MWGOOReco::Init - cannot access DST node" << endl;
    return 1;
  }
  
  // single muon and dimuon branches selection
  bool do_dimuons( true );
  if (RCP::file_ok("MWG.rcp")) RCP::get<bool>("MWG.rcp","dodimu", do_dimuons);
  
  // create containers
  PHMuoTracksOut *particle(0);
  if( do_dimuons ) particle = MWG::newPHdiMuoTracks();
  else particle = MWG::newPHMuoTracks();

  cout << "MWGOOReco::Init - using class " << particle->GetName() << " (version: " << MWGVersion::get( particle->GetName() ) << ")" << endl;
  
  // old and mew muon framework selection
  dstNode->addNode( new PHIODataNode<PHObject>(particle,"PHMuoTracksOO","PHObject") );
  
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//_______________________________________________________________
int MWGOOReco::InitRun(PHCompositeNode *top_node)
{
  _cutter->Initialize(top_node);
  PHNodeIterator iter(top_node);
  PHCompositeNode *runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","RUN"));
  if (!runNode) {
    cout << PHWHERE << "MWGOOReco:RUN Node missing doing nothing" << endl;
    return -1;
  } else {
    PHIODataNode<PHObject>* CutsNode =  new PHIODataNode<PHObject>(_cutter,"MWGCuts","PHObject");
    runNode->addNode(CutsNode);
  }
  return 0;
}

//_______________________________________________________________
int MWGOOReco::process_event(PHCompositeNode *top_node)
{
  
  // check event variables
  _timer.get()->restart();
  bool accepted = _cutter->GlobalOK(top_node);
  
  // loop over input particles; fill output; make cuts..
  try {
    
    // retrieve nodes
    set_node_ptrs( top_node );
    
    // retrieve pointer to particles
    PHMuoTracksOut *particle = TMutNode<PHMuoTracksOut>::find_io_node( _ndst_node, "PHMuoTracksOO");
    particle->Reset();
    
    // retrieve new framework muon tracks, map tracks unique IDs to local tracks IDs
    map< ULong_t, int > track_keys = do_muons( particle ) ;
    
    // retrieve dimuons
    if (_cutter->get_dodimu())  do_dimuons( particle, track_keys );
    
    // check if filtering is required
    if( _cutter->get_dofilter() ) {
      cout << "MWGOOReco::process_event - no filtering implemented" << endl;      
    }
    
    // dump unique IDs
    // for( int index = 0; index < particle->get_npart(); index++ )
    // { cout << "MWGOOReco::process_event - single muon index: " << index << " uid: " << particle->get_uid( index ) << endl; }
    
    // dump unique IDs
    // for( int index = 0; index < particle->get_ndimu(); index++ )
    // { cout << "MWGOOReco::process_event - di-muon index: " << index << " uid: " << particle->get_dimuon_uid( index ) << endl; }

  } catch( exception &e ) { cout << e.what() << endl; }
  
  _timer.get()->stop();
  
  return (accepted) ? EVENT_OK:DISCARDEVENT;
}

//______________________________________________________
int MWGOOReco::End(PHCompositeNode* top_node)
{
  _timer.get()->print_stat();
  return 0;
}

//______________________________________________________________________________________
map< ULong_t, int > MWGOOReco::do_muons( PHMuoTracksOut *particle )
{
  // list of accepted track keys
  map<ULong_t,int> keys;
  
  // retrieve tracks, check if empty
  TMutTrkMap *trk_map = TMutNode<TMutTrkMap>::find_node(_mutoo_node,"TMutTrkMap");
  if( trk_map->empty() ) return keys;
  
  // count accepted tracks
  int n_tracks(0);
  TMutTrkMap::const_iterator trk_iter = trk_map->range();
  while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
  { if( accept_track( trk_ptr ) ) n_tracks++; }
  
  particle->set_npart(n_tracks) ;
  particle->set_TClonesArraySize(n_tracks);
  
  // Loop over particles
  int itrk=0;
  trk_iter.reset();
  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()) 
  {
    
    // check if track is to be considered
    if( !accept_track( trk_ptr ) ) continue;
    
    // create new particle
    particle->AddPHParticle(itrk);
    
    // unique ID
    particle->set_uid( itrk, trk_ptr->get()->get_key().get_obj_key() );
    
    // Track parameters at primary vertex
    const TMutTrkPar* trk_par = trk_ptr->get()->get_trk_par_vtx();
    if( trk_par ) particle->set_charge(itrk, static_cast<int>(trk_ptr->get()->get_charge()) );
    
    /*
    retrieve chisquare. only the _w chisquare is retrieved
    since the _r chisquare does not make sense when using the KalmanFilter
    */
    particle->set_chisquare( itrk, trk_ptr->get()->get_w_chi_square_pdf() );
    particle->set_ndf( itrk, trk_ptr->get()->get_ndf() );
    
    particle->set_px(0, itrk, trk_par->get_px());
    particle->set_py(0, itrk, trk_par->get_py());
    particle->set_pz(0, itrk, trk_par->get_pz());
    particle->set_xpos(0, itrk, trk_par->get_x());
    particle->set_ypos(0, itrk, trk_par->get_y());
    particle->set_zpos(0, itrk, trk_par->get_z());
    
    // Covariance matrix at primary vertex
    //
    for (int iarr1=0; iarr1<5; iarr1++)
    {
      for (int iarr2=0; iarr2<5; iarr2++)
      { particle->set_cov(iarr1, iarr2, itrk, trk_par->get_covar(iarr1, iarr2)); }
    }
    
    // Track parameters at gap 0 of each station
    for (int sta=0; sta<3; ++sta)
    {
      const TMutTrkPar* sta_trk_par = trk_ptr->get()->get_trk_par_station(sta);
      if( sta_trk_par )
      {
        particle->set_px(sta+1, itrk, sta_trk_par->get_px());
        particle->set_py(sta+1, itrk, sta_trk_par->get_py());
        particle->set_pz(sta+1, itrk, sta_trk_par->get_pz());
        particle->set_xpos(sta+1, itrk, sta_trk_par->get_x());
        particle->set_ypos(sta+1, itrk, sta_trk_par->get_y());
        particle->set_zpos(sta+1, itrk, sta_trk_par->get_z());
      }
    }
    
    //Add the new kalman projection at Gap0
    if( !trk_ptr->get()->get_trk_par_list()->empty() )
    {
      const TMutTrkPar& track_par( trk_ptr->get()->get_trk_par_list()->back() );
      particle->set_px(4,itrk, track_par.get_px());
      particle->set_py(4,itrk, track_par.get_py());
      particle->set_pz(4,itrk, track_par.get_pz());
      particle->set_xpos(4,itrk, track_par.get_x());
      particle->set_ypos(4,itrk, track_par.get_y());
      particle->set_zpos(4,itrk, track_par.get_z());
    }
    
    // Hit bit pattern
    UShort_t pattern( trk_ptr->get()->get_hit_pattern() );
    particle->set_muTRhits(itrk, pattern);
    
    // get number of hits on track from hit_pattern
    int n_hit = 0;
    static const int numberOfCathodes( 16 );
    for( int i=0; i < numberOfCathodes; i++ ) { if( pattern & ( 1 << i ) ) n_hit++; }
    particle->set_nhits(itrk, n_hit );
    
    // All MUTOO ghost tracks have already been removed.
    // all remaining tracks have ghostflag set to 0
    //particle->set_ghostflag(itrk,0);
    particle->set_TMutTrk_status(itrk,trk_ptr->get()->get_status());
    
    // loop over associated roads
    TMuiRoadMapO::const_key_iterator mui_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
    
    short deepest_depth = 0;
    while(TMuiRoadMapO::const_pointer mui_ptr = mui_iter.next())
    {
      
      const short iroad = (mui_ptr->get()->get_depth())-2;
      particle->set_muIDOOhits(iroad, itrk, mui_ptr->get()->get_gapbit());
      
      // road parameters
      const TMutFitPar* road_par = mui_ptr->get()->get_const_fitpar();
      if( !road_par ) continue;
      
      particle->set_muIDOOchi(iroad, itrk, road_par->get_chi_square());
      
      PHPoint gap0_point = mui_ptr->get()->get_gap0_point();
      particle->set_muIDOO_gap0(0,iroad,itrk,(gap0_point.getX()));
      particle->set_muIDOO_gap0(1,iroad,itrk,(gap0_point.getY()));
      particle->set_muIDOO_gap0(2,iroad,itrk,(gap0_point.getZ()));
      particle->set_muIDOO_gap0(3,iroad,itrk,road_par->get_dxdz());
      particle->set_muIDOO_gap0(4,iroad,itrk,road_par->get_dydz());
      
      if (mui_ptr->get()->get_depth() > deepest_depth) 
      {
        deepest_depth = mui_ptr->get()->get_depth();
        
        for(int igap=0; igap<5; igap++){
          particle->set_muid_hit_x(igap, itrk, -8888.);
          particle->set_muid_hit_y(igap, itrk, -8888.);
        }

        // get the distance between the current road and its associated clusters (index=0)
        // retrieve associated clusters
        TMuiClusterMapO::key_iterator iClust = mui_ptr->get()->get_associated<TMuiClusterO>();
        
        // loop over clusters
        int index=0;
        while(TMuiClusterMapO::pointer pClust = iClust.next()) 
        {
          
//           PHPoint projpoint = TMutTrackUtil::linear_track_model(road_par,pClust->get()->get_centroidpos().getZ());
//           float dist = (float) PHGeometry::distanceLinePoint(pClust->get()->get_coord(),projpoint);
          
          // attribute a sign to the distance
//           int sign=-1;
//           if (pClust->get()->get_orientation()==kHORIZ) 
//           {
//             if (pClust->get()->get_centroidpos().getY() > projpoint.getY()) sign=1;
//           } else if (pClust->get()->get_centroidpos().getX() > projpoint.getX()) sign=1;
          
//           TMuiHitMapO::key_iterator iHit = pClust->get()->get_associated<TMuiHitO>();
//           particle->set_muID_proj_hit_dist(pClust->get()->get_plane(),
// 					   pClust->get()->get_orientation(),
// 					   0, itrk, sign*dist);
//           particle->set_muID_proj_hit_size(pClust->get()->get_plane(),
// 					   pClust->get()->get_orientation(),
// 					   0, itrk, iHit.count());

	  if(pClust->get()->get_orientation() == kVERT)
	    particle->set_muid_hit_x(pClust->get()->get_plane(), itrk, pClust->get()->get_centroidpos().getX());
	  else
	    particle->set_muid_hit_y(pClust->get()->get_plane(), itrk, pClust->get()->get_centroidpos().getY());

          index++;
        }
        
        // get the distance between the current road and all clusters (index>0)
//         std::multimap< float, TMuiClusterMapO::const_pointer > distance;
        
//         // retrieve muid clusters
//         TMuiClusterMapO *mui_cluster_map = TMutNode<TMuiClusterMapO>::find_node(_muioo_node,"TMuiClusterMapO");
        
//         // loop over clusters
//         TMuiClusterMapO::const_iterator cluster_iter = mui_cluster_map->range();
//         while(TMuiClusterMapO::const_pointer cluster_ptr = cluster_iter.next()) 
//         {
//           PHPoint projpoint = TMutTrackUtil::linear_track_model(road_par,cluster_ptr->get()->get_centroidpos().getZ());
//           float dist = (float) PHGeometry::distanceLinePoint(cluster_ptr->get()->get_coord(),projpoint);
//           distance.insert(pair<float, TMuiClusterMapO::const_pointer>(dist,cluster_ptr));
//         }
        
//         // sort the clusters starting from the closest
//         index=1;
//         for (multimap<float, TMuiClusterMapO::const_pointer>::iterator it = distance.begin(); it != distance.end(); it++) 
//         {
//           if (index >= 10 ) break;  // this is 10 in PHMuoTrackv11, changed from 30 in v9
          
//           // attribute a sign to the distance
//           int sign=-1;
//           PHPoint projpoint = TMutTrackUtil::linear_track_model (road_par,(*it).second->get()->get_centroidpos().getZ());
          
//           if ((*it).second->get()->get_orientation()==kHORIZ) {
// 	    if ((*it).second->get()->get_centroidpos().getY()>projpoint.getY()) sign=1;
// 	  } else {
// 	    if ((*it).second->get()->get_centroidpos().getX()>projpoint.getX()) sign=1;
//           }

//           // fill particle
//           particle->set_muID_proj_hit_dist((*it).second->get()->get_plane(),
// 					   (*it).second->get()->get_orientation(),
// 					   index, itrk, sign*((*it).first));
          
//           TMuiHitMapO::key_iterator iHit = (*it).second->get()->get_associated<TMuiHitO>();
//           particle->set_muID_proj_hit_size((*it).second->get()->get_plane(),
// 					   (*it).second->get()->get_orientation(),
// 					   index, itrk, iHit.count());
//           index++;
//         }
      } // depth check
    } // loop over associated roads
    
    // fill gap coordinate charge difference and errors
    TMutGapCoordMap::const_key_iterator gap_coord_iter( trk_ptr->get()->get_associated< TMutGapCoord >() );
    while( TMutGapCoordMap::const_pointer gap_coord_ptr = gap_coord_iter.next() )
    {
      
      // get associated coordinates
      TMutCoordMap::const_key_iterator coord_iter( gap_coord_ptr->get()->get_associated< TMutCoord >() );
      if( coord_iter.count() != 2 ) {
        cout << "MWGOOReco::do_muons_OO - gap coordinate does not match 2 coordinates. skipped" << endl;
        continue;
      }
      
      // save charges and error
      float q[2] = {0,0};
      float q_error[2] = {0,0};
      while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
      {
        q[ coord_ptr->get()->get_cathode() ] = static_cast<float>(coord_ptr->get()->get_q_tot());
        q_error[ coord_ptr->get()->get_cathode() ] = static_cast<float>(coord_ptr->get()->get_q_error());
      }
      
      // add 2D dependant correction to charge in cathode 1
      float correction( static_cast<float>(gap_coord_ptr->get()->get_charge_corr()));
      q[1]/=(1+correction);
      q_error[1]/=(1+correction);
      
      // calculate delta_q and error
      float delta_q = 2*( q[1]-q[0] )/( q[1] + q[0] );
      float delta_q_error = 4 *
        sqrt(
	     MUTOO::SQUARE( q[1]*q_error[0] ) +
	     MUTOO::SQUARE( q[0]*q_error[1] )) /
        MUTOO::SQUARE( q[0] + q[1] );
    
      // calculate gap coordinate location and append to track
      unsigned int index = gap_coord_ptr->get()->get_station()*3+gap_coord_ptr->get()->get_gap();
      particle->set_delta_q( index, itrk, delta_q );
      particle->set_delta_q_error( index, itrk, delta_q_error );
    
    }
    
    //---> NOT FILLED
//     particle->set_PID(itrk,0);                         
//     particle->set_MuonConfidence(itrk,0);              
//     particle->set_PionConfidence(itrk,0);       
    
    // particle bent plane momentum at station 1
    const TMutBPPar* bp_par( trk_ptr->get()->get_bp_par() );
    if( bp_par )
    {
      particle->set_st1_bp_P(0,itrk, bp_par->get_px_st1() ); 
      particle->set_st1_bp_P(1,itrk, bp_par->get_py_st1() ); 
      particle->set_st1_bp_P(2,itrk, bp_par->get_pz_st1() ); 
    }
    
    // track cuts
    if (_cutter->MuonOK(particle, itrk)) 
    {
      
      // Insert the track object key and itrk index into the map
      keys.insert(make_pair(trk_ptr->get()->get_key().get_obj_key(), itrk));
      
      // Increment the track if particle OK...
      itrk++;
      
    } else particle->RemovePHParticle(itrk);
    
  }
  
  // resize array
  particle->set_npart(itrk);
  particle->set_TClonesArraySize(itrk);
  
  return keys;
  
}

//______________________________________________________________________________________
void MWGOOReco::do_dimuons(PHMuoTracksOut *particle, const map<ULong_t,int>& keys )
{
  
  // iterator over track index vs key map
  typedef map<ULong_t,int>::const_iterator key_map_iterator;
  
  // retrieve vertex map and check size
  TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::find_node(_mutoo_node,"TMutVtxMap");
  if(vtx_map->empty()) return;
  
  //! resize arrays
  particle->set_ndimu(vtx_map->size());
  particle->Set_DimuArraySize(vtx_map->size());
  
  // Loop over TMutVtx objects
  int idimu=0;
  TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
  while(TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next())
  {
    
    // create new dimuon
    particle->AddPHDimuon(idimu);
    particle->set_dimuon_uid( idimu, vtx_ptr->get()->get_key().get_obj_key() );
    
    // Loop over associated tracks.  Find the index in the key vector
    // that matches the associated track key.  Set the particle index
    // accordingly.
    UShort_t itrk=0;
    TMutTrkMap::const_key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
    {
      
      // Set the index in the dimuon object corresponding to the track
      key_map_iterator iter = keys.find(trk_ptr->get()->get_key().get_obj_key());
      if(iter != keys.end() ) particle->set_ditrkIndex(itrk++, idimu, iter->second);
      else cerr
        << "MuonNanoDSTfuncs::do_dimuons_OO - could not find associated track "
        << trk_ptr->get()->get_key().get_obj_key()
        << " in keylist. " << endl;
    }
    
    //===== computing dimuon charge
    //  0 ==> +- dimuon pair
    //  1 ==> ++ dimuon pair
    // -1 ==> -- dimuon pair
    // -2 ==> one of the muon charge = 0
    if(vtx_ptr->get()->get_sign() == TMutVtx::POSNEG) particle->set_dicharge(idimu,0);
    else if(vtx_ptr->get()->get_sign() == TMutVtx::POSPOS) particle->set_dicharge(idimu,1);
    else if(vtx_ptr->get()->get_sign() == TMutVtx::NEGNEG) particle->set_dicharge(idimu,-1);
    else particle->set_dicharge(idimu,-2);
    
    particle->set_dipx(idimu,vtx_ptr->get()->get_px());
    particle->set_dipy(idimu,vtx_ptr->get()->get_py());
    particle->set_dipz(idimu,vtx_ptr->get()->get_pz());
    
    // vertex BP information
    particle->set_vtx_bp_xpos(idimu,vtx_ptr->get()->get_x_bp());
    particle->set_vtx_bp_ypos(idimu,vtx_ptr->get()->get_y_bp());
    particle->set_vtx_bp_zpos(idimu,vtx_ptr->get()->get_z_bp());
    particle->set_vtx_bp_dca(idimu,vtx_ptr->get()->get_dca_bp());
    
    // vertex fit information
    particle->set_vtx_xpos(idimu,vtx_ptr->get()->get_x());
    particle->set_vtx_ypos(idimu,vtx_ptr->get()->get_y());
    particle->set_vtx_zpos(idimu,vtx_ptr->get()->get_z());
    
    particle->set_vtx_chrg_1(idimu,vtx_ptr->get()->get_charge1());
    particle->set_vtx_px_1(idimu,vtx_ptr->get()->get_px1());
    particle->set_vtx_py_1(idimu,vtx_ptr->get()->get_py1());
    particle->set_vtx_pz_1(idimu,vtx_ptr->get()->get_pz1());
    
    particle->set_vtx_chrg_2(idimu,vtx_ptr->get()->get_charge2());
    particle->set_vtx_px_2(idimu,vtx_ptr->get()->get_px2());
    particle->set_vtx_py_2(idimu,vtx_ptr->get()->get_py2());
    particle->set_vtx_pz_2(idimu,vtx_ptr->get()->get_pz2());
    
    particle->set_dimass(idimu,vtx_ptr->get()->get_mass());
    particle->set_vtx_chisquare(idimu,vtx_ptr->get()->get_chi_square_pdf());
    particle->set_vtx_ndf(idimu,vtx_ptr->get()->get_ndf());
    
    // Note currently only 7 elements of 9 possible are used
    for (int icov=0; icov<7; icov++)
    {
      for (int jcov=0; jcov<7; jcov++)
      { particle->set_vtx_cov(icov,jcov,idimu,vtx_ptr->get()->get_covar(icov,jcov)); }
    }
    
    //===== Dimuon quality check (so far, nothing)
    if (_cutter->diMuonOK(particle, idimu)) idimu++;
    else particle->RemovePHDimuon(idimu);
    
  }
  
  // resize array
  particle->set_ndimu(idimu);
  particle->Set_DimuArraySize(idimu);
  
}

//____________________________________________________________
bool MWGOOReco::set_node_ptrs(PHCompositeNode* top_node)
{
  
  // look for mutoo node
  PHNodeIterator node_iter(top_node);
  
  // ndst node
  _ndst_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "NDST"));
  if(!_ndst_node) _ndst_node = top_node;
  
  //! mutoo node
  _mutoo_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "MUTOO" ));
  if( !_mutoo_node )
    throw logic_error( "MWGOOReco::set_node_ptrs - could not find node MUTOO. You shoud run the correct Unpacker to your macro." );
  
  // muioo node
  _muioo_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "MUIOO" ));
  if( !_muioo_node )
    throw logic_error( "MWGOOReco::set_node_ptrs - could not find node MUIOO. You shoud run the correct Unpacker to your macro." );
  
  return true;
}

//__________________________________________________________________
bool MWGOOReco::accept_track( TMutTrkMap::const_pointer trk_ptr ) const
{
  // check if track is a ghost
  if( trk_ptr->get()->get_ghost() ) return false;
  
  // check if track was fitted
  if( !trk_ptr->get()->get_reco_success() ) return false;
  
  // all checks passed
  return true;
  
}
