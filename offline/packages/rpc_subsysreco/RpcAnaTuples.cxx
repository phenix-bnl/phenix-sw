#include <PHTFileServer.h>
#include <TNtuple.h>
#include <EventHeader.h>
#include <Fun4AllReturnCodes.h>

#include <bitset>
#include <fstream>
#include <sstream>

//#include <TRpcTrk.h>
#include <TRpcTrk_v2.h>
#include <TRpcTrkMap.h>
#include <TRpcHit.h>
#include <TRpcMCHit.h>
#include <TRpcClusMap.h>
#include <TRpcCoordMap.h>
#include <TRpcCoord_v1.h>
#include <TRpcHitMap.h>
#include <TRpcMCHitMap.h>
#include <RpcStrip.h>
#include<RpcStrip_v1.h>
#include <RPCFULLGEOM.h>

#include<PHTrackIntegratorKF.h>
#include <PHPoint.h>
#include <PHLine.h>
#include <PHGeometry.h>
#include "RpcAnaTuples.h"
#include <TMutTrackUtil.h>

using namespace std;

//__________________________________________________________________________
RpcAnaTuples::RpcAnaTuples( const char* name, bool sim, const char* filename):
  SubsysReco( name ),
  SIM( sim ),
  rpc_clus_map( 0 ),
  rpc_trk_map( 0 ),
  mu_trk_map( 0 ),
  rpc_hit_map( 0 )
{
  _filename = filename;
  return;
}

//_________________________________________________________________________
int RpcAnaTuples::Init(PHCompositeNode *topNode)
{
  MUTOO::PRINT( cout , "RpcAnaTuples::Init");
  cout << "opening file: " << _filename << endl;
  PHTFileServer::get().open( _filename, "RECREATE");
  
  _rpc_trks = new TNtuple( "rpc_trks", "rpc_trks", "arm:px:py:pz:dca_r:dca_z:bbc_vtx:charge:dca_vtx:rpcclus3time:reco_w:reco_v:rpc3projX:rpc3projY:timing:q");
  cout << "track ntuple booked" << endl;
   
  _rpc_hits = new TNtuple( "rpc_hits", "rpc_hits", "arm:station:octant:half_octant:rseg:strip:charge:charge_err:timing" );
  cout << " RpcHit ntuple booked" << endl;
 
  _rpc_clus = new TNtuple( "rpc_clus", "rpc_clus", "arm:station:octant:half_octant:rseg:wid:chisq:ncentroid");
  cout << "Rpc cluster ntuple booked" << endl;

  return 0;
}  

//_________________________________________________________________________
int RpcAnaTuples::process_event(PHCompositeNode *topNode)
{
  PHTypedNodeIterator<PHGlobal> global_iter(topNode);
  PHIODataNode<PHGlobal> *global_ndst = global_iter.find("PHGlobal");
  if(global_ndst) {
    _global = global_ndst->getData();
  } else {
    _global = 0;
  }

  // load all maps
  try{ rpc_clus_map = TMutNode<TRpcClusMap>::find_node( topNode, "TRpcClusMap" ); }
  catch (exception &e){ MUTOO::TRACE(e.what());
                        rpc_clus_map = NULL; }


  try{ rpc_trk_map = TMutNode<TRpcTrkMap>::find_node( topNode, "TRpcTrkMap" ); }
  catch (exception &e){ MUTOO::TRACE(e.what());
                        rpc_trk_map = NULL; }

  try{ mu_trk_map = TMutNode<TMutTrkMap>::find_node( topNode, "TMutTrkMap" );
  } catch( std::exception& e){ MUTOO::TRACE(e.what());
                                mu_trk_map = NULL; }
  if(SIM)
  {
  try{  rpc_hit_map = TMutNode<TRpcMCHitMap>::find_node( topNode,"TRpcMCHitMap" ); }
  catch( std::exception& e){ MUTOO::TRACE(e.what());
                                rpc_hit_map = NULL; }
  }
 
  try{  rpc_real_hit_map = TMutNode<TRpcHitMap>::find_node( topNode,"TRpcHitMap" ); }
  catch( std::exception& e){ MUTOO::TRACE(e.what());
                                rpc_real_hit_map = NULL; }


  // fill trk variables
  write_rpc_trk_ntuple(topNode);
   
  //fill hit variables
  write_rpc_hit_ntuple(topNode);

  //fill clus variables
  write_rpc_clus_ntuple(topNode);
 

  return EVENT_OK;
} 

//______________________________________________________________________
void RpcAnaTuples::write_rpc_trk_ntuple(PHCompositeNode* topNode)
{

  float ntvar[100]={0};
  float arm, bbc_vtx;
  float px,py,pz;
  int rpc_n_trk = 0;
  TRpcTrkMap::const_iterator trk_iter = rpc_trk_map->range();
  while( TRpcTrkMap::const_pointer trk_ptr = trk_iter.next() )
  {
   rpc_n_trk++;
   arm = trk_ptr->get()->get_arm();
   TMutTrkPar *rpc_trk_par = (TMutTrkPar*) trk_ptr->get()->get_trk_par_vtx();
   px = rpc_trk_par->get_px();
   py = rpc_trk_par->get_py();
   pz = rpc_trk_par->get_pz();
   //dca = trk_ptr->get()->get_dca_trk();   
   if(_global) {
     bbc_vtx = _global->getBbcZVertex(); }
   else {
     bbc_vtx = -999; }

  // check if there are any matching mutr track
  int mu_n_trk=0;
  TMutTrkMap::const_iterator mu_trk_iter = mu_trk_map->range();
  while( TMutTrkMap::const_pointer mu_trk_ptr = mu_trk_iter.next() )
  {
   mu_n_trk++;
   // make sure they are in same arm
   if( mu_trk_ptr->get()->get_arm() != trk_ptr->get()->get_arm() ) continue ;
   const TMutTrkPar* mu_trk_par = mu_trk_ptr->get()->get_trk_par_vtx();
    

   // make sure their momentums are close enough to call them same track
   if( fabs( px - mu_trk_par->get_px() )<0.1 && fabs( py - mu_trk_par->get_py() )<0.1
	&& fabs( pz - mu_trk_par->get_pz() )<0.1 )
     {  
      
    // calculate distance to closest approch
     double dca( 9999 );
     double dca_r( 9999 );
     double dca_z( 9999 );
     double dca_r_tmp( 9999 );
     double dca_z_tmp( 9999 );
     double _reco_w( 9999 );
     double _reco_v( 9999 );
     double timing( 9999 );
     double q( 9999 );
     PHPoint trk_projection( trk_ptr->get()->get_trk_extrapolated_hit_x(), trk_ptr->get()->get_trk_extrapolated_hit_y() , RPCFULLGEOM::GlobalFramePosition_Z[2][mu_trk_ptr->get()->get_arm()]);

    // lets try projection from station 3
    if(mu_trk_ptr->get()->get_trk_par_list()->size() > 0 )
    {
      PHPoint trk_proj2( TMutTrackUtil::linear_track_model(
      		mu_trk_ptr->get()->get_trk_par_list()->back(),
          	RPCFULLGEOM::GlobalFramePosition_Z[2][mu_trk_ptr->get()->get_arm()] ) );
    
    // now find all clusters and their associated hit locations
    TRpcClusMap::const_iterator clus_iter = rpc_clus_map->range();
    while(TRpcClusMap::const_pointer clus_ptr = clus_iter.next())
    {
      if(clus_ptr->get()->get_arm()!=trk_ptr->get()->get_arm()) { continue; }
      if(clus_ptr->get()->get_station()!=2) { continue; }
 
      PHKeyIterator<TRpcHit> hit_iter = clus_ptr->get()->get_associated<TRpcHit>();
 
      while(TRpcHitMap::const_pointer hit_ptr = hit_iter.next() )
      { 
      /* TRpcMCHitMap::iterator hit_iter = rpc_hit_map->range();

      while(TRpcMCHitMap::const_pointer hit_ptr = hit_iter.next() )
      { */
      if( hit_ptr->get()->get_station() != 2) continue;
      RpcStrip *strip = new RpcStrip_v1();
      strip->SetStrip( hit_ptr->get()->get_arm(),
		       hit_ptr->get()->get_station(),
		       hit_ptr->get()->get_octant(),
		       hit_ptr->get()->get_half_octant(),
		       hit_ptr->get()->get_rseg(),
                       //hit_ptr->get()->get_strip_list().at(0)->get_stripid() );
                       hit_ptr->get()->get_strip() );
 
     // strip angle
      double angle = atan2( (strip->GetEnd().getY() - strip->GetBegin().getY()),
			    (strip->GetEnd().getX() - strip->GetBegin().getY()) ); 

      PHLine line(strip->GetEnd(),strip->GetBegin());
     //PHPoint projection( PHGeometry::closestApproachLinePoint( line, trk_projection) );    
     PHPoint projection( PHGeometry::closestApproachLinePoint( line, trk_proj2) );    
 
     dca_r_tmp = PHGeometry::distancePointToPoint(projection, trk_proj2) ;
     double strip_length = PHGeometry::distancePointToPoint(strip->GetEnd(),strip->GetBegin()) ;
     if( (PHGeometry::distancePointToPoint(projection, strip->GetEnd()) > strip_length) || (PHGeometry::distancePointToPoint(projection, strip->GetBegin()) > 		strip_length) )
     {
       dca_z_tmp = min( PHGeometry::distancePointToPoint(projection, strip->GetEnd()), PHGeometry::distancePointToPoint(projection, 						strip->GetBegin()) );
     } else{
       dca_z_tmp =0;
     }

     double dca_tmp = sqrt( pow(dca_r_tmp,2) + pow( dca_z_tmp,2) );
   

      //dca = min( dca, PHGeometry::distancePointToPoint(projection, trk_projection) );
      if( dca_tmp < dca )
      {
        dca_r = dca_r_tmp ;
        dca_z = dca_z_tmp;
        dca = dca_tmp;
       double cos_phi( cos(angle) );
       double sin_phi( sin(angle) );
      _reco_w = -sin_phi*trk_proj2.getX() + cos_phi*trk_proj2.getY();
      _reco_v = cos_phi*trk_proj2.getX() - sin_phi*trk_proj2.getY();

      //store corresponding timing and charge information
       timing = hit_ptr->get()->get_t();  
       q = hit_ptr->get()->get_q();
      }
     } // for HitMap
    }  // for ClusMap    
  


   ntvar[0] = arm;  // arm
   ntvar[1] = px;
   ntvar[2] = py;
   ntvar[3] = pz;
   ntvar[4] = dca_r; //trk_ptr->get()->get_dca_r_trk(); //dca_r;
   ntvar[5] = dca_z; //trk_ptr->get()->get_dca_z_trk();  //dca_z;
   ntvar[6] = bbc_vtx;
   ntvar[7] = trk_ptr->get()->get_charge(); // get charge
   //ntvar[7] = dca;
   ntvar[8] = trk_ptr->get()->get_dca_trk(); //trk_ptr->get()->get_dca_trk_vtx(); // vtx dca
   ntvar[9] = trk_ptr->get()->get_rpcclus3time(); // rpc 3 time
   ntvar[10] = _reco_w;
   ntvar[11] = _reco_v ;
   ntvar[12] = trk_proj2.getX();
   ntvar[13] = trk_proj2.getY();
   ntvar[14] = timing;
   ntvar[15] = q;
   _rpc_trks->Fill(ntvar);

    }  // trk par exists
    }  // end if loop for momentum check

   }  // end of mu_trk loop
  }  // end of rpc_trk loop

}

//______________________________________________________________________________
void  RpcAnaTuples::write_rpc_hit_ntuple(PHCompositeNode *topNode)
{
 
  float ntvar[100] = {0};
  /*TRpcMCHitMap::iterator hit_iter = rpc_hit_map->range();
  while(TRpcMCHitMap::const_pointer hit_ptr = hit_iter.next() ) */
  TRpcHitMap::iterator hit_iter = rpc_real_hit_map->range();
  while( TRpcHitMap::const_pointer hit_ptr = hit_iter.next() )
  {
   ntvar[0] = hit_ptr->get()->get_arm(); // arm
   ntvar[1] = hit_ptr->get()->get_station(); //station
   ntvar[2] = hit_ptr->get()->get_octant(); //octant
   ntvar[3] = hit_ptr->get()->get_half_octant(); // half-octant
   ntvar[4] = hit_ptr->get()->get_rseg(); // r segment 
   //ntvar[5] =  hit_ptr->get()->get_strip_list().at(0)->get_stripid();   
   ntvar[5] = hit_ptr->get()->get_strip(); // strip
   ntvar[6] = hit_ptr->get()->get_q(); // charge
   ntvar[7] = hit_ptr->get()->get_q_error(); // charge error
   ntvar[8] = hit_ptr->get()->get_t(); // timing
   
  _rpc_hits->Fill(ntvar);
  }  
}


//____________________________________________________________________________
void RpcAnaTuples::write_rpc_clus_ntuple(PHCompositeNode *topNode)
{

  TRpcClusMap::const_iterator clus_iter = rpc_clus_map->range();

  float ntvar[100] = { 0 };
  while( TRpcClusMap::const_pointer clus_ptr = clus_iter.next() )
  {
   ntvar[0] = clus_ptr->get()->get_arm(); //arm
   ntvar[1] = clus_ptr->get()->get_station(); //station
   ntvar[2] = clus_ptr->get()->get_octant(); //octant
   ntvar[3] = clus_ptr->get()->get_half_octant(); //half-octant
   ntvar[4] = clus_ptr->get()->get_rseg(); // r segment
   ntvar[5] = clus_ptr->get()->get_n_hits(); // cluster width
   ntvar[6] = clus_ptr->get()->get_chi_square(); // cluster fit chisquare
   ntvar[7] = clus_ptr->get()->get_n_centroid(); // number of centroid
 
 _rpc_clus->Fill(ntvar);
  }

}

//_____________________________________________________________________
int RpcAnaTuples::End(PHCompositeNode *topNode)
{
  PHTFileServer::get().write( _filename );
  return 0;
}

