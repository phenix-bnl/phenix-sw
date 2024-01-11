/*!
	\file mRpcTrack.cxx
	\brief Module to find and attach an RPC cluster to the MutTrk object
	\author R. S. Hollis (rhollis@ucr.edu)
	\version $Revision: 1.8 $
	\date $Date: 2017/07/11 16:24:41 $
*/

#include "mRpcTrack.h"
#include "TRpcTrkMap.h"
#include "TRpcTrk_v3.h"
#include "TRpcHit_v1.h"
#include "TRpcHitMap.h"
#include "TMutTrkMap.h"
#include "TMuiRoadMapO.h"
#include "RpcStrip_v2.h"
#include "RPCOO.h"
#include <iostream>

using namespace std;

//_____________________________________________
mRpcTrack::mRpcTrack() : 
	_timer( PHTimeServer::get()->insert_new("mRpcTrack") )
{
        RPCOO::TRACE("initializing module mRpcTrack",RPCOO::ALOT);	
}

//_____________________________________________
PHBoolean mRpcTrack::event(PHCompositeNode* top_node)
{
  //std::cout << "Building a track ... " << std::endl;
	_timer.get()->restart(); 
	try { 
				
		// Reset IOC pointers
		set_interface_ptrs(top_node);
		_rpc_trk_map->clear();
		
		// Find and attach an RPC cluster to the MuiRoad
		attach_track();
		
	} catch(std::exception& e) {
		RPCOO::TRACE(e.what());
		return False;
	}	

	// If verbose dump the contents of trks and primary
	//
	_timer.get()->stop();
	
	//}
	
	return True;
}

//_____________________________________________
void mRpcTrack::attach_track()
{
  //cout << "mRpcTrack::attach_track()" << endl;
  TMutTrkMap::iterator iter = _mut_trk_map->range();
  
  while( TMutTrkMap::pointer ptr = iter.next() ) {
    
    TRpcTrkMap::iterator thismap = _rpc_trk_map->insert_new(ptr->get()->get_arm());
    
    TRpcTrkMap::pointer _rpc_trk_ptr = thismap.current();
    _rpc_trk_ptr->get()->set_trk_par(*(ptr->get()->get_trk_par()));
    _rpc_trk_ptr->get()->set_trk_par_vtx(*(ptr->get()->get_trk_par_vtx()));
    
    //Take the hough angle from the momentum vector at it's vertex
    //multiply by the difference between the z position from the RPC
    //to the momentum vector's vertex Z
    //RPC3 - trk
    double fProjHitX=(ptr->get()->get_trk_par()->get_px()/ptr->get()->get_trk_par()->get_pz());
    fProjHitX *= (RPCFINALGEOM::GlobalFramePosition_Z[2][ptr->get()->get_arm()]-ptr->get()->get_trk_par()->get_z());
    fProjHitX += ptr->get()->get_trk_par()->get_x();
    double fProjHitY=(ptr->get()->get_trk_par()->get_py()/ptr->get()->get_trk_par()->get_pz());
    fProjHitY *= (RPCFINALGEOM::GlobalFramePosition_Z[2][ptr->get()->get_arm()]-ptr->get()->get_trk_par()->get_z());
    fProjHitY += ptr->get()->get_trk_par()->get_y();
    
    //RPC3 - vtx
    double fProjHitXVtx=(ptr->get()->get_trk_par_vtx()->get_px()/ptr->get()->get_trk_par_vtx()->get_pz());
    fProjHitXVtx *= (RPCFINALGEOM::GlobalFramePosition_Z[2][ptr->get()->get_arm()]-ptr->get()->get_trk_par_vtx()->get_z());
    fProjHitXVtx += ptr->get()->get_trk_par_vtx()->get_x();
    double fProjHitYVtx=(ptr->get()->get_trk_par_vtx()->get_py()/ptr->get()->get_trk_par_vtx()->get_pz());
    fProjHitYVtx *= (RPCFINALGEOM::GlobalFramePosition_Z[2][ptr->get()->get_arm()]-ptr->get()->get_trk_par_vtx()->get_z());
    fProjHitYVtx += ptr->get()->get_trk_par_vtx()->get_y();

    //RPC1 - trk
    double fProjHitX1=(ptr->get()->get_trk_par()->get_px()/ptr->get()->get_trk_par()->get_pz());
    fProjHitX1 *= (RPCFINALGEOM::GlobalFramePosition_Z[0][ptr->get()->get_arm()]-ptr->get()->get_trk_par()->get_z());
    fProjHitX1 += ptr->get()->get_trk_par()->get_x();
    double fProjHitY1=(ptr->get()->get_trk_par()->get_py()/ptr->get()->get_trk_par()->get_pz());
    fProjHitY1 *= (RPCFINALGEOM::GlobalFramePosition_Z[0][ptr->get()->get_arm()]-ptr->get()->get_trk_par()->get_z());
    fProjHitY1 += ptr->get()->get_trk_par()->get_y();
    
    //RPC1 - vtx
    double fProjHitXVtx1=(ptr->get()->get_trk_par_vtx()->get_px()/ptr->get()->get_trk_par_vtx()->get_pz());
    fProjHitXVtx1 *= (RPCFINALGEOM::GlobalFramePosition_Z[0][ptr->get()->get_arm()]-ptr->get()->get_trk_par_vtx()->get_z());
    fProjHitXVtx1 += ptr->get()->get_trk_par_vtx()->get_x();
    double fProjHitYVtx1=(ptr->get()->get_trk_par_vtx()->get_py()/ptr->get()->get_trk_par_vtx()->get_pz());
    fProjHitYVtx1 *= (RPCFINALGEOM::GlobalFramePosition_Z[0][ptr->get()->get_arm()]-ptr->get()->get_trk_par_vtx()->get_z());
    fProjHitYVtx1 += ptr->get()->get_trk_par_vtx()->get_y();

    TRpcClusMap::iterator clus_iter( _rpc_clus_map->range() );
    int counter_rpc_clus=0;
    
    float fLowDCA = -9999;
    //    TRpcClus *fLowClus = NULL;
    float fLowTime = -8888;
    //float fLowTimeError = -8888;

    float fLowDCAVtx = -9999;
    //    TRpcClus *fLowClusVtx = NULL;
    //This line taken out as the follow through code below was not used, and the scan build complained... and is SHOULD have been used (although the differences made would be infinitessimal in nature
    //    float fLowTimeVtx = -8888;
    //float fLowTimeErrorVtx = -8888;

    float fLowDCA1 = -9999;
    float fLowTime1 = -9999;
    float fLowDCAVtx1 = -9999;
    float fLowTimeVtx1 = -9999;
    
    while( TRpcClusMap::pointer clus_ptr = clus_iter.next() ) {
      if(clus_ptr->get()->get_arm()!=ptr->get()->get_arm()) { continue; }
      if(clus_ptr->get()->get_station()==2)
	{
	  PHKeyIterator<TRpcHit> str_iter = clus_ptr->get()->get_associated<TRpcHit>();
	  UShort_t fClusArm = clus_ptr->get()->get_arm();
	  UShort_t fClusSt  = clus_ptr->get()->get_station();
	  UShort_t fClusOct = clus_ptr->get()->get_octant();
	  UShort_t fClusHOct = clus_ptr->get()->get_half_octant();
	  UShort_t fClusRad = clus_ptr->get()->get_rseg();
	  float fClusTime = -1;
	  //This line taken out as the follow through code below was not used, and the scan build complained...
	  //	  float fClusTimeError = -1;
	  PHKeyIterator<TRpcCoord> coord_iter = clus_ptr->get()->get_associated<TRpcCoord>();
	  while(TRpcCoordMap::const_pointer coord_ptr = coord_iter.next()) {
	    fClusTime = coord_ptr->get()->get_t();
	  //This line taken out as the follow through code below was not used, and the scan build complained...
	    //fClusTimeError = coord_ptr->get()->get_t_error(); 

	  }
	  
	  RpcStrip *strip = new RpcStrip_v2();
	  int counter=0;
	  int counterVtx=0;
	  
	  while(TRpcHitMap::const_pointer str_ptr = str_iter.next()) {
	    int fcurr_strip = str_ptr->get()->get_strip();
	    strip->SetStrip(RPCFINALGEOM::ArmNumber(fClusArm),RPCFINALGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFINALGEOM::RadialSegment(fClusRad),fcurr_strip);
	    
	    float fDCA = strip->GetPointStripDCA(fProjHitX,fProjHitY,RPCFINALGEOM::ArmNumber(fClusArm),RPCFINALGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFINALGEOM::RadialSegment(fClusRad),fcurr_strip);
	    float fDCAVtx = strip->GetPointStripDCA(fProjHitXVtx,fProjHitYVtx,RPCFINALGEOM::ArmNumber(fClusArm),RPCFINALGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFINALGEOM::RadialSegment(fClusRad),fcurr_strip);
	    
	    if(fDCA>-998) { counter++; }
	    if(fDCAVtx>-998) { counterVtx++; }
	    
	    if(fDCA>fLowDCA) {
	      fLowDCA = fDCA;
	      //	      fLowClus = NULL;// clus_ptr->get();
	      fLowTime = fClusTime;
	      //fLowTimeError = fClusTimeError; 
	    }
	    if(fDCAVtx>fLowDCAVtx) {
	      fLowDCAVtx = fDCAVtx;
	      //	      fLowClusVtx = NULL; //clus_ptr->get();
	      //fLowTimeVtx = fClusTime; //SHOULD be used below??? was not
	      //fLowTimeErrorVtx = fClusTimeError; 
	    }
	    
	    counter_rpc_clus++; }//hits in cluster iterator
	  delete strip;
	}

      if(clus_ptr->get()->get_station()==0)//RPC1
	{
	  PHKeyIterator<TRpcHit> str_iter = clus_ptr->get()->get_associated<TRpcHit>();
	  UShort_t fClusArm = clus_ptr->get()->get_arm();
	  UShort_t fClusSt  = clus_ptr->get()->get_station();
	  UShort_t fClusOct = clus_ptr->get()->get_octant();
	  UShort_t fClusHOct = clus_ptr->get()->get_half_octant();
	  UShort_t fClusRad = clus_ptr->get()->get_rseg();
	  float fClusTime = -1;
	  //float fClusTimeError = -1;
	  PHKeyIterator<TRpcCoord> coord_iter = clus_ptr->get()->get_associated<TRpcCoord>();
	  while(TRpcCoordMap::const_pointer coord_ptr = coord_iter.next()) {
	    fClusTime = coord_ptr->get()->get_t();
	    //fClusTimeError = coord_ptr->get()->get_t_error();
	  }
	  
	  RpcStrip *strip = new RpcStrip_v2();
	  int counter=0;
	  int counterVtx=0;
	  
	  while(TRpcHitMap::const_pointer str_ptr = str_iter.next()) {
	    int fcurr_strip = str_ptr->get()->get_strip();
	    strip->SetStrip(RPCFINALGEOM::ArmNumber(fClusArm),RPCFINALGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFINALGEOM::RadialSegment(fClusRad),fcurr_strip);
	    
	    float fDCA = strip->GetPointStripDCA(fProjHitX1,fProjHitY1,RPCFINALGEOM::ArmNumber(fClusArm),RPCFINALGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFINALGEOM::RadialSegment(fClusRad),fcurr_strip);
	    float fDCAVtx = strip->GetPointStripDCA(fProjHitXVtx1,fProjHitYVtx1,RPCFINALGEOM::ArmNumber(fClusArm),RPCFINALGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFINALGEOM::RadialSegment(fClusRad),fcurr_strip);
	    
	    if(fDCA>-998) { counter++; }
	    if(fDCAVtx>-998) { counterVtx++; }
	    
	    if(fDCA>fLowDCA1) {
	      fLowDCA1 = fDCA;
	      fLowTime1 = fClusTime; }
	    if(fDCAVtx>fLowDCAVtx1) {
	      fLowDCAVtx1 = fDCAVtx;
	      fLowTimeVtx1 = fClusTime; }
	    
	    counter_rpc_clus++; }//hits in cluster iterator
	  delete strip;
	}


    }//rpc cluster iterator
    
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_x(fProjHitX);
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_y(fProjHitY);
    _rpc_trk_ptr->get()->set_trk_vtx_extrapolated_hit_x(fProjHitXVtx);
    _rpc_trk_ptr->get()->set_trk_vtx_extrapolated_hit_y(fProjHitYVtx);

    //Set track-position based variables
    if(fLowDCA>-998) {
      _rpc_trk_ptr->get()->set_dca_trk(fLowDCA);
      if(fLowDCAVtx>-998) {
	_rpc_trk_ptr->get()->set_dca_diff(fLowDCAVtx-fLowDCA); }
      _rpc_trk_ptr->get()->set_rpcclus3(NULL);//fLowClus);
      _rpc_trk_ptr->get()->set_rpcclus3time(fLowTime);
      _rpc_trk_ptr->get()->set_rpcclus3hitpos(-8888);
      _rpc_trk_ptr->get()->set_corr_pT(-8888); }
    else {
      _rpc_trk_ptr->get()->set_dca_trk(-9998);
      _rpc_trk_ptr->get()->set_dca_diff(-9998);
      _rpc_trk_ptr->get()->set_rpcclus3(NULL);
      _rpc_trk_ptr->get()->set_rpcclus3time(-9998);
      _rpc_trk_ptr->get()->set_rpcclus3hitpos(-9998);
      _rpc_trk_ptr->get()->set_corr_pT(-9998); }
    
    //Set vertex-position based variables
    if(fLowDCAVtx>-998) {
      _rpc_trk_ptr->get()->set_dca_trk_vtx(fLowDCAVtx);
      _rpc_trk_ptr->get()->set_rpcclus3_vtx(NULL);//fLowClusVtx);
      _rpc_trk_ptr->get()->set_rpcclus3time_vtx(fLowTime);//should be: fLowTimeVtx??
      _rpc_trk_ptr->get()->set_rpcclus3hitpos_vtx(-8888); }
    else {
      _rpc_trk_ptr->get()->set_dca_trk_vtx(-9998);
      _rpc_trk_ptr->get()->set_rpcclus3_vtx(NULL);
      _rpc_trk_ptr->get()->set_rpcclus3time_vtx(-9998);
      _rpc_trk_ptr->get()->set_rpcclus3hitpos_vtx(-9998); }
    
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_x1(fProjHitX1);
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_y1(fProjHitY1);
    _rpc_trk_ptr->get()->set_trk_vtx_extrapolated_hit_x1(fProjHitXVtx1);
    _rpc_trk_ptr->get()->set_trk_vtx_extrapolated_hit_y1(fProjHitYVtx1);

    //Set track-position based variables
    if(fLowDCA1>-998) {
      _rpc_trk_ptr->get()->set_dca_trk1(fLowDCA1);
      _rpc_trk_ptr->get()->set_rpcclus1time(fLowTime1); }
    else {
      _rpc_trk_ptr->get()->set_dca_trk1(-9998);
      _rpc_trk_ptr->get()->set_rpcclus1time(-9998); }
    
    //Set vertex-position based variables
    if(fLowDCAVtx1>-998) {
      _rpc_trk_ptr->get()->set_dca_trk_vtx1(fLowDCAVtx1);
      _rpc_trk_ptr->get()->set_rpcclus1time_vtx(fLowTimeVtx1); }
    else {
      _rpc_trk_ptr->get()->set_dca_trk_vtx1(-9998);
      _rpc_trk_ptr->get()->set_rpcclus1time_vtx(-9998); }
    
  }//mut track iterator
  
  return;
}
  
//______________________________________________________________________
void mRpcTrack::set_interface_ptrs(PHCompositeNode* top_node)
{	
  // IOC pointers
  _mui_road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");

  _mut_trk_map  = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  
  _rpc_trk_map  = TMutNode<TRpcTrkMap>::find_node(top_node,"TRpcTrkMap");
  
  _rpc_clus_map = TMutNode<TRpcClusMap>::find_node(top_node,"TRpcClusMap");
} 
