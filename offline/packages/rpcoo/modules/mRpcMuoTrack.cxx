/*!
	\file mRpcMuoTrack.cxx
	\brief Module to find and attach an RPC cluster to the MutTrk object
	\author R. S. Hollis (rhollis@ucr.edu)
	\version $Revision: 1.1 $
	\date $Date: 2012/04/03 18:48:46 $
*/

#include "mRpcMuoTrack.h"
#include "TRpcMuoTrkMap.h"
#include "TRpcMuoTrk_v1.h"
#include "TRpcHit_v1.h"
#include "TRpcHitMap.h"
#include "RpcStrip_v2.h"
#include <PHMuoTracksOut.h>
#include "RPCOO.h"
#include "RPCFINALGEOM.h"
#include "MWGCuts.h"
#include <iostream>

#include "getClass.h"

using namespace std;

//_____________________________________________
mRpcMuoTrack::mRpcMuoTrack() : 
        _timer( PHTimeServer::get()->insert_new("mRpcMuoTrack") )
{
        RPCOO::TRACE("initializing module mRpcMuoTrack",RPCOO::ALOT);
	_muocuts = new MWGCuts();
	_muocuts->init("none");
}

//_____________________________________________
PHBoolean mRpcMuoTrack::event(PHCompositeNode* top_node)
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
void mRpcMuoTrack::attach_track()
{
  if(!_muoo) {
    cout << PHWHERE << " Error - cannot find PHMuoTracksOut" << endl; 
    return; }

  cout << "There are " << _muoo->get_npart() << " tracks to match" << endl;
  //NEED TO ADD A TRACK NUMBER TO THE OUTPUT NODE

  for(uint itrk=0 ; itrk<_muoo->get_npart() ; itrk++) {
    //Set the arm
    int arm = (_muoo->get_pz(0,itrk)>0 ? 1:0);
    cout << "Arm ... " << arm << endl;
    //Insert a new RpcMuoTrk into the map
    TRpcMuoTrkMap::iterator thismap = _rpc_trk_map->insert_new(arm); 
    //Grab back the current RpcMuoTrk
    TRpcMuoTrkMap::pointer _rpc_trk_ptr = thismap.current();
    //    PHKey::associate( hit_ptr, _rpc_trk_ptr );    
    _rpc_trk_ptr->get()->set_muo_trk_number(itrk);

    float fRPC3z = RPCFINALGEOM::GlobalFramePosition_Z[2][arm];
    float fRPC1z = RPCFINALGEOM::GlobalFramePosition_Z[0][arm];

    //Load the bbc vertex
    /*float x_vtx = 0;
    float y_vtx = 0;
    float z_vtx = 0;*/

    //Grab momentum at vertex
    float x_vtx  = _muoo->get_xpos(0,itrk);
    float y_vtx  = _muoo->get_ypos(0,itrk);
    float z_vtx  = _muoo->get_zpos(0,itrk);
    float px_vtx = _muoo->get_px(0,itrk);
    float py_vtx = _muoo->get_py(0,itrk);
    float pz_vtx = _muoo->get_pz(0,itrk);
    _rpc_trk_ptr->get()->set_muo_trk_momentum(px_vtx,py_vtx,pz_vtx);
    
    //Take the hough angle from the momentum vector at it's vertex
    //multiply by the difference between the z position from the RPC
    //to the momentum vector's vertex Z.
    double fProjHitXVtx3 = (px_vtx/pz_vtx)*(fRPC3z-z_vtx) + x_vtx;
    double fProjHitYVtx3 = (py_vtx/pz_vtx)*(fRPC3z-z_vtx) + y_vtx;
    double fProjHitXVtx1 = (px_vtx/pz_vtx)*(fRPC1z-z_vtx) + x_vtx;
    double fProjHitYVtx1 = (py_vtx/pz_vtx)*(fRPC1z-z_vtx) + y_vtx;
    _rpc_trk_ptr->get()->set_vtx_extrapolated_hit_x(0,fProjHitXVtx1);
    _rpc_trk_ptr->get()->set_vtx_extrapolated_hit_y(0,fProjHitYVtx1);
    _rpc_trk_ptr->get()->set_vtx_extrapolated_hit_x(1,fProjHitXVtx3);
    _rpc_trk_ptr->get()->set_vtx_extrapolated_hit_y(1,fProjHitYVtx3);
    
    cout << "Projected RPC3 = " << fProjHitXVtx3 << " " << fProjHitYVtx3 << endl;
    cout << "Projected RPC1 = " << fProjHitXVtx1 << " " << fProjHitYVtx1 << endl;
    
    //Grab hit position & momentum at mutr St1
    float x_st1  = _muoo->get_xpos(1,itrk);
    float y_st1  = _muoo->get_ypos(1,itrk);
    float z_st1  = _muoo->get_zpos(1,itrk);
    float px_st1 = _muoo->get_px(1,itrk);
    float py_st1 = _muoo->get_py(1,itrk);
    float pz_st1 = _muoo->get_pz(1,itrk);
    
    double fProjHitXSt13 = (px_st1/pz_st1)*(fRPC3z-z_st1) + x_st1;
    double fProjHitYSt13 = (py_st1/pz_st1)*(fRPC3z-z_st1) + y_st1;
    double fProjHitXSt11 = (px_st1/pz_st1)*(fRPC1z-z_st1) + x_st1;
    double fProjHitYSt11 = (py_st1/pz_st1)*(fRPC1z-z_st1) + y_st1;
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_x(0,0,fProjHitXSt11);
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_y(0,0,fProjHitYSt11);
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_x(0,1,fProjHitXSt13);
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_y(0,1,fProjHitYSt13);
    
    cout << "Projected RPC3 = " << fProjHitXSt13 << " " << fProjHitYSt13 << endl;
    cout << "Projected RPC1 = " << fProjHitXSt11 << " " << fProjHitYSt11 << endl;
    
    //Grab hit position & momentum at mutr St3
    float x_st3  = _muoo->get_xpos(3,itrk);
    float y_st3  = _muoo->get_ypos(3,itrk);
    float z_st3  = _muoo->get_zpos(3,itrk);
    float px_st3 = _muoo->get_px(3,itrk);
    float py_st3 = _muoo->get_py(3,itrk);
    float pz_st3 = _muoo->get_pz(3,itrk);
    
    double fProjHitXSt33 = (px_st3/pz_st3)*(fRPC3z-z_st3) + x_st3;
    double fProjHitYSt33 = (py_st3/pz_st3)*(fRPC3z-z_st3) + y_st3;
    double fProjHitXSt31 = (px_st3/pz_st3)*(fRPC1z-z_st3) + x_st3;
    double fProjHitYSt31 = (py_st3/pz_st3)*(fRPC1z-z_st3) + y_st3;
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_x(1,0,fProjHitXSt31);
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_y(1,0,fProjHitYSt31);
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_x(1,1,fProjHitXSt33);
    _rpc_trk_ptr->get()->set_trk_extrapolated_hit_y(1,1,fProjHitYSt33);
    
    cout << "Projected RPC3 = " << fProjHitXSt33 << " " << fProjHitYSt33 << endl;
    cout << "Projected RPC1 = " << fProjHitXSt31 << " " << fProjHitYSt31 << endl;
    
    //Grab hit position & direction at muid
    int iroad = _muocuts->get_best_road_oo( itrk, _muoo );
    double fProjHitXMUID3 = -9999;
    double fProjHitYMUID3 = -9999;
    double fProjHitXMUID1 = -9999;
    double fProjHitYMUID1 = -9999;
    
    if(iroad>=0) 
      {
	float x_muid   = _muoo->get_muIDOO_gap0(0, iroad, itrk);
	float y_muid   = _muoo->get_muIDOO_gap0(1, iroad, itrk);
	float z_muid   = _muoo->get_muIDOO_gap0(2, iroad, itrk);
	float pxz_muid = _muoo->get_muIDOO_gap0(3, iroad, itrk);
	float pyz_muid = _muoo->get_muIDOO_gap0(4, iroad, itrk);
	fProjHitXMUID3 = pxz_muid*(fRPC3z-z_muid) + x_muid;
	fProjHitYMUID3 = pyz_muid*(fRPC3z-z_muid) + y_muid;
	fProjHitXMUID1 = pxz_muid*(fRPC1z-z_muid) + x_muid;
	fProjHitYMUID1 = pyz_muid*(fRPC1z-z_muid) + y_muid;
      }  
    else 
      {
	cout << PHWHERE << " Could not find a \"best\" road" << endl; 
      }
    _rpc_trk_ptr->get()->set_muid_extrapolated_hit_x(0,fProjHitXMUID1);
    _rpc_trk_ptr->get()->set_muid_extrapolated_hit_y(0,fProjHitYMUID1);
    _rpc_trk_ptr->get()->set_muid_extrapolated_hit_x(1,fProjHitXMUID3);
    _rpc_trk_ptr->get()->set_muid_extrapolated_hit_y(1,fProjHitYMUID3);
    
    cout << "Projected RPC3 = " << fProjHitXMUID3 << " " << fProjHitYMUID3 << endl;
    cout << "Projected RPC1 = " << fProjHitXMUID1 << " " << fProjHitYMUID1 << endl;

    //--------------------------
    // iterate over all clusters
    //--------------------------
    TRpcClusMap::iterator clus_iter( _rpc_clus_map->range() );
    
    float fLowDCATrk[2][2]       = {{9999,9999},{9999,9999}};
    float fLowTimeTrk[2][2]      = {{8888,8888},{8888,8888}};
    float fLowTimeErrorTrk[2][2] = {{8888,8888},{8888,8888}};

    float fLowDCAVtx[2]       = {8888,8888};
    float fLowTimeVtx[2]      = {8888,8888};
    float fLowTimeErrorVtx[2] = {8888,8888};

    float fLowDCAMuID[2]       = {8888,8888};
    float fLowTimeMuID[2]      = {8888,8888};
    float fLowTimeErrorMuID[2] = {8888,8888};
    
    int clus_counter[2] = {0,0};
    int clus_counter_reject[2] = {0,0};
    
    while( TRpcClusMap::pointer clus_ptr = clus_iter.next() ) {
      int rpcstation = (clus_ptr->get()->get_station()==0 ? 0:1);
      clus_counter[rpcstation]++;
      if(clus_ptr->get()->get_arm()!=arm) {
	clus_counter_reject[rpcstation]++; continue; }
      
      PHKeyIterator<TRpcHit> str_iter = clus_ptr->get()->get_associated<TRpcHit>();
      UShort_t fClusArm = clus_ptr->get()->get_arm();
      UShort_t fClusSt  = clus_ptr->get()->get_station();
      UShort_t fClusOct = clus_ptr->get()->get_octant();
      UShort_t fClusHOct = clus_ptr->get()->get_half_octant();
      UShort_t fClusRad = clus_ptr->get()->get_rseg();
      float fClusTime = -1;
      float fClusTimeError = -1;
      PHKeyIterator<TRpcCoord> coord_iter = clus_ptr->get()->get_associated<TRpcCoord>();
      while(TRpcCoordMap::const_pointer coord_ptr = coord_iter.next()) {
	fClusTime = coord_ptr->get()->get_t();
	fClusTimeError = coord_ptr->get()->get_t_error(); }

      RpcStrip *strip = new RpcStrip_v2();

      while(TRpcHitMap::const_pointer str_ptr = str_iter.next()) {
	int fcurr_strip = str_ptr->get()->get_strip();
	/*	cout << RPCFINALGEOM::ArmNumber(fClusArm) << " "
	  << RPCFINALGEOM::StationNumber(fClusSt) << " "
	  << fClusOct << " " << fClusHOct << " " 
	  << RPCFINALGEOM::RadialSegment(fClusRad) << " "
	  << fcurr_strip << " time " << fClusTime<< endl; */
	strip->SetStrip(RPCFINALGEOM::ArmNumber(fClusArm),RPCFINALGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFINALGEOM::RadialSegment(fClusRad),fcurr_strip);
	
	//=======
	//Check Vtx
	//=======
	double fProjX = (rpcstation==0 ? fProjHitXVtx1:fProjHitXVtx3);
	double fProjY = (rpcstation==0 ? fProjHitYVtx1:fProjHitYVtx3);
	float fDCA = fabs(strip->GetPointStripDCA(fProjX,fProjY,RPCFINALGEOM::ArmNumber(fClusArm),RPCFINALGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFINALGEOM::RadialSegment(fClusRad),fcurr_strip));
	if(fDCA<fLowDCAVtx[rpcstation]) {
	  fLowDCAVtx[rpcstation]       = fDCA;
	  if(fDCA<998) {
	    fLowTimeVtx[rpcstation]      = fClusTime;
	    fLowTimeErrorVtx[rpcstation] = fClusTimeError; } }

	//=======
	//Check MuTr Station 1
	//=======
	fProjX = (rpcstation==0 ? fProjHitXSt11:fProjHitXSt13);
	fProjY = (rpcstation==0 ? fProjHitYSt11:fProjHitYSt13);
	fDCA = fabs(strip->GetPointStripDCA(fProjX,fProjY,RPCFULLGEOM::ArmNumber(fClusArm),RPCFULLGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFULLGEOM::RadialSegment(fClusRad),fcurr_strip));
	if(fDCA<fLowDCATrk[0][rpcstation]) {
	  fLowDCATrk[0][rpcstation]       = fDCA;
	  if(fDCA<998) {
	    fLowTimeTrk[0][rpcstation]      = fClusTime;
	    fLowTimeErrorTrk[0][rpcstation] = fClusTimeError; } }

	//=======
	//Check MuTr Station 3
	//=======
	fProjX = (rpcstation==0 ? fProjHitXSt31:fProjHitXSt33);
	fProjY = (rpcstation==0 ? fProjHitYSt31:fProjHitYSt33);
	fDCA = fabs(strip->GetPointStripDCA(fProjX,fProjY,RPCFULLGEOM::ArmNumber(fClusArm),RPCFULLGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFULLGEOM::RadialSegment(fClusRad),fcurr_strip));
	if(fDCA<fLowDCATrk[1][rpcstation]) {
	  fLowDCATrk[1][rpcstation]       = fDCA;
	  if(fDCA<998) {
	    fLowTimeTrk[1][rpcstation]      = fClusTime;
	    fLowTimeErrorTrk[1][rpcstation] = fClusTimeError; } }

	//=======
	//Check MuID
	//=======
	fProjX = (rpcstation==0 ? fProjHitXMUID1:fProjHitXMUID3);
	fProjY = (rpcstation==0 ? fProjHitYMUID1:fProjHitYMUID3);
	fDCA = fabs(strip->GetPointStripDCA(fProjX,fProjY,RPCFULLGEOM::ArmNumber(fClusArm),RPCFULLGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFULLGEOM::RadialSegment(fClusRad),fcurr_strip));
	if(fDCA<fLowDCAMuID[rpcstation]) {
	  fLowDCAMuID[rpcstation]       = fDCA;
	  if(fDCA<998) {
	    fLowTimeMuID[rpcstation]      = fClusTime;
	    fLowTimeErrorMuID[rpcstation] = fClusTimeError; } }

      }//hits in cluster iterator
      delete strip;
    }//rpc cluster iterator
    
    /*
      cout << "Number of Clusters checked:  " << clus_counter[0] << " " << clus_counter[1] << endl;
      cout << "Number of Clusters rejected: " << clus_counter_reject[0] << " " << clus_counter_reject[1] << endl;
      cout << "MYDCA " << fLowDCATrk[0][0] << " " << fLowDCATrk[1][0] << " "
      << fLowDCAVtx[0] << " " << fLowDCAMuID[0] << " " << endl;
      
      cout << "MYDCA " << fLowDCATrk[0][1] << " " << fLowDCATrk[1][1] << " "
      << fLowDCAVtx[1] << " " << fLowDCAMuID[1] << " " << endl;
    */
  
    for(int irpc=0 ; irpc<2 ; irpc++)
      {
	for(int imutr=0 ; imutr<2 ; imutr++)
	  {
	    _rpc_trk_ptr->get()->set_dca_trk(imutr,irpc,fLowDCATrk[imutr][irpc]);
	    _rpc_trk_ptr->get()->set_rpcclus_time(imutr,irpc,fLowTimeTrk[imutr][irpc]);
	    _rpc_trk_ptr->get()->set_rpcclus_hitpos(imutr,irpc,-999);
	  }//mutr loop
	_rpc_trk_ptr->get()->set_dca_vtx(irpc,fLowDCAVtx[irpc]);
	_rpc_trk_ptr->get()->set_rpcclus_time_vtx(irpc,fLowTimeVtx[irpc]);
	_rpc_trk_ptr->get()->set_rpcclus_hitpos_vtx(irpc,-999);
	_rpc_trk_ptr->get()->set_dca_muid(irpc,fLowDCAMuID[irpc]);
	_rpc_trk_ptr->get()->set_rpcclus_time_muid(irpc,fLowTimeMuID[irpc]);
	_rpc_trk_ptr->get()->set_rpcclus_hitpos_muid(irpc,-999);
      }//rpc loop
    
    cout << "RPC1vtx " << sqrt(pow(px_vtx,2)+pow(py_vtx,2)) << " " << fLowDCAVtx[0] << " " << fLowTimeVtx[0] << endl;
    cout << "RPC3vtx " << fLowDCAVtx[1] << " " << fLowTimeVtx[1] << endl;
    
  }//mut track iterator
  
  return;
}
  
//______________________________________________________________________
void mRpcMuoTrack::set_interface_ptrs(PHCompositeNode* top_node)
{	
  // IOC pointers
  _rpc_trk_map  = TMutNode<TRpcMuoTrkMap>::find_node(top_node,"TRpcMuoTrkMap");
  
  _rpc_clus_map = TMutNode<TRpcClusMap>::find_node(top_node,"TRpcClusMap");

  _muoo = TMutNode<PHMuoTracksOut>::find_io_node(top_node,"PHMuoTracksOO");

  //_muocuts = TMutNode<PHInclusiveNanoCuts>::find_io_node(top_node,"MWGCuts");
} 
