/*!
	\file mRpcRoad.cxx
	\brief Module to find and attach an RPC cluster to the MuiRoad object
	\author R. S. Hollis (rhollis@ucr.edu)
	\version $Revision: 1.3 $
	\date $Date: 2011/07/14 04:34:20 $
*/

#include "mRpcRoad.h"
#include "TRpcRoadMap.h"
#include "TRpcRoad_v1.h"
#include "TRpcHit_v1.h"
#include "TRpcHitMap.h"
#include "TMuiRoadMapO.h"
#include "RpcStrip_v1.h"
#include "RPCOO.h"
#include <iostream>

using namespace std;

//_____________________________________________
mRpcRoad::mRpcRoad() : 
	_timer( PHTimeServer::get()->insert_new("mRpcRoad") )
{
  RPCOO::TRACE("initializing module mRpcRoad",RPCOO::ALOT);	
}

//_____________________________________________
PHBoolean mRpcRoad::event(PHCompositeNode* top_node)
{
  //std::cout << "Building a road ... " << std::endl;
	_timer.get()->restart(); 
	try { 
				
		// Reset IOC pointers
		set_interface_ptrs(top_node);
		_rpc_road_map->clear();
		
		// Find and attach an RPC cluster to the MuiRoad
		attach_road();
		
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
void mRpcRoad::attach_road()
{
  TMuiRoadMapO::iterator iter( _mui_road_map->range() );
  int counter=0;

  while( TMuiRoadMapO::pointer ptr = iter.next() )
    {
      counter++;
      
      TRpcRoadMap::iterator thismap = _rpc_road_map->insert_new(ptr->get()->get_arm());
      //TRpcRoadMap::pointer _rpc_road_ptr = thismap.current();//this guy
      thismap.current()->get()->set_index(ptr->get()->get_index());
      thismap.current()->get()->set_arm(ptr->get()->get_arm());
      thismap.current()->get()->set_muiroad(ptr->get());//specifically this guy? - no
      
      //FOR NOW, let's assume a straight line track from the vertex
      double fProjHitX=(ptr->get()->get_gap0_point().getX()-0);
      fProjHitX /= (ptr->get()->get_gap0_point().getZ()-0);
      fProjHitX *= RPCFULLGEOM::GlobalFramePosition_Z[2][ptr->get()->get_arm()];
      double fProjHitY=(ptr->get()->get_gap0_point().getY()-0);
      fProjHitY /= (ptr->get()->get_gap0_point().getZ()-0);
      fProjHitY *= RPCFULLGEOM::GlobalFramePosition_Z[2][ptr->get()->get_arm()];
      TRpcClusMap::iterator clus_iter( _rpc_clus_map->range() );
      int counter_rpc_clus=0;
      
      float fLowDCA = -9999;
      TRpcClus *fLowClus = NULL;
      
      
      while( TRpcClusMap::pointer clus_ptr = clus_iter.next() ) {
	if(clus_ptr->get()->get_arm()!=ptr->get()->get_arm()) { continue; }
	if(clus_ptr->get()->get_station()!=2) { continue; }
	
	PHKeyIterator<TRpcHit> str_iter = clus_ptr->get()->get_associated<TRpcHit>();
	UShort_t fClusArm = clus_ptr->get()->get_arm();
	UShort_t fClusSt  = clus_ptr->get()->get_station();
	UShort_t fClusOct = clus_ptr->get()->get_octant();
	UShort_t fClusHOct = clus_ptr->get()->get_half_octant();
	UShort_t fClusRad = clus_ptr->get()->get_rseg();
	
	RpcStrip *strip = new RpcStrip_v1();
	int counter=0;
	while(TRpcHitMap::const_pointer str_ptr = str_iter.next()) {
	  int fcurr_strip = str_ptr->get()->get_strip();
	  strip->SetStrip(RPCFULLGEOM::ArmNumber(fClusArm),RPCFULLGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFULLGEOM::RadialSegment(fClusRad),fcurr_strip);
	  
	  float fDCA = strip->GetPointStripDCA(fProjHitX,fProjHitY,RPCFULLGEOM::ArmNumber(fClusArm),RPCFULLGEOM::StationNumber(fClusSt),fClusOct,fClusHOct,RPCFULLGEOM::RadialSegment(fClusRad),fcurr_strip);
	  if(fDCA>-998) { counter++; }
	  
	  if(fDCA>fLowDCA) {	    
	    fLowDCA = fDCA;
	    fLowClus = clus_ptr->get(); }

	  counter_rpc_clus++; }//hits in cluster iterator
	delete strip;
	
      }//rpc cluster iterator
      
      //cout << "***** " << fLowDCA << endl;
      if(fLowDCA>-998) {
	thismap.current()->get()->set_dca3(fLowDCA);
	thismap.current()->get()->set_rpcclus3(fLowClus); }
      else {
	thismap.current()->get()->set_dca3(-9999);
	thismap.current()->get()->set_rpcclus3(NULL); }	
      //delete _rpc_road_ptr;//does not like this ...
      thismap.next();
      //      thismap.next()->Clear();
    }//mui road iterator
  
  return;
}
  
//______________________________________________________________________
void mRpcRoad::set_interface_ptrs(PHCompositeNode* top_node)
{	
  // IOC pointers
  _mui_road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
  
  _rpc_road_map = TMutNode<TRpcRoadMap>::find_node(top_node,"TRpcRoadMap");
  
  _rpc_clus_map = TMutNode<TRpcClusMap>::find_node(top_node,"TRpcClusMap");
} 
