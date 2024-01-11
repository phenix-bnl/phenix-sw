// $Id: mRpcFindClus.cxx,v 1.8 2015/02/09 16:09:16 richi Exp $

/*!
	\file mRpcFindClus.cxx
	\brief rpc cluster finding module. Generate TRpcClus object from adjacent TRcpHits.
	The current implementation is very basic. It creates one cluster for each single hit found 
	in the chambers.
	\author H. Pereira Da Costa
	\version $Revision: 1.8 $
	\date $Date: 2015/02/09 16:09:16 $
*/


#include "mRpcFindClus.h"
#include "RPCOO.h"
#include "RpcStrip_v1.h"
#include <iostream>

using namespace std;

//_____________________________________________
mRpcFindClus::mRpcFindClus() : 
	_timer( PHTimeServer::get()->insert_new("mRpcFindClus") )
{
        RPCOO::TRACE("initializing module mRpcFindClus",RPCOO::ALOT);

	mydb = RpcDBInfo::getInstance();
}

//_____________________________________________
PHBoolean mRpcFindClus::event(PHCompositeNode* top_node)
{
	_timer.get()->restart();	 
	try { 

		// Reset IOC pointers
		set_interface_ptrs(top_node);
		_clus_map->clear();
				
		// Loop over TMutMCTrk and do the digitization
		hit_loop();

	} catch(std::exception& e) {
		RPCOO::TRACE(e.what());
		return False;
	}	

	// If verbose dump the contents of trks and primary
	//
	_timer.get()->stop();
	if(_mod_par->get_verbosity() >= RPCOO::ALOT) _clus_map->print();
	return True;
}

//_____________________________________________
void mRpcFindClus::hit_loop()
{
	// get iterator over all tracks
	TRpcHitMap::iterator iter( _hit_map->range() );
	TRpcClusMap::pointer current_cluster_ptr = NULL;
	
	int fMaxClusterSize = 4;
	int fCurrentClusterSize=0;

	TRpcHitMap::pointer last_hit_ptr = NULL;
	bool kClusterCandidate = false;
	bool kCreateNewCluster = true;
	
	while( TRpcHitMap::pointer hit_ptr = iter.next() ) {
	  //-----------------
	  // Reject Dead Channels, start a new cluster
	  //-----------------
	  if(dead_check(hit_ptr)) { continue; }
	  
	  //-----------------
	  // Reject Out-Of-Time Hits
	  //-----------------
	  double fMinTDCTime = 0;
	  double fMaxTDCTime = 44;
	  if(hit_ptr->get()->get_t()<fMinTDCTime || hit_ptr->get()->get_t()>fMaxTDCTime) { continue; }
	  
	  
	  kClusterCandidate = true;
	  kCreateNewCluster = true;
	  if(_mod_par->get_verbosity() >= RPCOO::SOME) {
	    cout << hit_ptr->get()->get_arm()    << " " << hit_ptr->get()->get_station()<< " "
		 << hit_ptr->get()->get_octant() << " " << hit_ptr->get()->get_half_octant() << " "
		 << hit_ptr->get()->get_rseg()   << " " << hit_ptr->get()->get_strip() << " ... "; }
	    
	  if(last_hit_ptr!=NULL) {
	    if(hit_ptr->get()->get_arm()         != last_hit_ptr->get()->get_arm())           { kClusterCandidate=false; }
	    if(hit_ptr->get()->get_station()     != last_hit_ptr->get()->get_station())       { kClusterCandidate=false; }
	    if(hit_ptr->get()->get_octant()      != last_hit_ptr->get()->get_octant())        { kClusterCandidate=false; }
	    if(hit_ptr->get()->get_half_octant() != last_hit_ptr->get()->get_half_octant())   { kClusterCandidate=false; }
	    if(hit_ptr->get()->get_rseg()        != last_hit_ptr->get()->get_rseg())          { kClusterCandidate=false; }
	    if(!adjacent_strips(hit_ptr,last_hit_ptr))                                        { kClusterCandidate=false; }
	    
	    if(kClusterCandidate) {
	      if(_mod_par->get_verbosity() >= RPCOO::SOME) {
		cout << hit_ptr->get()->get_strip()-last_hit_ptr->get()->get_strip(); }
	      kCreateNewCluster = false;}
	  }
	  
	  if(kCreateNewCluster || fCurrentClusterSize>=fMaxClusterSize) {
	    if(_mod_par->get_verbosity() >= RPCOO::SOME) {
	      cout << "New Cluster!"; }
	    //Insert a new cluster
	    current_cluster_ptr = _clus_map->insert_new(hit_ptr->get()->get_arm(),
							hit_ptr->get()->get_station(),
							hit_ptr->get()->get_octant(),
							hit_ptr->get()->get_half_octant(),
							hit_ptr->get()->get_rseg()).current();
	    fCurrentClusterSize = 0; }
	  // associate hit and cluster hit
	  PHKey::associate( hit_ptr, current_cluster_ptr );
	  fCurrentClusterSize++;
	  
	  /* PHConstKeyIterator<TRpcHit> iter = current_cluster_ptr->get()->get_associated<TRpcHit>();
	  int counter=0;
	  while(iter.next()) {
	    counter++;
	    
	  }
	  
	  cout << counter << endl; */
	  last_hit_ptr = hit_ptr;
	  if(_mod_par->get_verbosity() >= RPCOO::ALOT) { 
	    cout << endl; }
	}
}


//_____________________________________________
bool mRpcFindClus::adjacent_strips(TRpcHitMap::pointer hit_ptr, TRpcHitMap::pointer last_hit_ptr)
{
  //Simplest case: when two strips are next to each other
  if(abs(hit_ptr->get()->get_strip()-last_hit_ptr->get()->get_strip())==1) { return true; }
  
  //NEED TO ADD:
  // - contingency for dead/hot channels
  // - a check for adjacent hits when strip numbers are not contiguous (e.g. for inner/out matching)
  //see mMutFindClus.cxx for example of how to excuse a hit
  return false;
}

bool mRpcFindClus::dead_check(TRpcHitMap::pointer hit_ptr)
{
  //Return 1 if the strip is dead...
  // Maybe this code should be moved into the RpcHit::Status, and just call status??
  bool is_dead = false;
  
  RpcStrip *strip = new RpcStrip_v1();
  strip->SetStrip(hit_ptr->get()->get_arm(),
		  hit_ptr->get()->get_station(),
		  hit_ptr->get()->get_octant(),
		  hit_ptr->get()->get_half_octant(),
		  hit_ptr->get()->get_rseg(),
		  hit_ptr->get()->get_strip());
  
  if(mydb->isDead(strip)==1) { is_dead = true; }
  
  delete strip;
  
  return is_dead;
}

//_____________________________________________
void mRpcFindClus::create_cluster( TRpcHitMap::pointer hit_ptr )
{
  return; //is obsolete code now
	/* 
		so far we create one Cluster for each found hit.
		No real clustering. Should put real algorithm here to look for 
		adjacent hits. Easy way would be to look over existing clusters,
		see if hit is adjacent to one of the already associated hits,
		associate it to found clusters, if any. Create a new cluster otherwise
	*/


	//	insert new cluster
  /*coverity
	TRpcClusMap::iterator clus_iter( _clus_map->insert_new( 
				hit_ptr->get()->get_arm(),
				hit_ptr->get()->get_station(),
				hit_ptr->get()->get_octant(),
				hit_ptr->get()->get_half_octant(),
				hit_ptr->get()->get_rseg()) );
		
	// associate hit and cluster hit
	PHKey::associate( hit_ptr, clus_iter.current() );
  */	
}

	
//______________________________________________________________________
/*! Reset IOC and external interface pointers */
void mRpcFindClus::set_interface_ptrs(PHCompositeNode* top_node)
{	
	// module runtime parameters
	_mod_par = TMutNode<mRpcFindClusPar>::find_node(top_node,"mRpcFindClusPar");

	// IOC pointers
	_hit_map = TMutNode<TRpcHitMap>::find_node(top_node,"TRpcHitMap");
	_clus_map = TMutNode<TRpcClusMap>::find_node(top_node,"TRpcClusMap");
} 

