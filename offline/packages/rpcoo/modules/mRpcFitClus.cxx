// $Id: mRpcFitClus.cxx,v 1.9 2014/12/08 17:39:51 richi Exp $

/*!
  \file mRpcFitClus.cxx
  \brief rpc cluster fitting module. Generate TRpcCoord object from TRpcClus centroids
  \author H. Pereira Da Costa, modified by R.Hollis (ucr)
  \version $Revision: 1.9 $
  \date $Date: 2014/12/08 17:39:51 $
*/

#include "mRpcFitClus.h"
#include "RPCOO.h"
#include "TRpcHitMap.h"
#include "TRpcUtil.h"
#include <RpcStrip_proto.h>
#include <RpcStrip_v1.h>
#include <RpcStrip_v2.h>
#include <RPCFULLGEOM.h>
#include <RPCFINALGEOM.h>
#include <RpcGeom_v1.h>
#include <RpcGeom_v2.h>
#include <iostream>
#include <TMath.h>
#include <recoConsts.h>


using namespace std;

//_____________________________________________
mRpcFitClus::mRpcFitClus() : 
  _timer( PHTimeServer::get()->insert_new("mRpcFitClus") )
{
  RPCOO::TRACE("initializing module mRpcFitClus",RPCOO::ALOT);
}

//_____________________________________________
PHBoolean mRpcFitClus::event(PHCompositeNode* top_node)
{
  _timer.get()->restart();	 
  try { 

    // Reset IOC pointers
    set_interface_ptrs(top_node);
    _coord_map->clear();

    // Loop over TMutMCTrk and do the digitization
    cluster_loop();

  } catch(std::exception& e) {
    RPCOO::TRACE(e.what());
    return False;
  }	

  // If verbose dump the contents of trks and primary
  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= RPCOO::ALOT) _coord_map->print();
  return True;
}

//_____________________________________________
void mRpcFitClus::cluster_loop()
{

  // get iterator over all clusters
  TRpcClusMap::iterator iter( _clus_map->range() );
  while( TRpcClusMap::pointer ptr = iter.next() )
    { 
      fit_cluster( ptr );
      create_coord( ptr );
    }
}

//_____________________________________________
void mRpcFitClus::fit_cluster( TRpcClusMap::pointer clus_ptr )
{
  recoConsts *myrc = recoConsts::instance();
  /* 
     OLD:
       retrieve list of associated hits
       so far, we create one cluster centroid for each associated hit.
       The position is put at the center of the strip;
       the error is calculated from the strip dimensions / sqrt(12)
     NEW:
       retrieve list of associated hits
       average the strip information, equal weights as there is no 
       charge information
  */

  TRpcHitMap::const_key_iterator hit_iter( clus_ptr->get()->get_associated<TRpcHit>() );
  
  //The average strip number of current cluster
  int   fStripNumber = 0;
  //Redundant variables for the charge (not used)
  float fQ  = 0; float fQe = 0;
  //Strip time
  float fT  = 0; float fTe = 0;
  //Track the max and min times
  float fT_min = 1000, fT_max = -1000;
  //Strip position
  float mid_x = 0;
  float mid_y = 0;
  float mid_z = 0;
  float mid_width = 0;
  float mid_length = 0;
  //For Covariance Matrices
  float mid_r = 0;
  float mid_re = 0;
  float phi_e = 0;

  //Counter for the number of strips in the cluster;
  int fStripCounter = 0;

  while( TRpcHitMap::const_pointer hit_ptr = hit_iter.next() ) {
    fStripNumber += hit_ptr->get()->get_strip();
    fQ  += hit_ptr->get()->get_q();
    fQe += hit_ptr->get()->get_q_error();
    fT  += hit_ptr->get()->get_t();
    //fTe += hit_ptr->get()->get_t_error();
    if(hit_ptr->get()->get_t()<fT_min) { fT_min = hit_ptr->get()->get_t(); }
    if(hit_ptr->get()->get_t()>fT_max) { fT_max = hit_ptr->get()->get_t(); }
    
    //Get the Geometry Information:
    RpcStrip *fThisRpcStrip = NULL;
    if(myrc->get_IntFlag("RpcGeomType")==1) {
      fThisRpcStrip = new RpcStrip_proto(); }
    else if(myrc->get_IntFlag("RpcGeomType")==2) {
      fThisRpcStrip = new RpcStrip_v1(); }
    else if(myrc->get_IntFlag("RpcGeomType")==3) {
      fThisRpcStrip = new RpcStrip_v2(); }
    else {
      cout << "mRpcFitClus::fit_cluster"
	   << "Help, I'm stuck, don't know what RpcGeomType=="
	   << myrc->get_IntFlag("RpcGeomType") << " means" << endl;
      break; }
    if(!fThisRpcStrip) { cout << "no strip!" << endl; }
    fThisRpcStrip->SetStrip(hit_ptr->get()->get_arm(),
			    hit_ptr->get()->get_station(),
			    hit_ptr->get()->get_octant(),
			    hit_ptr->get()->get_half_octant(),
			    hit_ptr->get()->get_rseg(),
			    hit_ptr->get()->get_strip());
    
    mid_x += fThisRpcStrip->GetMid().getX();
    mid_y += fThisRpcStrip->GetMid().getY();
    mid_z += fThisRpcStrip->GetMid().getZ();
    mid_width += fThisRpcStrip->GetStripWidth();
    mid_length = TMath::Min(mid_length,float(fThisRpcStrip->GetStripLength()));
    delete fThisRpcStrip;
    fStripCounter++; }
  
  if(fStripCounter==0) {
    cout << "mRpcFitClus::fit_cluster"
	 << " - Error!! cluster with no hits" << endl;
    return; }

  fStripNumber /= fStripCounter;//has to be an integer
  fQ  /= fStripCounter;
  fQe /= fStripCounter;
  fT  /= fStripCounter;
  //fTe /= fStripCounter;
  mid_x /= fStripCounter;
  mid_y /= fStripCounter;
  mid_z /= fStripCounter;
  mid_width /= fStripCounter;
  
  //fTe is not actively filled, we set the error
  //to the maximum deviation from the average
  //for 1 strip clusters, this is zero
  fTe = TMath::Max(fabs(fT-fT_min),fabs(fT-fT_max));

  TRpcClusCentroid* centroid( clus_ptr->get()->insert_new_centroid( fStripNumber ) );
  centroid->set_q_peak( fQ ); 
  centroid->set_q_tot( fQ ); 
  centroid->set_q_tot_error( fQe ); 
  centroid->set_t( fT ); 
  centroid->set_t_error( fTe ); 
  centroid->set_x( mid_x );
  centroid->set_y( mid_y );

  // set covariance matrix error
  
  //here we use approximate size in r,phi to get error
  //this isn't quite right, since pads are square and don't lie
  //correctly in phi.  We use delta phi at the center of the strip
  //This needs to be fixed.
  
  // get r window for strip
  mid_re = mid_length/sqrt(12.0);
  
  // get phi window for strip--This is taken at the center of the strip
  // and is only approximate
  mid_r = sqrt(pow(mid_x,2)+pow(mid_y,2));
  
  phi_e = mid_width/mid_r;
  PHPoint mid(mid_x,mid_y,mid_z);
  
  PHGslMatrix m = TRpcUtil::get_covar_xy( mid, mid_re, phi_e );
  for( unsigned int i=0; i<TRpcClusCentroid::COVAR_ROW; i++ ) {
    for( unsigned int j=0; j<TRpcClusCentroid::COVAR_ROW; j++ ) {
      centroid->set_covar( i, j, m(i,j) ); } }
}

//_____________________________________________
void mRpcFitClus::create_coord( TRpcClusMap::pointer clus_ptr )
{
  recoConsts *myrc = recoConsts::instance();

  // retrieve list of centroids
  TRpcClus::centroid_list centroids( clus_ptr->get()->get_centroid_list() );
  for( TRpcClus::centroid_iterator iter = centroids.begin(); iter != centroids.end(); iter++ )
    {
	
      // insert new hit
      TRpcCoordMap::iterator coord_iter( _coord_map->insert_new( 
								clus_ptr->get()->get_arm(),
								clus_ptr->get()->get_station(),
								clus_ptr->get()->get_octant(),
								clus_ptr->get()->get_half_octant(),
								clus_ptr->get()->get_rseg()) );
		
      TRpcClusCentroid* centroid( *iter );
		
      // copy charge/time information
      coord_iter->get()->set_peak_strip( centroid->get_peak_strip() );
      coord_iter->get()->set_q_peak( centroid->get_q_peak() ); 
      coord_iter->get()->set_q_tot( centroid->get_q_tot() ); 
      coord_iter->get()->set_q_tot_error( centroid->get_q_tot_error() ); 
      coord_iter->get()->set_t( centroid->get_t() ); 
      coord_iter->get()->set_t_error( centroid->get_t_error() ); 
		
      // copy point position
      coord_iter->get()->set_x( centroid->get_x() );
      coord_iter->get()->set_y( centroid->get_y() );
      if(myrc->get_IntFlag("RpcGeomType")==3) {
	coord_iter->get()->set_z( 
				 RpcGeom_v2::get_arm( clus_ptr->get()->get_arm() )
				 ->get_station( clus_ptr->get()->get_station() )
				 ->get_z() ); }
      else {
	coord_iter->get()->set_z( 
				 RpcGeom_v1::get_arm( clus_ptr->get()->get_arm() )
				 ->get_station( clus_ptr->get()->get_station() )
				 ->get_z() ); }
		
      // copy covariance matrix
      for( unsigned int i=0; i<TRpcCoord::COVAR_ROW; i++ )
	for( unsigned int j=0; j<TRpcCoord::COVAR_ROW; j++ )
	  coord_iter->get()->set_covar( i, j, centroid->get_covar( i, j ) );
		
      // associate cluster and coordinate
      PHKey::associate( clus_ptr, coord_iter.current() );
	
    }
		
}

	
//______________________________________________________________________
/*! Reset IOC and external interface pointers */
void mRpcFitClus::set_interface_ptrs(PHCompositeNode* top_node)
{	
  // module runtime parameters
  _mod_par = TMutNode<mRpcFitClusPar>::find_node(top_node,"mRpcFitClusPar");

  // IOC pointers
  _clus_map = TMutNode<TRpcClusMap>::find_node(top_node,"TRpcClusMap");
  _coord_map = TMutNode<TRpcCoordMap>::find_node(top_node,"TRpcCoordMap");
} 

