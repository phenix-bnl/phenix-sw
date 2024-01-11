#ifndef __MMUICLUSTERFINDER_H__
#define __MMUICLUSTERFINDER_H__

// $Id: mMuiClusterFinder.h,v 1.3 2007/05/05 17:48:00 hpereira Exp $

/*!
  \file    mMuiClusterFinder.h
  \brief   muid cluster finder
  \author  Jason Newby, Hugo Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2007/05/05 17:48:00 $
*/

#include "PHTimeServer.h"
#include "MuiGeomClasses.hh"


#include <mMuiClusterFinderPar.h>
#include <TMuiClusterMapO.h>
#include <TMuiHitMapO.h>

class PHCompositeNode;

/*! \@ingroup modules*/
//! muid cluster finder
class mMuiClusterFinder
{
 public:
		 
  //! Constructor
  mMuiClusterFinder();
 
  //! Destructor
  virtual ~mMuiClusterFinder()
  {}

  //! event method
  PHBoolean event(PHCompositeNode *);

 private:
  
  // private methods
	//! Reset IOC and external interface pointers 
  void set_interface_ptrs(PHCompositeNode* top_node);
  
	//! Find clusters from raw hits
  void find_clusters();

	//! create a new cluster from a TMuiHitO
  TMuiClusterMapO::pointer make_new_cluster(TMuiHitMapO::pointer);

	//! Calculate position of reconstructed clusters
  void calc_position(TMuiClusterMapO::pointer clus_ptr, TMuiPanelGeo *fpMuiPanelGeo);
	
	//! split clusters which are too big in two or more
  void split_cluster(TMuiClusterMapO::pointer clus_ptr);
	
  // Interface pointers
	//! parameter table
  const mMuiClusterFinderPar* _mod_par;
	
	//! muid hit map
  TMuiHitMapO* _mui_hit_map;
	
	//! muid cluster map
  TMuiClusterMapO* _mui_cluster_map;

	//! module timer
  PHTimeServer::timer _timer;

};

#endif /*__MMUICLUSTERFINDER_H__*/
