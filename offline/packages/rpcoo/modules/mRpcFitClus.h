// $Id: mRpcFitClus.h,v 1.2 2008/08/28 00:53:56 kempel Exp $
#ifndef __mRpcFitClus_h__
#define __mRpcFitClus_h__

/*!
  \file    mRpcFitClus.h
  \brief   rpc cluster fitting module. Generate TRpcCoord object from TMutClus centroids
  \author  H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:53:56 $
*/

// PHENIX includes
#include <PHPoint.h>

// Mutoo includes
#include <PHTimeServer.h>
#include <TMutMCTrkMap.h>

#include "mRpcFitClusPar.h"
#include "TRpcClusMap.h"
#include "TRpcCoordMap.h"

class PHCompositeNode;

//@{ 
/*! \ingroup modules */
//! rpc cluster fitting module. 
/*! rpc cluster fitting module. Generate TRpcCoord object from TMutClus centroids
	The current implementation is very basic. It creates one coordinate for each single cluster found.
	The coordinate center is taken at the center of the peak strip. 
	for rpc stations, the errors are derived from approximate strip theta/phi width/sqrt(12) this 
	isn't quite right and should be improved--since strips are actually rectangular and don't 
	sit in a consistent phi in the chambers.
*/
class mRpcFitClus
{
 public: 
  
	//! constructor
  mRpcFitClus(); 
 
 	//! destructor
  virtual ~mRpcFitClus(){}
	
	//! event method 
  virtual PHBoolean event(PHCompositeNode*);
	
 private:  

	//! get local pointers to needed nodes/maps
  void set_interface_ptrs(PHCompositeNode* top_node);

 	//! loop over MC hits
  void cluster_loop();

	//! fit the cluster
	/*! 
		fit the cluster. Fill TRpcClusCentroid list
  	current implementation creates one centroid per
		hit associated tot the cluster
	*/
	void fit_cluster(TRpcClusMap::pointer hit_ptr);

	//! create raw hits from MC hits
  void create_coord(TRpcClusMap::pointer hit_ptr);
 	
	//! parameter table
  const mRpcFitClusPar* _mod_par;           
		
	//! cluster container
  TRpcClusMap* _clus_map;                
		
	//! coordinate container
  TRpcCoordMap* _coord_map;                

  //! module timer
  PHTimeServer::timer _timer;
};

//@}

#endif
