// $Id: mRpcFindClus.h,v 1.4 2011/06/16 15:09:50 richi Exp $
#ifndef __mRpcFindClus_h__
#define __mRpcFindClus_h__

/*!
  \file    mRpcFindClus.h
  \brief   rpc cluster finding module. Generate TRpcClus object from adjacent TRcpHits.
	The current implementation is very basic. It creates one cluster for each single hit found 
	in the chambers.
  \author  H. Pereira Da Costa
  \version $Revision: 1.4 $
  \date    $Date: 2011/06/16 15:09:50 $
*/

// PHENIX includes
#include <PHPoint.h>

// Mutoo includes
#include <PHTimeServer.h>
#include <TMutMCTrkMap.h>

#include "mRpcFindClusPar.h"
#include "TRpcHitMap.h"
#include "TRpcClusMap.h"
#include "RpcDBInfo.h"

class PHCompositeNode;

//@{ 
/*! \ingroup modules */
//! rpc cluster finding module. 
/*! rpc cluster finding module. Generate TRpcClus object from adjacent TRcpHits.
	The current implementation is very basic. It creates one cluster for each single hit found 
	in the chambers.
*/
class mRpcFindClus
{
 public: 
  
	//! constructor
  mRpcFindClus(); 
 
 	//! destructor
  virtual ~mRpcFindClus(){}
	
	//! event method 
  virtual PHBoolean event(PHCompositeNode*);
  
 protected:
  
  RpcDBInfo *mydb;
  
 private:  

	//! get local pointers to needed nodes/maps
  void set_interface_ptrs(PHCompositeNode* top_node);

 	//! loop over MC hits
  void hit_loop();

	//! create raw hits from MC hits
  void create_cluster(TRpcHitMap::pointer hit_ptr);
 	
	//! check whether strips are adjacent
  bool adjacent_strips(TRpcHitMap::pointer hit_ptr, TRpcHitMap::pointer last_hit_ptr);

        //! check whether a strip is dead
  bool dead_check(TRpcHitMap::pointer hit_ptr);

	//! parameter table
  const mRpcFindClusPar* _mod_par;           
		
	//! raw hits container
  TRpcHitMap* _hit_map;                
		
	//! cluster container
  TRpcClusMap* _clus_map;                

  //! module timer
  PHTimeServer::timer _timer;
};

//@}

#endif
