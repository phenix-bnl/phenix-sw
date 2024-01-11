// $Id: mFvtxFindCoord.h,v 1.5 2013/06/01 10:55:54 keyaaron Exp $ 

#ifndef __MFVTXFINDCOORD_H__
#define __MFVTXFINDCOORD_H__

/*!
  \file mFvtxFindCoord.h
  \brief create coordinates from clusters
  \author M. Brooks
  \version $Revision: 1.5 $
  \date $Date: 2013/06/01 10:55:54 $
*/


#include <mFvtxFindCoordPar.h>
#include "TFvtxHitMap.h"
#include "TFvtxClusMap.h"
#include "TFvtxCoordMap.h"
#include "SvxPisaHitv1.h"
#include "FVTXOO.h"
#include "FvtxGeom.h"
#include "TFvtxIndex.h"
#include <PHTimeServer.h>

#include <mFvtxModuleBase.h>


class mFvtxFindCoord : public mFvtxModuleBase
{
  public:

  //! constructor
  mFvtxFindCoord();

  //! destructor
  ~mFvtxFindCoord() {}

  //! event method
  PHBoolean event(PHCompositeNode* top_node);
  void init(PHCompositeNode* top_node){}; 
  void init_run(PHCompositeNode* top_node){}; 
  void end(PHCompositeNode* top_node);

  private:

  //! local copy of needed nodes
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! Find the coordinates for each cluster
  void find_coords();

  //! parameters module
  const mFvtxFindCoordPar* _mod_par;

  //! cluster map
  TFvtxClusMap* _clus_map;
  
  //! coord map
  TFvtxCoordMap* _coord_map;
 
  PHTimeServer::timer _timer;

};

#endif
  
