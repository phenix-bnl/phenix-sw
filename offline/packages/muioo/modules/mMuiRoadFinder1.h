#ifndef __MMUIROADFINDER1_H__
#define __MMUIROADFINDER1_H__

// $Id: mMuiRoadFinder1.h,v 1.3 2007/05/29 20:10:57 hpereira Exp $

/*!
	 \file mMuiRoadFinder1.h
	 \brief find road from clusters
	 \author S. Kelly D. Silvermyr
	 \version $Revision: 1.3 $
	 \date $Date: 2007/05/29 20:10:57 $
*/

#include "PHTimeServer.h"

#include <mMuiRoadFinder1Par.h>
#include <TMuiRoadMapO.h>
#include <TMui1DRoadMapO.h>
#include <TMuiClusterMapO.h>

/*! \@ingroup modules*/
//! find road from clusters
class mMuiRoadFinder1
{
 public:

	//! Constructor. 
	mMuiRoadFinder1();
 
	//! Destructor. 
	virtual ~mMuiRoadFinder1()
  {}

	//! Find roads in one event's worth of data. 
	PHBoolean event(PHCompositeNode* baseNode);

 private:
	//! Find 1d roads in both arms. 
	PHBoolean find_1droads();
	
	//! Find 2d roads in both arms. 
	PHBoolean find_2droads();		
	
	//! Make the first cuts (including exact duplicate) on 1d roads in both arms. 
	PHBoolean cut_1droads();
	
	//! Recursive method that tracks a stub to the next search gap. 
	int track_seed(TMui1DRoadMapO::pointer road1dptr, int iLoop, int iSearch);
	
	//! Find all the necessary maps on the node tree. 
	void set_interface_ptrs(PHCompositeNode* baseNode);
	
	//! Form a 2D road out of two 1D roads 
	int make2d(TMuiRoadMapO::pointer cRoad,
			 TMui1DRoadMapO::pointer pRoadH,
			 TMui1DRoadMapO::pointer pRoadV);
    
	//! Firt 1D road using its associated clusters and update its FitPar. 
	int fit1d(TMui1DRoadMapO::pointer road1dptr);
	
	//! Calculate the distance in 1d of the road with the unassociated cluster. 
	double ClusterDistance(TMui1DRoadMapO::pointer road1dptr, TMuiClusterMapO::pointer clustptr);
	
	//! Check that two clusters of opposite orientation actually intersect one another. 
	bool intersectionOK(TMuiClusterMapO::const_pointer clustH,
		TMuiClusterMapO::const_pointer clustV) const;
	
	//! Group all 2D Roads and one per group as golden. 
	int flag_golden();

  //! clone a track, copy cluster associations
  TMui1DRoadMapO::pointer bifurcate( TMui1DRoadMapO::pointer );
  
  //! module parameters
  const mMuiRoadFinder1Par* _mod_par;	
  
  //! pointer to road map
	TMuiRoadMapO* _road_map;
  
  //! pointer to 1D road map
	TMui1DRoadMapO* _road1d_map;
	
  //! pointer to cluster map
  TMuiClusterMapO* _cluster_map;
  
  //! module timer
	PHTimeServer::timer _timer;

};


#endif
