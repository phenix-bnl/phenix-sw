// $Id: mFvtxFastSim.h,v 1.7 2015/06/11 20:59:17 snowball Exp $
#ifndef __mFvtxFastSim_h__
#define __mFvtxFastSim_h__

/*!
	\file mFvtxFastSim.h
	\brief fast forward vertex simulation module. Generate TFvtxMCHit from already generated TMutMCTrk
	\author H. Pereira Da Costa
	\version $Revision: 1.7 $
	\date $Date: 2015/06/11 20:59:17 $
*/

#include <map>
#include <boost/array.hpp>
#include <vector>
#include <PHPoint.h>
#include <PHTimeServer.h>
#include <TMutMCTrkMap.h>
#include <TFvtxMCHitMap.h>
#include <TTree.h>

#include "mFvtxFastSimPar.h"

// forward declaration
class TMutTrkPar;
class FvtxStation;
class PHTrackIntegratorKF;

//@{ 
/*! \ingroup modules */
//! fast rpc simulation module. Generate TFvtxMCHit from already generated TMutMCTrk
class mFvtxFastSim
{
	public: 
	
	//! constructor
	mFvtxFastSim(); 
 
 	//! destructor
	~mFvtxFastSim();
	
	//! event method 
	PHBoolean event(PHCompositeNode*);
	
	//! print summary of acceptance rejected hits
	void print_summary( std::ostream& out = std::cout );
	
	private:	

	//! get local pointers to needed nodes/maps
	void set_interface_ptrs(PHCompositeNode* top_node);
	
	//! loop over TMutMCTracks
	void simulator_loop( void );
	
	//! create TFvtxMCHit at each station from TMutMCTrack
	void trk_loop( TMutMCTrkMap::pointer trk_ptr );
	
	//! create TFvtxMCHit from track parameters and given station
	void create_mc_hits( const FvtxStation* station, TMutMCTrkMap::pointer trk_ptr, const TMutTrkPar& trk_par );
			
	//! parameter table
	const mFvtxFastSimPar* _mod_par;					 
	
	//! mc trks container
	TMutMCTrkMap* _mc_trk_map;								
	
	//! mc hits container
	TFvtxMCHitMap* _mc_hit_map;	
								
	//! number of MC hits/arm/station
	boost::array< unsigned int, mFvtxFastSimPar::n_acceptance_parameters > _total_mc_hits;
	
	//! number of accepted MC hits/arm/station
	boost::array< unsigned int, mFvtxFastSimPar::n_acceptance_parameters > _accepted_mc_hits;

	//! module timer
	PHTimeServer::timer _timer;

	//! Reference to PHTrackIntegratorKF
	PHTrackIntegratorKF *integrator;
};

//@}

#endif
