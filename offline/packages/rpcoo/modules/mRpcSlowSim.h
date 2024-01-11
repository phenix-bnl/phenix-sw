// $Id: mRpcSlowSim.h,v 1.4 2010/06/27 02:52:13 richi Exp $
#ifndef _mRpcSlowSim_h_
#define _mRpcSlowSim_h_

/*!
	\file mRpcSlowSim.h
	\brief rpc simulation module. Generate TRpcMCHit from pisa hits.
	\author H. Pereira Da Costa
	\version $Revision: 1.4 $
	\date $Date: 2010/06/27 02:52:13 $
*/


// PHENIX includes
#include <PHVector.h>
#include <PHTimeServer.h>
#include <TMutMCTrkMap.h>
#include <boost/array.hpp>

#include "mRpcSlowSimPar.h"
#include "TRpcMCHitMap.h"

// include PISA staff table.
#include<mupcghitWrapper.h>

class PHCompositeNode;

/*! @ingroup modules */
//! rpc simulation module. Generate TRpcMCHit from pisa hits.
class mRpcSlowSim
{
 public: 

	//! constructor
	mRpcSlowSim(); 
 
	//! destructor
	virtual ~mRpcSlowSim(){}
	
	//! event method
	virtual PHBoolean event(PHCompositeNode*); 
	
	//! print summary of acceptance rejected hits
	void print_summary( std::ostream& out = std::cout );

 private:	

	//! get local pointer to usefull nodes
	void set_interface_ptrs(PHCompositeNode* top_node);

	//! create MC hits from pisa
	void simulator_loop( unsigned int station, mupcghitWrapper* mupcghit );

	//! create MC hits from thin air
	void noise_loop();

	//! get track momentum from pisa track id 
	static PHVector get_pisa_trk_momentum( int trk_id );

	//! associate MC tracks and hits
	void associate_mc_trk(TRpcMCHitMap::pointer);
	
	//! create a new MC track
	void fill_new_mc_trk(TRpcMCHitMap::pointer, int trackID = 0);
	 
	//! Module parameter table
	const mRpcSlowSimPar* _mod_par;					 

	//! Interface object container of roc MC hit.
	TRpcMCHitMap* _mc_hit_map;

	//! Interface object container of muid MC hit.
	TMutMCTrkMap* _mc_trk_map;
	
	//! station1 pisa hits
	mupcghitWrapper* _mupc1ghit;
	
	//! station2 pisa hits
	mupcghitWrapper* _mupc2ghit;
	
	//! station3 pisa hits
	mupcghitWrapper* _mupc3ghit;
	
	//! number of MC hits/arm/station
	boost::array< unsigned int, mRpcSlowSimPar::n_acceptance_parameters > _total_mc_hits;
	
	//! number of accepted MC hits/arm/station
	boost::array< unsigned int, mRpcSlowSimPar::n_acceptance_parameters > _accepted_mc_hits;
 
	//! Total number of noise hits
	Int_t fNumNoiseHits;
 
	//! Timer
	PHTimeServer::timer _timer;
};

#endif
