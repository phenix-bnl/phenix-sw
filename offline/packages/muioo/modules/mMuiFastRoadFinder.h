#ifndef mMuiFastRoadFinder_h
#define mMuiFastRoadFinder_h

// $Id: mMuiFastRoadFinder.h,v 1.6 2009/06/09 17:20:07 hpereira Exp $

/*!
  \file	mMuiFastRoadFinder.h
  \brief  muid local level1 trigger emulator
  \author Hugo Pereira
  \version $Revision: 1.6 $
  \date	$Date: 2009/06/09 17:20:07 $
*/

#include <MUIOO.h>
#include <MuIDLl1.h>
#include <PHTimeServer.h>
#include "mMuiFastRoadFinderPar.h"

// forward declaration
class TMuiPseudoLL1Map;

/*! \@ingroup modules*/
/*! muid LL1 emulator */
class mMuiFastRoadFinder
{

 public:
	
  //! Constructor 
  mMuiFastRoadFinder();
  
  //! Destructor 
  virtual ~mMuiFastRoadFinder();
  
  //! Find roads in one event's worth of data.	 
  PHBoolean event(PHCompositeNode* top_node);
  
  //! Print info 
  void print(std::ostream& os = std::cout) const;
  
  //! ll1 decision
  int accept_event_decision(UShort_t arm);
  
 private:
  
  //! Find all the necessary maps on the node tree. 
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! fill pseudo-LL1 map
  void fill_map(UShort_t arm, int accept);
  
  //! ll1 emulator
  MuIDLl1& ll1_emulator( void );
  
  //! parameter table and IOCs 
  mMuiFastRoadFinderPar* _mod_par;

  //! muid trigger emulation internal module (from offline/packages/MuidTriggerEmulator)
  MuIDLl1* _ll1_emulator;

  //! trigger emulator decision map
  TMuiPseudoLL1Map* _ll1_map;
  
  //! timer
  PHTimeServer::timer _timer;	
  
  //! accept counters 
  int _naccept[MUIOO::MAX_ARM][mMuiFastRoadFinderPar::NMODES];

};


#endif 
