// $Id: MuiooReco.h,v 1.18 2013/02/26 17:24:00 silvermy Exp $
#ifndef __MUIOORECO_H__
#define __MUIOORECO_H__

/*!
  \file		MuiooReco.h
  \ingroup supermodules
  \brief	 muioo muid reconstruction event loop, starting for TMuiHitMapO
  \author	Sean Kelly
  \version $Revision: 1.18 $
  \date		$Date: 2013/02/26 17:24:00 $
*/

#include "MuonSubsysReco.h"
#include <MUTOO.h>

#ifndef __CINT__
#include <PHTimeServer.h>
#include <mMuiClusterFinder.h>
#include <mMuiRoadFinder1.h>
#include <mMuiFastRoadFinder.h>
#include <mMuiFindRoad.h>
#include <mMuiBLTEmulator.h>
#endif

class PHCompositeNode;


/*!
  \class	 MuiooReco
  \ingroup supermodules
  \brief	 muioo muid reconstruction event loop, starting for TMuiHitMapO
*/
class MuiooReco: public MuonSubsysReco

{
 public:

  // Mode to select which algorithm to run
  enum Flag
  {

    //! none
    NONE = 0,
    
   	//! The original/default version
    ROADORIG = 1<<0,

    //! New algorithm (under development), works similar to the mutoo stubfinder
    ROADNEW = 1<<1,
    
    //! enable BLT emulator
    BLT_EMULATOR_ENABLED = 1<<2,
    
    //! enable 
    LL1_EMULATOR_ENABLED = 1<<3
    
  };

  //! constructor
  MuiooReco();

  //! destructor
  virtual ~MuiooReco();

  //! initialization
  int Init(PHCompositeNode *topNode);

  //! run initialization
  int InitRun(PHCompositeNode *topNode);

  //! event method
  int process_event(PHCompositeNode *topNode);

  //! end of run
  int End(PHCompositeNode *topNode);

  //! flags
  void set_flags( const unsigned int& value )
  { _flags = value; }
  
  //! flags
  void set_flag( const Flag& flag, const bool& value )
  {
    if( value ) _flags |= flag;
    else _flags &= (~flag);
  }
    
  //! flags
  bool get_flag( const Flag& flag ) const
  { return _flags & flag; } 

  //! setter for verbosity 
  void set_verbosity(MUTOO::Verbosity v) { _verbosity = v; }

  //! set max occupancy per arm
  void set_max_occupancy_per_arm(UShort_t val)
  { _max_occupancy_per_arm = val;}

  //! set asymmetry cut parameter; used in check_for_sparks method
  void set_asymm_cut_par(UShort_t val)
  { _asymm_cut_par = val;}

  //! getter for verbosity 
  MUTOO::Verbosity get_verbosity() const { return _verbosity; }


  protected:

  //! create all needed nodes
  int CreateNodeTree(PHCompositeNode *topNode);

  int check_for_sparks(PHCompositeNode* top_node);

  //! algorihm flags
  /*! it is a bitwise or of the Flags enumaration */
  unsigned int _flags;

  //! muioo working node
  PHCompositeNode* muioo_node;

  #ifndef __CINT__

  //!@name reconstruction modules
  //@{

  //! cluster finding module
  mMuiClusterFinder mMuiClusterFinder_mod;

  //! road finding default module
  mMuiRoadFinder1 mMuiRoadFinder1_mod;

  //! fast (LL1) road finding module
  mMuiFastRoadFinder mMuiFastRoadFinder_mod;

  //! beta new road finding (similar to mutoo stub finder)
  mMuiFindRoad mMuiFindRoad_mod;

  //! blue logic trigger emulation
  mMuiBLTEmulator mMuiBLTEmulator_mod;

  //@}

  //! Timer
  PHTimeServer::timer _timer;
  #endif

  //! Verbosity Flag for messaging
  MUTOO::Verbosity _verbosity;

  UShort_t _max_occupancy_per_arm;
  UShort_t _asymm_cut_par;
};

#endif /* __MUIOORECO_H__ */
