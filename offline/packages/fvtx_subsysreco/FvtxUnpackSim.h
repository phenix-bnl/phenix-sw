// $Id: FvtxUnpackSim.h,v 1.7 2009/09/24 09:16:37 hpereira Exp $
#ifndef __FvtxUnpackSim_h__
#define __FvtxUnpackSim_h__

/*!
  \file FvtxUnpackSim.h
  \ingroup supermodules
  \brief FVTX Fast simulation module. Reads a slowsim DST. Creates TFvtx MC Hits
  from MC tracks. Runs the restonse to create Fvtx hits for later reconstruction
  \warning there is no embedding done so far in this module.
  \author X.R. Wang
  \version $Revision: 1.7 $
  \date $Date: 2009/09/24 09:16:37 $
*/

#include <string>
#include <MuonSubsysReco.h>

// Forward declerations
class PHCompositeNode;
class PHTimer;

#ifndef __CINT__
#include <mFvtxResponse.h>
#include <mFvtxEmbed.h>
#endif

//! FVTX Fast Simulation module
/*!
  \ingroup supermodules
  FVTX fast simulator. reads muid/mutr mc hit/track maps from a simulated DST
  runs the response to build muid/mutr hit maps for later reconstruction.
  \warning there is no embedding done so far in this module.
*/
class FvtxUnpackSim: public MuonSubsysReco
{
  public:

  //! embedding mode
  enum Mode {

    //! embed MC into real data background
    MC_SIGNAL_REAL_BG,

    //! embed MC into MC background
    MC_SIGNAL_MC_BG,

    //! no background
    MC_SIGNAL_NO_BG

  };

  //! constructor
  FvtxUnpackSim( const char* name= "FVTXUNPACKSIM", unsigned int mode = MC_SIGNAL_NO_BG );

  //! run initialization
  int InitRun(PHCompositeNode *topNode);

  //! event processing
  int process_event(PHCompositeNode *topNode);

  //! end of process
  int End(PHCompositeNode *topNode);

  //! changes embedding mode
  void SetMode(unsigned int mode);

  // ! changes signal node name
  void SetSignalNodeName(std::string name) { _signalNodeName = name; }

  // ! changes background node name
  void SetBackgroundNodeName(std::string name) { _backgroundNodeName = name; }

  //! retrieves embedding mode
  unsigned int GetMode() const
  { return _mode; }

  protected:

  //! create all new nodes
  int set_node_ptrs(PHCompositeNode *topNode);

  //! create needed interfaces
  int set_interface_ptrs(PHCompositeNode *topNode);

  //! create needed modules
  int set_module_ptrs(PHCompositeNode *topNode);

  //! fvtx working node
  PHCompositeNode* _fvtx_node;

  // Nodes for input signal
  //! signal node for MC DST
  std::string _signalNodeName;
  PHCompositeNode* _signal_node;

  //! background node for MC/RD DST
  PHCompositeNode* _ioc_signal_node;

  // Nodes for input background
  //! internal signal node (for response)
  std::string _backgroundNodeName;
  PHCompositeNode* _background_node;

  //! internal background node
  PHCompositeNode* _ioc_background_node;

  #ifndef __CINT__

  //! FVTX module data members
  mFvtxResponse _mFvtxResponse_mod;

  //! FVTX module data members
  mFvtxEmbed _mFvtxEmbed_mod;

  #endif

  //! embedding mode
  unsigned int _mode;

  //! module timer
  PHTimer* _timer;

};

#endif
