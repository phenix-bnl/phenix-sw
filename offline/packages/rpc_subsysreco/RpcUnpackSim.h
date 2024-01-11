// $Id: RpcUnpackSim.h,v 1.8 2009/09/24 09:17:00 hpereira Exp $
#ifndef __RpcUnpackSim_h__
#define __RpcUnpackSim_h__

/*!
  \file RpcUnpackSim.h
  \ingroup supermodules
  \brief RPC Fast simulation module. Reads a slowsim DST. Creates TRpc MC Hits
  from MC tracks. Runs the restonse to create Rpc hits for later reconstruction
  \warning there is no embedding done so far in this module.
  \author H. Pereira Da Costa
  \version $Revision: 1.8 $
  \date $Date: 2009/09/24 09:17:00 $
*/

#include <string>
#include <MuonSubsysReco.h>

// Forward declerations
class PHCompositeNode;

#ifndef __CINT__
#include <mRpcEmbed.h>
#include <mRpcResponse.h>
#include <PHTimeServer.h>
#endif

//! RPC Fast Simulation module
/*!
  \ingroup supermodules
  RPC fast simulator. reads muid/mutr mc hit/track maps from a simulated DST
  runs the response to build muid/mutr hit maps for later reconstruction.
  \warning there is no embedding done so far in this module.
*/
class RpcUnpackSim: public MuonSubsysReco
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
  RpcUnpackSim( const char* name= "RPCUNPACKSIM", unsigned int mode = MC_SIGNAL_NO_BG );

  //! run initialization
  int InitRun(PHCompositeNode*);

  //! event processing
  int process_event(PHCompositeNode*);

  //! end of process
  int End(PHCompositeNode*);

  //! "signal" top node name
  void SetSignalNodeName( std::string value )
  { _signalNodeName = value; }

  //! "background" top node name
  void SetBackgroundNodeName( std::string value )
  { _backgroundNodeName = value; }

  //! changes embedding mode
  void SetMode(unsigned int);

  //! embedding mode
  unsigned int GetMode() const
  { return _mode; }

  protected:

  //! check geometry version
  int load_geometry( PHCompositeNode* );

  //! create all new nodes
  int set_node_ptrs(PHCompositeNode*);

  //! create needed interfaces
  int set_interface_ptrs(PHCompositeNode*);

  //! create needed modules
  int set_module_ptrs(PHCompositeNode *topNode);

  //*! @name Nodes for merged hits
  //@{

  //! signal top node name
  std::string _signalNodeName;

  //! background top node name
  std::string _backgroundNodeName;

 	//! rpc working node
  PHCompositeNode* _rpc_node;

  // Nodes for input signal
  //! signal node for MC DST
  PHCompositeNode* _signal_node;

  //! background node for MC/RD DST
  PHCompositeNode* _ioc_signal_node;

  //! internal signal node (for response)
  PHCompositeNode* _background_node;

  //! internal background node
  PHCompositeNode* _ioc_background_node;

  #ifndef __CINT__

  //! RPC module data members
  mRpcResponse _mRpcResponse_mod;

  //! RPC module data members
  mRpcEmbed _mRpcEmbed_mod;

  //! Timer
  PHTimeServer::timer _timer;

  #endif

  //! embedding mode
  unsigned int _mode;

};

#endif







