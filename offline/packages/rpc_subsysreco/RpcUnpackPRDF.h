#ifndef __RPCUNPACKPRDF_H__
#define __RPCUNPACKPRDF_H__

/*!
  \file    RpcUnpackPRDF.h
  \ingroup supermodules
  \brief   creates rpc hits from PRDF data
  \author  Richard Hollis, UCR, rhollis@ucr.edu
  \version $Revision: 1.3 $
  \date    $Date: 2011/07/15 01:32:00 $
*/

#include <MUTOO.h>
#include <MuonSubsysReco.h>

#ifndef __CINT__
#include <mRpcUnpack.h>
#endif


// Forward declerations
class PHCompositeNode;
class PHTimer;

class RpcUnpackPRDF: public MuonSubsysReco
{
 public:

  //! constructor
  RpcUnpackPRDF( const char* name = "RPCUNPACKPRDF" );

  //! destructor
  virtual ~RpcUnpackPRDF();

  //! run initialization
  int Init(PHCompositeNode *);

  //! run initialization
  int InitRun(PHCompositeNode *);

  //! event method
  int process_event(PHCompositeNode *);

  //! finish processing
  int End(PHCompositeNode *);

  //! setter for verbosity 
  void set_verbosity(MUTOO::Verbosity v) { _verbosity = v; }

  //! getter for verbosity 
  MUTOO::Verbosity get_verbosity() const { return _verbosity; }

  protected:

  //! creates checks all needed nodes
  int CreateNodeTree(PHCompositeNode *);

  //! rpc node
  PHCompositeNode * rpc_node;

  //! dst node (for output)
  PHCompositeNode *dst_node;

  #ifndef __CINT__

  //! rpc PRDF unpacker
  mRpcUnpack _mRpcUnpack_mod;

  //! supermodule timer
  PHTimer* _timer;

  #endif

  //! Verbosity Flag for messaging
  MUTOO::Verbosity _verbosity;

};

#endif /* __RPCUNPACKPRDF_H__ */
