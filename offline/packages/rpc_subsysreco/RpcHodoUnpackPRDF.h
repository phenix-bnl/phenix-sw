#ifndef __RPCHODOUNPACKPRDF_H__
#define __RPCHODOUNPACKPRDF_H__

/*!
  \file    RpcHodoUnpackPRDF.h
  \ingroup supermodules
  \brief   creates rpc hits from PRDF data
  \author  Richard Hollis, UCR, rhollis@ucr.edu
  \version $Revision: 1.1 $
  \date    $Date: 2012/02/13 02:56:51 $
*/

#include <MUTOO.h>
#include <MuonSubsysReco.h>

#ifndef __CINT__
#include <mRpcHodoUnpack.h>
#endif


// Forward declerations
class PHCompositeNode;
class PHTimer;

class RpcHodoUnpackPRDF: public MuonSubsysReco
{
 public:

  //! constructor
  RpcHodoUnpackPRDF( const char* name = "RPCHODOUNPACKPRDF" );

  //! destructor
  virtual ~RpcHodoUnpackPRDF();

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

  //! rpc hodoscope PRDF unpacker
  mRpcHodoUnpack _mRpcHodoUnpack_mod;

  //! supermodule timer
  PHTimer* _timer;

  #endif

  //! Verbosity Flag for messaging
  MUTOO::Verbosity _verbosity;

};

#endif /* __RPCHODOUNPACKPRDF_H__ */
