// $Id: RpcUnpackPisa.h,v 1.4 2015/02/09 15:51:05 richi Exp $
#ifndef __RpcUnpackPisa_H__
#define __RpcUnpackPisa_H__

/*!
	\file RpcUnpackPisa.h	
	\ingroup supermodules
	\brief rpc pisa to dst supermodule
	\author Sean Kelly
	\version $Revision: 1.4 $
	\date $Date: 2015/02/09 15:51:05 $
*/

#include <MuonSubsysReco.h>
#include <TDataType.h>

// Forward declerations
class PHCompositeNode;
class PHTimer;

class mRpcSlowSim;
class mRpcResponse;

//! rpc pisa to dst supermodule
/*! 
	rpc pisa to dst supermodule
	reads pisa root files; fills RPC mc hit/track maps consequently;
	possibly runs the response, to build mutr/muid hit maps, writes to a simulated DST
	and possibly to a simulated PRDF. 
*/
class RpcUnpackPisa: public MuonSubsysReco
{
 public:

	//! constructor
	RpcUnpackPisa();

	//! destructor
	~RpcUnpackPisa();

	//! run initialization
	int InitRun(PHCompositeNode *topNode);
	
	//! event method
	int process_event(PHCompositeNode *topNode);
	
	//! end method
	int End(PHCompositeNode *topNode);
	 
	//! returns true if response is to be run and written to DST
	bool do_response() const 
	{ return _run_response;}
	
	//! sets the do_response flag to tell if response is to be run and written to DST
	void set_do_response( bool value = true ) 
	{ _run_response = true;} 

 protected:

	//! create all needed nodes
	int create_node_tree(PHCompositeNode *topNode);

	//! rpc working node
	PHCompositeNode * _rpc_node;
	
	//! mutoo working node
	PHCompositeNode * _mutoo_node;
	
	//! dst node
	PHCompositeNode * _dst_node;

	//! RPC slow simulator module
	mRpcSlowSim*	_mRpcSlowSim_mod;
	
	//! RPC response module
	mRpcResponse* _mRpcResponse_mod;
	
	//! if true, runs response on top of slowsim, write hitmaps to the DST
	bool _run_response;	 
	
	//! module timer
	PHTimer* _timer;	

};

#endif







