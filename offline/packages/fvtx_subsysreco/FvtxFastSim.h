#ifndef __MUONFastSim_H__
#define __MUONFastSim_H__

/*!
	\file FvtxFastSim.h	
	\ingroup supermodules
	\brief reads TMutMCtrkMap and create TFvtxMCHit accordingly
	\author Hugo Pereira
	\version $Revision: 1.3 $
	\date $Date: 2006/08/08 14:58:00 $
*/

#include <PHCompositeNode.h>
#include <MuonSubsysReco.h>

// Forward declerations
class PHCompositeNode;
class PHTimer;
class mFvtxFastSim;
class mFvtxResponse;

/*!
	\class FvtxFastSim 
	\ingroup supermodules
	\brief reads TMutMCtrkMap and create TFvtxMCHit accordingly
*/

class FvtxFastSim: public MuonSubsysReco
{
 public:

	//! constructor
	FvtxFastSim( const char* name = "FVTXFASTSIM" );

 	//! destructor
	virtual ~FvtxFastSim();
	
	//! run initialization
	int InitRun(PHCompositeNode *topNode);
	
	//! event method
	int process_event(PHCompositeNode *topNode);
	
	//! end of run
	int End(PHCompositeNode *topNode);

	protected:

	//! create neaded node
	int CreateNodeTree(PHCompositeNode *topNode);

	//! fvtxoo working node
	PHCompositeNode * _fvtxoo_node;
	
	//! dst io node
	PHCompositeNode * _dst_node;

	//! zero supression module
	mFvtxFastSim* _mFvtxFastSim_mod;
 
	//! FVTX module data members
	mFvtxResponse* _mFvtxResponse_mod;
	
	//! module timer
	PHTimer* _timer;

};

#endif
