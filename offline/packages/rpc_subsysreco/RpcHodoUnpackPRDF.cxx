#include<EventHeader.h>

//#include<mRpcRawUnpackPar.h>

#include <recoConsts.h>
#include <RpcGeom.h>

#include <PHTimer.h>
#include "TRpcHitMap.h"
#include "RpcHodoUnpackPRDF.h"

using namespace std;

//______________________________________________________
RpcHodoUnpackPRDF::RpcHodoUnpackPRDF( const char* name ) : MuonSubsysReco(name),
  _timer( new PHTimer("RPCHODOUNPACKPRDF") )
{
   _verbosity = MUTOO::SOME;
}

//______________________________________________________
RpcHodoUnpackPRDF::~RpcHodoUnpackPRDF()
{
  delete _timer;
}

//______________________________________________________
int RpcHodoUnpackPRDF::Init(PHCompositeNode *top_node)
{
  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  if(_verbosity>=MUTOO::ALOT) {
    MUTOO::PRINT( cout, "RpcHodoUnpackPRDF::Init" ); }
  MuonSubsysReco::Init( top_node );

  return 0;
}

//______________________________________________________
int RpcHodoUnpackPRDF::InitRun(PHCompositeNode *top_node)
{
  // Create Node Tree
  CreateNodeTree(top_node);

  //---------------------
  //check the rpcgeom flag...
  //---------------------
  recoConsts *flags = recoConsts::instance();
  //---------------------
  // Grab the run number
  //---------------------
  int thisrunnumber = flags->get_IntFlag( "RUNNUMBER" , -1);
  if(thisrunnumber<0) {
    cout << "RpcMuoReco::InitRun "
	 << "-- UNKNOWN RUN NUMBER " << thisrunnumber << endl; 
    return 0; }
  //---------------------
  // Grab the current RpcGeomType which is set
  //---------------------
  int current_flag = -1;
  if(flags->FlagExist("RpcGeomType")) {
    current_flag = flags->get_IntFlag( "RpcGeomType" ); }
  
  //Prototype-D Run 9:
  if(thisrunnumber<291580) {//291579 is the last Run 9 physics run
    flags->set_IntFlag( "RpcGeomType", 1 ); 
    if(current_flag!=-1 && current_flag != 1) {
      cout << "RpcMuoReco::InitRun "
	   << "-- CORRECTING RpcGeomType Flag!! "
	   << "was " << current_flag << " " 
	   << "now " << 1 << endl; }
    return 0;
  }//Prototype-D Run 9
  
  //Run 10+11: Full north RPC3 AND Run 11 full n+s RPC3 + RPC1 proto
  if(thisrunnumber<350578) {//350577 is the last Run 11 physics run
    flags->set_IntFlag( "RpcGeomType", 2 ); 
    if(current_flag!=-1 && current_flag != 2) {
      cout << "RpcMuoReco::InitRun "
	   << "-- CORRECTING RpcGeomType Flag!! "
	   << "was " << current_flag << " " 
	   << "now " << 2 << endl; } 
    return 0;
  }//Run 10+11

  //Everything else is flag 3 (full detector configuration)
  flags->set_IntFlag( "RpcGeomType", 3 ); 
  if(current_flag!=-1 && current_flag != 3) {
    cout << "RpcMuoReco::InitRun "
	 << "-- CORRECTING RpcGeomType Flag!! "
	 << "was " << current_flag << " " 
	 << "now " << 3 << endl; } 

  return 0;
}

//______________________________________________________
int RpcHodoUnpackPRDF::CreateNodeTree(PHCompositeNode *top_node)
{
  // RPCOO node
  {
    PHNodeIterator nodeItr(top_node);
    rpc_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RPCOO"));
    if(!rpc_node){
      rpc_node = new PHCompositeNode("RPCOO");
      top_node->addNode(rpc_node);
    }
  }

  {
    PHNodeIterator nodeItr(top_node);
    dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if(!dst_node) {
      dst_node = new PHCompositeNode("DST");
      top_node->addNode(dst_node);
    }
  }

  TRpcHodoHitMap* hodohit_map = TMutNode<TRpcHodoHitMap>::new_node(rpc_node,"TRpcHodoHitMap");
  hodohit_map->make_persistant(dst_node,"TRpcHodoHit");

  return 0;
}

//______________________________________________________
int RpcHodoUnpackPRDF::process_event(PHCompositeNode *top_node)
{
  _timer->restart();

  //Call MUTOO/MUIOO modules for unpacking and calibration
  try {

    // vertex
    load_vertex_if_needed( top_node );
    
    // hodoscope upacking
    _mRpcHodoUnpack_mod.event(top_node);
  } catch (std::exception& e) {

  }

  //! write persistant nodes
  write_maps_if_needed();//see MuonSubsysReco

  _timer->stop();
  return 0;
}

//______________________________________________________
int RpcHodoUnpackPRDF::End(PHCompositeNode* top_node)
{
  // print this module timer statistics
//   _timer->print_stat();

  return 0;
}
