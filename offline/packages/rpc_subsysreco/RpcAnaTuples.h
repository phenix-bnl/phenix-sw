#ifndef __RPCANATUPLES__
#define __RPCANATUPLES__

#include "SubsysReco.h"

// Forward declerations
class PHCompositeNode;
class TNtuple;

#ifndef __CINT__
#include<bitset>
#include<boost/array.hpp>
#include<PHTimeServer.h>
#include<TMutTrkMap.h>
#include<TRpcTrkMap.h>
#include<TRpcClusMap.h>
#include<TRpcCoordMap.h>
#include<TRpcHitMap.h>
#include<TRpcMCHitMap.h>

#endif

   
#include<string>
#include<PHGlobal.h>

class TRpcClusMap;
class TRpcTrkMap;
class TRpcHitMap;
class TRpcMCHitMap;
class TMutTrkMap;

class RpcAnaTuples : public SubsysReco
{
 public:
   // constructor
   RpcAnaTuples( const char* name="RpcAnaTuples",
                 bool SIM = false,
		 const char* filename="rpc_ana_ntuples.root");

   // run initialization
   int Init(PHCompositeNode *topNode);

   // event method
   int process_event(PHCompositeNode *topNode);
 
   // finish run, close ntuples
   int End(PHCompositeNode *topNode);

   // set filename
   void set_filename( const char* file)
   { if(file) _filename = file; }

  // make members for filling ntuples
   void write_rpc_trk_ntuple(PHCompositeNode*);

  // fill rpc hit informations
   void write_rpc_hit_ntuple(PHCompositeNode*);

  // fill rpc cluster infos
   void write_rpc_clus_ntuple(PHCompositeNode*);


 private:
  // filename
    std::string _filename;

 // REAL or SIM
  bool SIM;
  
 // Ntuples
  TNtuple* _rpc_trks;

 //hit ntuple
  TNtuple* _rpc_hits;

 //cluster ntuple
  TNtuple* _rpc_clus;
 
 //PHGlobal
  PHGlobal* _global;
 
 // Cluster Map
  TRpcClusMap* rpc_clus_map;

  TRpcTrkMap* rpc_trk_map;

  TMutTrkMap* mu_trk_map;

  TRpcMCHitMap* rpc_hit_map;
 
  TRpcHitMap* rpc_real_hit_map;

};

#endif  
