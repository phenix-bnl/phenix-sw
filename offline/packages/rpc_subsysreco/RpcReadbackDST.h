// $Id: RpcReadbackDST.h,v 1.6 2013/09/16 16:26:25 pinkenbu Exp $
#ifndef __RpcReadbackDST_h__
#define __RpcReadbackDST_h__

/*!
  \file    RpcReadbackDST.h
  \brief   reads all possible maps from a DST whatever its origin is.
  \author  Hugo PEREIRA
  \version $Revision: 1.6 $
  \date    $Date: 2013/09/16 16:26:25 $
*/

#include <MuonSubsysReco.h>

#ifndef __CINT__ 
  #include <TMutNode.h>
  #include <PHTimeServer.h>
  
	#include <TRpcClusMap.h>
	#include <TRpcCoordMap.h>
	#include <TRpcHitMap.h>
	#include <TRpcMCHitMap.h>
	#include <TRpcTrkMap.h>
	#include <TRpcRoadMap.h>
	#include <TRpcHodoHitMap.h>
	#include <TRpcMuoTrkMap.h>
#endif

class PHCompositeNode;

/*! \ingroup supermodules */
//! reads all possible maps from a DST whatever its origin is.
class RpcReadbackDST: public MuonSubsysReco
{
 public:

  //! construtor 
  RpcReadbackDST();
  
  //! run init
  int InitRun(PHCompositeNode *topNode);
  
  //! event loop
  int process_event(PHCompositeNode *topNode);
  
  //! full end
  int End(PHCompositeNode *topNode);
  
  protected:

  //! trees creation
  int CreateNodeTree(PHCompositeNode *topNode);
  
  #ifndef __CINT__
  //!< load a map from the DST, create the map if not found. 
  template < typename T >
  T* _load_map( PHCompositeNode* source_node, PHCompositeNode* dest_node, const char* map_name, const char* node_name )
  {
    try {
    
     return TMutNode<T>::new_dst_input_node(dest_node, map_name, source_node, node_name );
   
    } catch( std::exception &e ) {
    
      std::cout << e.what() << std::endl;
      std::cout << "RpcReadbackDST::_load_map - creating empty map.\n";
      return TMutNode<T>::new_node(dest_node, map_name );    
      
    }
  }

  //! module timer
  PHTimeServer::timer _timer;

  #endif
  
 
};

#endif /* __RpcReadbacKDST_H__ */







