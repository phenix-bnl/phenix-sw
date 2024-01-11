#ifndef __FVTXREADBACKDST_H__
#define __FVTXREADBACKDST_H__

/*!
  \file FvtxReadbackDST.h
  \ingroup supermodules
  \brief reads all possible maps from a DST whatever its origin is.
  \author D. Winter
  \version $Revision: 1.4 $
  \date $Date: 2013/02/25 04:34:38 $
*/

#include <MuonSubsysReco.h>

#ifndef __CINT__

#include <PHCompositeNode.h>
#include <PHTimer.h>
#include <TMutNode.h>
  
//#include<TMCPrimaryMap.h>
  
// #include<TMutMCHitMap.h>
// #include<TMutMCTrkMap.h>
// #include<TMutHitMap.h>
// #include<TMutClusMap.h>
// #include<TMutCoordMap.h>
// #include<TMutGapCoordMap.h>
// #include<TMutStubMap.h>
#include<TMutTrkMap.h>
#include<TMutCoordMap.h>
#include<TMutVtxMap.h>

// #include<TMuiMCHitMapO.h>
// #include<TMuiHitMapO.h>
// #include<TMui1DRoadMapO.h>
// #include<TMuiPseudoBLTMapO.h>
// #include<TMuiRoadMapO.h>
// #include<TMuiPseudoLL1Map.h>
// #include<TMuiClusterMapO.h>
#include<TMutExtVtx.h>

#else
class PHCompositeNode;
class PHTimer;
#endif

/*!
  \class FvtxReadbackDST
  \ingroup supermodules
  \brief reads all possible maps from a DST whatever its origin is.
*/
class FvtxReadbackDST: public MuonSubsysReco
{
public:

  //! construtor 
  FvtxReadbackDST();
  ~FvtxReadbackDST();
  
  //! run init
  int InitRun(PHCompositeNode *topNode);
  
  //! event loop
  int process_event(PHCompositeNode *topNode);
  
  //! full end
  int End(PHCompositeNode *topNode);
  
  const char *Name() const { return ThisName.c_str(); }

  //! changes the do_dbinit flag
 
protected:

  //! trees creation
  int CreateNodeTree(PHCompositeNode *topNode);
  
#ifndef __CINT__
  
  //! load a map from the DST, create the map if not found. 
  template < typename T >
  T* _load_map( PHCompositeNode* source_node, PHCompositeNode* dest_node, const char* map_name, const char* node_name )
  {
    try {
    
      return TMutNode<T>::new_dst_input_node( dest_node, map_name, source_node, node_name );
   
    } catch( std::exception &e ) {
    
      std::cout << "FvtxReadbackDST::_load_map - unable to load " << map_name << std::endl;
      return TMutNode<T>::new_node(dest_node, map_name );		
      
    }
  }
#endif

  // Node tree data members
  PHCompositeNode* _fvtxoo_node;
  PHCompositeNode* _dst_node;
  
  //! module timer
  PHTimer* _timer;
  
};

#endif /* __FVTXREADBACKDST_H__ */
