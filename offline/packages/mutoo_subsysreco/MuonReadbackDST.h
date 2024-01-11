#ifndef __MUONREADBACKDST_H__
#define __MUONREADBACKDST_H__

// $Id: MuonReadbackDST.h,v 1.33 2013/01/07 22:58:36 abhisek Exp $

/*!
  \file MuonReadbackDST.h
  \ingroup supermodules
  \brief reads all possible maps from a DST whatever its origin is.
  \author Hugo PEREIRA
  \version $Revision: 1.33 $
  \date $Date: 2013/01/07 22:58:36 $
*/

#include "MuonSubsysReco.h"

#ifndef __CINT__

  #include <PHCompositeNode.h>
  #include <PHTimeServer.h>
  #include <TMutNode.h>

  #include<TMCPrimaryMap.h>

  #include<TMutMCHitMap.h>
  #include<TMutMCTrkMap.h>
  #include<TMutHitMap.h>
  #include<TMutClusMap.h>
  #include<TMutCoordMap.h>
  #include<TMutGapCoordMap.h>
  #include<TMutStubMap.h>
  #include<TMutTrkMap.h>
  #include<TMutVtxMap.h>
  #include<TMutAlignParMap.h>

  #include<TMuiMCHitMapO.h>
  #include<TMuiHitMapO.h>
  #include<TMui1DRoadMapO.h>
  #include<TMuiPseudoBLTMapO.h>
  #include<TMuiRoadMapO.h>
  #include<TMuiPseudoLL1Map.h>
  #include<TMuiClusterMapO.h>
  #include<TMutExtVtx.h>

#else
  class PHCompositeNode;
#endif

/*!
  \class MuonReadbackDST
  \ingroup supermodules
  \brief reads all possible maps from a DST whatever its origin is.
*/
class MuonReadbackDST: public MuonSubsysReco
{
 public:

  //! construtor
  MuonReadbackDST( const char* name = "MUONREADBACKDST" );

  //! run init
  int InitRun(PHCompositeNode *topNode);

  //! event loop
  int process_event(PHCompositeNode *topNode);

  //! full end
  int End(PHCompositeNode *topNode);

  const char *Name() const
  {return ThisName.c_str(); }

  //! changes the do_dbinit flag
  void set_do_dbinit( bool value )
  { _do_dbinit = value; }

  protected:

  //! trees creation
  int CreateNodeTree(PHCompositeNode *topNode);

  #ifndef __CINT__

  //! load a map from the DST, create the map if not found.
  template < typename T >
  T* _load_map( PHCompositeNode* source_node, PHCompositeNode* dest_node, const char* map_name, const char* node_name )
  {
    try {

      // try read DST input map.
      return TMutNode<T>::new_dst_input_node( dest_node, map_name, source_node, node_name );

    } catch( std::exception &e ) {

      // map not found, create an empty map
      return TMutNode<T>::new_node(dest_node, map_name );

    }
  }

  //! module timer
  PHTimeServer::timer _timer;

  #endif

  // Node tree data members
  PHCompositeNode * _mutoo_node;
  PHCompositeNode * _muioo_node;
  PHCompositeNode * _dst_node;

  //! if true database initialization is done at InitRun
  bool _do_dbinit;

};

#endif /* __MUONREADBACKDST_H__ */







