// $Id: GlobalReco_muons.C,v 1.1 2008/07/30 17:41:15 hpereira Exp $
/*!
  \file    GlobalReco_muons.C
  \brief   Global (i.e. event wise) muon-arm specific variables
  \author  H. Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2008/07/30 17:41:15 $
*/

#include <PHGlobal_Centralv1.h>
#include <PHGlobal_Muonv1.h>
#include <TMuiHitMapO.h>
#include <TMutHitMap.h>

#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>

#include <iostream>

#include "GlobalReco_muons.h"

using namespace std;

//________________________________________________________________
GlobalReco_muons::GlobalReco_muons( const string &name ):
  SubsysReco(name)
{ return; }

//________________________________________________________________
int GlobalReco_muons::InitRun(PHCompositeNode *topNode)
{
  
  // look for existing PHGlobal from DST node. Create one otherwise
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
    
  // create the Muon PHGlobal only if a TMutHitMap and TMuiHitMapIO Nodes are around
  PHGlobal_Muon *global_muon = findNode::getClass<PHGlobal_Muon>(dstNode, "PHGlobal_MUON");
  if ( !global_muon )
  {
    try
    {
      
      /*
      try load mutr hit map and muid hit map.
      If not found, an exception is found and the global node 
      is not created
      */
      TMutNode<TMutHitMap>::find_node( topNode, "TMutHitMap" );
      TMutNode<TMuiHitMapO>::find_node( topNode, "TMuiHitMapO" );
      
      global_muon = new PHGlobal_Muonv1();
      PHIODataNode<PHObject> *PHGlobalNode = new PHIODataNode<PHObject>(global_muon, "PHGlobal_MUON", "PHObject");
      dstNode->addNode(PHGlobalNode);
      
    } catch ( exception& e) {
      cout << e.what() << endl;
      cout << "GlobalReco_muons::InitRun - PHGlobal_MUON not created" << endl;
    }
  }
  
  return 0;
}

//________________________________________________________________
int GlobalReco_muons::process_event(PHCompositeNode *topNode)
{
  fillMuon(topNode);
  return EVENT_OK;
}

//________________________________________________________________
int GlobalReco_muons::fillMuon(PHCompositeNode *topNode)
{
  
  PHGlobal_Muon *global_muon = findNode::getClass<PHGlobal_Muon>(topNode, "PHGlobal_MUON");
  if( global_muon )
  {
    try
    {
      
      // Mutr hits
      TMutHitMap* mut_hit_map = TMutNode<TMutHitMap>::find_node( topNode, "TMutHitMap" );
      for ( int arm = 0; arm < PHGlobal_Muon::MAX_ARM; arm++ )
      {
        for ( int station = 0; station < PHGlobal_Muon::MAX_STATION; station++ )
        {
          global_muon->set_nMutrHits( mut_hit_map->get( arm, station ).count(), arm, station );
        }
      }
    
      // MuID hits
      TMuiHitMapO* mui_hit_map = TMutNode<TMuiHitMapO>::find_node( topNode, "TMuiHitMapO" );
      for ( int arm = 0; arm < PHGlobal_Muon::MAX_ARM; arm++ )
      {
        for ( int plane = 0; plane < PHGlobal_Muon::MAX_PLANE; plane ++ )
        {
          global_muon->set_nMuidHits( mui_hit_map->get( arm, plane ).count(), arm, plane );
        }
      }
    } catch ( exception& e) { cout << e.what() << endl; }
  }
  
  return EVENT_OK;
  
}
