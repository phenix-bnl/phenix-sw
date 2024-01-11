#include <iostream>
#include <fstream>

#include <MpcEmbedReco.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContainerV1.h>
#include <mpcTowerContent.h>
#include <getClass.h>
#include <MpcMap.h>
#include <Fun4AllServer.h>

#include <PHNodeIterator.h>
#include <PHIODataNode.h>
#include <PHCompositeNode.h>
#include <PHObject.h>

#include <iostream>

using namespace std;
using namespace findNode;

typedef PHIODataNode<PHObject> PHObjectNode_t;



MpcEmbedReco::MpcEmbedReco(const std::string &name, const char* simname): SubsysReco(name) //what you embed the simulated clusters into is on topnode
{
  fVerbose = 1;
  merge_mode = MERGE_TREES;
  simNodename = simname;
  fScale = 1.0;
}

MpcEmbedReco::~MpcEmbedReco()
{
}

void MpcEmbedReco::SetNodesToMerge(const char *dest_node, const char *src1_node)
{
  merge_mode = MERGE_NODES;
  destination_NodeName = dest_node;
  src1_NodeName = src1_node;
}

int MpcEmbedReco::Init(PHCompositeNode *topNode)
{

  return 0;
}

int MpcEmbedReco::EndRun(const int runnumber)
{
  return 0;
}

int MpcEmbedReco::InitRun(PHCompositeNode *topNode)
{
  //  mpcmap = MpcMap::instance();
  mpcmap = findNode::getClass<MpcMap>(topNode, "MpcMap");
  return 0;
}


//top Node is the real
int MpcEmbedReco::process_event(PHCompositeNode *topNode)
{
  PHCompositeNode* simNode = 0;

  if ( merge_mode == MERGE_TREES )
    {
      //the user has to put the node with given nodename in macro using
      //the Fun4AllNoSync Manager

      Fun4AllServer *se = Fun4AllServer::instance();
      simNode= se->topNode(simNodename);
    }
  else if ( merge_mode == MERGE_NODES )
    {
      //the user has to put the node with given nodename in macro using
      //the Fun4AllNoSync Manager

      PHNodeIterator iter(topNode);
      simNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", src1_NodeName.c_str()));
    }

  //  CreateNodeTree(embNode); //hopefully the sync manager can take care of this operation
  
  if (!simNode)
    {
      cout << PHWHERE << "Unable to get Node " 
	   << simNode << ", Node is missing..." << endl;
      return False;
    }
  else if ( fVerbose>2 )
    {
      cout << "MpcEmbedReco: simNode" << endl;
      simNode->print();
    }
  
  mpcTowerContainer *mpctow = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  mpcTowerContainer *mpctow_sim = findNode::getClass<mpcTowerContainer>(simNode,"mpcTowerContainer");

  if (!mpctow || !mpctow_sim)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer(s), is Node missing?" << endl;
      cout << "mpctow: " << mpctow << endl;
      cout << "mpctow_sim, sngl pi0: " << mpctow_sim << endl;
      return False;
    }
  
  *mpctow_sim *= fScale;

  //just add the tower content together
  if(fVerbose > 2){
    cout << "Before\n";
    cout << "RealEvent\n";
    print(mpctow);
  }
  
  //first copy the real event, mpctow
  //then add the single pi0 on top of the real event

  *mpctow += *mpctow_sim;
  
  if(fVerbose > 2){
    cout << "Single Pi0 Event\n";
    print(mpctow_sim);
    cout << "After Embedding\n\n";
    cout << "Embedded Event\n";
    print(mpctow);
  }

  return 0;
}

void MpcEmbedReco::print(mpcTowerContainer *tow) {
  cout << "MpcEmbedReco::Print() tower energies" << endl;
  int ntow = tow->size();
  cout << "Size of tower container is: " << ntow << endl;
  cout << "Printing Energies\n";
  for (int itow=0;itow<ntow;itow++)
    {
      mpcTowerContent *tower = tow->getTower(itow);
      int tow_ch = tower->get_ch();               // tower channel (FEM numbering)
      int gridx = mpcmap->getGridX( tow_ch );
     
      if ( gridx == -1 )
        {
          cout << "ERROR, Non-existent channel " << tow_ch << endl;
          continue;
        }

      float e = tower->get_energy();
      cout << "tower: " << tow_ch << ", Energy: " << e << endl;

    }
  return;
}
 
int MpcEmbedReco::CreateNodeTree(PHCompositeNode *topNode)
{
  /*
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", embNodename));

  if (!dstNode)
    {
      cout << PHWHERE << "Embedding Node missing doing nothing" << endl;
      return 0;
    }

  //All I do here is to put an empty tower container on the Node tree
  //In the process_event function I copy the tower container from Node
  //DST to this one, then add the sim towers to make the embedded
  //event
 
  mpcTowerContainer *mpctower = new mpcTowerContainerV1();
  PHObjectNode_t *MpcTowerNode = new PHObjectNode_t(mpctower, "mpcTowerContainer", "PHObject");
  dstNode->addNode(MpcTowerNode);
  */
  return 1;
}


