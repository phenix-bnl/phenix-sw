#include <TfwSimreco.h>
#include <tfwghitWrapper.h>
#include <TfwGetGEA.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <recoConsts.h>
#include <phool.h>

#include <TofwSimEvent.h>
#include <TofwHitv1.h>
#include <TofwRawv1.h>
#include <TofwGeometry.h>

#include <cmath>
#include <sstream>

using namespace std;

typedef PHIODataNode<TofwRaw>      TofwRawNode_t;
typedef PHIODataNode<TofwHit>      TofwHitNode_t;

TfwSimreco::TfwSimreco(const string &name): SubsysReco(name)
{
   
   d_tofw = 0;
   d_raw  = 0;
   d_hit  = 0;

   tfwghit = 0;

   return ;
}

TfwSimreco::~TfwSimreco()
{
  delete d_tofw;
  return;
}


int TfwSimreco::Init(PHCompositeNode *topNode)
{
  
  d_tofw = new TofwSimEvent();
  
  return 0;
}

int TfwSimreco::InitRun(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);
  
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  d_raw = new TofwRawv1();
  d_hit = new TofwHitv1();

  TofwRawNode_t *raw = new TofwRawNode_t(d_raw, "TofwRaw", "PHObject");
  dstNode->addNode(raw);

  TofwHitNode_t *hit = new TofwHitNode_t(d_hit, "TofwHit", "PHObject");
  dstNode->addNode(hit);

  

  PHCompositeNode *parNode;
  parNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode)
    {
	   cout << PHWHERE << "PAR Node missing doing nothing" << endl;
	   return -1;

    }
  
  d_geom = new TofwGeometry();
  PHDataNode<TofwGeometry> *NewNode = new PHDataNode<TofwGeometry>(d_geom,"TofwGeometry");
  parNode->addNode(NewNode);
   
 
  PHCompositeNode *geaNode;
  geaNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "GEA"));
  if (!geaNode)
    {
      cout << PHWHERE << "GEA Node missing doing nothing" << endl;
      return -1;
    }
  
  int mr = 4500;
  tfwghit = new tfwghitWrapper("tfwghit", mr);
  PHIODataNode<PHTable>* tfwghitNode = new PHIODataNode<PHTable>(tfwghit, "tfwghit");
  geaNode->addNode(tfwghitNode);
  
  return 0;
}

int TfwSimreco::process_event(PHCompositeNode *topNode)
{

   TfwGetGEA(topNode);  //   // fill the tfwghit tables to start the detector response
   
   d_tofw->process_event(topNode,d_geom);
   
   return 0;
}

int TfwSimreco::ResetEvent(PHCompositeNode *topNode)
{
  d_tofw->Reset(topNode);
   return 0;
}
