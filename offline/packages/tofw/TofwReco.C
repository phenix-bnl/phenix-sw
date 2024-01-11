#include <TofwReco.h>

#include <TofwRawv1.h>
#include <TofwHitv1.h>
#include <TofwEvent.h>

#include <TofwGeometry.h>
#include <TofwCalib.h>

#include <RunHeader.h>

#include <recoConsts.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <phool.h>

using namespace std;

TofwReco::TofwReco(const string &name) : SubsysReco(name)
{
  d_tofw = 0;
  d_geom = 0;
  d_calib = 0;
  return;
}

TofwReco::~TofwReco()
{
  if (d_calib)
    {
      delete d_calib;
    }
  if (d_tofw)
    {
      delete d_tofw;
    }
  return;
}

int 
TofwReco::Init(PHCompositeNode* topNode)
{
  d_calib = new TofwCalib();

  d_tofw = new TofwEvent();
  return 0;
}


int
TofwReco::InitRun(PHCompositeNode* topNode)
{
  //In order to run embedding, the nodes have to be put in the InitRu.
  //otherwise phool will not be able to properly read in the events from DSTs
  
  CreateNodeTree(topNode);
  d_calib->set_debug(0);

  RunHeader* d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");

  if (!d_runhdr) cout << PHWHERE << "TofwReco:: runhdr not in Node Tree" << endl;

  if (d_runhdr){
    int runnumber = d_runhdr->get_RunNumber();
    d_calib->fetchToffset(runnumber);
    d_calib->fetchTvcConv(runnumber);
  }

  return 0;
}

int 
TofwReco::ResetEvent(PHCompositeNode* topNode)
{

  d_tofw->Reset(topNode);

  return 0;
}


int 
TofwReco::process_event(PHCompositeNode* topNode)
{
  return d_tofw->process_event(topNode, d_geom, d_calib);
}

int 
TofwReco::CreateNodeTree(PHCompositeNode* topNode)
{

  // Find the DST node so we can put objects there
  PHNodeIterator iter(topNode);
  PHCompositeNode* dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  TofwRaw *raw = findNode::getClass<TofwRaw>(topNode, "TofwRaw");
  if (!raw)
    {
      raw = new TofwRawv1();
      PHIODataNode<TofwRaw> *NewNode = new PHIODataNode<TofwRaw>(raw, "TofwRaw", "PHObject");
      dstNode->addNode(NewNode);
    }
  TofwHit *hit = findNode::getClass<TofwHit>(topNode, "TofwHit");
  if (!hit)
    {
      hit = new TofwHitv1();
      PHIODataNode<TofwHit> *NewNode = new PHIODataNode<TofwHit>(hit, "TofwHit", "PHObject");
      dstNode->addNode(NewNode);
    }

  // check and make the geometry object if neccessary
  d_geom = findNode::getClass<TofwGeometry>(topNode,"TofwGeometry");
  if(!d_geom)
    {
      PHCompositeNode* parNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
      d_geom = new TofwGeometry();
      PHDataNode<TofwGeometry> *NewNode = new PHDataNode<TofwGeometry>(d_geom,"TofwGeometry");
      parNode->addNode(NewNode);
    }

  return 0;
}
