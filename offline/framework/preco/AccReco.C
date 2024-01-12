#include <AccReco.h>

#include <AccRawv2.h>
#include <AccHitv1.h>
#include <AccEvent.h>
#include <AccGeometry.h>

#include <RunHeader.h>

#include <getClass.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHDataNode.h>
#include <phool.h>


typedef PHIODataNode <AccRaw>   AccRawNode_t;
typedef PHIODataNode <AccHit>   AccHitNode_t;
typedef PHDataNode <AccGeometry> AccGeometryNode_t;

using namespace std;

AccReco::AccReco(const string &name, const int ver)
 : SubsysReco(name)
{
  version = ver; 
  d_acc = 0;
  d_geo = 0;
  d_hit = 0;
  d_raw = 0;
  return;
}

AccReco::~AccReco()
{
  delete d_acc;
  return;
}

int AccReco::Init(PHCompositeNode* topNode)
{
  d_acc = new AccEvent();
  return 0;
}


int AccReco::InitRun(PHCompositeNode* topNode)
{
  //In order to run embedding, the nodes have to be put in the InitRu.
  //otherwise phool will not be able to properly read in the events from DSTs 
  CreateNodeTree(topNode);

  RunHeader* d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");

  if (!d_runhdr) cout << PHWHERE << "AccReco:: runhdr not in Node Tree" << endl;

  if (d_runhdr){
    int runNumber = d_runhdr->get_RunNumber();
    d_geo->fetch(runNumber);
  }

  return 0;
}

int AccReco::ResetEvent(PHCompositeNode* topNode)
{

  d_acc->Reset(topNode);
  return 0;
}


int AccReco::process_event(PHCompositeNode* topNode)
{

  return d_acc->process_event(topNode);
}

bool AccReco::CreateNodeTree(PHCompositeNode* topNode)
{

  // Find the DST node so we can put objects there
  PHNodeIterator iter(topNode);
  PHCompositeNode* dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));
  PHCompositeNode* parNode;
  parNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","PAR"));

  AccRawNode_t *raw =  static_cast<AccRawNode_t*>(iter.findFirst("PHIODataNode","AccRaw"));
  AccHitNode_t *hit =  static_cast<AccHitNode_t*>(iter.findFirst("PHIODataNode","AccHit"));

  // make the data objects
  switch ( version ){
    case 1:
      if(!raw){
	d_raw = new AccRawv2();
	raw = new PHIODataNode<AccRaw>(d_raw,"AccRaw","PHObject");
	dstNode->addNode(raw);
      }else{
	d_raw = raw->getData();
      }
      if(!hit){
	d_hit = new AccHitv1();
	hit = new PHIODataNode<AccHit>(d_hit,"AccHit","PHObject");
	dstNode->addNode(hit);
      }else{
	d_hit = hit->getData();
      }
      
      break;

    default:
      cout << PHWHERE << Name() << "... Unknown version requested: " << version << endl;
      cout << PHWHERE << Name() << "... You will receive version 1 instead. " << endl;

      if(!raw){
	d_raw = new AccRawv2();
	raw = new PHIODataNode<AccRaw>(d_raw,"AccRaw","PHObject");
	dstNode->addNode(raw);
      }else{
	d_raw = raw->getData();
      }
      if(!hit){
	d_hit = new AccHitv1();
	hit = new PHIODataNode<AccHit>(d_hit,"AccHit","PHObject");
	dstNode->addNode(hit);
      }else{
	d_hit = hit->getData();
      }
      break;
  }

  // Make nodes containing the objects
  AccGeometryNode_t *geo =  static_cast<AccGeometryNode_t*>(iter.findFirst("PHDataNode","AccGeometry"));

  if(!geo){
    // make the geometry object
    d_geo = new AccGeometry();
    geo = new PHDataNode<AccGeometry>(d_geo,"AccGeometry");
    // put the nodes in the tree below DST
    parNode->addNode(geo);
  }else{
    d_geo = geo->getData();
  }

  return True;
}
