#include <sstream>

#include <PHObject.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <recoConsts.h>
#include <phool.h>
#include <Event.h>

#include <RunHeader.h>
#include <cmath>

#include <hbdghitWrapper.h>
#include <HbdGetGEA.h>
#include <hbdDetectorGeo.hh>

#include "HbdSimreco.h"

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;
typedef PHDataNode <hbdDetectorGeo> hbdDetectorGeoNode_t;

//____________________________________________________
HbdSimreco::HbdSimreco(const char *name):
  SubsysReco( name )
{
  hbdghit = 0;
  d_hbdgeo = 0;
  return ;
}

//_______________________________________________________________
HbdSimreco::~HbdSimreco( void )
{
  // nothing to be deleted here
  // the only pointers that are created with "new" are stored
  // in the node tree and thus deleted by Fun4All.
}

//____________________________________________________
int HbdSimreco::InitRun(PHCompositeNode *topNode)
{

  d_hbdgeo = new hbdDetectorGeo();
  
  //run number that gets final HBD geometry
  d_hbdgeo->fetch(215574);  
  
  PHNodeIterator iter(topNode);
  
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
	 {
    cout << PHWHERE << "DST Node missing doing nothing" << endl;
    return -1;
	 }
  
  PHCompositeNode *parNode;
  parNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode)
  {
    cout << PHWHERE << "PAR Node missing doing nothing" << endl;
    return -1;
  }
  
  PHCompositeNode *dcmNode;
  dcmNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DCM"));
  if (!dcmNode)
  {
    cout << PHWHERE << "DCM Node missing doing nothing" << endl;
    return -1;
  }
  
  PHCompositeNode *evaNode;
  evaNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "EVA"));
  if (!evaNode)
  {
    cout << PHWHERE << "EVA Node missing doing nothing" << endl;
    return -1;
  }
  
  PHCompositeNode *geaNode;
  geaNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "GEA"));
  if (!geaNode)
  {
    cout << PHWHERE << "GEA Node missing doing nothing" << endl;
    return -1;
  }
  
  hbdDetectorGeoNode_t *hbdgeo = new hbdDetectorGeoNode_t(d_hbdgeo,"hbdDetectorGeo");
  parNode->addNode(hbdgeo);
  
  int mr = 4500;
  hbdghit = new hbdghitWrapper("hbdghit", mr);
  PHIODataNode<PHTable>* hbdghitNode = new PHIODataNode<PHTable>(hbdghit, "hbdghit");
  geaNode->addNode(hbdghitNode);
  
  PHCompositeNode* hbdNode = new PHCompositeNode("HBD");
  topNode->addNode(hbdNode);
  
  HbdSlowSim.Init(topNode);
  //HbdCluster.Init(topNode);

  return 0;
}

//___________________________________________________________________
int HbdSimreco::process_event(PHCompositeNode *topNode)
{


   HbdGetGEA(topNode);  
   HbdSlowSim.process_event(topNode);
   //HbdCluster.process_event(topNode);

   return 0;
}

//___________________________________________________________________
int HbdSimreco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("HBD")) mainIter.forEach(reset);
  return 0;
}
