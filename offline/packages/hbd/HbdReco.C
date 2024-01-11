
#include <HbdReco.h>

#include <HbdDcmRaw.h>
#include <HbdMiniCellListv2.h>
#include <hbdDetectorGeo.hh>

#include <Event.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <phool.h>
#include <getClass.h>
#include <recoConsts.h>

#include <iostream>

using namespace std;

typedef PHIODataNode <PHObject>    PHObjectNode_t;
typedef PHIODataNode <HbdMiniCellList> HbdMiniCellListNode_t;
typedef PHDataNode <hbdDetectorGeo> hbdDetectorGeoNode_t;
typedef PHIODataNode <Event>       EventNode_t;


HbdReco::HbdReco(const string &name): SubsysReco(name)
{
  geo = new hbdDetectorGeo();

  // Make an HbdDcmRaw module.
  d_dcm = new HbdDcmRaw();

  d_dcm->TakeNthSampleIntoCell(9);
  d_dcm->SetPhysicsDecode(1);
//  d_dcm->SetChargeThreshold(10);

//  int dislist[2] = {19, 23};
//  d_dcm->SetDisablePadId(dislist, 2);

  return ;
}

HbdReco::~HbdReco()
{
   delete d_dcm;
}

int HbdReco::Init(PHCompositeNode *topNode)
{
  //
  //  In this routine, we typically make all the
  //  Objects necessary for analysis.  However,
  //  at this stage, we *DO NOT KNOW* the run number.
  //  Most initialization, therefore will occur during the
  //  InitRun() method.
  //                   TKH 8-25-2003
  //

  // Fill up the Node Tree with "our" Nodes.
  CreateNodeTree(topNode);

  return 0;
}

int HbdReco::InitRun(PHCompositeNode *topNode)
{
   recoConsts *rc = recoConsts::instance();
   int runnumber = rc->get_IntFlag("RUNNUMBER");
   geo->fetch(runnumber);

   d_dcm->Init();
   return 0;
}

int 
HbdReco::process_event(PHCompositeNode *topNode)
{
  if (verbosity) cout << "Called HBD process_event" << endl;

  HbdMiniCellList *minicelllist = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
  Event *evt = findNode::getClass<Event>(topNode, "PRDF");

  if (verbosity) cout << "HBD::MiniCell node is:" << minicelllist <<endl;

  // Fetch and decode event (Save to HbdMiniCell)
  d_dcm->GetEventToMiniCell(evt, minicelllist);
//  cout << "HbdReco::ConvFac: " << minicelllist->GetConvFactor() << endl; 
  if (verbosity) cout << "HBD::Finished GetEventToMiniCell "<<endl;

  return 0;
}

int
HbdReco::CreateNodeTree(PHCompositeNode *topNode)
{
  //
  //  This routine places our data objects
  //  into the node tree both for the convenience of
  //  out private analysis *AND* to allow other users
  //  to look at our data *AND* to allow our objects to
  //  be saved into the output.
  //                      TKH 8-25-2003
  //
  PHNodeIterator iter(topNode);

  PHCompositeNode *parNode;

  parNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","PAR"));

  if(!parNode){
     cout <<PHWHERE << "Could not find PAR node" << endl;
     return -1;
  }

  hbdDetectorGeoNode_t *hbdgeo = new PHDataNode<hbdDetectorGeo>(geo,"hbdDetectorGeo","PHDataNode");
 
  // Find the DST node so we can put objects there...
  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << " Could not find DST node" << endl;
      return -1;
    }

  HbdMiniCellList *d_minicell = new HbdMiniCellListv2();

  // Make nodes containing the objects...
  HbdMiniCellListNode_t *minicell = new PHIODataNode<HbdMiniCellList>(d_minicell, "HbdMiniCellList", "PHObject");

  // Put the nodes in the tree below "DST"...
  dstNode->addNode(minicell);
  parNode->addNode(hbdgeo);

  return 0;
}

