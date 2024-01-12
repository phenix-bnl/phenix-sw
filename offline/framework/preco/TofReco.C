#include "TofReco.h"

#include "TofEvent.hh"
#include "TofAddressObject.hh"
#include "TofCalibObject.hh"
#include "TofGeometryObject.hh"

#include "dTofRawRecWrapper.h"
#include "dTofRawWrapper.h"
#include "dTofReconstructedWrapper.h"

#include "TofOutv2.h"

#include "recoConsts.h"
#include "getClass.h"

#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "PHIODataNode.h"

#include <iostream>
#include <string>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;
typedef PHIODataNode <TofOut> TofOutNode_t;

TofReco::TofReco(const string &name): SubsysReco(name)
{
  tofevent = 0;
  TofAddress = 0;
  TofCalib = 0;
  TofGeometry = 0;
  return ;
}

TofReco::~TofReco()
{
  delete tofevent;
  delete TofAddress;
  delete TofCalib;
  return;
}

int TofReco::Init(PHCompositeNode *topNode)
{
  tofevent   = new TofEvent();
  TofAddress = new TofAddressObject();
  TofCalib = new TofCalibObject();
  int iret = CreateNodeTree(topNode);
  return iret;
}

int TofReco::InitRun(PHCompositeNode *topNode)
{
  int iret = 0;
  recoConsts *rc = recoConsts::instance();
  PHTimeStamp TimeStp = rc->get_TimeStamp();
  TofAddress->setTimeStamp(TimeStp);
  TofAddress->fetch();
  TofGeometry->setTimeStamp(TimeStp);
  TofGeometry->fetch();
  TofCalib->setTimeStamp(TimeStp);
  TofCalib->fetch();
  return iret;
}

int TofReco::CreateNodeTree(PHCompositeNode *topNode)
{
  // first test if neccessary nodes have been created, if not bail out
  enum {DSTNODE, PARNODE, LAST}; // leave LAST at end - it is used for loops
  const char *NName[] = {
    "DST",
    "PAR"};

  PHCompositeNode *outNode[LAST];
  PHNodeIterator iter(topNode);
  for (short int i = 0;i < LAST;i++)
    {
      outNode[i] = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", NName[i]));
      if (!outNode[i])
        {
          cout << PHWHERE << NName[i] << " Node is missing doing nothing" << endl;
          return -1;
        }
    }

  // here comes the TOF
  PHCompositeNode* tofNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "TOF"));
  if (!tofNode)
    {
      tofNode = new PHCompositeNode("TOF");
      topNode->addNode(tofNode);
    }


  TofGeometry = new TofGeometryObject();
  PHDataNode<TofGeometryObject> *TofDetGeoNode =
    new PHDataNode<TofGeometryObject>(TofGeometry, "TofGeometry");
  outNode[PARNODE]->addNode(TofDetGeoNode);

  int mr;
  mr = 960;
  dTofRawWrapper *dTofRaw = new dTofRawWrapper("dTofRaw", mr);
  PHTableNode_t *dTofRawNode =
    new PHTableNode_t(dTofRaw, "dTofRaw");
  tofNode->addNode(dTofRawNode);

  dTofReconstructedWrapper *dTofReconstructed =
    new dTofReconstructedWrapper("dTofReconstructed", mr);
  PHTableNode_t *dTofReconstructedNode =
    new PHTableNode_t(dTofReconstructed, "dTofReconstructed");
  outNode[DSTNODE]->addNode(dTofReconstructedNode);

  dTofRawRecWrapper *dTofRawRec = new dTofRawRecWrapper("dTofRawRec", mr);
  PHTableNode_t *dTofRawRecNode =
    new PHTableNode_t(dTofRawRec, "dTofRawRec");
  tofNode->addNode(dTofRawRecNode);

  TofOut *tofout = new TofOutv2();
  PHObjectNode_t *TofOutNode =
    new PHIODataNode<PHObject>(tofout, "TofOut", "PHObject");
  outNode[DSTNODE]->addNode(TofOutNode);


  return 0;
}

int TofReco::process_event(PHCompositeNode *topNode)
{
  tofevent->DcmToRaw(topNode, TofAddress);

  tofevent->RawToDst(topNode, TofAddress, TofGeometry, TofCalib);

  copyWrapper(topNode);

  return 0;
}

int TofReco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("TOF"))
    {
      mainIter.forEach(reset);
    }
  return 0;
}

int TofReco::copyWrapper(PHCompositeNode *topNode)
{
  dTofReconstructedWrapper *dtofout = findNode::getClass<dTofReconstructedWrapper>(topNode,"dTofReconstructed");

  TofOut *tofout = findNode::getClass<TofOut>(topNode,"TofOut");

  if (dtofout && tofout)
    {
      if (!tofout->isValid() && dtofout->RowCount())
        {
          tofout->FillFromWrapper(dtofout);
        }
      if (verbosity > 0)
	{
          tofout->identify();
	}
    }
  return 0;
}
