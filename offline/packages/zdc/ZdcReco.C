#include <ZdcReco.h>
#include <ZdcEvent.hh>
#include <ZdcRawv1.h>
#include <ZdcOutv2.h>
#include <SmdOutv1.h>
#include <recoConsts.h>

#include <PHIODataNode.h>
#include <PHObject.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <PHTimeStamp.h>

#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

ZdcReco::ZdcReco(const string  &name): SubsysReco(name)
{
  mZdcEvent = 0;
  return ;
}

ZdcReco::~ZdcReco()
{
  delete mZdcEvent;
  return;
}

int ZdcReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int ZdcReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  mZdcEvent = new ZdcEvent();	            // this module does all the reconstruction work
  PHTimeStamp TimeStp = rc->get_TimeStamp();
  mZdcEvent->setCalibDataAll(TimeStp);	    // set calibration by TimeStamp
                                            // also fills the zdc pmt map

  
  mZdcEvent->setEventNumber(0);
  return 0;
}

int ZdcReco::process_event(PHCompositeNode *topNode)
{
  mZdcEvent->Clear();                // reset to default values
  mZdcEvent->setRawData(topNode);    // gets raw data from zdc packet
  mZdcEvent->PutEndProduct(topNode); // reconstruct and put in ZdcOutNode

  return 0;
}

int ZdcReco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  PHCompositeNode* zdcNode = new PHCompositeNode("ZDC");
  topNode->addNode(zdcNode);

  //-* contains raw values (TDCs, ADCs, etc)
  ZdcRaw *zdcraw = new ZdcRawv1();
  PHObjectNode_t *ZdcRawNode = new PHObjectNode_t(zdcraw, "ZdcRaw", "PHObject");
  dstNode->addNode(ZdcRawNode);

  //-* contains final physics products (energies, tof, etc)
  ZdcOut *zdcout = new ZdcOutv2();
  PHObjectNode_t *ZdcOutNode = new PHObjectNode_t(zdcout, "ZdcOut", "PHObject");
  dstNode->addNode(ZdcOutNode);

  SmdOut *smdout = new SmdOutv1();
  PHObjectNode_t *SmdOutNode = new PHObjectNode_t(smdout, "SmdOut", "PHObject");
  dstNode->addNode(SmdOutNode);

  return 0;
}

int ZdcReco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd(Name()))
    {
      mainIter.forEach(reset);
    }
  return 0;
}
