#include "ZdcSimreco.h"

#include "PHObject.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "recoConsts.h"
#include "PHTimeStamp.h"


#include "ZdcRawv1.h"
#include "ZdcOutv1.h"
#include "ZdcEvent.hh"
#include "ZdcResponse.h"
#include "ZdcGeaHitsv1.h"
#include "ZdcGetDCM.h"

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

ZdcSimreco::ZdcSimreco(const string &name): SubsysReco(name)
{}

int 
ZdcSimreco::Init(PHCompositeNode *topNode)
{
  mZdcEvent    = NULL;
  mZdcResponse = NULL;

  return 0;
}

int 
ZdcSimreco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();

  PHNodeIterator iter(topNode);

  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  PHCompositeNode* zdcNode = new PHCompositeNode("ZDC");
  topNode->addNode(zdcNode);

  //-* contains raw values (TDCs, ADCs, etc)
  ZdcRaw *zdcraw = new ZdcRawv1();
  PHObjectNode_t *ZdcRawNode = new PHObjectNode_t(zdcraw,"ZdcRaw","PHObject");
  dstNode->addNode(ZdcRawNode);

  //-* contains final physics products (energies, tof, etc)
  ZdcOut *zdcout = new ZdcOutv1();
  PHObjectNode_t *ZdcOutNode = new PHObjectNode_t(zdcout,"ZdcOut","PHObject");
  dstNode->addNode(ZdcOutNode);

  ZdcGeaHits *zdcgea = new ZdcGeaHitsv1();
  PHObjectNode_t *ZdcGeaNode = new PHIODataNode<PHObject>(zdcgea,"ZdcGeaHits","PHObject");
  
  PHCompositeNode *geaNode =
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "GEA"));
  geaNode->addNode(ZdcGeaNode);
  
  //-* response module
  mZdcResponse = new ZdcResponse();
  mZdcEvent = new ZdcEvent();	// this module does all the reconstruction work
  PHTimeStamp TimeStp = rc->get_TimeStamp();
  mZdcEvent->setCalibDataAll(TimeStp);	// set calibration by TimeStamp
  mZdcEvent->setEventNumber(0);

  return 0;
}

int 
ZdcSimreco::process_event(PHCompositeNode *topNode)
{
  // run response
  if ( mZdcResponse )
    {
      //mZdcResponse->Clear();		    // reset to default values
      mZdcResponse->PutEndProduct(topNode); // copy geant hits to PRDF
      mZdcEvent->setCalibDataAll();
    }

  // run reconstruction
  if ( mZdcEvent )
    {
      mZdcEvent->Clear();                // reset to default values
      if (!mZdcResponse)
        {
          ZdcGetDCM(topNode);                // copy packet from PRDF to ZdcRaw
        }
      mZdcEvent->PutEndProduct(topNode); // reconstruct and put in ZdcOutNode
    }

  return 0;
}

int 
ZdcSimreco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("ZDC"))
    {
      mainIter.forEach(reset);
    }

  return 0;
}
