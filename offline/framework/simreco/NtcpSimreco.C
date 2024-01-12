#include "TObject.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "recoConsts.h"

#include "RunHeader.h"
#include <cmath>

#include "NtcpSimreco.h"
#include "NtcpEvent.h"
#include "NtcpRawv1.h"
#include "NtcpOutv2.h"

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;
typedef PHIODataNode <NtcpRaw> NtcpRawNode_t;
typedef PHIODataNode<PHObject> PHObjectNode_t;


NtcpSimreco::NtcpSimreco(const char *name)
{
  ThisName = name;

  mNtcpEvent = NULL;
}

int NtcpSimreco::Init(PHCompositeNode *topNode)
{
  return 0;
}

int NtcpSimreco::InitRun(PHCompositeNode *topNode)
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
  
  NtcpRaw* ntcpraw = new NtcpRawv1();
  PHIODataNode<PHObject>* NtcpRawNode = 
    new PHIODataNode<PHObject>(ntcpraw,"NtcpRaw","PHObject");
  dstNode->addNode(NtcpRawNode);
  
  NtcpOut *ntcpout = new NtcpOutv2();
  PHObjectNode_t *NtcpOutNode = 
    new PHIODataNode<PHObject>(ntcpout,"NtcpOut","PHObject");
  dstNode->addNode(NtcpOutNode);
  
  cout << PHWHERE << "Ntcp is not implemented for Simulationflag = " 
       << rc->get_IntFlag("SIMULATIONFLAG") << endl;

  return 0;
}

int NtcpSimreco::CreateNodeTree(PHCompositeNode *topNode)
{
  return 0;
}

int NtcpSimreco::process_event(PHCompositeNode *topNode)
{
  mNtcpEvent->clear();
  mNtcpEvent->event(topNode);
  mNtcpEvent->putEndProduct(topNode);

  return 0;
}

int NtcpSimreco::ResetEvent(PHCompositeNode *topNode)
{
  //  Nothing in Main reco so nothing here...
  return 0;
}
