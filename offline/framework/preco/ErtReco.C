#include <ErtReco.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>

#include <EMCalRichDecode.h>
#include <ErtOutv1.h>

#include <recoConsts.h>

typedef PHIODataNode <PHObject> PHObjectNode_t;

using namespace std;

ErtReco::ErtReco(const string &name): SubsysReco(name)
{
  ERTdecode = 0;
  return ;
}

ErtReco::~ErtReco()
{
  delete ERTdecode;
  return;
}

int ErtReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int ErtReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int runnumber = rc->get_IntFlag("RUNNUMBER");
  int iret = 0;
  ERTdecode = new EMCalRichDecode(runnumber);
  return iret;
}

int ErtReco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);

  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  ErtOut* ertout = new ErtOutv1();
  PHObjectNode_t *ErtOutNode = new PHObjectNode_t(ertout, "ErtOut", "PHObject");
  dstNode->addNode(ErtOutNode);


  return 0;
}

int ErtReco::process_event(PHCompositeNode *topNode)
{
  int iret = 0;
  ERTdecode->Decode(topNode, 0);
  ERTdecode->DstStore(topNode);
  return iret;
}

