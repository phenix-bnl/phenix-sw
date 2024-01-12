#include <CrkReco.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <recoConsts.h>

#include <CrkDAO.h>
#include <TCrkModule.h>

#include <CrkHitv1.h>
#include <dCrkCalWrapper.h>
#include <dCrkHitWrapper.h>
#include <dCrkRawHitParWrapper.h>
#include <dCrkRawWrapper.h>


#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode <CrkHit> CrkHitNode_t;

CrkReco::CrkReco(const string &name): SubsysReco(name)
{
  crkdao = 0;
  return ;
}

CrkReco::~CrkReco()
{
  delete crkdao;
  return;
}

int CrkReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int CrkReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  PHTimeStamp TimeStp = rc->get_TimeStamp();
  TCrkModule::setCal(topNode, TimeStp);
  crkdao = new CrkDAO("crk_cabling.txt");
  return 0;
}

int CrkReco::CreateNodeTree(PHCompositeNode *topNode)
{
  // first test if neccessary nodes have been created, if not bail out
  const char *NName[] = {
    "DST",
    "PAR"};
  short int NNode = 2;
  PHNodeIterator iter(topNode);
  PHCompositeNode *testNode;
  for (short int i = 0;i < NNode;i++)
    {
      testNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", NName[i]));
      if (!testNode)
        {
          cout << PHWHERE << NName[i] << " Node is missing doing nothing" << endl;
          return -1;
        }
    }

  PHCompositeNode* crkNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "CRK"));
  if (!crkNode)
    {
      crkNode = new PHCompositeNode("CRK");
      topNode->addNode(crkNode);
    }

  PHCompositeNode* parNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
  PHCompositeNode* dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));


  int mr, nrc;
  mr = 1;
  dCrkRawHitParWrapper *dCrkRawHitPar = new dCrkRawHitParWrapper("dCrkRawHitPar", mr);
  PHTableNode_t *dCrkRawHitParNode = new PHTableNode_t(dCrkRawHitPar, "dCrkRawHitPar");
  parNode->addNode(dCrkRawHitParNode);

  mr = 5120;
  dCrkCalWrapper* dCrkCal = new dCrkCalWrapper("dCrkCal", mr);
  PHTableNode_t *dCrkCalNode = new PHTableNode_t(dCrkCal, "dCrkCal");
  parNode->addNode(dCrkCalNode);

  dCrkRawWrapper* dCrkRaw = new dCrkRawWrapper("dCrkRaw", mr);
  PHTableNode_t *dCrkRawNode = new PHTableNode_t(dCrkRaw, "dCrkRaw");
  crkNode->addNode(dCrkRawNode);

  dCrkRawWrapper* dCrkRawReCal = new dCrkRawWrapper("dCrkRawReCal", mr);
  PHTableNode_t *dCrkRawReCalNode = new PHTableNode_t(dCrkRawReCal, "dCrkRawReCal");
  crkNode->addNode(dCrkRawReCalNode);

  dCrkHitWrapper* dCrkHit = new dCrkHitWrapper("dCrkHit", mr);
  PHTableNode_t *dCrkHitNode = new PHTableNode_t(dCrkHit, "dCrkHit");
  dstNode->addNode(dCrkHitNode);

  CrkHit *crkhit = new CrkHitv1();
  PHObjectNode_t *CrkHitNode =
    new PHIODataNode<PHObject>(crkhit, "CrkHit", "PHObject");
  dstNode->addNode(CrkHitNode);

  nrc = 1;
  dCrkRawHitPar->SetRowCount(nrc);
  dCrkRawHitPar->set_min_pe(0, 0.1);


  return 0;
}

int CrkReco::process_event(PHCompositeNode *topNode)
{
  TCrkModule::prdfRaw(topNode, crkdao);
  TCrkModule::rawHit(topNode);
  copyWrapper(topNode);
  return 0;
}

int CrkReco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("CRK"))
    {
      mainIter.forEach(reset);
    }
  return 0;
}

int CrkReco::copyWrapper(PHCompositeNode *topNode)
{
  dCrkHitWrapper *dcrkhit = 0;
  PHTypedNodeIterator<dCrkHitWrapper> dcrkhit_iter(topNode);
  PHIODataNode <dCrkHitWrapper> *dCrkHitNode = dcrkhit_iter.find("dCrkHit");
  if (dCrkHitNode)
    {
      dcrkhit = dCrkHitNode->getData();
    }
  else
    {
      return -1;
    }

  CrkHit *crkhit = 0;
  PHTypedNodeIterator<CrkHit> crkhititer(topNode);
  CrkHitNode_t *CrkHitNode = crkhititer.find("CrkHit");
  if (CrkHitNode)
    {
      crkhit = CrkHitNode->getData();
    }
  if (dcrkhit && crkhit)
    {
      if (!crkhit->isValid() && dcrkhit->RowCount())
        {
          crkhit->FillFromWrapper(dcrkhit);
        }
    }
  return 0;
}
