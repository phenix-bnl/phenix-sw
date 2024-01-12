#include "CrkSimreco.h"

#include "TCrkModule.h"

#include "crkghitWrapper.h"
#include "dCrkCalWrapper.h"
#include "dCrkDCMparWrapper.h"
#include "dCrkDCMWrapper.h"
#include "dCrkFEMWrapper.h"
#include "dCrkGeoWrapper.h"
#include "dCrkGhitRawParWrapper.h"
#include "dCrkHitWrapper.h"
#include "dCrkRawFEMparWrapper.h"
#include "dCrkRawHitParWrapper.h"
#include "dCrkRawWrapper.h"
#include "dCrkRel2sWrapper.h"
#include "dCrkUcalWrapper.h"

#include "CrkHitv1.h"

#include "CrkGetGEA.h"
#include "CrkPutDCM.h"
#include "CrkPutDCMReCal.h"

#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "recoConsts.h"
#include <fstream>

#include "getClass.h"

using namespace std;

//_______________________________________________________________
CrkSimreco::CrkSimreco(const string &name): 
  SubsysReco(name),
  crkdao( "crk_cabling_vrdc.txt" )
{}

//_______________________________________________________________
CrkSimreco::~CrkSimreco( void )
{
  // nothing to be deleted here
  // the only pointers that are created with "new" are stored
  // in the node tree and thus deleted by Fun4All.
}

//_______________________________________________________________
int CrkSimreco::InitRun(PHCompositeNode *topNode)
{
  enum {DCMNODE, DSTNODE, EVANODE, GEANODE, PARNODE, LAST};
  const char *NName[] = {
    "DCM",
    "DST",
    "EVA",
    "GEA",
    "PAR"};

  PHNodeIterator iter(topNode);
  PHCompositeNode *outNode[LAST];
  for (short int i = 0;i < LAST;i++)
    {
      outNode[i] = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", NName[i]));
      if (!outNode[i])
        {
          cout << PHWHERE << NName[i] << " Node is missing doing nothing" << endl;
          return -1;
        }
    }

  recoConsts *rc = recoConsts::instance();


  PHCompositeNode* crkNode = new PHCompositeNode("CRK");
  topNode->addNode(crkNode);


  dCrkDCMparWrapper* dCrkDCMpar = new dCrkDCMparWrapper("dCrkDCMpar", 1);
  PHIODataNode<PHTable>* dCrkDCMparNode = new PHIODataNode<PHTable>(dCrkDCMpar, "dCrkDCMpar");
  outNode[PARNODE]->addNode(dCrkDCMparNode);
  dCrkDCMpar->set_mode(0, 1);
  dCrkDCMpar->set_threshold(0, 0);
  dCrkDCMpar->SetRowCount(1);


  dCrkGeoWrapper* dCrkGeo = new dCrkGeoWrapper("dCrkGeo", 1);
  PHIODataNode<PHTable>* dCrkGeoNode = new PHIODataNode<PHTable>(dCrkGeo, "dCrkGeo");
  outNode[PARNODE]->addNode(dCrkGeoNode);


  dCrkGhitRawParWrapper* dCrkGhitRawPar = new dCrkGhitRawParWrapper("dCrkGhitRawPar", 1);
  PHIODataNode<PHTable>* dCrkGhitRawParNode = new PHIODataNode<PHTable>(dCrkGhitRawPar, "dCrkGhitRawPar");
  outNode[PARNODE]->addNode(dCrkGhitRawParNode);
  dCrkGhitRawPar->set_ghitraw(0, 0);
  dCrkGhitRawPar->set_max_adc(0, 1024);
  dCrkGhitRawPar->set_max_tdc(0, 1024);
  dCrkGhitRawPar->set_min_tdc(0, 0);
  dCrkGhitRawPar->set_N0_pisa(0, 200.0);
  dCrkGhitRawPar->set_sinTmax(0, 0.5);
  dCrkGhitRawPar->SetRowCount(1);

  dCrkRawFEMparWrapper* dCrkRawFEMpar = new dCrkRawFEMparWrapper("dCrkRawFEMpar", 1);
  PHIODataNode<PHTable>* dCrkRawFEMparNode = new PHIODataNode<PHTable>(dCrkRawFEMpar, "dCrkRawFEMpar");
  outNode[PARNODE]->addNode(dCrkRawFEMparNode);
  dCrkRawFEMpar->set_mplex(0, 1);
  dCrkRawFEMpar->set_fem_mask(0, 0xFFFF);
  dCrkRawFEMpar->SetRowCount(1);


  dCrkRawHitParWrapper* dCrkRawHitPar = new dCrkRawHitParWrapper("dCrkRawHitPar", 1);
  PHIODataNode<PHTable>* dCrkRawHitParNode = new PHIODataNode<PHTable>(dCrkRawHitPar, "dCrkRawHitPar");
  outNode[PARNODE]->addNode(dCrkRawHitParNode);
  dCrkRawHitPar->set_min_pe(0, 0.1);
  dCrkRawHitPar->SetRowCount(1);

  dCrkUcalWrapper* dCrkUcal = new dCrkUcalWrapper("dCrkUcal", 5120);
  PHIODataNode<PHTable>* dCrkUcalNode = new PHIODataNode<PHTable>(dCrkUcal, "dCrkUcal");
  outNode[PARNODE]->addNode(dCrkUcalNode);
  dCrkUcal->set_gain(0, 80.);
  dCrkUcal->set_ped(0, 10.);
  dCrkUcal->set_clock(0, 10.);
  dCrkUcal->set_t0(0, 100.);
  dCrkUcal->set_slew(0, 0.);
  dCrkUcal->set_N0p(0, 150.);
  dCrkUcal->set_P_noise(0, 0.);
  dCrkUcal->set_mean_noise(0, 10.);
  dCrkUcal->set_sigma_pe(0, 0.3);
  dCrkUcal->set_sigma_t(0, 0.25);
  dCrkUcal->SetRowCount(1);
  // this copies the initialization to all 5120 table entries
  TCrkModule::setUcal(topNode);


  dCrkRel2sWrapper* dCrkRel2s = new dCrkRel2sWrapper("dCrkRel2s", 5120);
  PHIODataNode<PHTable>* dCrkRel2sNode = new PHIODataNode<PHTable>(dCrkRel2s, "dCrkRel2s");
  outNode[EVANODE]->addNode(dCrkRel2sNode);


  dCrkFEMWrapper* dCrkFEM = new dCrkFEMWrapper("dCrkFEM", 1500);
  PHIODataNode<PHTable>* dCrkFEMNode = new PHIODataNode<PHTable>(dCrkFEM, "dCrkFEM");
  crkNode->addNode(dCrkFEMNode);

  dCrkFEMWrapper* dCrkFEMReCal = new dCrkFEMWrapper("dCrkFEMReCal", 1500);
  PHIODataNode<PHTable>* dCrkFEMReCalNode = new PHIODataNode<PHTable>(dCrkFEMReCal, "dCrkFEMReCal");
  crkNode->addNode(dCrkFEMReCalNode);

  dCrkDCMWrapper* dCrkDCM = new dCrkDCMWrapper("dCrkDCM", 1500);
  PHIODataNode<PHTable>* dCrkDCMNode = new PHIODataNode<PHTable>(dCrkDCM, "dCrkDCM");
  outNode[DCMNODE]->addNode(dCrkDCMNode);

  dCrkDCMWrapper* dCrkDCMReCal = new dCrkDCMWrapper("dCrkDCMReCal", 1500);
  PHIODataNode<PHTable>* dCrkDCMReCalNode = new PHIODataNode<PHTable>(dCrkDCMReCal, "dCrkDCMReCal");
  outNode[DCMNODE]->addNode(dCrkDCMReCalNode);

  crkghitWrapper* crkghit = new crkghitWrapper("crkghit", 1500);
  PHIODataNode<PHTable>* crkghitNode = new PHIODataNode<PHTable>(crkghit, "crkghit");
  outNode[GEANODE]->addNode(crkghitNode);

  dCrkCalWrapper* dCrkCal = new dCrkCalWrapper("dCrkCal", 5120);
  PHIODataNode<PHTable>* dCrkCalNode = new PHIODataNode<PHTable>(dCrkCal, "dCrkCal");
  outNode[PARNODE]->addNode(dCrkCalNode);

  dCrkRawWrapper* dCrkRaw = findNode::getClass<dCrkRawWrapper>(topNode, "dCrkRaw");
  if (!dCrkRaw)
    {
      dCrkRaw = new dCrkRawWrapper("dCrkRaw", 5120);
      PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(dCrkRaw, "dCrkRaw");
      crkNode->addNode(NewNode);
    }

  dCrkRawWrapper* dCrkRawReCal = new dCrkRawWrapper("dCrkRawReCal", 5120);
  PHIODataNode<PHTable>* dCrkRawReCalNode = new PHIODataNode<PHTable>(dCrkRawReCal, "dCrkRawReCal");
  crkNode->addNode(dCrkRawReCalNode);

  dCrkHitWrapper* dCrkHit = findNode::getClass<dCrkHitWrapper>(topNode, "dCrkHit");
  if (!dCrkHit)
    {
      dCrkHit = new dCrkHitWrapper("dCrkHit", 5120);
      PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(dCrkHit, "dCrkHit");
      outNode[DSTNODE]->addNode(NewNode);
    }

  CrkHit *crkhit = findNode::getClass<CrkHit>(topNode, "CrkHit");
  if (!crkhit)
    {
      crkhit = new CrkHitv1();
      PHIODataNode<PHObject> *NewNode =
        new PHIODataNode<PHObject>(crkhit, "CrkHit", "PHObject");
      outNode[DSTNODE]->addNode(NewNode);
    }


  TCrkModule::setGeo(topNode);
  mCrkSimuRawReCal.SetCalibConst(rc->get_IntFlag("RUNNUMBER"));

  TCrkModule::setCal(topNode, rc->get_IntFlag("RUNNUMBER"));

  if (rc->FlagExist("CRKDEADPMTFILE"))
  {
      const char* deadpmts = rc->get_CharFlag("CRKDEADPMTFILE");
      std::cout << "CrkReco:: Using dead pmts from " << deadpmts << std::endl;
      int ipmt;
      ifstream fdead;
      fdead.open(deadpmts);
      while (fdead >> ipmt)
      {
	  dCrkUcal->set_gain(ipmt,0);
      }
      fdead.close();
  }

  return 0;
}

int
CrkSimreco::process_event(PHCompositeNode *topNode)
{
  CrkGetGEA(topNode);

  TCrkModule::ghitRaw(topNode);

  TCrkModule::rawFem(topNode);
  TCrkModule::femDcm(topNode);

  mCrkSimuRawReCal.event(topNode);
  TCrkModule::rawFem(topNode, "dCrkRawReCal", "dCrkFEMReCal");
  TCrkModule::femDcm(topNode, "dCrkFEMReCal", "dCrkDCMReCal");

  TCrkModule::rawHit(topNode);

  copyWrapper(topNode);

  return 0;
}

int
CrkSimreco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("CRK"))
    {
      mainIter.forEach(reset);
    }
  return 0;
}

int
CrkSimreco::copyWrapper(PHCompositeNode *topNode)
{
  dCrkHitWrapper *dcrkhit = findNode::getClass<dCrkHitWrapper>(topNode, "dCrkHit");
  if (dcrkhit)
    {
      CrkHit *crkhit = findNode::getClass<CrkHit>(topNode, "CrkHit");
      if (crkhit)
        {
          if (!crkhit->isValid() && dcrkhit->RowCount())
            {
              crkhit->FillFromWrapper(dcrkhit);
            }
        }
    }
  return 0;
}
