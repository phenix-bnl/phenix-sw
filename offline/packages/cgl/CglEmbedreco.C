#include "CglEmbedreco.h"

#include "cglDetectorGeo.hh"
#include "cglHitAssociate.hh"
#include "mPHDchTrackModel.hh"
#include "mPHLineTrack.hh"
#include "PHDetectorGeometry.h"
#include "dPHDchTrackWrapper.h"
#include "PHDchTrackOutv1.h"

#include "CglTrackv1.h"
#include "CglTrackv2.h"
#include "CglTrackv3.h"
#include "CglTrackv4.h"
#include "CglTrackv5.h"
#include "CglTrackv6.h"
#include "CglTrackv7.h"
#include "CglTrackv8.h"
#include "PHTrackOutv1.h"
#include "PHTrackOutv2.h"
#include "PHTrackOutv3.h"
#include "PHTrackOutv4.h"
#include "PHTrackOutv5.h"
#include "PHTrackOutv6.h"
#include "PHTrackOutv7.h"
#include "PHTrackOutv8.h"
#include "CglRecoDefs.h"
#include "DetectorGeometryv1.h"
#include <RunHeader.h>
#include <Acc.h>
#include <recoConsts.h>
#include <SvxClusterList.h>
#include <TofwHit.h>
#include <HbdBlobList.h>
#include <getClass.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <PHObject.h>
#include <PHTypedNodeIterator.h>

#include <iostream>

/*
#define SVXLAYERNUMBER 4
#define SVXLADDERNUMBER 20

static const double cglDetectorGeo_pc1Radius = 248.891;
static const double cglDetectorGeo_pc2Radius = 419.173;
static const double cglDetectorGeo_pc3Radius = 492.012;
static const double cglDetectorGeo_dchRadius = 220.0;
static const double cglDetectorGeo_crkRadius = 260.0;
static const double cglDetectorGeo_tecRadius = 430.0;
static const double cglDetectorGeo_tofRadius = 503.0;
static const double cglDetectorGeo_pbscRadius = 507.2;
static const double cglDetectorGeo_pbglRadius = 540.0;
static const double cglDetectorGeo_accRadius = 456.0;
static const double cglDetectorGeo_svxRadius[SVXLAYERNUMBER] = {2.5, 5.0, 10.0, 14.0};
static const double cglDetectorGeo_tofwRadius = 475.0;
static const double cglDetectorGeo_hbdRadius = 58.0;

static const short cglDetectorGeo_dchActive_east = 1;
static const short cglDetectorGeo_dchActive_west = 1;
static const short cglDetectorGeo_crkActive_east = 1;
static const short cglDetectorGeo_crkActive_west = 1;
static const short cglDetectorGeo_pc1Active_east[8] = {1, 1, 1, 1, 1, 1, 1, 1};
static const short cglDetectorGeo_pc1Active_west[8] = {1, 1, 1, 1, 1, 1, 1, 1};
static const short cglDetectorGeo_svxActive_east[SVXLAYERNUMBER][SVXLADDERNUMBER]
= {{1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
   {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
   {1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
   {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0}};
static const short cglDetectorGeo_svxActive_west[SVXLAYERNUMBER][SVXLADDERNUMBER]
= {{1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
   {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
   {1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
   {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0}};
*/

// PC2 EAST is enabled here because
// rich rings need the projections in case there is only
// PC1 with no PC2/PC3/EMC match. It then takes the PC2 projections
// to get an estimate of the ring position
// The order in which the RICH ring search is done is
// (analysis meeting Aug 4 2003 Kyoichiro Ozawa)
// PC1 - PC2
// PC1 - PC3
// PC1 - EMC
// PC1 - PC2 projection
// we loose the last ones if the projections are not done in cgl
// after disabling PC2 EAST
/*
static const short cglDetectorGeo_pc2Active_east[8] = {1, 1, 1, 1, 1, 1, 1, 1};
static const short cglDetectorGeo_pc2Active_east_sim[8] = {1, 1, 1, 1, 0, 0, 0, 0};
static const short cglDetectorGeo_pc2Active_west[8] = {1, 1, 1, 1, 1, 1, 1, 1};
static const short cglDetectorGeo_pc2Active_west_sim[8] = {1, 1, 1, 1, 0, 0, 0, 0};
static const short cglDetectorGeo_pc3Active_east[8] = {1, 1, 1, 1, 1, 1, 1, 1};
static const short cglDetectorGeo_pc3Active_east_sim[8] = {1, 1, 1, 1, 0, 0, 0, 0};
static const short cglDetectorGeo_pc3Active_west[8] = {1, 1, 1, 1, 1, 1, 1, 1};
static const short cglDetectorGeo_pc3Active_west_sim[8] = {1, 1, 1, 1, 0, 0, 0, 0};
static const short cglDetectorGeo_tecActive_east[4] = {0, 0, 0, 0}; // TKH--Run4 temporary
static const short cglDetectorGeo_tecActive_west[4] = {0, 0, 0, 0};
static const short cglDetectorGeo_tofActive_east[4] = {1, 1, 0, 0};
static const short cglDetectorGeo_tofActive_west[4] = {0, 0, 0, 0};
static const short cglDetectorGeo_tofwActive_east[4] = {0, 0, 0, 0};
static const short cglDetectorGeo_tofwActive_west[4] = {0, 1, 1, 0}; // for Run7 (W2 w3)
static const short cglDetectorGeo_pbscActive_east[4] = {0, 0, 1, 1};
static const short cglDetectorGeo_pbscActive_west[4] = {1, 1, 1, 1};
static const short cglDetectorGeo_pbglActive_east[4] = {1, 1, 0, 0};
static const short cglDetectorGeo_pbglActive_west[4] = {0, 0, 0, 0};
static const short cglDetectorGeo_accActive = 1;
static const short cglDetectorGeo_hbdActive_east[6] = {1, 1, 1, 1, 1, 1}; // HBD out for Run-08
static const short cglDetectorGeo_hbdActive_west[6] = {1, 1, 1, 1, 1, 1}; 
*/
using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;
typedef PHIODataNode <PHDchTrackOut> PHDchTrackNode_t;

CglEmbedreco::CglEmbedreco(const char *name):
  SubsysReco(name),
  dchTrackModel(NULL),
  cglDetGeo(NULL),
  cglLineTrack(NULL),
  CglHitAssociate(NULL),
  FieldOnFlag(1),
  TrigHelp(NULL)
{}

CglEmbedreco::~CglEmbedreco()
{
  delete dchTrackModel;
  delete cglLineTrack;
  delete CglHitAssociate;
}

int CglEmbedreco::Init(PHCompositeNode *topNode)
{
  dchTrackModel = new mPHDchTrackModel();
  cglLineTrack = new mPHLineTrack();
  CglHitAssociate = new cglHitAssociate();

  return 0;
}

int CglEmbedreco::InitRun(PHCompositeNode *topNode)
{

  CreateNodeTree(topNode);
  int iret = 0;
  recoConsts *rc = recoConsts::instance();

  cglDetGeo->set_pc1Radius(cglDetectorGeo_pc1Radius);
  cglDetGeo->set_pc2Radius(cglDetectorGeo_pc2Radius);
  cglDetGeo->set_pc3Radius(cglDetectorGeo_pc3Radius);
  cglDetGeo->set_dchRadius(cglDetectorGeo_dchRadius);
  cglDetGeo->set_crkRadius(cglDetectorGeo_crkRadius);
  cglDetGeo->set_tecRadius(cglDetectorGeo_tecRadius);
  cglDetGeo->set_tofRadius(cglDetectorGeo_tofRadius);
  cglDetGeo->set_pbscRadius(cglDetectorGeo_pbscRadius);
  cglDetGeo->set_pbglRadius(cglDetectorGeo_pbglRadius);
  cglDetGeo->set_accRadius(cglDetectorGeo_accRadius);
  cglDetGeo->set_tofwRadius(cglDetectorGeo_tofwRadius);
  for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++){
    cglDetGeo->set_svxRadius(ilayer, cglDetectorGeo_svxRadius[ilayer]);
  }
  cglDetGeo->set_hbdRadius(cglDetectorGeo_hbdRadius);
  
  cglDetGeo->set_dchActive(EAST, cglDetectorGeo_dchActive_east);
  cglDetGeo->set_dchActive(WEST, cglDetectorGeo_dchActive_west);
  cglDetGeo->set_crkActive(EAST, cglDetectorGeo_crkActive_east);
  cglDetGeo->set_crkActive(WEST, cglDetectorGeo_crkActive_west);
  cglDetGeo->set_pc1Active(EAST, cglDetectorGeo_pc1Active_east);
  cglDetGeo->set_pc1Active(WEST, cglDetectorGeo_pc1Active_west);
  cglDetGeo->set_pc2Active(EAST, cglDetectorGeo_pc2Active_east);
  cglDetGeo->set_pc2Active(EAST, cglDetectorGeo_pc2Active_east_sim);
  cglDetGeo->set_pc2Active(WEST, cglDetectorGeo_pc2Active_west);
  cglDetGeo->set_pc2Active(WEST, cglDetectorGeo_pc2Active_west_sim);
  cglDetGeo->set_pc3Active(EAST, cglDetectorGeo_pc3Active_east);
  cglDetGeo->set_pc3Active(EAST, cglDetectorGeo_pc3Active_east_sim);
  cglDetGeo->set_pc3Active(WEST, cglDetectorGeo_pc3Active_west); 
  cglDetGeo->set_pc3Active(WEST, cglDetectorGeo_pc3Active_west_sim);

  cglDetGeo->set_tecActive(EAST, cglDetectorGeo_tecActive_east);
  cglDetGeo->set_tecActive(WEST, cglDetectorGeo_tecActive_west);
  cglDetGeo->set_tofActive(EAST, cglDetectorGeo_tofActive_east);
  cglDetGeo->set_tofActive(WEST, cglDetectorGeo_tofActive_west);
  cglDetGeo->set_accActive(cglDetectorGeo_accActive);
  cglDetGeo->set_pbscActive(EAST, cglDetectorGeo_pbscActive_east);
  cglDetGeo->set_pbscActive(WEST, cglDetectorGeo_pbscActive_west);
  cglDetGeo->set_pbglActive(EAST, cglDetectorGeo_pbglActive_east);
  cglDetGeo->set_pbglActive(WEST, cglDetectorGeo_pbglActive_west);

  cglDetGeo->set_tofwActive(EAST, cglDetectorGeo_tofwActive_east);
  cglDetGeo->set_tofwActive(WEST, cglDetectorGeo_tofwActive_west);
  cglDetGeo->set_hbdActive(EAST, cglDetectorGeo_hbdActive_east);
  cglDetGeo->set_hbdActive(WEST, cglDetectorGeo_hbdActive_west);

  short int svxactive = 0;
  if(rc->FlagExist("SVXACTIVE"))
    {
      if(rc->get_IntFlag("SVXACTIVE") > 0)
	{
	  svxactive = 1;
	}
    }
  if(svxactive)
    {
      for(int ilayer = 0; ilayer < SVXLAYERNUMBER; ilayer++)
	{
	  cglDetGeo->set_svxActive(EAST, ilayer, cglDetectorGeo_svxActive_east[ilayer]);
	  cglDetGeo->set_svxActive(WEST, ilayer, cglDetectorGeo_svxActive_west[ilayer]);
	  cglDetGeo->set_SvxActive(1);
	}
    }
  else
    {
      short cglDetectorGeo_svxInActive[SVXLAYERNUMBER][SVXLADDERNUMBER]
        = {{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	   {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	   {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	   {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};
      for(int ilayer = 0; ilayer < SVXLAYERNUMBER; ilayer++)
	{
	  cglDetGeo->set_svxActive(EAST, ilayer, cglDetectorGeo_svxInActive[ilayer]);
	  cglDetGeo->set_svxActive(WEST, ilayer, cglDetectorGeo_svxInActive[ilayer]);
	  cglDetGeo->set_SvxActive(0);
	}
    }


  cglDetGeo->fetchAllGeo(topNode);

  // Setting cglHitAssociate Parameters
  short cglHitAssociate_Verbose = 0;
  CglHitAssociate->set_Verbose(cglHitAssociate_Verbose);
  double cglHitAssociate_TECZ0Buffer = 10.0;
  CglHitAssociate->set_TECZ0Buffer(cglHitAssociate_TECZ0Buffer);
  double cglHitAssociate_TECSlopeCut = 1.0;
  CglHitAssociate->set_TECSlopeCut(cglHitAssociate_TECSlopeCut);
  short cglHitAssociate_RemoveHits = 0;
  CglHitAssociate->set_RemoveHits(cglHitAssociate_RemoveHits);
  short cglHitAssociate_UseFlag[CGLMAXDET] = {1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0};

    if(svxactive) {
      cglHitAssociate_UseFlag[14] = 1;
      cglHitAssociate_UseFlag[15] = 1;
      cglHitAssociate_UseFlag[16] = 1;
      cglHitAssociate_UseFlag[17] = 1;
    }

  CglHitAssociate->set_UseFlag(cglHitAssociate_UseFlag);
  double cglHitAssociate_PhiRoad[CGLMAXDET] = {0.0, 0.0, 30.0, 15.0, 15.0, 0.0, 15.0, 15.0, 15.0, 15.0, 15.0, 15.0, 15.0, 15.0, 15.0, 15.0, 15.0, 15.0};
  double cglHitAssociate_ZRoad[CGLMAXDET] = {0.0, 0.0, 40.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0};
  CglHitAssociate->set_PhiRoad(cglHitAssociate_PhiRoad);
  CglHitAssociate->set_ZRoad(cglHitAssociate_ZRoad);
  double cglHitAssociate_MinPhiWidth = 0.12;
  CglHitAssociate->set_MinPhiWidth(cglHitAssociate_MinPhiWidth);
  double cglHitAssociate_MinZWidth = 50.0;
  CglHitAssociate->set_MinZWidth(cglHitAssociate_MinZWidth);

  int currentCentral = -9999;
  RunHeader *runheader = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if (!runheader)
    {
      cout << "CglEmbedreco::InitRun no Run Header" << endl;
    }
  else
    {
      currentCentral = runheader->get_currentCentral();
    }

  if (abs(currentCentral) < 100)
    {
      CglHitAssociate->set_PredictMomentum(0);
      CglHitAssociate->set_TrackModelFlag(0);
      FieldOnFlag = 0;
    }
  else
    {
      CglHitAssociate->set_PredictMomentum(1);
      CglHitAssociate->set_TrackModelFlag(2);
      FieldOnFlag = 1;
    }

  short cglHitAssociate_MinDchQuality = 0;
  CglHitAssociate->set_MinDchQuality(cglHitAssociate_MinDchQuality);
  short cglHitAssociate_PhiRoadOnly = 1;
  CglHitAssociate->set_PhiRoadOnly(cglHitAssociate_PhiRoadOnly);
  short cglHitAssociate_MaxDchQuality = 4;
  CglHitAssociate->set_MaxDchQuality(cglHitAssociate_MaxDchQuality);
  CglHitAssociate->set_UseFlag(10, 0);
  CglHitAssociate->set_UseFlag(11, 0);
  CglHitAssociate->set_UseFlag(12, 0);  //aerogel
  CglHitAssociate->set_UseFlag(13, 1);  //tofw
  CglHitAssociate->set_UseFlag(18, 1);  //hbd 
  CglHitAssociate->PrintParameters();
  PHDetectorGeometry *detgeom = PHDetectorGeometry::instance();
  detgeom->copycglDetectorGeo(topNode);
  return iret;
}

int CglEmbedreco::CreateNodeTree(PHCompositeNode *topNode)
{

  enum {DSTNODE, PARNODE, RUNNODE, LAST};
  // first test if neccessary nodes have been created, if not bail out
  const char *NName[] = {
    "DST",
    "PAR",
    "RUN"};

  PHNodeIterator iter(topNode);
  PHCompositeNode *outNode[LAST];
  for (short int i = 0;i < LAST;i++)
    {
      outNode[i] = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", NName[i]));
      if (!outNode[i])
        {
          cerr << PHWHERE << NName[i] << " Node is missing doing nothing" << endl;
          return -1;
        }
    }

  PHCompositeNode* cglNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "CGL"));
  if (! cglNode)
    {
      cglNode = new PHCompositeNode("CGL");
      topNode->addNode(cglNode);
    }

  cglDetGeo = new cglDetectorGeo();
  PHDataNode<cglDetectorGeo> *cglDetectorGeoNode = new PHDataNode<cglDetectorGeo>(cglDetGeo, "cglDetectorGeo");
  outNode[PARNODE]->addNode(cglDetectorGeoNode);

  PHObjectNode_t *svgeomNode = static_cast<PHObjectNode_t*>(iter.findFirst("PHIODataNode", "DetectorGeometry"));
  if(!svgeomNode){
    DetectorGeometry *svgeom = new DetectorGeometryv1();
    svgeomNode = new PHObjectNode_t(svgeom,"DetectorGeometry");
    outNode[RUNNODE]->addNode(svgeomNode);
  }

  TofwHit *tofw = findNode::getClass<TofwHit>(topNode, "TofwHit");
  SvxClusterList *svx = findNode::getClass<SvxClusterList>(topNode, "SvxClusterList");
  HbdBlobList *hbd = findNode::getClass<HbdBlobList>(topNode, "HbdBlobList");

  CglTrack *cgltrack = findNode::getClass<CglTrack>(topNode, "CglTrack");
  if (!cgltrack)
    {
      if(tofw || hbd) cgltrack = new CglTrackv8();
      else
        {
          if(svx) {cgltrack = new CglTrackv6();}
	  else {cgltrack = new CglTrackv4();}
        }
      PHObjectNode_t *CglTrackNode = new PHObjectNode_t(cgltrack, "CglTrack", "PHObject");
      outNode[DSTNODE]->addNode(CglTrackNode);
    }

  CglTrack *cgltrackback = findNode::getClass<CglTrack>(topNode, "CglTrackBack");
  if (!cgltrackback)
    {
      if(tofw || hbd) cgltrackback = new CglTrackv8();
      else
        {
          if(svx) {cgltrackback = new CglTrackv6();}
	  else {cgltrackback = new CglTrackv4();}
        }

      PHObjectNode_t *CglTrackBackNode =
        new PHObjectNode_t(cgltrackback, "CglTrackBack", "PHObject");
      outNode[DSTNODE]->addNode(CglTrackBackNode);
    }

  PHTrackOut *phtrackout = findNode::getClass<PHTrackOut>(topNode, "PHTrackOut");
  if (!phtrackout)
    {
      if(tofw || hbd ) phtrackout = new PHTrackOutv8();
      else
        {
          if(svx) {phtrackout = new PHTrackOutv6();}
	  else {phtrackout = new PHTrackOutv4();}
        }

      PHObjectNode_t *PHTrackOutNode =
        new PHObjectNode_t(phtrackout, "PHTrackOut", "PHObject"); // contain PHObject
      outNode[DSTNODE]->addNode(PHTrackOutNode);
    }


  PHTrackOut *phtrackbackout = findNode::getClass<PHTrackOut>(topNode, "PHTrackOutBack");
  if (!phtrackbackout)
    {
      if(tofw || hbd) phtrackbackout = new PHTrackOutv8();
      else
        {
          if(svx) {phtrackbackout = new PHTrackOutv6();}
	  else {phtrackbackout = new PHTrackOutv4();}
        }
      
      PHObjectNode_t *PHTrackBackOutNode =
        new PHObjectNode_t(phtrackbackout, "PHTrackOutBack", "PHObject"); // contain PHObjec
      outNode[DSTNODE]->addNode(PHTrackBackOutNode);
    }

  PHObjectNode_t *PHDchTrackNode = static_cast<PHObjectNode_t*>(iter.findFirst("PHIODataNode", "PHDchTrackOut"));
  if (!PHDchTrackNode)
    {
      PHDchTrackOut *phdchtrackout = new PHDchTrackOutv1();
      PHDchTrackNode =
        new PHObjectNode_t(phdchtrackout, "PHDchTrackOut", "PHObject");
      outNode[DSTNODE]->addNode(PHDchTrackNode);
    }

  PHTableNode_t *dPHDchTrackNode = static_cast<PHTableNode_t*>(iter.findFirst("PHIODataNode", "dPHDchTrack"));
  if (!dPHDchTrackNode)
    {
      int mr = 2000;
      dPHDchTrackWrapper* dPHDchTrack = new dPHDchTrackWrapper("dPHDchTrack", mr);
      dPHDchTrackNode = new PHTableNode_t(dPHDchTrack, "dPHDchTrack");
      cglNode->addNode(dPHDchTrackNode);
    }
  return 0;
}

int CglEmbedreco::process_event(PHCompositeNode *topNode)
{
  int iret = 0;
  if (FieldOnFlag)
    {
      dchTrackModel->event(topNode);
    }
  else
    {
      cglLineTrack->event(topNode);
    }
  CglHitAssociate->event(topNode);
  copyWrapper(topNode);
  return iret;
}

int CglEmbedreco::ResetEvent(PHCompositeNode *topNode)
{

  dchTrackModel->ResetEvent(topNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("CGL"))
    {
      mainIter.forEach(reset);
    }
  return 0;
}

int CglEmbedreco::copyWrapper(PHCompositeNode *topNode)
{
  dPHDchTrackWrapper *dphdchtrack = 0;
  PHTypedNodeIterator<dPHDchTrackWrapper> phdchtrack_iter(topNode);
  PHIODataNode <dPHDchTrackWrapper> *dPHDchtrackNode = phdchtrack_iter.find("dPHDchTrack");
  if (dPHDchtrackNode)
    {
      dphdchtrack = dPHDchtrackNode->getData();
    }
  else
    {
      return -1;
    }

  PHDchTrackOut *phdchtrackout = 0;
  PHTypedNodeIterator<PHDchTrackOut> phdchtrackoutiter(topNode);
  PHDchTrackNode_t *PHDchTrackNode = phdchtrackoutiter.find("PHDchTrackOut");
  if (PHDchTrackNode)
    {
      phdchtrackout = PHDchTrackNode->getData();
    }
  if (dphdchtrack && phdchtrackout)
    {
      if (!phdchtrackout->isValid() && dphdchtrack->RowCount())
        {
          phdchtrackout->FillFromWrapper(dphdchtrack);
        }
    }
  return 0;
}
