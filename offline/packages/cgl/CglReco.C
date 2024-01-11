#include "CglReco.h"


#include "cglDetectorGeo.hh"
#include "cglHitAssociate.hh"
#include "cglTransformDST.hh"
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
#include "DetectorGeometryv1.h"
#include "CglRecoDefs.h"

#include <RunHeader.h>
#include <Acc.h>
#include <recoConsts.h>
#include <TofwHit.h>
#include <SvxClusterList.h>
#include <HbdMiniCellList.h>
#include <HbdCellList.h>

#include <getClass.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <PHObject.h>
#include <PHTypedNodeIterator.h>
#include <TriggerHelper.h>


#include <iostream>
using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode <PHDchTrackOut> PHDchTrackNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;

CglReco::CglReco(const string  &name):
  SubsysReco(name),
  dchTrackModel(NULL),
  CglDetGeo(NULL),
  cglLineTrack(NULL),
  CglHitAssociate(NULL),
  FieldOnFlag(1),
  TrigHelp(NULL),
  SvxUseAsciiFile(false)
{}

CglReco::~CglReco()
{
  delete dchTrackModel;
  delete cglLineTrack;
  delete CglHitAssociate;
  delete TrigHelp;
}

int CglReco::Init(PHCompositeNode *topNode)
{
  dchTrackModel = new mPHDchTrackModel();
  cglLineTrack = new mPHLineTrack();
  CglHitAssociate = new cglHitAssociate();

  return 0;
}

int CglReco::InitRun(PHCompositeNode *topNode)
{
  TrigHelp = new TriggerHelper(topNode);
  CreateNodeTree(topNode);
  int iret = 0;
  recoConsts *rc = recoConsts::instance();


  CglDetGeo->set_Verbose(10);

  CglDetGeo->set_pc1Radius(cglDetectorGeo_pc1Radius);
  CglDetGeo->set_pc2Radius(cglDetectorGeo_pc2Radius);
  CglDetGeo->set_pc3Radius(cglDetectorGeo_pc3Radius);
  CglDetGeo->set_dchRadius(cglDetectorGeo_dchRadius);
  CglDetGeo->set_crkRadius(cglDetectorGeo_crkRadius);
  CglDetGeo->set_tecRadius(cglDetectorGeo_tecRadius);
  CglDetGeo->set_tofRadius(cglDetectorGeo_tofRadius);
  CglDetGeo->set_pbscRadius(cglDetectorGeo_pbscRadius);
  CglDetGeo->set_pbglRadius(cglDetectorGeo_pbglRadius);
  CglDetGeo->set_accRadius(cglDetectorGeo_accRadius);
  CglDetGeo->set_tofwRadius(cglDetectorGeo_tofwRadius);
  for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++){
    CglDetGeo->set_svxRadius(ilayer, cglDetectorGeo_svxRadius[ilayer]);
  }
  CglDetGeo->set_hbdRadius(cglDetectorGeo_hbdRadius);

  CglDetGeo->set_dchActive(EAST, cglDetectorGeo_dchActive_east);
  CglDetGeo->set_dchActive(WEST, cglDetectorGeo_dchActive_west);
  CglDetGeo->set_crkActive(EAST, cglDetectorGeo_crkActive_east);
  CglDetGeo->set_crkActive(WEST, cglDetectorGeo_crkActive_west);
  CglDetGeo->set_pc1Active(EAST, cglDetectorGeo_pc1Active_east);
  CglDetGeo->set_pc1Active(WEST, cglDetectorGeo_pc1Active_west);
  if (rc->FlagExist("SIMULATIONFLAG") && rc->get_IntFlag("SIMULATIONFLAG") > 0 && rc->get_IntFlag("LOOKATSIMFROMEMBED",1) == 1)
    {
      CglDetGeo->set_pc2Active(EAST, cglDetectorGeo_pc2Active_east_sim);
      CglDetGeo->set_pc2Active(WEST, cglDetectorGeo_pc2Active_west_sim);
      CglDetGeo->set_pc3Active(EAST, cglDetectorGeo_pc3Active_east_sim);
      CglDetGeo->set_pc3Active(WEST, cglDetectorGeo_pc3Active_west_sim);
    }
  else
    {
      CglDetGeo->set_pc2Active(EAST, cglDetectorGeo_pc2Active_east);
      CglDetGeo->set_pc2Active(WEST, cglDetectorGeo_pc2Active_west);
      CglDetGeo->set_pc3Active(EAST, cglDetectorGeo_pc3Active_east);
      CglDetGeo->set_pc3Active(WEST, cglDetectorGeo_pc3Active_west);
    }
  CglDetGeo->set_tecActive(EAST, cglDetectorGeo_tecActive_east);
  CglDetGeo->set_tecActive(WEST, cglDetectorGeo_tecActive_west);
  CglDetGeo->set_tofActive(EAST, cglDetectorGeo_tofActive_east);
  CglDetGeo->set_tofActive(WEST, cglDetectorGeo_tofActive_west);
  CglDetGeo->set_accActive(cglDetectorGeo_accActive);
  CglDetGeo->set_pbscActive(EAST, cglDetectorGeo_pbscActive_east);
  CglDetGeo->set_pbscActive(WEST, cglDetectorGeo_pbscActive_west);
  CglDetGeo->set_pbglActive(EAST, cglDetectorGeo_pbglActive_east);
  CglDetGeo->set_pbglActive(WEST, cglDetectorGeo_pbglActive_west);
  CglDetGeo->set_tofwActive(EAST, cglDetectorGeo_tofwActive_east);
  CglDetGeo->set_tofwActive(WEST, cglDetectorGeo_tofwActive_west);
  CglDetGeo->set_hbdActive(EAST, cglDetectorGeo_hbdActive_east);
  CglDetGeo->set_hbdActive(WEST, cglDetectorGeo_hbdActive_west);

  short int svxactive = 0;
  if(SvxUseAsciiFile)
    {
      CglDetGeo->set_SvxUseAsciiFile(true);
    }
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
	  CglDetGeo->set_svxActive(EAST, ilayer, cglDetectorGeo_svxActive_east[ilayer]);
	  CglDetGeo->set_svxActive(WEST, ilayer, cglDetectorGeo_svxActive_west[ilayer]);
	  CglDetGeo->set_SvxActive(1);
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
	  CglDetGeo->set_svxActive(EAST, ilayer, cglDetectorGeo_svxInActive[ilayer]);
	  CglDetGeo->set_svxActive(WEST, ilayer, cglDetectorGeo_svxInActive[ilayer]);
	  CglDetGeo->set_SvxActive(0);
	}
    }


  CglDetGeo->fetchAllGeo(topNode);

  // Setting cglHitAssociate Parameters
  short cglHitAssociate_Verbose = 10;
  CglHitAssociate->set_Verbose(cglHitAssociate_Verbose);
  double cglHitAssociate_TECZ0Buffer = 10.0;
  CglHitAssociate->set_TECZ0Buffer(cglHitAssociate_TECZ0Buffer);
  double cglHitAssociate_TECSlopeCut = 1.0;
  CglHitAssociate->set_TECSlopeCut(cglHitAssociate_TECSlopeCut);
  short cglHitAssociate_RemoveHits = 0;
  CglHitAssociate->set_RemoveHits(cglHitAssociate_RemoveHits);
  short cglHitAssociate_UseFlag[CGLMAXDET] = {1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0};
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

  RunHeader *runheader = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if (!runheader)
    {
      cerr << "CglReco::InitRun no Run Header" << endl;
      exit(1);
    }
  int currentCentral = runheader->get_currentCentral();
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

  cout << PHWHERE << "what field flag is selected?, FieldOnFlag = " << FieldOnFlag << endl;

  short cglHitAssociate_MinDchQuality = 0;
  CglHitAssociate->set_MinDchQuality(cglHitAssociate_MinDchQuality);
  short cglHitAssociate_PhiRoadOnly = 1;
  CglHitAssociate->set_PhiRoadOnly(cglHitAssociate_PhiRoadOnly);
  short cglHitAssociate_MaxDchQuality = 4;
  CglHitAssociate->set_MaxDchQuality(cglHitAssociate_MaxDchQuality);
  CglHitAssociate->set_UseFlag(10, 0);
  CglHitAssociate->set_UseFlag(11, 0);
  CglHitAssociate->set_UseFlag(12, 1);  //aerogel
  CglHitAssociate->set_UseFlag(13, 1);  //tofw
//  CglHitAssociate->set_UseFlag(18, 0);  //hbd - out for Run-08
  CglHitAssociate->set_UseFlag(18, 1);  //hbd - in for Run-09

  CglHitAssociate->PrintParameters();
  PHDetectorGeometry *detgeom = PHDetectorGeometry::instance();
  detgeom->copycglDetectorGeo(topNode);

  return iret;
}

int CglReco::CreateNodeTree(PHCompositeNode *topNode)
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

  CglDetGeo = new cglDetectorGeo();
  PHDataNode<cglDetectorGeo> *cglDetectorGeoNode = new PHDataNode<cglDetectorGeo>(CglDetGeo, "cglDetectorGeo");
  outNode[PARNODE]->addNode(cglDetectorGeoNode);

  PHObjectNode_t *svgeomNode = static_cast<PHObjectNode_t*>(iter.findFirst("PHIODataNode", "DetectorGeometry"));
  if (!svgeomNode)
    {
      DetectorGeometry *svgeom = new DetectorGeometryv1();
      svgeomNode = new PHObjectNode_t(svgeom, "DetectorGeometry");
      outNode[RUNNODE]->addNode(svgeomNode);
    }

  TofwHit *tofw = findNode::getClass<TofwHit>(topNode, "TofwHit");
  SvxClusterList *svx = findNode::getClass<SvxClusterList>(topNode, "SvxClusterList");
  HbdMiniCellList *hbdmini = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
  HbdCellList * hbd = findNode::getClass<HbdCellList>(topNode, "HbdCellList");

  CglTrack *cgltrack = findNode::getClass<CglTrack>(topNode, "CglTrack");
  if (!cgltrack)
    {
//      else if(hbd) cgltrack = new CglTrackv7();
      if(tofw || hbd || hbdmini)
      {
	 cgltrack = new CglTrackv8();
	 cout << PHWHERE  <<  "CglTrackv8 added" << endl;
      }
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
      if(tofw || hbd || hbdmini)
      {
         cgltrackback = new CglTrackv8();
         cout <<  PHWHERE  << "CglTrackBackv8 added" << endl;
      }
//      else if(hbd) cgltrackback = new CglTrackv7();
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
      if(tofw || hbd || hbdmini) 
      {
         phtrackout = new PHTrackOutv8();
	cout <<  PHWHERE  << "PHTrackOutv8 added" << endl;
      }
//      else if(hbd) phtrackout = new PHTrackOutv7();
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
      if(tofw || hbd || hbdmini) 
      {
         phtrackbackout = new PHTrackOutv8();
        cout <<  PHWHERE  << "PHTrackOutBackv8 added" << endl;
      }
//      else if(hbd) phtrackbackout = new PHTrackOutv7();
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
      int mr = 2000; // has to be the same as for dch
      dPHDchTrackWrapper* dPHDchTrack = new dPHDchTrackWrapper("dPHDchTrack", mr);
      dPHDchTrackNode = new PHTableNode_t(dPHDchTrack, "dPHDchTrack");
      cglNode->addNode(dPHDchTrackNode);
    }
  return 0;
}

int CglReco::process_event(PHCompositeNode *topNode)
{
  int iret = 0;
  // do not process ppg events
  if (TrigHelp->didLevel1TriggerGetScaled("PPG(Laser)")
      || TrigHelp->didLevel1TriggerGetScaled("PPG(Pedestal)")
      || TrigHelp->didLevel1TriggerGetScaled("PPG(Test Pulse)"))
    {
      if (verbosity > 0)
        {
          cout << PHWHERE << "PPG Trigger not processed " << endl;
        }
      return 0;
    }
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

int CglReco::ResetEvent(PHCompositeNode *topNode)
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

int CglReco::copyWrapper(PHCompositeNode *topNode)
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
