#include "DchReco.h"

#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeReset.h>

#include <RawDataCheck.h>

// Modules
#include <mNewDchCalibrator.hh>
#include <mNewDchCandidatory.hh>
#include <mNewDchEfficiencyCalibrator.hh>
#include <mNewDchInitializer.hh>
#include <mNewDchUnpacker.hh>
#include <PHDchGeometryObject.h>
#include <PHDchHistogrammer.hh>

#include <DchHitLineTablev2.hh>
#include <DchTrackv1.h>
#include <dDchHitWrapper.h>
#include <dDchReconstructionParWrapper.h>
#include <dDchTracksExtWrapper.h>
#include <dDchTracksWrapper.h>
#include <dDchUnpackParWrapper.h>

#include <getClass.h>
#include <recoConsts.h>

#include <iostream>

using namespace std;

DchReco::DchReco(const string &name): 
  SubsysReco(name),
  mDchInitializer(NULL),
  mDchUnpacker(NULL),
  mDchCalibrator(NULL),
  mDchCandidatory(NULL),
  mDchEfficiencyCalibrator(NULL),
  saveHistoes(0), // Don't save unless explicitly asked...
  norawdatacheck(0) // run with raw data check
{
  return ;
}

DchReco::~DchReco()
{
  delete mDchUnpacker;
  delete mDchCalibrator;
  delete mDchInitializer;
  delete mDchCandidatory;
  delete mDchEfficiencyCalibrator;
  return;
}

int DchReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  mDchUnpacker = new mNewDchUnpacker();
  mDchCalibrator = new mNewDchCalibrator();
  return iret;
}

int DchReco::End(PHCompositeNode *topNode)
{
  if (saveHistoes > 0 )
    {
      cout << PHWHERE << "Saving Dch Histogrammer." << endl;
      PHDchHistogrammer *dchHistogrammer = mDchInitializer->getHistogrammer();
      if (dchHistogrammer)
	{
	  dchHistogrammer->saveToFile(2);
	}
    }
  return 0;
}

int 
DchReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int iret = 0;
  mDchInitializer = new mNewDchInitializer(0, 1, 1, rc->get_IntFlag("RUNNUMBER")); // run 2000 (db access)
  mDchInitializer->setCalibrationFileName("DchCalibration.Real2001", "DchCalibrationSlew.Real2001", "DchCalibrationLocal.Real2001", "DchCalibrationStereo.Real"); // only until stereo info in DB...
  mDchInitializer->setNoiseFileName
    ("/afs/rhic/phenix/PHENIX_LIB/calibration/new/AlwaysDeadCh.dat",
     "/afs/rhic/phenix/PHENIX_LIB/calibration/new/DchEfficiency0_run2_v03-average.Real");       // Average map for AuAu run

  float eastdv=rc->get_FloatFlag("DVEASTSCALE",0.985);
  float westdv=rc->get_FloatFlag("DVWESTSCALE",0.985);
  float eastt0=rc->get_FloatFlag("T0EASTSHIFT",-6.65);
  float westt0=rc->get_FloatFlag("T0WESTSHIFT",-2.21);
  
  mDchCalibrator->setCalibCorr(eastt0, eastdv, westt0, westdv); // 1.4% correction to


  // east drift velocity for AuAu
  mDchCandidatory = new mNewDchCandidatory();
  mDchCandidatory->setMaxEventForUnsuppressedHits(100000);
  mDchEfficiencyCalibrator = new mNewDchEfficiencyCalibrator();
  mDchInitializer->event(topNode);
  mDchInitializer->getDGO()->rotateAndTranslate();
  return iret;
}

int 
DchReco::CreateNodeTree(PHCompositeNode *topNode)
{
  const char *NName[] = {
    "DST",
    "PAR"};
  PHNodeIterator iter(topNode);
  PHCompositeNode *testNode;
  for (short int i = 0;i < 2;i++)
    {
      testNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", NName[i]));
      if (!testNode)
        {
          cout << PHWHERE << NName[i] << " Node is missing doing nothing" << endl;
          return -1;
        }
    }

  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  PHCompositeNode *parNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));

  PHCompositeNode* dchNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DCH"));
  if (! dchNode)
    {
      dchNode = new PHCompositeNode("DCH");
      topNode->addNode(dchNode);
    }

  DchHitLineTable* dchhitlinetable = new DchHitLineTablev2();
  PHIODataNode<PHObject> *DchHitLineTableNode =
    new PHIODataNode<PHObject>(dchhitlinetable, "DchHitLineTable", "PHObject"); // contain PHObject
  dstNode->addNode(DchHitLineTableNode);

  DchTrack* dchtrack = new DchTrackv1();
  PHIODataNode<PHObject> *DchTrackNode =
    new PHIODataNode<PHObject>(dchtrack, "DchTrack", "PHObject"); // contain PHObject
  dstNode->addNode(DchTrackNode);

  dDchReconstructionParWrapper* dDchRecoPar = new dDchReconstructionParWrapper("dDchRecoPar", 1);
  PHIODataNode<PHTable>* dDchRecoParNode = new PHIODataNode<PHTable>(dDchRecoPar, "dDchRecoPar");
  parNode->addNode(dDchRecoParNode);

  dDchUnpackParWrapper* dDchUnpackPar = new dDchUnpackParWrapper("dDchUnpackPar", 1);
  PHIODataNode<PHTable>* dDchUnpackParNode = new PHIODataNode<PHTable>(dDchUnpackPar, "dDchUnpackPar");
  parNode->addNode(dDchUnpackParNode);

  dDchUnpackPar->SetRowCount(1);
  dDchUnpackPar->set_detIdWord(0, 1);
  dDchUnpackPar->set_dc111(0, 0);

  dDchRecoPar->SetRowCount(1);
  dDchRecoPar->set_houghThresholdOnXCell(0, 10) ;  //
  dDchRecoPar->set_houghThresholdOnXMask(0, 15) ;  //
  dDchRecoPar->set_houghThresholdOnUVCell(0, 3);  //
  dDchRecoPar->set_houghThresholdOnUVMask(0, 6) ; //
  dDchRecoPar->set_purgeCandidateThreshold(0, 15) ; //
  dDchRecoPar->set_firstXHoughThreshold(0, 10) ;  // mc is 15
  dDchRecoPar->set_secondXHoughThreshold(0, 10) ; // mc is 15
  dDchRecoPar->set_minimumNumberOfXHits(0, 8);
  dDchRecoPar->set_minimumNumberOfUVHits(0, 0);
  dDchRecoPar->set_XHitsThreshold(0, 10);
  dDchRecoPar->set_cellDifferenceCut(0, 8);
  dDchRecoPar->set_delBetaCut(0, 0.2);
  dDchRecoPar->set_deltaBetaCut(0, 0.2);
  dDchRecoPar->set_wireResolution(0, 0.015); //
  dDchRecoPar->set_initUVChi2(0, 10); //
  dDchRecoPar->set_initXChi2(0, 5);  //
  dDchRecoPar->set_deltaBetaVertexCut(0, 0.5); //
  dDchRecoPar->set_numberOfAlphaBins(0, 300);
  dDchRecoPar->set_numberOfPhiBins(0, 6000);
  dDchRecoPar->set_maxAlpha(0, 0.8);
  dDchRecoPar->set_minAlpha(0, -0.8);
  dDchRecoPar->set_maxPhi(0, 1.);
  dDchRecoPar->set_minPhi(0, -0.65);
  dDchRecoPar->set_numberOfBetaBins(0, 60);
  dDchRecoPar->set_numberOfZedBins(0, 200);
  dDchRecoPar->set_maxBeta(0, 2.5);
  dDchRecoPar->set_minBeta(0, 0.6);
  dDchRecoPar->set_maxZed(0, 100.);
  dDchRecoPar->set_minZed(0, -100.);
  dDchRecoPar->set_mirrorHitAnalysis(0, 1); // 0 for no stereo. 1 for stereo

  dDchHitWrapper* dDchHit = new dDchHitWrapper("dDchHit", 60000);
  PHIODataNode<PHTable>* dDchHitNode = new PHIODataNode<PHTable>(dDchHit, "dDchHit");
  dchNode->addNode(dDchHitNode);

  dDchTracksWrapper *dDchTrack = new dDchTracksWrapper("dDchTracks", 2000);
  PHIODataNode<PHTable>* dDchTrackNode = new PHIODataNode<PHTable>(dDchTrack, "dDchTracks");
  dchNode->addNode(dDchTrackNode);

  return 0;
}

int
DchReco::process_event(PHCompositeNode *topNode)
{
  Event *evt = findNode::getClass<Event>(topNode, "PRDF");
  if (! evt)
    {
      cout << PHWHERE << ": PRDF Node missing returning" << endl;
      return 0;
    }
  int iret = EVENT_OK;
  iret = mDchUnpacker->event(topNode, evt);
  if (iret == -2)
    {
      if (! norawdatacheck)
	{
	  RawDataCheck *raw = RawDataCheck::instance();
	  raw->AddToList(evt, "DCHTOOMANYRAWHITS");
	}
      return ABORTEVENT; // set iret to ABORTEVENT if mDchCandidatory returns False
    }

  if (mDchCalibrator->event(topNode))
    {
      if (mDchCandidatory->event(topNode))
        {
          mDchEfficiencyCalibrator->event(topNode); //if you want run Efficiency in ana_dch()
        }
      else
        {
	  if (! norawdatacheck)
	    {
	      RawDataCheck *raw = RawDataCheck::instance();
	      raw->AddToList(evt, "DCHERROR");
	    }
          iret = ABORTEVENT; // set iret to ABORTEVENT if mDchCandidatory returns False
        }
    }
  return iret;
}

int 
DchReco::ResetEvent(PHCompositeNode *topNode)
{
  mDchCalibrator->ResetEvent(topNode);
  mDchCandidatory->ResetEvent(topNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("DCH"))
    {
      mainIter.forEach(reset);
      mainIter.cd();
    }

  return 0;
}
