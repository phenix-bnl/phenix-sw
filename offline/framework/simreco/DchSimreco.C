#include "DchSimreco.h"

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <recoConsts.h>
#include <phool.h>
#include <getClass.h>
#include <RunNumberRanges.h>

// Modules
#include <mDchDCMModule.h>
#include <mNewDchBuilder.hh>
#include <mNewDchCalibrator.hh>
#include <mNewDchCandidatory.hh>
#include <mNewDchDriftVelocityCalibrator.hh>
#include <mNewDchEfficiencyCalibrator.hh>
#include <mNewDchFastSimulator.hh>
#include <mNewDchFEMModule.hh>
#include <mNewDchInitializer.hh>
#include <mNewDchNoFieldCandidatory.hh>
#include <mNewDchNoiseAnalyzer.hh>
#include <mNewDchPerfectTracker.hh>
#include <mNewDchSimulator.hh>
#include <mNewDchUnpacker.hh>
#include <PHDchGeometryObject.h>
#include <PHDchHistogrammer.hh>

// i/o tables (staff and new classes)
#include <DchPISAHit.h>
#include <dcghitWrapper.h>
#include <DchHitLineTablev2.hh>
#include <DchTrackv1.h>
#include <McEvalSingleList_v1.h>
#include <dDchDCMParWrapper.h>
#include <dDchDCMWrapper.h>
#include <dDchFastSimParWrapper.h>
#include <dDchFEMWrapper.h>
#include <dDchGhitHitsWrapper.h>
#include <dDchGhitRawWrapper.h>
#include <dDchNibbleGhitWrapper.h>
#include <dDchPerfParWrapper.h>
#include <dDchReconstructionParWrapper.h>
#include <dDchTracksExtWrapper.h>
#include <dDchTracksWrapper.h>
#include <dDchUnpackParWrapper.h>

using namespace std;

//_________________________________________________________________________
DchSimreco::DchSimreco(const string &name):
  SubsysReco(name),
  seed(-1),
  useperfecttracker(0),
  deadmapfile("DchAlwaysDead.dat"),
  efficiencyfile("DchEfficiency.Real"),
  mNewDchFastSim(NULL),
  mNewDchPerf(NULL),
  mDchFEM(NULL),
  mDchDCM(NULL),
  mDchInitializer(NULL),
  mDchUnpacker(NULL),
  mDchCalibrator(NULL),
  mDchCandidatory(NULL)
{
  return;
}

//_________________________________________________________________________
DchSimreco::~DchSimreco()
{
  delete mNewDchFastSim;
  delete mNewDchPerf;
  delete mDchFEM;
  delete mDchDCM;
  delete mDchInitializer;
  delete mDchUnpacker;
  delete mDchCalibrator;
  delete mDchCandidatory;

  return;
}

//_________________________________________________________________________
int DchSimreco::InitRun(PHCompositeNode *topNode)
{

  cout << "DchSimreco::InitRun - initializing drift chambers simulations" << endl;
  enum {DCMNODE, DSTNODE, EVANODE, GEANODE, PARNODE, LAST};
  const char *NName[] =
    {
      "DCM",
      "DST",
      "EVA",
      "GEA",
      "PAR"
    };

  PHNodeIterator iter(topNode);
  PHCompositeNode *outNode[LAST];
  for (short int i = 0; i < LAST; i++)
    {
      outNode[i] = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", NName[i]));
      if (!outNode[i])
        {
          cout << PHWHERE << NName[i] << " Node is missing doing nothing" << endl;
          return -1;
        }
    }


  recoConsts *rc = recoConsts::instance();

  PHCompositeNode* dchNode = new PHCompositeNode("DCH");
  topNode->addNode(dchNode);

  // set up parameter tables (there are a lot, so alphabetically please)

  dDchDCMParWrapper* dDchDCMPar =
    new dDchDCMParWrapper("dDchDCMPar", 1);
  PHIODataNode<PHTable>* dDchDCMParNode =
    new PHIODataNode<PHTable>(dDchDCMPar, "dDchDCMPar");
  outNode[PARNODE]->addNode(dDchDCMParNode);
  // Setting dDchDCMPar Parameters
  dDchDCMPar->set_scheme(0, 1);  // Dch is zero suppressed
  dDchDCMPar->SetRowCount(1);

  dDchFastSimParWrapper* dDchFastSimPar =
    new dDchFastSimParWrapper("dDchFastSimPar", 1);
  PHIODataNode<PHTable>* dDchFastSimParNode =
    new PHIODataNode<PHTable>(dDchFastSimPar, "dDchFastSimPar");
  outNode[PARNODE]->addNode(dDchFastSimParNode);
  dDchFastSimPar->set_randseed(0, -376386);
  dDchFastSimPar->set_wire_eff(0, 0.99);
  dDchFastSimPar->set_rphires(0, 0.015);
  dDchFastSimPar->set_rphiprop(0, 0.015);
  dDchFastSimPar->set_twotrksep(0, 0.0);
  dDchFastSimPar->set_back_eff(0, 0.0);
  dDchFastSimPar->set_verbose(0, 0);
  dDchFastSimPar->SetRowCount(1);

  dDchPerfParWrapper* dDchPerfPar = new dDchPerfParWrapper("dDchPerfPar", 1);
  PHIODataNode<PHTable>* dDchPerfParNode = new PHIODataNode<PHTable>(dDchPerfPar, "dDchPerfPar");
  outNode[PARNODE]->addNode(dDchPerfParNode);
  dDchPerfPar->set_verbose(0, 0);
  dDchPerfPar->set_localStudy(0, 0);
  dDchPerfPar->SetRowCount(1);


  dDchReconstructionParWrapper* dDchRecoPar = new dDchReconstructionParWrapper("dDchRecoPar", 1);
  PHIODataNode<PHTable>* dDchRecoParNode = new PHIODataNode<PHTable>(dDchRecoPar, "dDchRecoPar");
  outNode[PARNODE]->addNode(dDchRecoParNode);

  dDchRecoPar->set_houghThresholdOnXCell(0, 10);  //
  dDchRecoPar->set_houghThresholdOnXMask(0, 15);  //
  dDchRecoPar->set_houghThresholdOnUVCell(0, 3);  //
  dDchRecoPar->set_houghThresholdOnUVMask(0, 6); //
  dDchRecoPar->set_purgeCandidateThreshold(0, 15); //
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

  dDchRecoPar->set_firstXHoughThreshold(0, 15);  // data is 10, mc is 15
  dDchRecoPar->set_secondXHoughThreshold(0, 15); // data is 10, mc is 15

  dDchRecoPar->SetRowCount(1);

  dDchUnpackParWrapper* dDchUnpackPar = new dDchUnpackParWrapper("dDchUnpackPar", 1);
  PHIODataNode<PHTable>* dDchUnpackParNode = new PHIODataNode<PHTable>(dDchUnpackPar, "dDchUnpackPar");
  outNode[PARNODE]->addNode(dDchUnpackParNode);

  dDchUnpackPar->SetRowCount(1);
  dDchUnpackPar->set_detIdWord(0, 1);
  dDchUnpackPar->set_dc111(0, 0);



  //set up i/o tables
  DchHitLineTable* dchhitlinetable = findNode::getClass<DchHitLineTable>(topNode, "DchHitLineTable");
  if (!dchhitlinetable)
    {
      dchhitlinetable = new DchHitLineTablev2();
      PHIODataNode<PHObject> *NewNode =
        new PHIODataNode<PHObject>(dchhitlinetable, "DchHitLineTable", "PHObject"); // contain PHObject
      outNode[DSTNODE]->addNode(NewNode);
    }

  dchhitlinetable = findNode::getClass<DchHitLineTable>(topNode, "DchHitLineTablev1");
  if (!dchhitlinetable)
    {
      dchhitlinetable = new DchHitLineTablev1();
      PHIODataNode<PHObject> *NewNode =
        new PHIODataNode<PHObject>(dchhitlinetable, "DchHitLineTablev1", "PHObject"); // contain PHObject
      outNode[DSTNODE]->addNode(NewNode);
    }

  DchTrack* dchtrack = findNode::getClass<DchTrack>(topNode, "DchTrack");
  if (!dchtrack)
    {
      DchTrack* dchtrack = new DchTrackv1();
      PHIODataNode<PHObject> *NewNode =
        new PHIODataNode<PHObject>(dchtrack, "DchTrack", "PHObject"); // contain PHObject
      outNode[DSTNODE]->addNode(NewNode);
    }


  dDchHitWrapper* dDchHit = findNode::getClass<dDchHitWrapper>(topNode, "dDchHit");
  if (!dDchHit)
    {
      dDchHit = new dDchHitWrapper("dDchHit", 60000);
      PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(dDchHit, "dDchHit");
      outNode[DSTNODE]->addNode(NewNode);
    }

  dDchTracksWrapper *dDchTrack = findNode::getClass<dDchTracksWrapper>(topNode, "dDchTracks");
  if (!dDchTrack)
    {
      dDchTrack = new dDchTracksWrapper("dDchTracks", 2000);
      PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(dDchTrack, "dDchTracks");
      outNode[DSTNODE]->addNode(NewNode);
    }

  dDchTrack = findNode::getClass<dDchTracksWrapper>(topNode, "dDchTracksPerf");
  if (!dDchTrack)
    {
      dDchTrack = new dDchTracksWrapper("dDchTracksPerf", 2000);
      PHIODataNode<PHTable> *NewNode =
        new PHIODataNode<PHTable>(dDchTrack, "dDchTracksPerf");
      outNode[DSTNODE]->addNode(NewNode);
    }

  dDchTracksExtWrapper *dDchTrackExt = findNode::getClass<dDchTracksExtWrapper>(topNode, "dDchTracksExt");
  if (!dDchTrackExt)
    {
      dDchTrackExt = new dDchTracksExtWrapper("dDchTracksExt", 2000);
      PHIODataNode<PHTable> *NewNode =
        new PHIODataNode<PHTable>(dDchTrackExt, "dDchTracksExt");
      outNode[DSTNODE]->addNode(NewNode);
    }

  dDchTrackExt = findNode::getClass<dDchTracksExtWrapper>(topNode, "dDchTracksExtPerf");
  if (!dDchTrackExt)
    {
      dDchTrackExt = new dDchTracksExtWrapper("dDchTracksExtPerf", 2000);
      PHIODataNode<PHTable> *NewNode =
        new PHIODataNode<PHTable>(dDchTrackExt,
                                  "dDchTracksExtPerf");
      outNode[DSTNODE]->addNode(NewNode);
    }

  dDchGhitRawWrapper* dDchGhitRaw =
    new dDchGhitRawWrapper("dDchGhitRaw", 60000);
  PHIODataNode<PHTable>* dDchGhitRawNode =
    new PHIODataNode<PHTable>(dDchGhitRaw, "dDchGhitRaw");
  dchNode->addNode(dDchGhitRawNode);

  dDchGhitHitsWrapper* dDchGhitHits =
    new dDchGhitHitsWrapper("dDchGhitHits", 60000);
  PHIODataNode<PHTable>* dDchGhitHitsNode =
    new PHIODataNode<PHTable>(dDchGhitHits, "dDchGhitHits");
  outNode[EVANODE]->addNode(dDchGhitHitsNode);

  dDchDCMWrapper* dDchDCM = new dDchDCMWrapper("dDchDCM", 160);
  PHIODataNode<PHTable>* dDchDCMNode =
    new PHIODataNode<PHTable>(dDchDCM, "dDchDCM");
  outNode[DCMNODE]->addNode(dDchDCMNode);

  dDchFEMWrapper* dDchFEM = new dDchFEMWrapper("dDchFEM", 160);
  PHIODataNode<PHTable>* dDchFEMNode =
    new PHIODataNode<PHTable>(dDchFEM, "dDchFEM");
  dchNode->addNode(dDchFEMNode);

  dDchNibbleGhitWrapper* dDchNibbleGhit =
    new dDchNibbleGhitWrapper("dDchNibbleGhit", 60000);
  PHIODataNode<PHTable>* dDchNibbleGhitNode =
    new PHIODataNode<PHTable>(dDchNibbleGhit, "dDchNibbleGhit");
  outNode[EVANODE]->addNode(dDchNibbleGhitNode);

  // modules
  mNewDchFastSim  = new mNewDchSimulator(seed);
  mNewDchFastSim->setSmearConstants(0.01, // Global drift velocity uncertainty  - 1%
                                    0.005, // Keystone to keystone drift velocity sigma -0.5%
                                    0.01, // Wire to wire drift velocity sigma  - 1%
                                    0.5); // Wire by wire t0 sigma - 0.5 tbin

  mNewDchPerf     = new mNewDchPerfectTracker();

  // get run number
  int run_number = fabs( rc->get_IntFlag( "RUNNUMBER", 0 ) );
  if ( run_number == 0 )
    {
      cout << "DchSimreco::InitRun - missing Run number, needed to decide ";
      cout << "DchSimreco::InitRun - calibration time stamp. Exiting" << endl;
      exit(1);
    }

  // Call the initializer which does the most internal initialization work
  mDchInitializer = new mNewDchInitializer(1, 0, 0, run_number );

  cout << "DchSimreco::InitRun - version May 20, 2006 (modified for Run5 dead channel option)\n\n";

  if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
    {

      // Use local softlinks for Dch simulation geometry files (CFM addition, Jan. 17, 2005)
      mDchInitializer->setGeometryFileNames(
					    "DchGeometry.info",
					    "DchGeometry.wireMc",
					    "DchGeometry.frame00NoRetracted");

    }
  else
    {

      mDchInitializer->setGeometryFileNames(
					    "/afs/rhic.bnl.gov/phenix/software/calibration/sim00/DchGeometry.info",
					    "/afs/rhic.bnl.gov/phenix/software/calibration/sim00/DchGeometry.wireMc",
					    "/afs/rhic.bnl.gov/phenix/software/calibration/sim00/DchGeometry.frame00NoRetracted");

    }


  if ( run_number < BEGIN_OF_RUN2_PP )
    {

      // Run2 Au+Au
      cout << "DchSimreco::InitRun - Using Run2 Au+Au dead channel and efficiency maps" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName("AlwaysDeadCh.dat", "DchEfficiency0_run2_v03-average.Real");

        }
      else
        {

          mDchInitializer->setNoiseFileName(
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/new/AlwaysDeadCh.dat",
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/new/DchEfficiency0_run2_v03-average.Real");

        }

    }
  else if ( run_number < BEGIN_OF_RUN3 )
    {

      // Run2 p+p
      cout << "DchSimreco::InitRun - Using p+p dead channel and efficiency maps" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for pp  Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName( "AlwaysDeadCh_pp.dat", "DchEfficiency0_run2_pp-average.Real");

        }
      else
        {
          mDchInitializer->setNoiseFileName(
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/new/AlwaysDeadCh_pp.dat",
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/new/DchEfficiency0_run2_pp-average.Real");
        }

    }
  else if ( run_number < BEGIN_OF_RUN3_PP )
    {

      // For Run3 dAu and afterwards a constant (95%) Dch efficiency file is being used
      // This file was originally named DchEfficiency_dAu.Real in Run3
      // For future runs there is a softlink to this file named DchEfficiency_Constant.Real
      cout << "DchSimreco::InitRun - Using Run3 dAU dead channel and efficiency maps" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName( "AlwaysDeadCh_dAu_72096.dat", "DchEfficiency_dAu.Real");

        }
      else
        {

          mDchInitializer->setNoiseFileName(
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2003/AlwaysDeadCh_dAu_72096.dat",
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2003/DchEfficiency_dAu.Real");

        }

    }
  else if ( run_number < BEGIN_OF_RUN4 )
    {

      // Run3 p+p
      cout << "DchSimreco::InitRun - Using Run3 pp dead channel and efficiency maps" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName("AlwaysDeadCh_pp_Run03.dat", "DchEfficiency_Constant.Real");

        }
      else
        {

          mDchInitializer->setNoiseFileName(
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2003/AlwaysDeadCh_pp_Run03.dat",
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2003/DchEfficiency_Constant.Real");

        }

    }
  else if ( run_number < BEGIN_OF_RUN4_AUAU_63 )
    {

      // Run4 Au+Au 200 GeV
      cout << "DchSimreco::InitRun - Using Run4 Au+Au 200 GeV dead channel and efficiency maps" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName("AlwaysDeadCh_AuAu_Run04.dat", "DchEfficiency_Constant.Real");

        }
      else
        {

          mDchInitializer->setNoiseFileName(
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2004/AlwaysDeadCh_AuAu_Run04.dat",
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2004/DchEfficiency_Constant.Real");

        }

    }
  else if ( run_number < BEGIN_OF_RUN5 )
    {

      // Run4 Au+Au @63GeV
      // Run4 p+p
      cout << "DchSimreco::InitRun - Using Run4 Au+Au 62.4 GeV dead channel and efficiency maps" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName
	    ("AlwaysDeadCh_AuAu63_122929.dat",
	     "DchEfficiency_AuAu63.Real");
        }
      else
        {

          mDchInitializer->setNoiseFileName
	    ("/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2004/AlwaysDeadCh_AuAu63_122929.dat",
	     "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2004/DchEfficiency_AuAu63.Real");

        }

    }
  else if ( run_number < BEGIN_OF_RUN6 )
    {

      // Run5 p+p
      cout << "DchSimreco::InitRun - Using Run5 pp 200 GeV dead channel and efficiency maps (Run 168676" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName ("AlwaysDeadCh_pp200_168676.dat", "DchEfficiency_pp200.Real");

        }
      else
        {

          mDchInitializer->setNoiseFileName(
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2005/AlwaysDeadCh_pp200_168676.dat",
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2005/DchEfficiency_pp200.Real");

        }

    }
  else if ( run_number < BEGIN_OF_RUN6_PP_63 )
    {

      // Run6 p+p 200 GeV
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName
	    ("AlwaysDeadCh_pp200_168676.dat",
	     "DchEfficiency_pp200.Real");

        }
      else
        {
          cout << "DchSimreco::InitRun - Using my Run6 pp 200 GeV dead channel map" << endl;

          mDchInitializer->setNoiseFileName(
					    "/direct/phenix+u/workarea/mjuszkie/pisadch/run6/pisaToDst/AlwaysDead_198383_final.dat",
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2005/DchEfficiency_pp200.Real");

        }

    }
  else if ( run_number < BEGIN_OF_RUN7 )
    {

      // Run6 p+p 62.4 GeV
      cout << "DchSimreco::InitRun - Using Run6 pp 62.4 GeV dead channel map" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName("AlwaysDeadCh_pp200_168676.dat", "DchEfficiency_pp200.Real");

        }
      else
        {

          mDchInitializer->setNoiseFileName(
					    "/direct/phenix+u/workarea/mjuszkie/pisadch/run6/pisaToDst/AlwaysDead_206287_final.dat",
					    "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2005/DchEfficiency_pp200.Real");

        }

    }
  else if ( run_number < BEGIN_OF_RUN8 )
    {

      // Run7 Au+Au and p+p
      cout << "DchSimreco::InitRun - Using Run7 AuAu 200 GeV dead channel and efficiency maps (Run 231428)" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName
	    ("AlwaysDeadCh_231428.dat",
	     "DchEfficiency_AuAu_231428.Real");

        }
      else
        {

          mDchInitializer->setNoiseFileName
	    ("/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2007/AlwaysDeadCh_231428.dat",
	     "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2007/DchEfficiency_AuAu_231428.Real");

        }

    }
  else if ( run_number < BEGIN_OF_RUN8_PP )
    {

      // Run8 d+Au
      cout << "DchSimreco::InitRun - Using Run8 dAu 200 GeV dead channel and efficiency maps" << endl;
      cout << "DchSimreco::InitRun - WARNING - so far, still using Run7 maps (Run 231428)" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName( "AlwaysDeadCh_231428.dat", "DchEfficiency_AuAu_231428.Real");

        }
      else
        {

          cout << "DchSimreco::InitRun - IGNOREWARNING - Testing run8 dead map and effs" << endl;
          /*
	    mDchInitializer->setNoiseFileName
            ("/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2007/AlwaysDeadCh_231428.dat",
            "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/run2007/DchEfficiency_AuAu_231428.Real");
          */
          mDchInitializer->setNoiseFileName(
					    "/direct/phenix+u/workarea/mjuszkie/pisadch/run8/pisaToDst/AlwaysDead_dAu_Run08.dat",
					    "/direct/phenix+u/workarea/mjuszkie/pisadch/run8/pisaToDst/DchEfficiency_AuAu_231428_mod.Real");
        }

    }
  else if ( run_number < BEGIN_OF_RUN9 )
    {

      // Run 8 p+p
      cout << "DchSimreco::InitRun - Using Run8 pp 200 GeV dead channel and efficiency maps" << endl;
      cout << "DchSimreco::InitRun - WARNING - so far, still using Run7 maps (Run 231428)" << endl;
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          mDchInitializer->setNoiseFileName("AlwaysDeadCh_231428.dat", "DchEfficiency_AuAu_231428.Real");

        }
      else
        {

          cout << "DchSimreco::InitRun - IGNOREWARNING - Testing run8 pp dead map and effs" << endl;
          mDchInitializer->setNoiseFileName(
					    "/phenix/u/workarea/mjuszkie/pisadch/run8/pisaToDst/AlwaysDead_pp_Run08_2574142.dat",
					    "/phenix/u/workarea/mjuszkie/pisadch/run8/pisaToDst/DchEfficiency_AuAu_231428_mod.Real");
        }

    }
  else if ( run_number < BEGIN_OF_RUN11 )
    {

      // Run9 and later
      cout << "DchSimreco::InitRun - Using Run9 pp dead channel and efficiency maps" << endl;

      // Use local softlink to override Dch simulation dead map and efficiency files (why wasn't this here already? :) )
      if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
        {

          // Use local softlinks for Dch simulation dead and efficiency files
          // when running with AFSABSENT=1 and RUNNUM>BEGIN_OF_RUN9, supply two files
          // AlwaysDeadCh.dat and DchEfficiency.Real for dead channel list and wire by wire efficiency
          mDchInitializer->setNoiseFileName("AlwaysDeadCh.dat", "DchEfficiency.Real");

        }
      else
        {

          mDchInitializer->setNoiseFileName(
					    "/phenix/u/workarea/mjuszkie/pisadch/run9/pisaToDst/AlwaysDead_pp_Run09_500GeV_278084_v2.dat",
					    "/phenix/u/workarea/mjuszkie/pisadch/run8/pisaToDst/DchEfficiency_AuAu_231428_mod.Real");
        }

    }
  else
    {
      if (deadmapfile.size() == 0)
	{
	  cout << "no deadmap given" << endl;
	}
      if (efficiencyfile.size() == 0)
	{
	  cout << "no efficiency given" << endl;
	}

      mDchInitializer->setNoiseFileName(deadmapfile.c_str(), efficiencyfile.c_str());
    }

  if (rc->FlagExist("AFSABSENT") && rc->get_IntFlag("AFSABSENT") == 1)
    {

      // Use local softlinks for Dch simulation calibration file
      /*
	note: the slew, local, stereo files are set to 0 because they
	are not used in the simulations. Doing so avoids warnings about not
	finding these (unused) files in mNewDchInitializer.
	Hugo Pereira Da Costa, 2007/10/23
      */
      mDchInitializer->setCalibrationFileName("DchCalibration.Mc", 0, 0, 0);

    }
  else
    {
      /*
	note: the slew, local, stereo files are set to 0 because they
	are not used in the simulations. Doing so avoids warnings about not
	finding these (unused) files in mNewDchInitializer.
	Hugo Pereira Da Costa, 2007/10/23
      */
      mDchInitializer->setCalibrationFileName(
					      "/afs/rhic.bnl.gov/phenix/software/calibration/sim00/DchCalibration.Mc", 0, 0, 0
      );
    }

  mDchInitializer->event(topNode);
  mDchInitializer->getDGO()->rotateAndTranslate();

  mDchFEM = new mNewDchFEMModule;
  mDchDCM = new mDchDCMModule;
  mDchUnpacker = new mNewDchUnpacker;
  mDchCalibrator = new mNewDchCalibrator;

  // 1.4% correction to east drift velocity for AuAu
  mDchCalibrator->setCalibCorr(0.0, 1.00, 0.0, 1.00);
  mDchCandidatory = new mNewDchCandidatory;

  return 0;
}

int DchSimreco::process_event(PHCompositeNode *topNode)
{
  mNewDchFastSim->event(topNode);
  mDchFEM->event(topNode);
  mDchDCM->event(topNode);

  mDchCalibrator->event(topNode);
  mNewDchPerf->event(topNode);
  if (useperfecttracker)
    {
      mNewDchPerf->CopyWrapper(topNode);
    }
  else
    {
      mDchCandidatory->event(topNode);
    }
  return 0;
}

int DchSimreco::ResetEvent(PHCompositeNode *topNode)
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
