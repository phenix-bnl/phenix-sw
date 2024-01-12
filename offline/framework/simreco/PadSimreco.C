#include <PadSimreco.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <recoConsts.h>
#include <phool.h>
#include <Event.h>

#include <RunHeader.h>


#include <mPadDCMModule.h>
#include <mPadEvaluateModule.h>
#include <mPadFEMModule.h>
#include <mPadSlowSimModule.h>
#include <padEvtToRaw.hh>
#include <padInclBad.hh>
#include <PadRecModule.hh>

#include <dPad23ParWrapper.h>  /* is this still necessary in simulation? */
#include <dPadClusterWrapper.h>
#include <dPadDCMParWrapper.h>
#include <dPadDCMWrapper.h>
#include <dPadEvalParWrapper.h>
#include <dPadEvalWrapper.h>
#include <dPadFEMParWrapper.h>
#include <dPadFEMWrapper.h>
#include <dPadGeomWrapper.h>
#include <dPadGhitClusWrapper.h>
#include <dPadGhitRawWrapper.h>
#include <dPadNibbleGhitWrapper.h>
#include <dPadRawClusWrapper.h>
#include <dPadRawWrapper.h>
#include <dPadRecParWrapper.h>  /* is this still necessary in simulation? */
#include <dPadSlowSimParWrapper.h>  /* is this still necessary in simulation? */
#include <pcghitWrapper.h>
#include <PadPISAHit.h>

#include <PadRawv1.h>
#include <PadClusterv2.h>

#include <RunToTime.hh>
#include <getClass.h>

#include <sstream>

using namespace std;

long PadPutDCM(PHCompositeNode* topNode, int pcNumber);

long GeaTrkStack(const int& mcTrack, int& idPart, int& idParent,
                 float& pTot, float& rVertex, float& zVertex,
                 float& pTheta, float& pPhi, int& nFile,
                 int& itParent, int& itOrigin, int& idOrigin);

int checkPairEMCal(int reqPairKey);

PadSimreco::PadSimreco(const string &name): SubsysReco(name)
{
  PadInclBad = 0;
  mPadDetGeo = 0;
  PadEvtToRaw = 0;
  Pc1Rec = 0;
  Pc2Rec = 0;
  Pc3Rec = 0;
  mPc1SlowSim = 0;
  mPc2SlowSim = 0;
  mPc3SlowSim = 0;
  mPc1FEM = 0;
  mPc2FEM = 0;
  mPc3FEM = 0;
  mPc1DCM = 0;
  mPc2DCM = 0;
  mPc3DCM = 0;
  mPc1Evaluate = 0;
  mPc2Evaluate = 0;
  mPc3Evaluate = 0;
  dPadFEMPar = 0;
  dPadSlowSimPar = 0;
  dPadEvalPar = 0;
  padSplitMode = 0;  // default 0 means no splitting of clusters (Run2 v03 DSTs choice)
}

PadSimreco::~PadSimreco()
{
  delete PadInclBad;
  delete PadEvtToRaw;
  delete Pc1Rec;
  delete Pc2Rec;
  delete Pc3Rec;
  delete mPc1SlowSim;
  delete mPc2SlowSim;
  delete mPc3SlowSim;
  delete mPc1FEM;
  delete mPc2FEM;
  delete mPc3FEM;
  delete mPc1DCM;
  delete mPc2DCM;
  delete mPc3DCM;
  delete mPc1Evaluate;
  delete mPc2Evaluate;
  delete mPc3Evaluate;
  return ;
}

int
PadSimreco::InitRun(PHCompositeNode *topNode)
{

  recoConsts *rc = recoConsts::instance();

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


  PHCompositeNode* padNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAD"));
  if (!padNode)
    {
      padNode = new PHCompositeNode("PAD");
      topNode->addNode(padNode);
    }

  dPadGeomWrapper* dPadGeom = new dPadGeomWrapper("dPadGeom", 1);
  PHIODataNode<PHTable>* dPadGeomNode = new PHIODataNode<PHTable>(dPadGeom, "dPadGeom");
  outNode[PARNODE]->addNode(dPadGeomNode);

  // Setting dPadGeom Parameters
  dPadGeom->set_pdxoff(0, 0, -24.31);
  dPadGeom->set_pdxoff(1, 0, -81.2);
  dPadGeom->set_pdxoff(2, 0, -95.7);
  dPadGeom->set_pdzoff(0, 0, -89.5575);
  dPadGeom->set_pdzoff(1, 0, -152.475);
  dPadGeom->set_pdzoff(2, 0, -178.69);
  dPadGeom->set_pdgas(0, 0, 0.60);
  dPadGeom->set_pdgas(1, 0, 1.00);
  dPadGeom->set_pdgas(2, 0, 1.20);
  dPadGeom->set_aasep(0, 0, 0.84);
  dPadGeom->set_aasep(1, 0, 1.40);
  dPadGeom->set_aasep(2, 0, 1.65);
  dPadGeom->set_pxlen(0, 0, 0.82);
  dPadGeom->set_pxlen(1, 0, 1.375);
  dPadGeom->set_pxlen(2, 0, 1.622);
  dPadGeom->set_wside(0, 0, 0.27);
  dPadGeom->set_wside(1, 0, 0.47);
  dPadGeom->set_wside(2, 0, 0.55);
  dPadGeom->set_wcent(0, 0, 0.15);
  dPadGeom->set_wcent(1, 0, 0.26);
  dPadGeom->set_wcent(2, 0, 0.31);
  dPadGeom->set_pxsep(0, 0, 0.025);
  dPadGeom->set_pxsep(1, 0, 0.025);
  dPadGeom->set_pxsep(2, 0, 0.025);
  dPadGeom->set_clsep(0, 0, 0.1);
  dPadGeom->set_clsep(1, 0, 0.15);
  dPadGeom->set_clsep(2, 0, 0.2);
  dPadGeom->set_npdsec(0, 0, 16);
  dPadGeom->set_npdsec(1, 0, 8);
  dPadGeom->set_npdsec(2, 0, 8);
  dPadGeom->set_npdwr(0, 0, 58);
  dPadGeom->set_npdwr(1, 0, 116);
  dPadGeom->set_npdwr(2, 0, 116);
  dPadGeom->set_npdx(0, 0, 20);
  dPadGeom->set_npdx(1, 0, 40);
  dPadGeom->set_npdx(2, 0, 40);
  dPadGeom->set_npdz(0, 0, 216);
  dPadGeom->set_npdz(1, 0, 216);
  dPadGeom->set_npdz(2, 0, 216);
  dPadGeom->set_sectperarm(0, 0, 8);
  dPadGeom->set_sectperarm(1, 0, 4);
  dPadGeom->set_sectperarm(2, 0, 4);
  dPadGeom->set_inradius(0, 0, 248.891);
  dPadGeom->set_inradius(1, 0, 419.173);
  dPadGeom->set_inradius(2, 0, 492.012);
  dPadGeom->set_zgap(0, 0, 0.0);
  dPadGeom->set_zgap(1, 0, 8.106);
  dPadGeom->set_zgap(2, 0, 8.106);
  dPadGeom->set_phibote(0, 213.75);
  dPadGeom->set_phitope(0, 123.75);
  dPadGeom->set_phibotw(0, -33.75);
  dPadGeom->set_phitopw(0, 56.25);
  dPadGeom->SetRowCount(1);

  dPad23ParWrapper* dPad23Par = new dPad23ParWrapper("dPad23Par", 1);
  PHIODataNode<PHTable>* dPad23ParNode = new PHIODataNode<PHTable>(dPad23Par, "dPad23Par");
  outNode[PARNODE]->addNode(dPad23ParNode);
  dPad23Par->SetRowCount(1);
  dPad23Par->set_idatePC23(0, 19981225);

  dPadSlowSimPar = new dPadSlowSimParWrapper("dPadSlowSimPar", 1);
  PHIODataNode<PHTable>* dPadSlowSimParNode = new PHIODataNode<PHTable>(dPadSlowSimPar, "dPadSlowSimPar");
  outNode[PARNODE]->addNode(dPadSlowSimParNode);
  dPadSlowSimPar->set_verbose(0, 0);
  dPadSlowSimPar->set_randseed(0, 0, -298574);
  dPadSlowSimPar->set_randseed(1, 0, -398574);
  dPadSlowSimPar->set_randseed(2, 0, -498574);
  dPadSlowSimPar->set_qnoise(0, 0, 0.0007);
  dPadSlowSimPar->set_qnoise(1, 0, 0.002);
  dPadSlowSimPar->set_qnoise(2, 0, 0.0027);
  dPadSlowSimPar->set_threshold(0, 0, 0.02);
  dPadSlowSimPar->set_threshold(1, 0, 0.033);
  dPadSlowSimPar->set_threshold(2, 0, 0.04);
  dPadSlowSimPar->SetRowCount(1);

  dPadRecParWrapper* dPadRecPar = new dPadRecParWrapper("dPadRecPar", 1);
  PHIODataNode<PHTable>* dPadRecParNode = new PHIODataNode<PHTable>(dPadRecPar, "dPadRecPar");
  outNode[PARNODE]->addNode(dPadRecParNode);
  dPadRecPar->set_verbose(0, 0);
  dPadRecPar->set_method(0, 0, 0);
  dPadRecPar->set_method(1, 0, 0);
  dPadRecPar->set_method(2, 0, 0);
  dPadRecPar->SetRowCount(1);

  dPadEvalPar = new dPadEvalParWrapper("dPadEvalPar", 1);
  PHIODataNode<PHTable>* dPadEvalParNode = new PHIODataNode<PHTable>(dPadEvalPar, "dPadEvalPar");
  outNode[PARNODE]->addNode(dPadEvalParNode);
  dPadEvalPar->set_verbose(0, 0);
  dPadEvalPar->set_fillclus(0, 0);
  dPadEvalPar->set_fillghit(0, 0);
  dPadEvalPar->set_filleval(0, 0);
  dPadEvalPar->set_fillstat(0, 1);
  dPadEvalPar->set_printstat(0, 0);
  dPadEvalPar->set_pcnumber(0, 0);
  dPadEvalPar->set_rcutpc1(0, 8.0);
  dPadEvalPar->set_rcutpc2(0, 14.0);
  dPadEvalPar->set_rcutpc3(0, 16.0);
  dPadEvalPar->SetRowCount(1);

  //
  // Level2 simulations request DCM Tables in PISA-to-DST mode
  //
  dPadFEMPar = new dPadFEMParWrapper("dPadFEMPar", 1);
  PHIODataNode<PHTable>* dPadFEMParNode = new PHIODataNode<PHTable>(dPadFEMPar, "dPadFEMPar");
  outNode[PARNODE]->addNode(dPadFEMParNode);
  dPadFEMPar->set_pcnumber(0, 0);
  dPadFEMPar->set_debug(0, 0);
  dPadFEMPar->set_fout(0, 0);
  dPadFEMPar->set_mode(0, 0);
  dPadFEMPar->set_last(0, 0);
  dPadFEMPar->set_skipg(0, 0);
  dPadFEMPar->SetRowCount(1);

  dPadDCMParWrapper *dPadDCMPar = new dPadDCMParWrapper("dPadDCMPar", 1);
  PHIODataNode<PHTable>* dPadDCMParNode = new PHIODataNode<PHTable>(dPadDCMPar, "dPadDCMPar");
  outNode[PARNODE]->addNode(dPadDCMParNode);
  dPadDCMPar->set_debug(0, 0);
  dPadDCMPar->set_fout(0, 0);
  dPadDCMPar->set_scheme(0, 0);
  dPadDCMPar->set_idx(0, 0);
  dPadDCMPar->SetRowCount(1);

  ostringstream NodeName;
  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "dPc" << i << "Eval";
      dPadEvalWrapper *pceval = findNode::getClass<dPadEvalWrapper>(topNode, NodeName.str().c_str());
      if (!pceval)
        {
          pceval = new dPadEvalWrapper(NodeName.str().c_str(), 1500);
          PHIODataNode<PHTable>* PcEvalNode = new PHIODataNode<PHTable>(pceval, NodeName.str().c_str() );
          padNode->addNode(PcEvalNode);
        }
    }


  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "dPc" << i << "GhitRaw";
      dPadGhitRawWrapper *pcghitraw = findNode::getClass<dPadGhitRawWrapper>(topNode, NodeName.str().c_str());
      if (!pcghitraw)
        {
          pcghitraw = new dPadGhitRawWrapper(NodeName.str().c_str(), 6000);
          PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(pcghitraw, NodeName.str().c_str());
          outNode[EVANODE]->addNode(NewNode);
        }
    }

  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "dPc" << i << "GhitClus";
      dPadGhitClusWrapper *pcghitclus = findNode::getClass<dPadGhitClusWrapper>(topNode, NodeName.str().c_str());
      if (!pcghitclus)
        {
          pcghitclus = new dPadGhitClusWrapper(NodeName.str().c_str(), 6000);
          PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(pcghitclus, NodeName.str().c_str());
          outNode[EVANODE]->addNode(NewNode);
        }
    }

  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "dPc" << i << "FEM";
      dPadFEMWrapper *wrapper = findNode::getClass<dPadFEMWrapper>(topNode, NodeName.str().c_str());
      if (!wrapper)
        {
          wrapper = new dPadFEMWrapper(NodeName.str().c_str(), 32);
          PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(wrapper, NodeName.str().c_str());
          padNode->addNode(NewNode);
        }
    }

  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "dPc" << i << "DCM";
      dPadDCMWrapper *wrapper = findNode::getClass<dPadDCMWrapper>(topNode, NodeName.str().c_str());
      if (!wrapper)
        {
          wrapper = new dPadDCMWrapper(NodeName.str().c_str(), 64);
          PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(wrapper, NodeName.str().c_str());
          outNode[DCMNODE]->addNode(NewNode);
        }
    }

  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "dPc" << i << "NibbleGhit";
      dPadNibbleGhitWrapper *wrapper = findNode::getClass<dPadNibbleGhitWrapper>(topNode, NodeName.str().c_str());
      if (!wrapper)
        {
          wrapper = new dPadNibbleGhitWrapper(NodeName.str().c_str(), 18000);
          PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(wrapper, NodeName.str().c_str());
          outNode[EVANODE]->addNode(NewNode);
        }
    }

  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "dPc" << i << "Raw";
      dPadRawWrapper *wrapper = findNode::getClass<dPadRawWrapper>(topNode, NodeName.str().c_str());
      if (!wrapper)
        {
          wrapper = new dPadRawWrapper(NodeName.str().c_str(), 60000);
          PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(wrapper, NodeName.str().c_str());
          outNode[DSTNODE]->addNode(NewNode);
        }
    }

  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "dPc" << i << "Cluster";
      dPadClusterWrapper *wrapper = findNode::getClass<dPadClusterWrapper>(topNode, NodeName.str().c_str());
      if (!wrapper)
        {
          wrapper = new dPadClusterWrapper(NodeName.str().c_str(), 60000);
          PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(wrapper, NodeName.str().c_str());
          outNode[DSTNODE]->addNode(NewNode);
        }
    }

  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "dPc" << i << "RawClus";
      dPadRawClusWrapper *wrapper = findNode::getClass<dPadRawClusWrapper>(topNode, NodeName.str().c_str());
      if (!wrapper)
        {
          wrapper = new dPadRawClusWrapper(NodeName.str().c_str(), 60000);
          PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(wrapper, NodeName.str().c_str());
          outNode[DSTNODE]->addNode(NewNode);
        }
    }


  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "Pc" << i << "Raw";
      PadRaw *phobject = findNode::getClass<PadRaw>(topNode, NodeName.str().c_str());
      if (!phobject)
        {
          phobject = new PadRawv1();
          PHIODataNode<PHObject> *NewNode = new PHIODataNode<PHObject>(phobject, NodeName.str().c_str(), "PHObject");
          outNode[DSTNODE]->addNode(NewNode);
        }
    }

  for (int i = 1;i <= 3;i++)
    {
      NodeName.str("");
      NodeName << "Pc" << i << "Cluster";
      PadCluster *phobject = findNode::getClass<PadCluster>(topNode, NodeName.str().c_str());
      if (!phobject)
        {
          phobject = new PadClusterv2();
          PHIODataNode<PHObject> *NewNode = new PHIODataNode<PHObject>(phobject, NodeName.str().c_str(), "PHObject");
          outNode[DSTNODE]->addNode(NewNode);
        }
    }

  RunToTime *rt = RunToTime::instance();
  int runnumber = rc->get_IntFlag("RUNNUMBER");
  PHTimeStamp *ts = rt->getBeginTime(runnumber);
  PHTimeStamp TS = *ts;
  delete ts;

  mPc1SlowSim = new mPadSlowSimModule;
  mPc2SlowSim = new mPadSlowSimModule;
  mPc3SlowSim = new mPadSlowSimModule;

  mPc1FEM = new mPadFEMModule;
  mPc2FEM = new mPadFEMModule;
  mPc3FEM = new mPadFEMModule;
  mPc1DCM = new mPadDCMModule;
  mPc2DCM = new mPadDCMModule;
  mPc3DCM = new mPadDCMModule;



  mPc1Evaluate = new mPadEvaluateModule;
  mPc2Evaluate = new mPadEvaluateModule;
  mPc3Evaluate = new mPadEvaluateModule;

  PadEvtToRaw = new padEvtToRaw();
  PadInclBad = new padInclBad();
  PadInclBad->doNotInclBadChs();
  PadInclBad->doInclBadROCs();
  PadInclBad->RemoveHotROCs();
  PadInclBad->RemoveUnSynchROCs();
  PadInclBad->doNotAddHotROCs();
  PadInclBad->doNotAddInactiveROCs();
  PadInclBad->doNotAddUnSynchROCs();
  if (rc->FlagExist("PADDEADROCFILE") && rc->FlagExist("PADDEADCHFILE"))
    {
      const char* badchfile = rc->get_CharFlag("PADDEADCHFILE");
      const char* badrocfile = rc->get_CharFlag("PADDEADROCFILE");
      std::cout << "PadReco:: Using dead map from " << badchfile << " and " <<  badrocfile << std::endl;
      PadInclBad->FetchCalDataFromFiles(badchfile, badrocfile);
    }
  else
	PadInclBad->FetchCalDataFromObjy(TS);

  mPadDetGeo = new padDetectorGeo();
  // Setting mPadDetGeo Parameters
  const double pc1Radius = 248.891;
  const double pc2Radius = 419.173;
  const double pc3Radius = 492.012;
  const double ThetaArm[2] =
    {
      -0.589049, 2.15984
    };
  mPadDetGeo->set_pc1Radius(pc1Radius);
  mPadDetGeo->set_pc2Radius(pc2Radius);
  mPadDetGeo->set_pc3Radius(pc3Radius);
  mPadDetGeo->set_Theta0(ThetaArm);
  mPadDetGeo->FetchFromSimDatabase(TS); //reading from sim DB
  mPadDetGeo->Fetch_dPadGeom(topNode);

  PHDataNode<padDetectorGeo>* mPadDetGeoNode = new PHDataNode<padDetectorGeo>(mPadDetGeo, "mPadDetGeo");
  outNode[PARNODE]->addNode(mPadDetGeoNode);


  Pc1Rec = new PadRecModule(0);
  Pc2Rec = new PadRecModule( -1);
  Pc3Rec = new PadRecModule( -2);

  Pc1Rec->setSplitMode(padSplitMode); // Split mode 0: Do not split large clusters
  Pc2Rec->setSplitMode(padSplitMode); // Split mode 1: Split large clusters
  Pc3Rec->setSplitMode(padSplitMode);

  // The idea behind the reconstruction code is to place an imaginative box
  // around each cluster and to measure its size. These values set the maximum
  // size of the box that covers the cluster created by a single particle.
  short oneW = 4; // Box height
  short oneZ = 4; // Box width
  short oneL = 11; // Maximum number of allowed cells within a one particle box
  short twoW = 8;
  short twoZ = 8;
  short twoL = 16;
  short threeW = 100;
  short threeZ = 100;
  short threeL = 100;
  Pc1Rec->setClusterSizes(oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL);
  Pc2Rec->setClusterSizes(oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL);
  Pc3Rec->setClusterSizes(oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL);


  return 0;
}

int
PadSimreco::process_event(PHCompositeNode *topNode)
{


  dPadSlowSimPar->set_pcnumber(0, 0);

  mPc1SlowSim->set_pcnumber(0);

  mPc1SlowSim->event(topNode);

  dPadSlowSimPar->set_pcnumber(0, 1);
  mPc2SlowSim->set_pcnumber(1);
  mPc2SlowSim->event(topNode);

  dPadSlowSimPar->set_pcnumber(0, 2);
  mPc3SlowSim->set_pcnumber(2);
  mPc3SlowSim->event(topNode);

  dPadFEMPar->set_pcnumber(0, 0);
  mPc1FEM->set_pcnumber(0);
  mPc1FEM->event(topNode);

  dPadFEMPar->set_pcnumber(0, 1);
  mPc2FEM->set_pcnumber(1);
  mPc2FEM->event(topNode);

  dPadFEMPar->set_pcnumber(0, 2);
  mPc3FEM->set_pcnumber(2);
  mPc3FEM->event(topNode);

  mPc1DCM->set_pcnumber(0);
  mPc1DCM->event(topNode);

  mPc2DCM->set_pcnumber(1);
  mPc2DCM->event(topNode);

  mPc3DCM->set_pcnumber(2);
  mPc3DCM->event(topNode);

  PadPutDCM(topNode, 0);
  PadPutDCM(topNode, 1);
  PadPutDCM(topNode, 2);

  PadInclBad->event(topNode);

  Pc1Rec->event(0, mPadDetGeo, topNode);
  Pc2Rec->event(1, mPadDetGeo, topNode);
  Pc3Rec->event(2, mPadDetGeo, topNode);

  //
  // Evaluation in PISA-to-DST mode
  //
  dPadEvalPar->set_pcnumber(0, 0);
  mPc1Evaluate->set_pcnumber(0);
  mPc1Evaluate->event(topNode);

  dPadEvalPar->set_pcnumber(0, 1);
  mPc2Evaluate->set_pcnumber(1);
  mPc2Evaluate->event(topNode);

  dPadEvalPar->set_pcnumber(0, 2);
  mPc3Evaluate->set_pcnumber(2);
  mPc3Evaluate->event(topNode);

  copyWrapper(topNode);
  return 0;
}

int PadSimreco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("PAD"))
    {
      mainIter.forEach(reset);
      mainIter.cd();
    }

  return 0;
}

void PadSimreco::setPadSplitMode(const short padMode)
{
  switch (padMode)
    {
    case 0:
      cout << "\n Pad Chamber cluster splitting is disabled\n" << endl;
      padSplitMode = padMode;
      break;
    case 1:
      cout << "\n Pad Chamber cluster splitting is enabled\n" << endl;
      padSplitMode = padMode;
      break;
    default:
      cout << "\n Unrecognized Pad Chamber cluster splitting choice " << padMode << endl;
    }
  return ;  // setPadSplitMode()
}

short PadSimreco::getPadSplitMode() const
{
  switch (padSplitMode)
    {
    case 0:
      cout << "\n Pad Chamber cluster splitting is disabled\n" << endl;
      break;
    case 1:
      cout << "\n Pad Chamber cluster splitting is enabled\n" << endl;
      break;
    default:
      cout << "\n Unrecognized Pad Chamber cluster splitting choice" << padSplitMode << endl;
    }
  return padSplitMode;
}


int PadSimreco::copyWrapper(PHCompositeNode *topNode)
{
  int iret = 0;
  ostringstream WrapperNodeName, PhobjectNodeName;

  for (short int j = 1; j <= 3; j++)
    {
      WrapperNodeName << "dPc" << j << "Cluster";
      dPadClusterWrapper *dpadcluster = findNode::getClass<dPadClusterWrapper>(topNode, WrapperNodeName.str().c_str());
      PhobjectNodeName << "Pc" << j << "Cluster";
      PadCluster *padcluster = findNode::getClass<PadCluster>(topNode, PhobjectNodeName.str().c_str());
      if (dpadcluster && padcluster)
        {
          if (!padcluster->isValid() && dpadcluster->RowCount())
            {
              padcluster->FillFromWrapper(dpadcluster);
            }
        }
      WrapperNodeName.str("");
      PhobjectNodeName.str("");
    }


  for (short int j = 1; j <= 3; j++)
    {
      WrapperNodeName << "dPc" << j << "Raw";
      dPadRawWrapper *dpadraw = findNode::getClass<dPadRawWrapper>(topNode, WrapperNodeName.str().c_str());
      PhobjectNodeName << "Pc" << j << "Raw";
      PadRaw *padraw = findNode::getClass<PadRaw>(topNode, PhobjectNodeName.str().c_str());
      if (dpadraw && padraw)
        {
          if (!padraw->isValid() && dpadraw->RowCount())
            {
              padraw->FillFromWrapper(dpadraw);
            }
        }
      WrapperNodeName.str("");
      PhobjectNodeName.str("");
    }
  return iret;
}
