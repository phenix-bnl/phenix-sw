#include <TecReco.h>

#include <phool.h>
#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeReset.h>

#include <mTecCalibModule.h>
#include <mTecUnpackModule.h>
#include <mTecHitClusteringModule.h>
#include <mTecDrawModule.h>
#include <mTecPIDModule.h>
#include <TecAddressObject.hh>
#include <TecCalibrationObject.hh>
#include <TecGeometryObject.hh>
#include <TecClusterContainerV1.hh>

#include <TecOutV7.hh>
#include <TecOutV6.hh>
#include <recoConsts.h>

#include <TObject.h>

#include <cstdlib>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;


TecReco::TecReco(const string &name): SubsysReco(name)
{

  mTecCalib = 0;
  mTecUnpack = 0;
  mTecHitClustering = 0;
  mTecDraw = 0;
  TecAddress = 0;
  TecGeometry = 0;
  TecCalibration = 0;
  TecDrawFlag=-1;
  return ;
}

TecReco::~TecReco()
{
  delete TecAddress;
  delete mTecUnpack;
  delete mTecCalib;
  delete mTecHitClustering;
  delete mTecDraw;
  return;
}

int TecReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);

  TecAddress = new TecAddressObject(); 
// Geometry and calibration objects are created in CreateNodeTree
  mTecUnpack = new mTecUnpackModule();
  mTecCalib = new mTecCalibModule();
  mTecHitClustering = new mTecHitClusteringModule();

  recoConsts *rc = recoConsts::instance();
  if(rc->FlagExist("TECDRAW")) { 
    TecDrawFlag = rc->get_IntFlag("TECDRAW"); 
    if(TecDrawFlag>-1 && TecDrawFlag<4) { mTecDraw = new mTecDrawModule(); }
  }

  return iret;
}

int TecReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  PHTimeStamp TimeStp = rc->get_TimeStamp();

  if(rc->FlagExist("TECFADCCUT")) {
    int TecFadcCut = rc->get_IntFlag("TECFADCCUT");
    mTecUnpack->set_FadcCut(TecFadcCut);
  }
  else {
    mTecUnpack->set_FadcCutFromTimeStamp(&TimeStp);
  }

  TecAddress->setTimeStamp(TimeStp);
  TecGeometry->setTimeStamp(TimeStp);
  TecCalibration->setTimeStamp(TimeStp);

  // If database is not accessible, stop execution.
  PHBoolean status1 = TecAddress->Fetch();
  if (!status1)
    {
      cout << PHWHERE << "ERROR: Can not access the database (TAO). Execution terminated." << endl;
      exit(1);
    }
  PHBoolean status2 = TecGeometry->Fetch();
  if (!status2)
    {
      cout << PHWHERE << "ERROR: Can not access the database (TGO). Execution terminated." << endl;
      exit(1);
    }
  PHBoolean status3 = TecCalibration->Fetch();
  if (!status3)
    {
      cout << PHWHERE << "ERROR: Can not access the database (TCO). Execution terminated." << endl;
      exit(1);
    }
  return 0;
}

int TecReco::CreateNodeTree(PHCompositeNode *topNode)
{
  // first test if neccessary nodes have been created, if not bail out
  enum {DSTNODE, PARNODE, DCMNODE, LAST}; // leave LAST at end - it is used for loops
  const char *NName[] = {
    "DST",
    "PAR",
    "DCM"};
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

  PHCompositeNode* tecNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "TEC"));
  if (!tecNode)
    {
      tecNode = new PHCompositeNode("TEC");
      topNode->addNode(tecNode);
    }

  // Adding geometry and calibration objects to par node is necessary for running TecPidReco



  TecGeometry = new TecGeometryObject();
  TObjectNode_t *TecDetGeoNode = new TObjectNode_t(TecGeometry, "TecGeometry");
  outNode[PARNODE]->addNode(TecDetGeoNode);

  TecCalibration = new TecCalibrationObject();
  PHDataNode<TecCalibrationObject> *TecCalibNode = new PHDataNode<TecCalibrationObject>(TecCalibration, "TecCalibration");
  outNode[PARNODE]->addNode(TecCalibNode);

  TecOut *techitout = new TecOutV7();
  PHObjectNode_t *TecHitOutNode = new PHObjectNode_t(techitout, "TecHitOut", "PHObject");
  outNode[DSTNODE]->addNode(TecHitOutNode);

  TecClusterContainer *teccluster = new TecClusterContainerV1();
  PHObjectNode_t *TecClusterNode = new PHObjectNode_t(teccluster, "TecClusterContainer", "PHObject");
  outNode[DSTNODE]->addNode(TecClusterNode);
 
  return 0;
}


int TecReco::process_event(PHCompositeNode *topNode)
{

  mTecUnpack->event(topNode, TecAddress);
  mTecCalib->event(topNode, TecAddress, TecCalibration);
  mTecHitClustering->event(topNode);

  if(TecDrawFlag>-1 && TecDrawFlag<4) { 
    EPHENIXSide side = kBoth;
    EPHENIXSector sector = (EPHENIXSector)TecDrawFlag;
    mTecDraw->Draw(topNode, sector, side); 
  }

  return 0;
}

int TecReco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("TEC"))
    {
      mainIter.forEach(reset);
    }
  return 0;
}
