#include "TecSimreco.h"

#include <RunNumberRanges.h>
#include <TecGeometryObject.hh>
#include <TecCalibrationObject.hh>
#include <TecOutV7.hh>
#include <TecOutV6.hh>

#include <dTecFemDataWrapper.h>
#include <dTecGhitRawWrapper.h>
#include <tecghitWrapper.h>

#include <TecGetGEA.h>

#include <recoConsts.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>

#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;

long TecPutDCM(PHCompositeNode* topNode);

//_______________________________________________________________
TecSimreco::TecSimreco(const string &name):
  SubsysReco(name)
{
  TecGeometry = 0;
  TecEvalFlag = 0;
  TecDrawFlag = -1;
}

//_______________________________________________________________
TecSimreco::~TecSimreco( void )
{
  // nothing to be deleted here
  // the only pointers that are created with "new" are stored
  // in the node tree and thus deleted by Fun4All.
}

//_______________________________________________________________
int TecSimreco::Init(PHCompositeNode *topNode)
{

  TecGeometry = new TecGeometryObject();
  TecCalibration = new TecCalibrationObject();

  recoConsts *rc = recoConsts::instance();
  if(rc->FlagExist("TECDRAW")) TecDrawFlag = rc->get_IntFlag("TECDRAW");
  if(rc->FlagExist("TECEVAL")) TecEvalFlag = rc->get_IntFlag("TECEVAL");

  return 0;
}

//_______________________________________________________________
int TecSimreco::InitRun(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);

  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) { cout << PHWHERE << "DST Node missing doing nothing" << endl; return -1; }

  PHCompositeNode *parNode;
  parNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode) { cout << PHWHERE << "PAR Node missing doing nothing" << endl; return -1; }

  PHCompositeNode *dcmNode;
  dcmNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DCM"));
  if (!dcmNode) { cout << PHWHERE << "DCM Node missing doing nothing" << endl; return -1; }

  PHCompositeNode *evaNode;
  evaNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "EVA"));
  if (!evaNode) { cout << PHWHERE << "EVA Node missing doing nothing" << endl; return -1; }

  PHCompositeNode *geaNode;
  geaNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "GEA"));
  if (!geaNode) { cout << PHWHERE << "GEA Node missing doing nothing" << endl; return -1; }

  /* here comes the TEC */
  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  topNode->addNode(tecNode);

  PHObjectNode_t *TecOutNode =  static_cast<PHObjectNode_t*>(iter.findFirst("PHIODataNode","TecOut"));
  if(!TecOutNode)
  {

    TecOut *tecout = new TecOutV7();
    TecOutNode = new PHObjectNode_t(tecout, "TecOut", "PHObject");
    dstNode->addNode(TecOutNode);

  }

  PHObjectNode_t *TecOutShortNode =  static_cast<PHObjectNode_t*>(iter.findFirst("PHIODataNode","TecOutShort"));
  if(!TecOutShortNode)
  {

    TecOut *tecout = new TecOutV6();
    TecOutNode = new PHObjectNode_t(tecout, "TecOutShort", "PHObject");
    dstNode->addNode(TecOutNode);

  }

  PHObjectNode_t *TecHitOutNode =  static_cast<PHObjectNode_t*>(iter.findFirst("PHIODataNode","TecHitOut"));
  if(!TecHitOutNode)
  {

    TecOut *techitout = new TecOutV7();
    TecHitOutNode = new PHObjectNode_t(techitout, "TecHitOut", "PHObject");
    dstNode->addNode(TecHitOutNode);

  }

  if(TecEvalFlag==11 || TecEvalFlag==1)
  {

    // output nodes for evaluation
    PHObjectNode_t *TecOutEvalNode =  static_cast<PHObjectNode_t*>(iter.findFirst("PHIODataNode","TecOutEVAL"));
    if(!TecOutEvalNode)
    {

      TecOut *tecouteval = new TecOutV7();
      TecOutEvalNode = new PHObjectNode_t(tecouteval, "TecOutEVAL", "PHObject");
      dstNode->addNode(TecOutEvalNode);

    }

  }

  int mr;

  mr = 4500;
  tecghitWrapper* tecghit = new tecghitWrapper("tecghit", mr);
  PHIODataNode<PHTable>* tecghitNode = new PHIODataNode<PHTable>(tecghit, "tecghit");
  geaNode->addNode(tecghitNode);

  mr = 150000;
  dTecGhitRawWrapper* dTecGhitRaw = new dTecGhitRawWrapper("dTecGhitRaw", mr);
  PHIODataNode<PHTable>* dTecGhitRawNode = new PHIODataNode<PHTable>(dTecGhitRaw, "dTecGhitRaw");
  evaNode->addNode(dTecGhitRawNode);

  mr = 8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData", mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData, "dTecFemData");
  tecNode->addNode(dTecFemDataNode);

  PHIODataNode<TObject>* TecDetGeoNode = new PHIODataNode<TObject> (TecGeometry, "TecGeometry");
  parNode->addNode(TecDetGeoNode);

  PHDataNode<TecCalibrationObject>* TecCalibNode = new PHDataNode<TecCalibrationObject> (TecCalibration, "TecCalibration");
  parNode->addNode(TecCalibNode);

  // In case of simulation the Time Stamp will correspond to system
  // time. OK for standard simulation, but could cause problems
  // for non-standard geometry (e.g. retracted arms).
  recoConsts *rc = recoConsts::instance();
  int runref = rc->get_IntFlag("RUNNUMBER");

  TecAddress.SetRunNumber(runref);
  TecAddress.UseSimulationDatabase();

  TecGeometry->SetRunNumber(runref);
  TecGeometry->UseSimulationDatabase();

  TecCalibration->SetRunNumber(runref);
  TecCalibration->UseSimulationDatabase();

  TecCalibrationObject TCO;
  if( runref >= BEGIN_OF_RUN4 && runref < BEGIN_OF_RUN5 )
  {

    // during Run4 there is no Online Calibration for TEC
    TCO.setCalibName("calib.tec.tecgain_run04");

  }

  TCO.setRunNumber(runref);
  TCO.FetchAbsGain();
  for (int k=0; k<48; k++)
  {
    float gain = 2240/(TCO.getAbsoluteGain(k)-0.2124);
    if (gain<0) gain = 0.;
    mTecSlowSim.set_PlaneGasGain(k,gain);
    cout << "TecGasGain[" << k << "]=" << gain << endl;
  }

  // If database is not accessible, stop execution.
  PHBoolean status1 = TecAddress.Fetch();
  if (!status1)
  {
    cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
    cout << PHWHERE << "ERROR: Can not access the database (TAO). Execution terminated." << endl;
    exit(1);
  }

  PHBoolean status2 = TecGeometry->Fetch();
  if (!status2)
  {
    cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
    cout << PHWHERE << "ERROR: Can not access the database (TGO). Execution terminated." << endl;
    exit(1);
  }
  PHBoolean status3 = TecCalibration->Fetch();
  if (!status3)
  {
    cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
    cout << PHWHERE << "ERROR: Can not access the database (TCO). Execution terminated." << endl;
    exit(1);
  }

  return 0;
}

//_______________________________________________________________
int TecSimreco::process_event(PHCompositeNode *topNode)
{
  int iret = 0;

  // assume ThisMode = 0 legacy operation of Fun4AllPisaInputManager class
  TecGetGEA(topNode);

  mTecSlowSim.event(topNode, &TecAddress);
  mTecCalib.event(topNode, &TecAddress, TecCalibration);

  if(TecEvalFlag==2) mTecTrackEval.MergeSingleTracks(topNode);

  mTecHoughTrack.set_fillRawTrack(0);

  if(TecEvalFlag==2) mTecTrackEval.Evaluate(topNode);

  mTecHoughTrack.event(topNode);

  if(TecEvalFlag==1) mTecTrackEval.WriteSingleTracks(topNode);
  if(TecEvalFlag==11) iret = mTecTrackEval.WriteSimulatedEvent(topNode);

  if(TecDrawFlag>-1 && TecDrawFlag<4)
  {

    EPHENIXSide side = kBoth;
    EPHENIXSector sector = (EPHENIXSector)TecDrawFlag;
    mTecDraw.Draw(topNode, sector, side);

  }

  return iret;
}

int TecSimreco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("TEC"))
  {
    mainIter.forEach(reset);
  }

  return 0;

}
