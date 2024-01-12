#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "recoConsts.h"

#include "RunHeader.h"

#include "EmcSimreco.h"

// modules
#include "emcCalibratorFactory.h"
#include "emcCalibrator.h"
#include <emcDataManager.h>
#include <emcRawDataCalibrator.h>
#include <mEmcCalibTowerModule.h>
#include <mEmcCalibratorModule.h>
#include <mEmcClusterNewModule.h>
#include <mEmcDCMToRawModule.h>
#include <mEmcDefGeomModule.h>
#include <mEmcEventModule.h>
#include <mEmcFEMToDCMModule.h>
#include <mEmcGeaClusterEvalModule.h>
#include <mEmcGeaEventModule.h>
#include <mEmcGeaMakeRawModule.h>
#include <mEmcGeaParamsModule.h>
#include <mEmcGeaTrackModule.h>
#include <mEmcGeometryModule.h>
#include <mEmcMIPCorrModule.h>
#include <mEmcMIPCorr3Module.h>
#include <mEmcPRDFToRaw.h>
#include <mEmcRawToFEMModule.h>
// to be removed, mazsi@bnl.gov // #include <mEmcRawToLongModule.h>
#include <mEmcRealEventModule.h>
#include <mEmcTOFCorr2Module.h>
#include <mEmcTOFCorr4Module.h>
#include <mEmcToolsModule.h>
#include <pbscTimingFixes.h>
#include "EmcSimuRawDataReCal.h"

// i/o tables/classes
#include <dEmcCalibTowerWrapper.h>
#include <dEmcClusterLocalExtWrapper.h>
#include <dEmcClusterLocalWrapper.h>
#include <dEmcDCMDataWrapper.h>
#include <dEmcDCMLongDataWrapper.h>
#include <dEmcEventWrapper.h>
#include <dEmcFEMDataWrapper.h>
#include <dEmcGeaClusterTrackWrapper.h>
#include <dEmcGeaHitWrapper.h>
#include <dEmcGeaParamsWrapper.h>
#include <dEmcGeaTowerTrackWrapper.h>
#include <dEmcGeaTrackClusterWrapper.h>
#include <dEmcGeaTrackTowerWrapper.h>
#include <dEmcGeaTrackWrapper.h>
#include <dEmcGeometryWrapper.h>
#include <dEmcRawDataWrapper.h>
#include <dEmcRecoParWrapper.h>
#include <dEmcRespParWrapper.h>
#include <emcCalibratedDataObject.h>
#include <emcghitWrapper.h>
#include <emcparWrapper.h>

#include "EmcCalibTowerv1.h"
#include "EmcClusterLocalExtv1.h"
#include "mEmcCalibratorModule.h"

#include "EmcGetGEA.h"

#include <cmath>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;
typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode <EmcCalibTower> EmcCalibTowerNode_t;
typedef PHIODataNode <EmcClusterLocalExt> EmcClusterLocalExtNode_t;


long EmcPutDCM(PHCompositeNode* topNode);
long EmcPutDCMLong(PHCompositeNode* topNode);
long EmcPutDCMLongReCal(PHCompositeNode* topNode);
long EmcGetDCM(PHCompositeNode* topNode);

EmcSimreco::EmcSimreco(const string &name): SubsysReco(name)
{
  mEmcCalibrator     = 0;
  mEmcCalibTower     = 0;
  mEmcClusterNew     = 0;
  mEmcDCMToRaw       = 0;
  mEmcDefGeom        = 0;
  mEmcEvent          = 0;
  mEmcFEMToDCM       = 0;
  mEmcGeaClusterEval = 0;
  mEmcGeaEvent       = 0;
  mEmcGeaMakeRaw     = 0;
  mEmcGeaParams      = 0;
  mEmcGeaTrack       = 0;
  mEmcGeometry       = 0;
  mEmcMIPCorr        = 0;
  mEmcMIPCorr3       = 0;
  mEmcRawToFEM       = 0;	// writes sim raw data to fem
// to be removed, mazsi@bnl.gov //   mEmcRawToLong      = 0;	// writes sim raw data to hit format IDPBSC_FPGA0SUP
  mEmcRealEvent      = 0;
  mEmcTOFCorr        = 0;
  mEmcTOFCorr4       = 0;
  mEmcTools          = 0;
  mEmcSimuRawReCal   = 0;
}

int 
EmcSimreco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  PHTimeStamp TimeStp = rc->get_TimeStamp();

  size_t mr, nrc;
  PHNodeIterator iter(topNode);
  
  //  This file was rather tangled...
  //  I selected SIMULATIONFLAG==2 && YEAR==3 as the only code to appear here.
  //                        TKH  Thanksgiving 2003.
  runnumber=rc->get_IntFlag("RUNNUMBER");
  cout<<"setup_emc: run number from recoConsts:"<<runnumber<<endl;
  
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }
  PHCompositeNode *parNode;
  parNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
  
  PHCompositeNode *dcmNode;
  dcmNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DCM"));
  
  PHCompositeNode *evaNode;
  evaNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "EVA"));
  
  PHCompositeNode *geaNode;
  geaNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "GEA"));
  
  
  PHCompositeNode* emcNode = new PHCompositeNode("EMC");
  topNode->addNode(emcNode);
  
  PHCompositeNode* emcNode2 = new PHCompositeNode("EMC2");
  topNode->addNode(emcNode2);
  
  //_____________________________________________________________________________
  // REAL & SIMULATED DATA
  // We need access to the DB also for simulated data
  
  // The calibrator is of type emcRawDataCalibratorV2,
  // and the configuration is fetched from DB (using TimeStamp).
  
  cout<<"setup_emc: processing run # "<<runnumber<<", timestamp "<<TimeStp<<endl;
  
  mEmcCalibrator = new mEmcCalibratorModule(TimeStp, "emcRawDataCalibratorV2");
  
  // Tell the calibrator not to try to fetch the timestamp
  // from each event, but use the global one instead.
  mEmcCalibrator->UseTimeStamp(TimeStp) ;
  emcCalibrator* rdc = emcCalibratorFactory::GetCalibrator() ;
  assert(rdc != 0) ;
  
  rdc->SelectSource("QAs",emcManageable::kDB_Objy);
  
  // MV 2003/01/30 first rejection list for the dAu run
  rdc->SetExtraRejectListFilename("/afs/rhic.bnl.gov/phenix/users/maximv/emc/emc_extra_reject_Run3dAu.list");
  
  rdc->Print() ;
  
  
  //_____________________________________________________________________________
  // SIMULATED DATA
  // simulationFlag 1 == "PRDF-to-DST"
  // simulationFlag 2 == "PISA-to-DST" (standard direct way)
  // simulationFlag 3 == "PISA-to-PRDF"
  
  mEmcGeaParams = new mEmcGeaParamsModule;
  mEmcGeaMakeRaw = new mEmcGeaMakeRawModule;
  mEmcGeaTrack = new mEmcGeaTrackModule;
  mEmcDCMToRaw = new mEmcDCMToRawModule;
  mEmcGeaEvent = new mEmcGeaEventModule;
  mEmcCalibTower = new mEmcCalibTowerModule;
  mEmcSimuRawReCal = new EmcSimuRawDataReCal;
  cout << "<I> using run 32382 for EmcSimuRawRecCal" << endl;
  mEmcSimuRawReCal->SetCalibConst(32382);
  
  mEmcGeometry = new mEmcGeometryModule(mEmcGeometryModule::kPISA);
  mEmcTools = mEmcToolsModule::instance();
  int deadmapcollection = mEmcTools->EmcCollectDeadMap(TimeStp);
  if (deadmapcollection!=-1)
    {
      cout << "<W> mEmcTools: Could not collect any dead map from DB for date: " 
	   << TimeStp << endl;
    }
  else
    {
      cout << "<I> mEmcTools: Real-data dead map collected from DB for date: " 
	   << TimeStp << endl;
    }
  
  // PISA-to-PRDF mode
// to be removed, mazsi@bnl.gov //   // Level 2 requests DCM Tables in PISA-to-DST mode
// to be removed, mazsi@bnl.gov //   mEmcRawToLong = new mEmcRawToLongModule;
  
  // Raw PISA Data
  mr = 25000;
  dEmcRawDataWrapper* dEmcRawData = new dEmcRawDataWrapper("dEmcRawData", mr);
  PHIODataNode<PHTable>* dEmcRawDataNode = new PHIODataNode<PHTable>(dEmcRawData, "dEmcRawData");
  emcNode->addNode(dEmcRawDataNode);
  
  mr=25000;
  dEmcRawDataWrapper* dEmcRawDataReCal = new dEmcRawDataWrapper("dEmcRawDataReCal",mr);
  PHIODataNode<PHTable>* dEmcRawDataReCalNode = new PHIODataNode<PHTable>(dEmcRawDataReCal,"dEmcRawDataReCal");
  emcNode->addNode(dEmcRawDataReCalNode);
  
  // Simulated FEM Data
  mr = 500;
  dEmcFEMDataWrapper* dEmcFEMData = new dEmcFEMDataWrapper("dEmcFEMData", mr);
  PHIODataNode<PHTable>* dEmcFEMDataNode = new PHIODataNode<PHTable>(dEmcFEMData, "dEmcFEMData");
  emcNode->addNode(dEmcFEMDataNode);
  
  mr = 500;
  dEmcDCMLongDataWrapper* dEmcDCMLongData = new dEmcDCMLongDataWrapper("dEmcDCMLongData", mr);
  PHIODataNode<PHTable>* dEmcDCMLongDataNode = new PHIODataNode<PHTable>(dEmcDCMLongData, "dEmcDCMLongData");
  dcmNode->addNode(dEmcDCMLongDataNode);
  
  mr=500;
  dEmcDCMLongDataWrapper* dEmcDCMLongDataReCal = new dEmcDCMLongDataWrapper("dEmcDCMLongDataReCal",mr);
  PHIODataNode<PHTable>* dEmcDCMLongDataReCalNode = new PHIODataNode<PHTable>(dEmcDCMLongDataReCal,"dEmcDCMLongDataReCal");
  dcmNode->addNode(dEmcDCMLongDataReCalNode);
  
  PHIODataNode<PHTable>* dEmcGeaTrackTowerNode = 
    (PHIODataNode<PHTable>*)iter.findFirst("PHIODataNode","dEmcGeaTrackTower");
  if(!dEmcGeaTrackTowerNode){
    mr = 15000;
    dEmcGeaTrackTowerWrapper* dEmcGeaTrackTower = new dEmcGeaTrackTowerWrapper("dEmcGeaTrackTower", mr);
    dEmcGeaTrackTowerNode = new PHIODataNode<PHTable>(dEmcGeaTrackTower, "dEmcGeaTrackTower");
    evaNode->addNode(dEmcGeaTrackTowerNode);
  }
  PHIODataNode<PHTable>* dEmcGeaTrackNode =
    (PHIODataNode<PHTable>*)iter.findFirst("PHIODataNode","dEmcGeaTrack");
  if(!dEmcGeaTrackNode){
    mr = 7500;
    dEmcGeaTrackWrapper* dEmcGeaTrack = new dEmcGeaTrackWrapper("dEmcGeaTrack", mr);
    dEmcGeaTrackNode = new PHIODataNode<PHTable>(dEmcGeaTrack, "dEmcGeaTrack");
    evaNode->addNode(dEmcGeaTrackNode);
  }
  
  mr = 8;
  dEmcGeaParamsWrapper* dEmcGeaParams = new dEmcGeaParamsWrapper("dEmcGeaParams", mr);
  PHIODataNode<PHTable>* dEmcGeaParamsNode = new PHIODataNode<PHTable>(dEmcGeaParams, "dEmcGeaParams");
  parNode->addNode(dEmcGeaParamsNode);
  PHIODataNode<PHTable>* dEmcGeaHitNode = 
    (PHIODataNode<PHTable>*)iter.findFirst("PHIODataNode","dEmcGeaHit");
  if(!dEmcGeaHitNode){
    mr = 525000;
    dEmcGeaHitWrapper* dEmcGeaHit = new dEmcGeaHitWrapper("dEmcGeaHit", mr);
    dEmcGeaHitNode = new PHIODataNode<PHTable>(dEmcGeaHit, "dEmcGeaHit");
    geaNode->addNode(dEmcGeaHitNode);
  }
  dEmcGeaHitNode->makeTransient();
  PHIODataNode<PHTable>* dEmcGeaTowerTrackNode  =
    (PHIODataNode<PHTable>*)iter.findFirst("PHIODataNode","dEmcGeaTowerTrack");
  if(!dEmcGeaTowerTrackNode){
    mr = 15000;
    dEmcGeaTowerTrackWrapper* dEmcGeaTowerTrack = new dEmcGeaTowerTrackWrapper("dEmcGeaTowerTrack", mr);
    dEmcGeaTowerTrackNode = new PHIODataNode<PHTable>(dEmcGeaTowerTrack, "dEmcGeaTowerTrack");
    evaNode->addNode(dEmcGeaTowerTrackNode);
  }
  mr = 1;
  dEmcRespParWrapper* dEmcRespPar = new dEmcRespParWrapper("dEmcRespPar", mr);
  PHIODataNode<PHTable>* dEmcRespParNode = new PHIODataNode<PHTable>(dEmcRespPar, "dEmcRespPar");
  parNode->addNode(dEmcRespParNode);
  
  mr = 8;
  emcparWrapper* emcpar = new emcparWrapper("emcpar", mr);
  PHIODataNode<PHTable>* emcparNode = new PHIODataNode<PHTable>(emcpar, "emcpar");
  parNode->addNode(emcparNode);
  
  // Setting dEmcRespPar Parameters
  nrc = 1;
  dEmcRespPar->SetRowCount(nrc);
  dEmcRespPar->set_anyset(0, 1);
  dEmcRespPar->set_sim_timing(0, 0);  // reset to LED timing  May 23, 2001
  dEmcRespPar->set_pbgl_response(0, 3);
  
  mr = 30000;
  dEmcGeometryWrapper* dEmcGeometry = new dEmcGeometryWrapper("dEmcGeometry", mr);
  PHIODataNode<PHTable>* dEmcGeometryNode = new PHIODataNode<PHTable>(dEmcGeometry, "dEmcGeometry");
  parNode->addNode(dEmcGeometryNode);
  
  //	  PHIODataNode<PHTable>* dEmcGeometryNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcGeometry");
  //	  dEmcGeometryWrapper* dEmcGeometry = (dEmcGeometryWrapper*)dEmcGeometryNode->getData();
  
  mEmcDefGeom = new mEmcDefGeomModule();
  
  
  if (rc->get_IntFlag("EVALUATIONFLAG"))
    {
      mEmcGeaClusterEval = new mEmcGeaClusterEvalModule;
      PHIODataNode<PHTable>* dEmcGeaTrackClusterNode =  
	(PHIODataNode<PHTable>*)iter.findFirst("PHIODataNode","dEmcGeaTrackCluster");
      if(!dEmcGeaTrackClusterNode){
	mr = 7500;
	dEmcGeaTrackClusterWrapper* dEmcGeaTrackCluster = new dEmcGeaTrackClusterWrapper("dEmcGeaTrackCluster", mr);
	dEmcGeaTrackClusterNode = new PHIODataNode<PHTable>(dEmcGeaTrackCluster, "dEmcGeaTrackCluster");
	evaNode->addNode(dEmcGeaTrackClusterNode);
      }
      PHIODataNode<PHTable>* dEmcGeaClusterTrackNode = 
	(PHIODataNode<PHTable>*)iter.findFirst("PHIODataNode","dEmcGeaClusterTrack");
      if(!dEmcGeaClusterTrackNode){
	mr = 7500;
	dEmcGeaClusterTrackWrapper* dEmcGeaClusterTrack = new dEmcGeaClusterTrackWrapper("dEmcGeaClusterTrack", mr);
	dEmcGeaClusterTrackNode = new PHIODataNode<PHTable>(dEmcGeaClusterTrack, "dEmcGeaClusterTrack");
	evaNode->addNode(dEmcGeaClusterTrackNode);
      }
    }
  

  //_____________________________________________________________________________
  // REAL & SIMULATED DATA
  // Common Modules & Tables
  
  // align geo and write in parNode for access by cgl
  PHIODataNode<TObject>* mEmcDetGeoNode = new PHIODataNode<TObject>(mEmcGeometry, "mEmcGeometry");
  parNode->addNode(mEmcDetGeoNode);
  
  mEmcClusterNew = new mEmcClusterNewModule(mEmcGeometry);
  
  PHIODataNode<PHTable>* dEmcCalibTowerNode  = 
    (PHIODataNode<PHTable>*)iter.findFirst("PHIODataNode","dEmcCalibTower");
  if(!dEmcCalibTowerNode){ 
    //  mr=30000;
    mr = 25000;
    dEmcCalibTowerWrapper* dEmcCalibTower = new dEmcCalibTowerWrapper("dEmcCalibTower", mr);
    dEmcCalibTowerNode = new PHIODataNode<PHTable>(dEmcCalibTower, "dEmcCalibTower");
    dstNode->addNode(dEmcCalibTowerNode);
  }
  emcCalibratedDataObject * EmcCdo = new emcCalibratedDataObject() ;
  PHIODataNode<TObject>* EmcCdoNode = new PHIODataNode<TObject>(EmcCdo, "EmcCdo");
  emcNode2->addNode(EmcCdoNode) ;
  
  mr = 30000;
  dEmcClusterLocalWrapper* dEmcClusterLocal = new dEmcClusterLocalWrapper("dEmcClusterLocal", mr);
  PHIODataNode<PHTable>* dEmcClusterLocalNode = new PHIODataNode<PHTable>(dEmcClusterLocal, "dEmcClusterLocal");
  emcNode->addNode(dEmcClusterLocalNode);
  
  mr = 1;
  dEmcEventWrapper* dEmcEvent = new dEmcEventWrapper("dEmcEvent", mr);
  PHIODataNode<PHTable>* dEmcEventNode = new PHIODataNode<PHTable>(dEmcEvent, "dEmcEvent");
  emcNode->addNode(dEmcEventNode);
  
  PHIODataNode<PHTable>* dEmcClusterLocalExtNode = 
    (PHIODataNode<PHTable>*)iter.findFirst("PHIODataNode","dEmcClusterLocalExt");
  if(!dEmcClusterLocalExtNode){ 
    mr = 30000;
    dEmcClusterLocalExtWrapper* dEmcClusterLocalExt = new dEmcClusterLocalExtWrapper("dEmcClusterLocalExt", mr);
    dEmcClusterLocalExtNode = new PHIODataNode<PHTable>(dEmcClusterLocalExt, "dEmcClusterLocalExt");
    dstNode->addNode(dEmcClusterLocalExtNode);
  }
  mEmcClusterNew->SetTowerThresholdPbSc(0.010);
  mEmcClusterNew->SetTowerThresholdPbGl(0.014);
  
  mEmcClusterNew->SetMinClusterEnergyPbSc(0.015);
  mEmcClusterNew->SetMinClusterEnergyPbGl(0.060);

  // Here are the new output objects root 3.01.05 kills xyz in
  // EmcClusterLocalExtv1 so they don't go to dst for now
  EmcClusterLocalExt *emcclusterlocalext = new EmcClusterLocalExtv1();
  PHObjectNode_t *EmcClusterLocalExtNode = new PHObjectNode_t(emcclusterlocalext, "EmcClusterLocalExt", "PHObject");
  EmcClusterLocalExtNode->makeTransient();
  dstNode->addNode(EmcClusterLocalExtNode);

  EmcCalibTower *emccalibtower = new EmcCalibTowerv1();
  PHObjectNode_t *EmcCalibTowerNode = new PHObjectNode_t (emccalibtower, "EmcCalibTower", "PHObject");
  EmcCalibTowerNode->makeTransient();
  dstNode->addNode(EmcCalibTowerNode);

  return 0;
}

int 
EmcSimreco::process_event(PHCompositeNode *topNode)
{
  EmcGetGEA(topNode);  // assume legacy operation of Fun4AllPisaInputManager class

  recoConsts *rc = recoConsts::instance();
  static int iFirst = 1;

  mEmcGeaEvent->process_event(topNode);
  
  if ( iFirst == 1 )
    {
      iFirst = 0;
      mEmcGeaParams->process_event(topNode);
      cout << "\n mEmcGeaParams finished " << endl;
    } // first event check
  
  mEmcGeaMakeRaw->process_event(topNode);
  mEmcGeaTrack->process_event(topNode);
  
// to be removed, mazsi@bnl.gov //   mEmcRawToLong->process_event(topNode);
// to be removed, mazsi@bnl.gov //   EmcPutDCMLong(topNode);
// to be removed, mazsi@bnl.gov //   cout << "<I> Running the chain that makes the Lvl2 packet for the EMC" << endl;
	  
  // Call the emc data recalibrator here for trigger simulations
  
  mEmcSimuRawReCal->event(topNode);
  
  // Now follow chain to take the recalibrated data from raw to
  // a new PRDF node named PRDFRECAL
  // I assume we do not need this for year 1 data - it is for Lvl2
  mEmcCalibTower->process_event(topNode);
  mEmcTools->AssignRealDeadMaptoSimulTowers(topNode);
   
  mEmcClusterNew->process_event(topNode);

  //_____________________________________________________________________________
  // SIMULATED DATA EVALUATION
  
  if (rc->get_IntFlag("EVALUATIONFLAG"))
    {
      mEmcGeaClusterEval->process_event(topNode);
    }
  
  copyWrapper(topNode);
  return 0;
}

int EmcSimreco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("EMC"))
    {
      mainIter.forEach(reset);
    }
  return 0;
}

int EmcSimreco::copyWrapper(PHCompositeNode *topNode)
{
  int iret = 0;
  dEmcCalibTowerWrapper *demccalibtower = NULL;
  PHTypedNodeIterator<dEmcCalibTowerWrapper> demccalibtower_iter(topNode);
  PHIODataNode <dEmcCalibTowerWrapper> *dEmcCalibTowerNode = demccalibtower_iter.find("dEmcCalibTower");
  if (dEmcCalibTowerNode)
    {
      demccalibtower = dEmcCalibTowerNode->getData();
    }

  EmcCalibTower *emccalibtower = NULL;
  PHTypedNodeIterator<EmcCalibTower> emccalibtoweriter(topNode);
  EmcCalibTowerNode_t *EmcCalibTowerNode = emccalibtoweriter.find("EmcCalibTower");
  if (EmcCalibTowerNode)
    {
      emccalibtower = EmcCalibTowerNode->getData();
    }
  if (demccalibtower && emccalibtower)
    {
      if (!emccalibtower->isValid() && demccalibtower->RowCount())
        {
          emccalibtower->FillFromWrapper(demccalibtower);
        }
    }
  dEmcClusterLocalExtWrapper *demcclusterlocalext = NULL;
  PHTypedNodeIterator<dEmcClusterLocalExtWrapper> demcclusterlocalext_iter(topNode);
  PHIODataNode <dEmcClusterLocalExtWrapper> *dEmcClusterLocalExtNode = demcclusterlocalext_iter.find("dEmcClusterLocalExt");
  if (dEmcClusterLocalExtNode)
    {
      demcclusterlocalext = dEmcClusterLocalExtNode->getData();
    }

  EmcClusterLocalExt *emcclusterlocalext = NULL;
  PHTypedNodeIterator<EmcClusterLocalExt> emcclusterlocalextiter(topNode);
  EmcClusterLocalExtNode_t *EmcClusterLocalExtNode = emcclusterlocalextiter.find("EmcClusterLocalExt");
  if (EmcClusterLocalExtNode)
    {
      emcclusterlocalext = EmcClusterLocalExtNode->getData();
    }
  if (demcclusterlocalext && emcclusterlocalext)
    {
      if (!emcclusterlocalext->isValid() && demcclusterlocalext->RowCount())
        {
          emcclusterlocalext->FillFromWrapper(demcclusterlocalext);
        }
    }

  return iret;
}


