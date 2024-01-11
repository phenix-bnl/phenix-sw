#include <EmcEmbedDriver.h>

#include <fkinWrapper.h>
#include <primaryWrapper.h>
#include <dEmcGeaClusterTrackWrapper.h>
#include <dEmcGeaTrackWrapper.h>
#include <dEmcGeaTrackClusterWrapper.h>
#include <emcNodeHelper.h>
#include <emcTowerContainerv1M.h>
#include <emcClusterContainerv1M.h>
#include <PHGlobal.h>
#include <SyncObject.h>
#include <VtxOut.h>
#include <VertexGetter.h>

#include <Fun4AllServer.h>
#include <Fun4AllInputManager.h>
#include <getClass.h>

#include <RunHeader.h>

#include <cassert>
#include <memory>
//_____________________________________________________________________________
EmcEmbedDriver::EmcEmbedDriver(const char* realnode /* ="REAL" */,
			       const char* simunode /* ="SIMU" */,
			       const char* mergednode /* ="TOP" */,
			       const char* realinputmanagername)
  : SubsysReco("EmcEmbedDriver"),
    fRealNode(realnode),
    fSimuNode(simunode),
    fMergedNode(mergednode),
    fRealInputManager(realinputmanagername)
{
}

//_____________________________________________________________________________
int
EmcEmbedDriver::InitRun(PHCompositeNode*)
{
  std::cout << PHWHERE << "InitRun" << std::endl;
  static bool first = true;

  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode!=0);
  PHCompositeNode* realTopNode = se->topNode(fRealNode.c_str());
  assert(realTopNode!=0);
  PHCompositeNode* simuTopNode = se->topNode(fSimuNode.c_str());
  assert(simuTopNode!=0);

  RunHeader* rhm = findNode::getClass<RunHeader>(mergedTopNode,"RunHeader");
  RunHeader* rhs = findNode::getClass<RunHeader>(simuTopNode,"RunHeader");
  RunHeader* rhr = findNode::getClass<RunHeader>(realTopNode,"RunHeader");

  std::cout << "RunNumbers : simu=";
  if ( rhs ) 
    {
      std::cout << rhs->get_RunNumber();
    }
  else
    {
      std::cout << " nope";
    }
  std::cout << " real=";
  if ( rhr )
    {
      std::cout << rhr->get_RunNumber();
    }
  else
    {
      std::cout << " nope";
    }
  std::cout << " merged=";
  if ( rhm ) 
    {
      std::cout << rhm->get_RunNumber();
    }
  else
    {
      std::cout << " nope";
    }
  std::cout << std::endl;

  // Create the merged objects we'll need.
  emcNodeHelper nh;

  bool ok = nh.makeCompositeNode(mergedTopNode,"DST","-p");
  assert(ok==true);
  ok = nh.makeCompositeNode(mergedTopNode,"DST/EVA","-p");
  assert(ok==true);
  ok = nh.makeCompositeNode(mergedTopNode,"EMC","-p");
  assert(ok==true);
  ok = nh.makeCompositeNode(mergedTopNode,"RUN","-p");
  assert(ok==true);

  PHCompositeNode* dstNode = 
    nh.findCompositeNode(mergedTopNode,"DST");
  assert(dstNode!=0);

  PHCompositeNode* runNode = 
    nh.findCompositeNode(mergedTopNode,"RUN");
  assert(runNode!=0);

  PHCompositeNode* emcNode = 
    nh.findCompositeNode(mergedTopNode,"EMC");
  assert(emcNode!=0);

  PHCompositeNode* evaNode = 
    nh.findCompositeNode(mergedTopNode,"DST/EVA");
  assert(evaNode!=0);

  // The merged towers.
  nh.addObject<emcTowerContainerv1M>(emcNode,"emcTowerContainer");

  // The merged clusters (from clusterizing of the above merged towers).
  nh.addObject<emcClusterContainerv1M>(emcNode,"emcClusterContainer");

  // The filtered merged clusters.
  nh.addObject<emcClusterContainerv1M>(dstNode,"emcClusterContainer");

  // To create the VtxOut (needed by the reclusterizer), we'll clone
  // the simu vertex, so we don't have to know its type. Cool ;-)

  VtxOut* vtxout = findNode::getClass<VtxOut>(simuTopNode,"VtxOut");
  assert(vtxout!=0);

  // check that the clone does not return zero !
  std::auto_ptr<VtxOut> vclone(vtxout->clone());
  assert(vclone.get()!=0);

  nh.insertObject<VtxOut>(emcNode,vtxout->clone(),"VtxOut",true,"VtxOut");

  // put it also in the merged/DST node so we can transmit the
  // simulated vertex to the output that way.

  nh.insertObject<VtxOut>(dstNode,vtxout->clone(),"VtxOut",true,"VtxOut");

  // PHGlobal 
  PHGlobal* global = findNode::getClass<PHGlobal>(realTopNode,"PHGlobal");
  assert(global!=0);

  PHGlobal* outglobal = global->clone();
  assert(outglobal!=0);
  nh.insertObject<PHGlobal>(dstNode,outglobal,"PHGlobal",true,
			    "PHGlobal");

  // Simulated clusters will go to merged/EVA node too.
  // here again we use the clone (well, the create one in fact, which
  // is the same except we do not bother copying the data, only
  // the type is used).

  emcClusterContainer* simuClusters =
    findNode::getClass<emcClusterContainer>(simuTopNode,"emcClusterContainer");
  assert(simuClusters!=0);

  // the create() method is equivalent to 
  //   emcClusterContainer* copySimuClusters = simuClusters->clone();
  //   copySimuClusters->Reset();
  // but it saves the actual call to the copy part...

  nh.insertObject<emcClusterContainer>(dstNode,simuClusters->create(),
				       "emcSimClusterContainer",true,
				       "emcSimClusterContainer");

  SyncObject* sync = 
    findNode::getClass<SyncObject>(realTopNode,"Sync");
  assert(sync!=0);
  nh.insertObject<SyncObject>(dstNode,sync->clone(),
			      "Sync",true,"Sync");

  // The (argh!) very antique STAF Evaluation tables...

  emcNodeHelper::addTable<dEmcGeaTrackWrapper>(evaNode,"dEmcGeaTrack",7500);
  emcNodeHelper::addTable<dEmcGeaTrackClusterWrapper>(evaNode,"dEmcGeaTrackCluster",7500);
  emcNodeHelper::addTable<dEmcGeaClusterTrackWrapper>(evaNode,"dEmcGeaClusterTrack",7500);
  emcNodeHelper::addTable<fkinWrapper>(evaNode,"fkin",30000);
  emcNodeHelper::addTable<primaryWrapper>(evaNode,"primary",10);

  // Last but not least, read real run node under merged node.

  Fun4AllInputManager* im = se->getInputManager(fRealInputManager.c_str());
  assert(im!=0);

  PHNodeIOManager* dst = new PHNodeIOManager(im->Filename(),
					     PHReadOnly,PHRunTree);
  assert(dst!=0);

  dst->read(runNode);
  
  delete dst;

  if ( first )
    {
      se->Print();
      first=false;
    }
  else
    {
      std::cerr << PHWHERE << " Twice in InitRun !" << std::endl;
      return ABORTRUN;
    }

  return 0;
}

//_____________________________________________________________________________
int
EmcEmbedDriver::process_event(PHCompositeNode*)
{
  // copy the vertex from real node (PHGlobal) to EMC working node
  // (VtxOut).

  float zvertex = VertexGetter::getVertex(fRealNode);

  assert(isnan(zvertex)==0);

  Fun4AllServer* se = Fun4AllServer::instance();

  emcNodeHelper nh;

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode!=0);

  PHCompositeNode* emcNode = 
    nh.findCompositeNode(mergedTopNode,"EMC");
  assert(emcNode!=0);

  VtxOut* vtxout = findNode::getClass<VtxOut>(emcNode,"VtxOut");
  assert(vtxout!=0);

  vtxout->Reset();
  float vtx[3];
  float vtxerr[3];

  vtx[0]=vtx[1]=0.0;
  vtx[2]=zvertex;

  vtxerr[0]=vtxerr[1]=vtxerr[2]=0.0;

  vtxout->AddVtx("EMCEMBED",vtx,vtxerr,100);

  return 0;
}
