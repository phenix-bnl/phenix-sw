/*
 * see header for description
 *
 */


#include <stack>
#include <string>
#include <iostream>

#include <PHObject.h>
#include <PHNodeOperation.h>
#include <PHNodeIterator.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <SubsysRecoStack.h>
#include <emcNodeHelper.h>

#include <mEmcClusterizerv0.h>
#include <emcClusterContainer.h>
#include <emcGeaTrackContainer.h>
#include <EmcGeaContainerImporter.h>
#include <EmcRealContainerImporter.h>
#include <EmcApplyQA.h>

#include <EmcEmbedReclusterizer.h>
#include <CopyNonEMCNodes.h>
#include <EmcUnclusterizer.h>
//#include <EmcSimTowerSmearer.h>
#include <EmcTowerScalerSmearer.h>
#include <EmcEmbedDriver2.h>
#include <EmcDataMerger.h>



using namespace std;



ClassImp(EmcEmbedDriver2);



namespace {

  const static float fgTowerThresholdPbSc = 0.010;
  const static float fgTowerThresholdPbGl = 0.014;
  const static float fgMinClusterEnergyPbSc = 0.015;
  const static float fgMinClusterEnergyPbGl = 0.060;

}





EmcEmbedDriver2::EmcEmbedDriver2(std::string realnode, std::string simnode): SubsysRecoStack("EmcEmbedDriver2"){
  this->realnode = realnode;
  this->simnode = simnode;
  inited = false;

  setcopy();

  tmprealimp = new SubsysRecoStack();
  tmpsimimp = new SubsysRecoStack();

  realimp = simimp = NULL;
  setinputtype();

  setcalib();

  geom = NULL;
  setgeomrealm();
}





EmcEmbedDriver2::~EmcEmbedDriver2(){
  // note: no need to delete realimp/simimp: ~SubsysRecoStack()
  // will do that.

  delete tmpsimimp;
  delete tmprealimp;
}





void EmcEmbedDriver2::setinputtype(EmcEmbedDriver2::inputtype_t real, EmcEmbedDriver2::inputtype_t sim){ 
  assert( inited == false );
  this->realtype = real;
  this->simtype = sim;
}





void EmcEmbedDriver2::setcalib(calib_t calib){
  assert( inited == false );
  this->calib = calib;
}





void EmcEmbedDriver2::setgeomrealm(mEmcGeometryModule::ERealm realm){
  assert( inited == false );
  this->realm = realm;
}





int EmcEmbedDriver2::Init(PHCompositeNode * root){

  PHCompositeNode * real = Fun4AllServer::instance()->topNode( realnode.c_str() );
  PHCompositeNode * sim = Fun4AllServer::instance()->topNode( simnode.c_str() ); 


  // copy nonemc data: our modules will need some of them as input
  if( copynonemc ){
    push_back( new CopyNonEMCNodes(sim, "CopyNonEMCNodesSimPre") ); // copy from simulation..
    push_back( new CopyNonEMCNodes(real, "CopyNonEMCNodesRealPre") ); // ..and overwrite with real
  }


  // import real data
  if( realtype == LIBEMCV1 ) realimp = new EmcRealContainerImporter( real );
  else realimp = new EmcGeaContainerImporter( real );

  realimp->push_back( new EmcUnclusterizer() );
  if( calib & REAL ) realimp->push_back( new EmcApplyQA( EmcApplyQA::TOWER ) ); // importer does a poor job
  push_back( realimp );

  realimp->splice( realimp->end(), *tmprealimp );


  // import simulated data
  if( simtype == LIBEMCV1 ) simimp = new EmcRealContainerImporter(sim);
  else simimp = new EmcGeaContainerImporter(sim);

  simimp->push_back( new EmcUnclusterizer() );
  if( calib & SIM ) simimp->push_back( new EmcApplyQA( EmcApplyQA::TOWER ) );
  //simimp->push_back( new EmcSimTowerSmearer(simname, 0.02) ); // smear simulated data with 2%
  simimp->push_back( new EmcTowerScalerSmearer(1.0, 0.02) ); // smear simulated data with 2%
  push_back( simimp );

  simimp->splice( simimp->end(), *tmpsimimp );


  // merge data
  EmcDataMerger * merger = new EmcDataMerger();
  merger->AddSourceNode( realnode );
  merger->AddSourceNode( simnode );
  push_back( merger );

  if( calib & MERGED ) push_back( new EmcApplyQA( EmcApplyQA::TOWER ) );
  
  
  // clusterize data
#if 0
  geom = new mEmcGeometryModule( realm );
  push_back( geom );

  mEmcClusterizerv0 * clusterizer = new mEmcClusterizerv0( geom );
  clusterizer->SetTowerThresholdPbSc(fgTowerThresholdPbSc);
  clusterizer->SetTowerThresholdPbGl(fgTowerThresholdPbGl);
  clusterizer->SetMinClusterEnergyPbSc(fgMinClusterEnergyPbSc);
  clusterizer->SetMinClusterEnergyPbGl(fgMinClusterEnergyPbGl);
  push_back( clusterizer );
#else
  push_back( new EmcEmbedReclusterizer("TOP", "TOP", "TOP", "", realm) );
#endif


  inited = true;


  // print stack
  if( verbosity ){
    cout << "===== " << __PRETTY_FUNCTION__ << " =====" << endl;
    Print2("SHORT", "    ");
    cout << endl;
  }


  // init chain
  return SubsysRecoStack::Init(root); 
}





int EmcEmbedDriver2::InitRun(PHCompositeNode * root){
  firstevent = true;
  return SubsysRecoStack::InitRun(root);
}





int EmcEmbedDriver2::process_event(PHCompositeNode * root){
  int rc = SubsysRecoStack::process_event( root );

  if( verbosity && firstevent ){
    cout << endl;
    cout << "===== " << __PRETTY_FUNCTION__ << ": PHCompositeNodes after first event: =====" << endl;
    cout << endl;

    PHCompositeNode * real = Fun4AllServer::instance()->topNode( realnode.c_str() );
    real->print();
    cout << endl;
    
    PHCompositeNode * sim = Fun4AllServer::instance()->topNode( simnode.c_str() ); 
    sim->print();
    cout << endl;
    
    PHCompositeNode * realroot = getroot( root );
    realroot->print();
    cout << endl;

    cout << endl;
  }

  firstevent = false;

  return rc;
}



