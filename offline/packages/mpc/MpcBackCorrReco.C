//  GENERal PHENIX tools
#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>

//  Data classes I am using in analysis
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <PHGlobal.h>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <MpcBackCorrReco.h>
#include <MpcMap.h>
#include <RunHeader.h>

//  Root histogram types
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TFile.h>
#include <TLorentzVector.h>
#include <TVector3.h>
#include <cmath>


using namespace std;
using namespace findNode;


MpcBackCorrReco::MpcBackCorrReco(const char* name) : SubsysReco(name)
{
  return;
}

MpcBackCorrReco::~MpcBackCorrReco()
{
  //  delete trighelp;
}

int MpcBackCorrReco::InitRun(PHCompositeNode *topNode)
{
  mpcmap = getClass<MpcMap>(topNode, "MpcMap");
/*
  if ( new_mpcmap==0 ) new_mpcmap = new MpcMap(topNode);
  else
    {
      // get new MpcMap since the run number has changed
      
      delete new_mpcmap;
      new_mpcmap = new MpcMap(topNode);
    }
*/
  
  
//  trighelp = new TriggerHelper(topNode);
//  if ( trighelp==0 )
//    {
//      cout << "MpcBackCorrReco::InitRun, TriggerHelper not found" << endl;
//      return ABORTRUN;
//    }
  runheader = getClass<RunHeader>   (topNode, "RunHeader");
  if(!runheader)
    {
      cout << "RunHeader not found\n";
      return ABORTRUN;
    }
  runnum = runheader -> get_RunNumber();
  daflag = 1;
  if(runnum>=254945) daflag = 0;
  
  return 0;
}


int MpcBackCorrReco::Init(PHCompositeNode *topNode)
{
  offset[0][0] = 0.287192;
  offset[0][1] = 0.179907;
  offset[0][2] = 0.153795;
  offset[0][3] = 0.145455;
  offset[0][4] = 0.134522;
  offset[0][5] = 0.116834;
  offset[0][6] = 0.107699;
  offset[0][7] = 0.0696471;
  offset[0][8] = 0.0639894;
  offset[1][0] = 0.304825;
  offset[1][1] = 0.202046;
  offset[1][2] = 0.179017;
  offset[1][3] = 0.166717;
  offset[1][4] = 0.152725;
  offset[1][5] = 0.140938;
  offset[1][6] = 0.121345;
  offset[1][7] = 0.118275;
  offset[1][8] = 0.0765483;
  offset[2][0] = 1.2;
  offset[2][1] = 0.401493;
  offset[2][2] = 0.484416;
  offset[2][3] = 0.185455;
  offset[2][4] = 0.202614;
  offset[2][5] = 0.144444;
  offset[2][6] = 0.131111;
  offset[2][7] = 0.130742;
  offset[2][8] = 0.0826923;
  offset[3][0] = 1.19726;
  offset[3][1] = 0.399074;
  offset[3][2] = 0.204294;
  offset[3][3] = 0.196087;
  offset[3][4] = 0.177848;
  offset[3][5] = 0.159406;
  offset[3][6] = 0.150625;
  offset[3][7] = 0.122147;
  offset[3][8] = 0.0845902;
  
  return 0;
}

int MpcBackCorrReco::process_event(PHCompositeNode *topNode)
{
  mpcmap = getClass<MpcMap>(topNode, "MpcMap");
  
  // informational message...
  if(daflag == 0) return 0; //no background subtraction for pp
  static int ncalls = 0;
  ncalls++;
  if (ncalls % 1000 == 0 && verbosity> 2)
    {
      cout << "MpcBackCorrReco Ncalls = " << ncalls << endl;
    }
  
  global = getClass<PHGlobal>(topNode, "PHGlobal");
  if (global == 0)
    {
      if(verbosity > 2){
	cout << "MpcBackCorrReco::process_event global not found" << endl;
      }
      
      return 0; //let someone else abort the event
    }
  mpcclus = getClass<mpcClusterContainer>(topNode,"mpcClusterContainer");
  
  if(!mpcclus){
    if(verbosity > 2)
      {
	cout << "cant' find mpccluster\n"
	     << "mpcclus: " << mpcclus << endl
	     << endl;
      }
    return 0;
  }
  
  
  float cent = -1;
  if(daflag) cent = global->getCentrality();
  if(cent<-0.10 || cent > 100.1){ return ABORTEVENT;}
  
  int nclus = mpcclus->size();
  
  for (int iclus=0; iclus<nclus; iclus++)
    {
      mpcClusterContent *clus = mpcclus->getCluster(iclus);
      int arm = clus->arm();
      if(arm == 1) continue; // no corrections for the north arm
      float ecore = clus->ecore();
      if(ecore < 1.75) continue; // do nothing is cluster energy is small
      float eta = fabs(GetEta(clus));
      float e_offset = GetOffset(eta,cent);
      float ecorecorr = ecore-e_offset;
      if(verbosity > 2) {
	int ix,iy,arm;
	ix=clus->ixpos();
	iy=clus->iypos();
	arm = clus->arm();
	int ch = mpcmap->getFeeCh(ix,iy,arm);
	cout << "ch,ix,iy,fabs(Eta),cent " << ch << ", " 
	     << ix << ", " << iy << ", " 
	     << eta << ", " << cent << endl;
	cout << "Eorig, offset, Efinal: " 
	     << ecore << ", " << e_offset << ", " << ecorecorr << endl;
      }
      clus->set_ecore(ecorecorr);
      if(verbosity > 2)
	cout << "New Cluster Energy is: " << clus->ecore() << endl;
    }      
  return EVENT_OK;
}



int MpcBackCorrReco::End(PHCompositeNode *topNode)
{
  return 0;
}


float MpcBackCorrReco::GetOffset(float eta, float cent){
  int centclass = (int) cent/10;
  int etaclass = -1;
  if(centclass < 0 || centclass > 8) return 0;
  if(eta >= 3.0 && eta < 3.2){
    etaclass = 0;
  }
  else if(eta >= 3.2 && eta < 3.3){
    etaclass = 1;
  }
  else if(eta >= 3.3 && eta < 3.5){
    etaclass = 2;
  }
  else if(eta >= 3.5 && eta < 4.0){
    etaclass = 3;
  }
  
  if(etaclass < 0) return 0;
  return offset[etaclass][centclass];
}



float MpcBackCorrReco::GetEta(int ch)
{
  float pseudorapidity = -9999;
  if(mpcmap != 0)
    {
      if( mpcmap->getGridX(ch) >= 0 )
        {
          TVector3 v3d( mpcmap->getX(ch),mpcmap->getY(ch),mpcmap->getZ(ch) );
          pseudorapidity = v3d.PseudoRapidity(); 
        }
    }
  else cout << "Need to initialize MpcMap object" << endl;
  return pseudorapidity;
}

float MpcBackCorrReco::GetEta(mpcClusterContent* clus)
{
  
  int ixpos,iypos,arm;
  if(clus!= 0)
    {
      ixpos=clus->ixpos();
      iypos=clus->iypos();
      arm = clus->arm();
      int ch = mpcmap->getFeeCh(ixpos,iypos,arm);
      return GetEta(ch);
    }
  else
    {
      return -9999;
    }
}
