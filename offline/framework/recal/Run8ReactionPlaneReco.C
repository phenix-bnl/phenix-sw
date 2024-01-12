//
// This is for Run-8 d+Au reaction plane reco
//
#include "Run8ReactionPlaneReco.h"
#include "PHGlobal.h"
#include "ReactionPlaneCalibv2.h"
#include "ReactionPlaneObject.h"
#include "ReactionPlaneObjectv3.h"
#include "RpConst.h"
#include "RpSumXYObjectv2.h"

#include <Bbc.hh>
#include <BbcRaw.h>
#include <BbcCalib.hh>
#include <BbcGeo.hh>
#include <RunToTime.hh>

#include <SmdOut.h>

#include "MpcMap.h"
#include "mpcTowerContainer.h"
#include "mpcTowerContent.h"

#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "PHCentralTrackv23.h"
#include "PHSnglCentralTrackv23.h"

#include <TMutNode.h>
#include <Fun4AllReturnCodes.h>
#include <PHMapManager.h>
#include <TRxnpRawScintMap.h>
#include <TRxnpScintMap.h>
#include <TRxnpRawXangMap.h>

#include <PHCompositeNode.h>

#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "recoConsts.h"
#include "getClass.h"

#include "PHNodeReset.h"
#include "PHNodeIterator.h"
#include "PHIODataNode.h"
#include "TH1D.h"


#include <iostream>
#include <cmath>

typedef PHIODataNode <ReactionPlaneObject> ReactionPlaneObjectNode_t;
typedef PHIODataNode<RpSumXYObject> RpSumXYObjectNode_t;

using namespace RP;
using namespace std;

Run8ReactionPlaneReco::Run8ReactionPlaneReco()
  : Recalibrator("Run8ReactionPlaneReco")
{
  d_rp = 0;
  rpcalib = 0;
//  baseclasses.insert("RpSumXYObject");
}

Run8ReactionPlaneReco::~Run8ReactionPlaneReco()
{
  delete rpcalib;
  return;
}

int
Run8ReactionPlaneReco::isValidRun(const int runno) const
{
   if (runno >= 246444 && runno <= 253701) // Run8 d+Au 200 GeV
   {
     return 1;
   }

  return 0;
}

int Run8ReactionPlaneReco::InitRun(PHCompositeNode* topNode)
{
  Fun4AllServer* se = Fun4AllServer::instance();

  recoConsts *rc = recoConsts::instance();
  runNumber = rc->get_IntFlag("RUNNUMBER");

  CreateNodeTree(topNode, runNumber);

  rpcalib = new ReactionPlaneCalibv2();
  if (rpcalib->Fetch(runNumber)) // not 0 means failure to load calibs
    {
      cout << PHWHERE << "No Reaction Plane calibration for run " 
	   << runNumber 
	   << ", no Reaction Plane for this run" 
           << endl;
      calibration_ok = 0;
    }
  else
    {
      calibration_ok = 1;
    }

  bbccalib = new BbcCalib();
  bbcgeo = new BbcGeo();

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp* ts( runTime->getBeginTime(runNumber) );
  PHTimeStamp tstart = *ts;
  delete ts;
  int bbccalib_version = 4002;
  bbccalib->restore(tstart, bbccalib_version);

  {
    PHNodeIterator nodeItr(topNode);
    dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!dst_node) {
      cerr << "Run8ReactionPlane::InitRun - could not find RXN DST node.\n";
    }
  }

  {  
    PHNodeIterator nodeItr(topNode);
    rxnp_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RXNP"));
    if(!rxnp_node){
      rxnp_node = new PHCompositeNode("RXNP");
      topNode->addNode(rxnp_node);
    }
  }
  
  TMutNode<TRxnpRawScintMap>::new_dst_input_node(rxnp_node, "TRxnpRawScintMap", dst_node, "TRxnpRawScint");


  // RPHist making
  //
  // MPC S
  h_MPCrp00 = new TH1D("h_MPCrp00","h_MPCrp00",360,-3.1416,3.1416);
  h_MPCrp10 = new TH1D("h_MPCrp10","h_MPCrp10",360,-3.1416,3.1416);
  se->registerHisto(h_MPCrp00);
  se->registerHisto(h_MPCrp10);

  // MPC N
  h_MPCrp01 = new TH1D("h_MPCrp01","h_MPCrp01",360,-3.1416,3.1416);
  h_MPCrp11 = new TH1D("h_MPCrp11","h_MPCrp11",360,-3.1416,3.1416);
  se->registerHisto(h_MPCrp01);
  se->registerHisto(h_MPCrp11);

  // BBC S
  h_BBCrp00 = new TH1D("h_BBCrp00","h_BBCrp00",360,-3.1416,3.1416);
  h_BBCrp10 = new TH1D("h_BBCrp10","h_BBCrp10",360,-3.1416,3.1416);
  se->registerHisto(h_BBCrp00);
  se->registerHisto(h_BBCrp10);
 
  // BBC N
  h_BBCrp01 = new TH1D("h_BBCrp01","h_BBCrp01",360,-3.1416,3.1416);
  h_BBCrp11 = new TH1D("h_BBCrp11","h_BBCrp11",360,-3.1416,3.1416);
  se->registerHisto(h_BBCrp01);
  se->registerHisto(h_BBCrp11);

  // RxN inner S
  h_RXNrp00 = new TH1D("h_RXNrp00","h_RXNrp00",360,-3.1416,3.1416);
  h_RXNrp10 = new TH1D("h_RXNrp10","h_RXNrp10",360,-3.1416,3.1416);
  se->registerHisto(h_RXNrp00);
  se->registerHisto(h_RXNrp10);

  // RxN outer S
  h_RXNrp01 = new TH1D("h_RXNrp01","h_RXNrp01",360,-3.1416,3.1416);
  h_RXNrp11 = new TH1D("h_RXNrp11","h_RXNrp11",360,-3.1416,3.1416);
  se->registerHisto(h_RXNrp01);
  se->registerHisto(h_RXNrp11);

  // RxN in+out S
  h_RXNrp02 = new TH1D("h_RXNrp02","h_RXNrp02",360,-3.1416,3.1416);
  h_RXNrp12 = new TH1D("h_RXNrp12","h_RXNrp12",360,-3.1416,3.1416);
  se->registerHisto(h_RXNrp02);
  se->registerHisto(h_RXNrp12);

  // RxN inner N
  h_RXNrp03 = new TH1D("h_RXNrp03","h_RXNrp03",360,-3.1416,3.1416);
  h_RXNrp13 = new TH1D("h_RXNrp13","h_RXNrp13",360,-3.1416,3.1416);
  se->registerHisto(h_RXNrp03);
  se->registerHisto(h_RXNrp13);

  // RxN outer N
  h_RXNrp04 = new TH1D("h_RXNrp04","h_RXNrp04",360,-3.1416,3.1416);
  h_RXNrp14 = new TH1D("h_RXNrp14","h_RXNrp14",360,-3.1416,3.1416);
  se->registerHisto(h_RXNrp04);
  se->registerHisto(h_RXNrp14);

  // RxN in+out N
  h_RXNrp05 = new TH1D("h_RXNrp05","h_RXNrp05",360,-3.1416,3.1416);
  h_RXNrp15 = new TH1D("h_RXNrp15","h_RXNrp15",360,-3.1416,3.1416);
  se->registerHisto(h_RXNrp05);
  se->registerHisto(h_RXNrp15);
 
  // SMD S
  h_SMDrp00 = new TH1D("h_SMDrp00","h_SMDrp00",360,-3.1416,3.1416);
  se->registerHisto(h_SMDrp00);

  // CNT (Psi2 onlt, no zed classification)
  h_CNTrp14 = new TH1D("h_CNTrp14","h_CNTrp14",360,-3.1416,3.1416);
  se->registerHisto(h_CNTrp14);

  ievent = 0;

  return 0;
}

int 
Run8ReactionPlaneReco::process_event(PHCompositeNode* topNode)
{
  if (!calibration_ok)
    {
      return EVENT_OK;
    }

  PHGlobal* global = NULL;
  global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if(!global) 
    {
      cerr << PHWHERE << " No PHGlobal object !" << endl;
      return ABORTEVENT;
    }


  d_rpsumxy = findNode::getClass<RpSumXYObject>(topNode, "RpSumXYObject");

  if(!d_rpsumxy) 
    {
      cerr << PHWHERE << " No RpSumXYObject object !" << endl;
      return ABORTEVENT;
    }

  BbcRaw *bbcraw = findNode::getClass<BbcRaw>(topNode,"BbcRaw");

  MpcMap *mpcmap = MpcMap::instance();
  mpcTowerContainer *mpctower = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctower) {
    cout << PHWHERE << "could not get mpcTowerContainer, is Node missing?" << endl;
    return 1;
  }
  mpcTowerContent *tower;


  TRxnpRawScintMap* raw_map;

  try{
  raw_map = TMutNode<TRxnpRawScintMap>::find_node(topNode, "TRxnpRawScintMap");
  }

  catch ( exception& e)
    {
      return 1 ;
    }


  if(ievent%10000==0) cout<< "IEVENT: "<<ievent<<endl;
  ievent ++;

  float bbcz = global->getBbcZVertex();
  bool isBbcZVertexOK = fabs(bbcz)<30.0;
  if( !isBbcZVertexOK ) {
    d_rp->Reset(); // set rp to -9999.
    return EVENT_OK;
  }

  int cent=-1;
  cent = (int)global->getCentrality();
  int imul = rpcalib->GetCentralityBin(cent);
  int izps = rpcalib->GetZVertexBin(bbcz);

  if(imul<0 || izps<0){
    if(verbosity>1){
      cout << PHWHERE << " Error : Multiplicity or Z-vertex, out of range. "
	   << "(imul,izps)=(" << imul << "," << izps << ")" << endl;
    }
    d_rp->Reset(); // set rp to -9999.

    return EVENT_OK;
  }


//  TRxnpRawScintMap* raw_map =  findNode::getClass<TRxnpRawScintMap>(topNode,"TRxnpRawScintMap");

/*
  TRxnpRawScintMap* raw_map =  findNode::getClass<TRxnpRawScintMap>(topNode,"TRxnpRawScint");
  if (!raw_map) {
    cout << PHWHERE << "could not get TRxnpRawScintMap, is Node missing?" << endl;
  }
*/

/*
  try { 
    raw_map = TMutNode<TRxnpRawScintMap>::find_node(topNode, "TRxnpRawScintMap");
    //  cor_map = TMutNode<TRxnpScintMap>::find_node(topNode, "TRxnpScintMap");
  }
  catch(std::exception& e) { cout <<"RxNP"<<endl; return 1; }
*/


  float pi = acos(-1);
  //
  // Temporary Q-vector calculation storage
  //
  float    sumxy[2][21][3];

  for (int ih=0; ih<2; ih++) {
    for (int id=0; id<21; id++) {
      for (int iv=0; iv<3; iv++) {
        sumxy[ih][id][iv]=0.0;
      }
    }
  }


  //
  // RxNP Q-vector calculation
  //
  // reaction plane detector (rxn) r.p. // -----------------------------------
  char name[80];
  int nrxn=0;
  int jr = 0;

  
  for(unsigned short iarm = 0; iarm < 2; iarm++) {// iarm==0 south, iarm==1 north
    for(unsigned short iring = 0; iring < 2; iring++) {// iring==0 inner, iring==1 outer
      for(unsigned short iscint = 0; iscint < 12; iscint++) {
        int ir = 0;
        TRxnpRawScintMap::const_iterator raw_iter = raw_map->get(iarm, iring, iscint);
        while(TRxnpRawScintMap::const_pointer raw_ptr= raw_iter.next()) {
          cout <<"RxNP calculation"<<endl;
          int chanid      = raw_ptr->get()->get_chanid();
          float low_pre   = raw_ptr->get()->get_low_pre();
          float low_post  = raw_ptr->get()->get_low_post();
          float tdc       = raw_ptr->get()->get_tdc();
          float phi       = raw_ptr->get()->get_phi(); // +0.075*dumflt->GetRandom();
          phi             = atan2(sin(phi),cos(phi));
          int iphi        = (int)(12.0*(phi+pi)/(2.0*pi));
          float theta     = raw_ptr->get()->get_theta();
          float rxnz      = 39.0;
          if (iarm==0) rxnz = -39.0;
          float rad       = rxnz*tan(theta);
          float the       = atan2(rad,rxnz-bbcz);
          float eta       = -log(tan(0.5*the));
          float val       = low_post - low_pre;


          if (val>0.0 && tdc>500.0 && tdc<1800.0) {
           if (val>0) {
            if ((iarm-0.5)*eta<0) cout << "rxn vtx: " << iarm*24+iring*12+iscint
                                       << " " << eta << " " << rxnz << " " << bbcz << endl;
            if (nrxn<1000) nrxn++;
            else cout << "exceed nrxn buffer " << ir << " " << jr << endl;
           }
           
           for (int ih=0; ih<2; ih++) {
            int nfil=1;
            float fdiv=0;
            float dphi=0;

            if (iarm==0 && iring==0 && (iphi==8 || iphi==10)) {
             nfil=2;
             fdiv=0.5;
             dphi=2.0*pi/12.0;
             if (iphi==10) dphi*=-1.0; 
            }
            for (int in=0; in<nfil; in++) {
             int jring=iring;
             float wal=val;
             float phj=phi;
             if (in>0) {
              jring=0;
              wal=val*fdiv;
              phj=phi+dphi;
             }
             float vc=wal*cos((ih+1.0)*phj);
             float vs=wal*sin((ih+1.0)*phj);
             if (iarm==0 && jring==0) {sumxy[ih][0][0]+=vc; sumxy[ih][0][1]+=vs; sumxy[ih][0][2]+=wal;}
             if (iarm==0 && jring==1) {sumxy[ih][1][0]+=vc; sumxy[ih][1][1]+=vs; sumxy[ih][1][2]+=wal;}
             if (iarm==1 && jring==0) {sumxy[ih][3][0]+=vc; sumxy[ih][3][1]+=vs; sumxy[ih][3][2]+=wal;}
             if (iarm==1 && jring==1) {sumxy[ih][4][0]+=vc; sumxy[ih][4][1]+=vs; sumxy[ih][4][2]+=wal;}
            } // for in
           } // for ih
          } // if cut
          if (chanid<0 || chanid>47) cout << "error in chanid" << endl;
          ir++;
        }
        if (ir>1) {
          sprintf(name,"error %d %d %d %d",iarm,iring,iscint,ir);
          cout << name << endl;
        }
        jr+=ir;
      }
    }
  }

  //
  // MPC Q-vector calculation
  //
  // muon piston calorimeter (mpc) r.p. // -----------------------------------
  int ntowers=mpctower->size();
  for (int itower=0; itower<ntowers; itower++){
    tower = mpctower->getTower(itower);
    int fee_ch = tower->get_ch();
    float mpc_e = tower->get_energy();        // get energy in 1 tower
    float mpc_tof = tower->get_tof();         // get tof of hit in tower
    float mpc_x = mpcmap->getX(fee_ch);       // get x position of tower
    float mpc_y = mpcmap->getY(fee_ch);       // get y position of tower
    float mpc_z = mpcmap->getZ(fee_ch);       // z>0 (north), z<0 (south)
    float mpc_phi = atan2(mpc_y,mpc_x); // + 0.015*dumflt->GetRandom();
    float mpc_rad = sqrt(mpc_x*mpc_x+mpc_y*mpc_y);
    float mpc_the = atan2(mpc_rad,mpc_z); // mpc_z-bbcv);

    //cout<<"************* "<<mpc_z<<" "<<bbcv<<endl;

    if(BadTower(fee_ch)) continue;

    if (mpc_e<0.0 || mpc_tof<500.0 || mpc_tof>2200.0) continue;

    if (fabs(mpc_z) < 1.0) continue;          // skip non-existent channels

    int iarm = 0; //south
    if (mpc_z > 0.0) iarm = 1;//north
    float val = mpc_e*sin(mpc_the);

    //if(mpc_e<=3.0) continue;

    //reaction plane
    for (int ih=0; ih<2; ih++) {
      float vc=val*cos((ih+1.0)*mpc_phi);
      float vs=val*sin((ih+1.0)*mpc_phi);
      if (iarm==0) {sumxy[ih][12][0]+=vc; sumxy[ih][12][1]+=vs; sumxy[ih][12][2]+=val;}
      if (iarm==1) {sumxy[ih][13][0]+=vc; sumxy[ih][13][1]+=vs; sumxy[ih][13][2]+=val;}
    }
  }

  //
  // BBC Q-vector calculation
  //
  if(bbcraw){
    for (int ipmt=0; ipmt<128; ipmt++) {
      short adc = bbcraw->get_Adc(ipmt);
      short tdc = bbcraw->get_Tdc0(ipmt);
      float time0=bbccalib->getHitTime0(ipmt, tdc, adc);
      float charge=bbccalib->getCharge(ipmt, adc);

      float bbcx= bbcgeo->getX(ipmt);
      float bbcy= bbcgeo->getY(ipmt);
      float bbczp= bbcgeo->getZ(ipmt);

      if (time0>0.0 && charge>0.0) {
        int iarm = 0;
        if (bbczp > 0.0) iarm = 1;
        float phi=atan2(bbcy,bbcx);

        float val=charge;

        float rad = sqrt(bbcx*bbcx+bbcy*bbcy);
        float the = atan2(rad,bbczp); // bbcz-bbcv*10.0);
        float eta = -log(tan(0.5*the));

        if (val>0.0) {
          if (((float)iarm-0.5)*eta<0.0) cout << "bbc vtx: " << ipmt << " " << eta << " " << bbczp << " " << bbcz << endl;
          for (int ih=0; ih<2; ih++) {
            float vc=val*cos((ih+1.0)*phi);
            float vs=val*sin((ih+1.0)*phi);
            if (iarm==0) {sumxy[ih][16][0]+=vc; sumxy[ih][16][1]+=vs; sumxy[ih][16][2]+=val;}
            if (iarm==1) {sumxy[ih][17][0]+=vc; sumxy[ih][17][1]+=vs; sumxy[ih][17][2]+=val;}
          }
        }
      }
    }
  }

  //
  // SMD Q-vector calculation
  //
  static float Qsmd[4][2][3];
  memset(Qsmd, 0, sizeof(Qsmd));

  //SMD
  SmdOut* smdout = findNode::getClass<SmdOut>(topNode, "SmdOut");
  if ( smdout ){
    for (int iarm = 0; iarm < 2; iarm++){
      if (isfinite(smdout->get_Xpos(iarm)) && isfinite(smdout->get_Ypos(iarm))
          && smdout->get_Xpos(iarm) < 9999 && smdout->get_Ypos(iarm) < 9999){
        Qsmd[0][iarm][0] = smdout->get_Xpos(iarm);
        Qsmd[0][iarm][1] = smdout->get_Ypos(iarm);
        Qsmd[0][iarm][2] = 1.0;
      }
    }
  }

  //CNT
  PHCentralTrack* central = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if ( central ){
    for(unsigned int itrk=0;itrk<central->get_npart();itrk++){
      PHSnglCentralTrack *d_scnt = central->get_track(itrk);
      
      //CNT plane
      float pt = d_scnt->get_mom()*sin(d_scnt->get_the0());
      
      if (pt<2.0 && pt>0.2 &&
          fabs(d_scnt->get_pc3dphi())<0.4 && fabs(d_scnt->get_pc3dz())<8.0
          && d_scnt->get_quality() > 7){
        
        float phi = d_scnt->get_phi0();
        
        for(int ih=0; ih<2; ih++){
          float vc=cos((ih+1.0)*phi);
          float vs=sin((ih+1.0)*phi);
          sumxy[ih][20][0]+=pt*vc;
          sumxy[ih][20][1]+=pt*vs;
          sumxy[ih][20][2]+=pt;
        }
      }
    }
  }


  //
  // Store Q-vector info into sumXXX in RpSumXYObject
  //

  // MPC S
  if(sumxy[0][12][2]>0){
    d_rpsumxy->setMPCsumX00(sumxy[0][12][0]/sumxy[0][12][2]);
    d_rpsumxy->setMPCsumY00(sumxy[0][12][1]/sumxy[0][12][2]);
  }
  if(sumxy[1][12][2]>0){
    d_rpsumxy->setMPCsumX10(sumxy[1][12][0]/sumxy[1][12][2]);
    d_rpsumxy->setMPCsumY10(sumxy[1][12][1]/sumxy[1][12][2]);
  }

  // MPC N
  if(sumxy[0][13][2]>0){
    d_rpsumxy->setMPCsumX01(sumxy[0][13][0]/sumxy[0][13][2]);
    d_rpsumxy->setMPCsumY01(sumxy[0][13][1]/sumxy[0][13][2]);
  }
  if(sumxy[1][13][2]>0){
    d_rpsumxy->setMPCsumX11(sumxy[1][13][0]/sumxy[1][13][2]);
    d_rpsumxy->setMPCsumY11(sumxy[1][13][1]/sumxy[1][13][2]);
  }
 

  // BBC S
  if(sumxy[0][16][2]>0){
    d_rpsumxy->setBBCsumX00(sumxy[0][16][0]/sumxy[0][16][2]);
    d_rpsumxy->setBBCsumY00(sumxy[0][16][1]/sumxy[0][16][2]);
  }
  if(sumxy[1][16][2]>0){
    d_rpsumxy->setBBCsumX10(sumxy[1][16][0]/sumxy[1][16][2]);
    d_rpsumxy->setBBCsumY10(sumxy[1][16][1]/sumxy[1][16][2]);
  }

  // BBC N
  if(sumxy[0][17][2]>0){
    d_rpsumxy->setBBCsumX01(sumxy[0][17][0]/sumxy[0][17][2]);
    d_rpsumxy->setBBCsumY01(sumxy[0][17][1]/sumxy[0][17][2]);
  }
  if(sumxy[1][17][2]>0){
    d_rpsumxy->setBBCsumX11(sumxy[1][17][0]/sumxy[1][17][2]);
    d_rpsumxy->setBBCsumY11(sumxy[1][17][1]/sumxy[1][17][2]);
  }


  // RXN inner S
  if(sumxy[0][0][2]>0){
    d_rpsumxy->setRXNsumX00(sumxy[0][0][0]/sumxy[0][0][2]);
    d_rpsumxy->setRXNsumY00(sumxy[0][0][1]/sumxy[0][0][2]);
  }
  if(sumxy[1][0][2]>0){
    cout <<"RXNP inner s"<<endl;
    d_rpsumxy->setRXNsumX10(sumxy[1][0][0]/sumxy[1][0][2]);
    d_rpsumxy->setRXNsumY10(sumxy[1][0][1]/sumxy[1][0][2]);
  }

  // RXN outer S
  if(sumxy[0][1][2]>0){
    d_rpsumxy->setRXNsumX01(sumxy[0][1][0]/sumxy[0][1][2]);
    d_rpsumxy->setRXNsumY01(sumxy[0][1][1]/sumxy[0][1][2]);
  }
  if(sumxy[1][1][2]>0){
    d_rpsumxy->setRXNsumX11(sumxy[1][1][0]/sumxy[1][1][2]);
    d_rpsumxy->setRXNsumY11(sumxy[1][1][1]/sumxy[1][1][2]);
  }

  // RXN in+out S
  if(sumxy[0][0][2]+sumxy[0][1][2]>0){
    d_rpsumxy->setRXNsumX02((sumxy[0][0][0]+sumxy[0][1][0])/(sumxy[0][0][2]+sumxy[0][1][2]));
    d_rpsumxy->setRXNsumY02((sumxy[0][0][1]+sumxy[0][1][1])/(sumxy[0][0][2]+sumxy[0][1][2]));
  }
  if(sumxy[1][0][2]+sumxy[1][1][2]>0){
    d_rpsumxy->setRXNsumX12((sumxy[1][0][0]+sumxy[1][1][0])/(sumxy[1][0][2]+sumxy[1][1][2]));
    d_rpsumxy->setRXNsumY12((sumxy[1][0][1]+sumxy[1][1][1])/(sumxy[1][0][2]+sumxy[1][1][2]));
  }

 
  // RXN inner N
  if(sumxy[0][3][2]>0){
    d_rpsumxy->setRXNsumX03(sumxy[0][3][0]/sumxy[0][2][2]);
    d_rpsumxy->setRXNsumY03(sumxy[0][3][1]/sumxy[0][3][2]);
  }
  if(sumxy[1][3][2]>0){
    d_rpsumxy->setRXNsumX13(sumxy[1][3][0]/sumxy[1][3][2]);
    d_rpsumxy->setRXNsumY13(sumxy[1][3][1]/sumxy[1][3][2]);
  }

  // RXN outer N
  if(sumxy[0][4][2]>0){
    d_rpsumxy->setRXNsumX04(sumxy[0][4][0]/sumxy[0][4][2]);
    d_rpsumxy->setRXNsumY04(sumxy[0][4][1]/sumxy[0][4][2]);
  }
  if(sumxy[1][4][2]>0){
    d_rpsumxy->setRXNsumX14(sumxy[1][4][0]/sumxy[1][4][2]);
    d_rpsumxy->setRXNsumY14(sumxy[1][4][1]/sumxy[1][4][2]);
  }

  // RXN in+out N
  if(sumxy[0][3][2]+sumxy[0][4][2]>0){
    d_rpsumxy->setRXNsumX05((sumxy[0][3][0]+sumxy[0][4][0])/(sumxy[0][3][2]+sumxy[0][4][2]));
    d_rpsumxy->setRXNsumY05((sumxy[0][3][1]+sumxy[0][4][1])/(sumxy[0][3][2]+sumxy[0][4][2]));
  }
  if(sumxy[1][3][2]+sumxy[1][4][2]>0){
    d_rpsumxy->setRXNsumX15((sumxy[1][3][0]+sumxy[1][4][0])/(sumxy[1][3][2]+sumxy[1][4][2]));
    d_rpsumxy->setRXNsumY15((sumxy[1][3][1]+sumxy[1][4][1])/(sumxy[1][3][2]+sumxy[1][4][2]));
  }
    
 
  // SMD S (Psi1 only)
  if(Qsmd[0][0][2]>0){
    d_rpsumxy->setSMDsumX00(Qsmd[0][0][0]/Qsmd[0][0][2]);
    d_rpsumxy->setSMDsumY00(Qsmd[0][0][1]/Qsmd[0][0][2]);
  }

  // CNT (Psi2 only, no zed classification)
  if(sumxy[1][20][2]>0){
    d_rpsumxy->setCNTsumX14(sumxy[1][20][0]/sumxy[1][20][2]);
    d_rpsumxy->setCNTsumY14(sumxy[1][20][1]/sumxy[1][20][2]);
  }


  float Qx[NDET5][NHAR5];
  float Qy[NDET5][NHAR5];
  float Qx_cor[NDET5][NHAR5];
  float Qy_cor[NDET5][NHAR5];
  float psi[NDET5][NHAR5];

  for(int id=0;id<NDET5;id++){
    for(int ih=0;ih<NHAR5;ih++){
      Qx[id][ih]  = -9999.;
      Qy[id][ih]  = -9999.;
      Qx_cor[id][ih]  = -9999.;
      Qy_cor[id][ih]  = -9999.;
      psi[id][ih] = -9999;
    }
  }

  // We have to look at the following class: 
  // offline/framework/preco/RpSumXYReco.C

  //
  // Q-vector reading
  //

  // MPC S
  Qx[0][0]  = d_rpsumxy->getMPCsumX00();
  Qy[0][0]  = d_rpsumxy->getMPCsumY00();
  Qx[0][1]  = d_rpsumxy->getMPCsumX10();
  Qy[0][1]  = d_rpsumxy->getMPCsumY10();

  // MPC N
  Qx[1][0]  = d_rpsumxy->getMPCsumX01();
  Qy[1][0]  = d_rpsumxy->getMPCsumY01();
  Qx[1][1]  = d_rpsumxy->getMPCsumX11();
  Qy[1][1]  = d_rpsumxy->getMPCsumY11();

  // BBC S
  Qx[2][0]  = d_rpsumxy->getBBCsumX00();
  Qy[2][0]  = d_rpsumxy->getBBCsumY00();
  Qx[2][1]  = d_rpsumxy->getBBCsumX10();
  Qy[2][1]  = d_rpsumxy->getBBCsumY10();

  // BBC N
  Qx[3][0]  = d_rpsumxy->getBBCsumX01();
  Qy[3][0]  = d_rpsumxy->getBBCsumY01();
  Qx[3][1]  = d_rpsumxy->getBBCsumX11();
  Qy[3][1]  = d_rpsumxy->getBBCsumY11();

  // RXN inner S
  Qx[4][0] = d_rpsumxy->getRXNsumX00();
  Qy[4][0] = d_rpsumxy->getRXNsumY00();
  Qx[4][1] = d_rpsumxy->getRXNsumX10();
  Qy[4][1] = d_rpsumxy->getRXNsumY10();

  // RXN outer S
  Qx[5][0] = d_rpsumxy->getRXNsumX01();
  Qy[5][0] = d_rpsumxy->getRXNsumY01();
  Qx[5][1] = d_rpsumxy->getRXNsumX11();
  Qy[5][1] = d_rpsumxy->getRXNsumY11();

  // RXN in+out S
  Qx[6][0] = d_rpsumxy->getRXNsumX02();
  Qy[6][0] = d_rpsumxy->getRXNsumY02();
  Qx[6][1] = d_rpsumxy->getRXNsumX12();
  Qy[6][1] = d_rpsumxy->getRXNsumY12();

  // RXN inner N
  Qx[7][0] = d_rpsumxy->getRXNsumX03();
  Qy[7][0] = d_rpsumxy->getRXNsumY03();
  Qx[7][1] = d_rpsumxy->getRXNsumX13();
  Qy[7][1] = d_rpsumxy->getRXNsumY13();

  // RXN outer N
  Qx[8][0] = d_rpsumxy->getRXNsumX04();
  Qy[8][0] = d_rpsumxy->getRXNsumY04();
  Qx[8][1] = d_rpsumxy->getRXNsumX14();
  Qy[8][1] = d_rpsumxy->getRXNsumY14();

  // RXN in+out N
  Qx[9][0] = d_rpsumxy->getRXNsumX05();
  Qy[9][0] = d_rpsumxy->getRXNsumY05();
  Qx[9][1] = d_rpsumxy->getRXNsumX15();
  Qy[9][1] = d_rpsumxy->getRXNsumY15();

  // SMD S (Psi1 only)
  Qx[10][0]  = d_rpsumxy->getSMDsumX00();
  Qy[10][0]  = d_rpsumxy->getSMDsumY00();

  // CNT (Psi2 only, no zed classification)
  Qx[11][1] = d_rpsumxy->getCNTsumX14();
  Qy[11][1] = d_rpsumxy->getCNTsumY14();


  //
  // First, centering correction
  //
  for(int id=0;id<NDET5;id++){
    for(int ih=0;ih<NHAR5;ih++){
      if( Qx[id][ih]>-9999 && Qy[id][ih]>-9999 ){
        float xmean  = rpcalib->GetSumXmean(id, ih, imul, izps);
        float xsigma = rpcalib->GetSumXsigma(id, ih, imul, izps);
        float ymean  = rpcalib->GetSumYmean(id, ih, imul, izps);
        float ysigma = rpcalib->GetSumYsigma(id, ih, imul, izps);

        if(xsigma==0 || ysigma==0){
          if(verbosity>1){
            cout << PHWHERE << " sigma == 0. (idet,ihar)=(" << id << "," << ih << ")" << endl;
          }

          Qx_cor[id][ih] = -9999.;
          Qy_cor[id][ih] = -9999.;

          continue;
        }

        Qx_cor[id][ih] = ( Qx[id][ih] - xmean ) / xsigma;
        Qy_cor[id][ih] = ( Qy[id][ih] - ymean ) / ysigma;
      }
      else{
        Qx_cor[id][ih] = -9999.;
        Qy_cor[id][ih] = -9999.;
      }
    }
  }

  //
  // Next, flattening correction and make event plane
  //
  for (int idet = 0;idet < NDET5;idet++) {
    for (int ihar = 0;ihar < NHAR5;ihar++) {
      if( Qx_cor[idet][ihar]>-9999 && Qy_cor[idet][ihar]>-9999){
        float psiObs    = atan2(Qy_cor[idet][ihar], Qx_cor[idet][ihar]) / (ihar+1.0);
	psi[idet][ihar] = rpcalib->Flattening(idet, ihar, imul, izps, psiObs);
      }
    }
  }


  //
  // Set reaction plane in ReactionPlaneObject
  //

  // MPC S
  d_rp->setMPCrp00( psi[0][0] );
  d_rp->setMPCrp10( psi[0][1] );
  h_MPCrp00->Fill( psi[0][0] );
  h_MPCrp10->Fill( psi[0][1] );

  // MPC N
  d_rp->setMPCrp01( psi[1][0] );
  d_rp->setMPCrp11( psi[1][1] );
  h_MPCrp01->Fill( psi[1][0] );
  h_MPCrp11->Fill( psi[1][1] );

  // BBC S
  d_rp->setBBCrp00( psi[2][0] );
  d_rp->setBBCrp10( psi[2][1] );
  h_BBCrp00->Fill( psi[2][0] );
  h_BBCrp10->Fill( psi[2][1] );
 
  // BBC N
  d_rp->setBBCrp01( psi[3][0] );
  d_rp->setBBCrp11( psi[3][1] );
  h_BBCrp01->Fill( psi[3][0] );
  h_BBCrp11->Fill( psi[3][1] );

  // RxN inner S
  d_rp->setRXNrp00( psi[4][0] );
  d_rp->setRXNrp10( psi[4][1] );
  h_RXNrp00->Fill( psi[4][0] );
  h_RXNrp10->Fill( psi[4][1] );

  // RxN outer S
  d_rp->setRXNrp01( psi[5][0] );
  d_rp->setRXNrp11( psi[5][1] );
  h_RXNrp01->Fill( psi[5][0] );
  h_RXNrp11->Fill( psi[5][1] );

  // RxN in+out S
  d_rp->setRXNrp02( psi[6][0] );
  d_rp->setRXNrp12( psi[6][1] );
  h_RXNrp02->Fill( psi[6][0] );
  h_RXNrp12->Fill( psi[6][1] );

  // RxN inner N
  d_rp->setRXNrp03( psi[7][0] );
  d_rp->setRXNrp13( psi[7][1] );
  h_RXNrp03->Fill( psi[7][0] );
  h_RXNrp13->Fill( psi[7][1] );

  // RxN outer N
  d_rp->setRXNrp04( psi[8][0] );
  d_rp->setRXNrp14( psi[8][1] );
  h_RXNrp04->Fill( psi[8][0] );
  h_RXNrp14->Fill( psi[8][1] );

  // RxN in+out N
  d_rp->setRXNrp05( psi[9][0] );
  d_rp->setRXNrp15( psi[9][1] );
  h_RXNrp05->Fill( psi[9][0] );
  h_RXNrp15->Fill( psi[9][1] );
 
  // SMD S
  d_rp->setSMDrp00( psi[10][0] );
  h_SMDrp00->Fill( psi[10][0] );

  // CNT (Psi2 onlt, no zed classification)
  d_rp->setCNTrp14( psi[11][1] );
  h_CNTrp14->Fill( psi[11][1] );

  return EVENT_OK;
}

bool Run8ReactionPlaneReco::CreateNodeTree(PHCompositeNode* topNode, int runno)
{

  PHNodeIterator iter(topNode);
  PHCompositeNode* dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  RpSumXYObjectNode_t *iRPSUMXY =  static_cast<RpSumXYObjectNode_t*>(iter.findFirst("PHIODataNode","RpSumXYObject"));

  // make the data objects
  if( !iRPSUMXY ){
    d_rpsumxy = new RpSumXYObjectv2();
    iRPSUMXY  = new PHIODataNode<RpSumXYObject>(d_rpsumxy,"RpSumXYObject","PHObject");

    dstNode->addNode(iRPSUMXY);
  }else{
    d_rpsumxy = iRPSUMXY->getData();
    cout << "RpSumXYObjectv2 has been given" << endl;
  }
 
  ReactionPlaneObjectNode_t* rp = static_cast<ReactionPlaneObjectNode_t*>(iter.findFirst("PHIODataNode","ReactionPlaneObject"));

  // Make the data object for Run-8
  if(!rp)
  {
    d_rp = new ReactionPlaneObjectv3();
    rp = new PHIODataNode<ReactionPlaneObject>(d_rp, "ReactionPlaneObject", "PHObject");
    dstNode->addNode(rp);
    cout << " ... ReacionPlaneObject version 3 instead. " << endl;
  }
  else{d_rp = rp->getData();}

  return True;
}

void Run8ReactionPlaneReco::Verbosity(const int v)
{
  verbosity = v;
  rpcalib->Verbosity(v);
}

bool Run8ReactionPlaneReco::BadTower(int ich){
  //south side
  if(ich==95||ich==192||ich==193||ich==262) return true;
  //north side
  else if(ich==392||ich==396||ich==398||ich==402||ich==403
	  ||ich==420||ich==421||ich==422||ich==423||ich==426
	  ||ich==427||ich==444||ich==445||ich==446||ich==447
	  ||ich==448||ich==450||ich==451||ich==465||ich==457
	  ||ich==462||ich==469||ich==471||ich==480||ich==481
	  ||ich==486||ich==487||ich==504||ich==505||ich==510
	  ||ich==511||ich==528||ich==529||ich==535) return true;
  else return false;
}

