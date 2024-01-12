// $Id: NewSngmuonsRun9.C,v 1.4 2014/12/09 14:05:16 rseidl Exp $

/*!
   \file SngmuonsRun9.C
   \brief single muon ntuple booking and filling
   \version $Revision: 1.4 $
   \date $Date: 2014/12/09 14:05:16 $
*/

#include <iostream>
#include <boost/array.hpp>
#include <EventHeader.h>
#include <PHGlobal.h>
#include <ReactionPlaneObject.h> // event information
#include <RpSumXYObject.h> // event information

#include <MWGVertex.h>
#include <PHMuoTracksOut.h>
#include <RunHeader.h>
#include <SpinDataEventOut.h>
#include <stdexcept>
#include <string>
#include <TChain.h>
#include <TH1.h>
#include<TTree.h>
#include<TBranch.h>
#include<TLeaf.h>
#include<TMath.h>
#include<TFile.h>


#include <TMutTrkMap.h>
#include <TMutMCTrkMap.h>
#include <TMutMCHitMap.h>
#include <TMutVtxMap.h>
#include <TMCPrimaryMap.h>

#include<TMuiMCHitMapO.h>
#include<TMuiHitMapO.h>
#include<TMuiRoadMapO.h>
#include<TMuiClusterMapO.h>
//#include<TMuiClusterO.h>
//#include<TMuiClusterO_v2.h>
#include<TMutTrk_v4.hh>
#include<TMutTrackUtil.h>
#include<TMutTrkRes.hh>
#include<TMutHitMap.h>

#include<TDataType.h>
#include<TMutCoordMap.h>
#include<TMutStubMap.h>
#include<TMutGapCoordMap.h>
#include<TMutGeo.h>
#include<PHGeometry.h>
#include<algorithm>




#include <TRpcTrk.h>
#include <TRpcTrk_v2.h>
#include <TRpcTrkMap.h>
#include <TRpcHit.h>
#include <TRpcMCHit.h>
#include <TRpcClusMap.h>
#include <TRpcCoordMap.h>
#include <TRpcCoord_v1.h>
#include <TRpcHitMap.h>
#include <TRpcMCHitMap.h>
#include <RpcStrip.h>


#include <root_ptrk.h>
#include <TNtuple.h>
#include <TRandom.h>
#include <TriggerHelper.h>
#include <utiCentrality.h>
#include <vector>


#include <MUTOO.h>
#include "../MWGpico.h"
#include <Tools.h>
#include <MWGConsts.h>
#include <MWGVersion.h>

#include "TrigLvl1v1.h"

#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "MuonUtil.h"

using namespace std;
using namespace PhUtilities;


TTree *fMuonTree;


//Int_t fNTracks;

Int_t  fNRpcHits;
    Int_t fNMU;

  Int_t *fRunNumber;
  Int_t *EventNumber;

//__________________________________________________
void MWGpico::BookNewSngmuonsNtupleRun9(TTree*& newsngmuons, TString m_name, TString m_title)
{

  //    cout << " in booknewsngmuons" << endl;
  //  fMuonTree = new TTree("SINGLEMUONS","SINGLEMUONS"); // maybe not needed to creat a new tree here anymore...
  //fMuonTree->Branch("Info",0,"");

 newsngmuons = new TTree(m_name,m_title); // maybe not needed to creat a new tree here anymore...
//  newsngmuons->Branch("Info",0,"");
  
  
  //  fMuonTree->Branch("_RecoTracks",0,"_RecoTracks/I");
  newsngmuons->Branch("_RecoTracks",0,"_RecoTracks/I");


  //  fMuonTree->Branch("RecoTracks",0,"Evt_Nmu[_RecoTracks]/I:px[_RecoTracks]/F");
  //  fMuonTree->Branch("Eventdata",0,"Run_Number/I:Evt_Number/I:Evt_Z/F:Evt_bbcZ/F:Evt_bbcCentrality/F:Evt_zdcEnerN/F:Evt_realtrigS_1D/I:Evt_realtrigS_1S/I:Evt_realtrigN_1D/I:Evt_realtrigN_1S/I:Evt_livetrigS_1D/I:"
  newsngmuons->Branch("Eventdata",0,"Run_Number/I:Evt_Number/I:Evt_Z/F:Evt_bbcZ/F:Evt_bbcCentrality/F:Evt_zdcEnerN/F:Evt_realtrigS_1D/I:Evt_realtrigS_1S/I:Evt_realtrigN_1D/I:Evt_realtrigN_1S/I:Evt_livetrigS_1D/I:"
		    "Evt_livetrigS_1S/I:Evt_livetrigN_1D/I:Evt_livetrigN_1S/I:triggerbit/I:triggerlive/I:clockcross/I:Clock_trig/I:Evt_realtrig_MB/I:SpinX_ID/F:Pol_Y/F:Pol_B/F:GL1X_ID/F:"
		    "RPbbcrp10/F:RPbbcrp11/F:RPbbcrp12/F:delphi_RP/F:"
		    "RPbbcrp01/F:RPbbcrp02/F:RPsmdrp00/F:RPsmdrp01/F:RPsmdrp02/F");

  //    cout << " booked new eventdata" << endl; 

  newsngmuons->Branch("RecoTracks",0,"Evt_Nmu[_RecoTracks]/I:charge[_RecoTracks]/F:px[_RecoTracks]/F:py[_RecoTracks]/F:"
		    "pz[_RecoTracks]/F:pxSt1[_RecoTracks]/F:pySt1[_RecoTracks]/F:pzSt1[_RecoTracks]/F:pxSt3[_RecoTracks]/F:pySt3[_RecoTracks]/F:pzSt3[_RecoTracks]/F:pT[_RecoTracks]/F:p[_RecoTracks]/F:pSt1[_RecoTracks]/F:"
		    "ELoss[_RecoTracks]/F:chi2[_RecoTracks]/F:idhits[_RecoTracks]/I:idquad[_RecoTracks]/I:trhits[_RecoTracks]/I:DS3[_RecoTracks]/F:DS3ctp[_RecoTracks]/F:idchi2[_RecoTracks]/F:gap0x[_RecoTracks]/F:gap0y[_RecoTracks]/F:"
		    "gap0z[_RecoTracks]/F:dxdz[_RecoTracks]/F:dydz[_RecoTracks]/F:trstat[_RecoTracks]/I:ghost[_RecoTracks]/F:lastGap[_RecoTracks]/I:eta[_RecoTracks]/F:phi[_RecoTracks]/F:DG0[_RecoTracks]/F:DDG0[_RecoTracks]/F:"
		    "DS0[_RecoTracks]/F:refX[_RecoTracks]/F:refY[_RecoTracks]/F:mutr_nhits[_RecoTracks]/F:muid_nhits[_RecoTracks]/F:Evt_pseudotrigS_1D[_RecoTracks]/I:Evt_pseudotrigS_1S[_RecoTracks]/I:Evt_pseudotrigN_1D[_RecoTracks]/I:Evt_pseudotrigN_1S[_RecoTracks]/I:Evt_recoS_1D[_RecoTracks]/I:Evt_recoS_1S[_RecoTracks]/I:Evt_recoN_1D[_RecoTracks]/I:"
		    "Evt_recoN_1S[_RecoTracks]/F:xSt1[_RecoTracks]/F:ySt1[_RecoTracks]/F:zSt1[_RecoTracks]/F:xSt2[_RecoTracks]/F:ySt2[_RecoTracks]/F:zSt2[_RecoTracks]/F:xSt3[_RecoTracks]/F:ySt3[_RecoTracks]/F:zSt3[_RecoTracks]/F:"
		    "ref_vtx_rdca[_RecoTracks]/F:ref_vtx_r[_RecoTracks]/F:ref_vtx_z[_RecoTracks]/F:refit_zvtx[_RecoTracks]/F:"
		    "dAngle[_RecoTracks]/F:dAngle_xyz[_RecoTracks]/F:dTheta[_RecoTracks]/F:dPhi[_RecoTracks]/F:road_slope[_RecoTracks]/F:mc_n_part[_RecoTracks]/F:mc_px[_RecoTracks]/F:mc_py[_RecoTracks]/F:mc_pz[_RecoTracks]/F:mc_ptot[_RecoTracks]/F:"
		    "mc_z[_RecoTracks]/F:mc_pid[_RecoTracks]/F:mc_hits[_RecoTracks]/F:mc_p_pid[_RecoTracks]/F:mc_p_px[_RecoTracks]/F:mc_p_py[_RecoTracks]/F:mc_p_pz[_RecoTracks]/F:mc_p_ptot[_RecoTracks]/F:mc_p_z[_RecoTracks]/F:mc_d_pid[_RecoTracks]/F:"
		    "mc_d_px[_RecoTracks]/F:mc_d_py[_RecoTracks]/F:mc_d_pz[_RecoTracks]/F:mc_d_ptot[_RecoTracks]/F:mc_d_z[_RecoTracks]/F:mc_d_n[_RecoTracks]/F:mc_x[_RecoTracks]/F:mc_y[_RecoTracks]/F:mc_g_pid[_RecoTracks]/F:mc_g_px[_RecoTracks]/F:"
		    "mc_g_py[_RecoTracks]/F:mc_g_pz[_RecoTracks]/F:mc_g_ptot[_RecoTracks]/F:mc_g_z[_RecoTracks]/F:mc_trk[_RecoTracks]/I:RPC2_x[_RecoTracks]/F:RPC2_y[_RecoTracks]/F:RPC3_x[_RecoTracks]/F:RPC3_y[_RecoTracks]/F:RPC3r_x[_RecoTracks]/F:" 
		      "RPC3r_y[_RecoTracks]/F:RPC2r_x[_RecoTracks]/F:RPC2r_y[_RecoTracks]/F:"
		      "DG4[_RecoTracks]/F:DCA_r[_RecoTracks]/F:DCA_z[_RecoTracks]/F");
  /*		    */


  newsngmuons->Branch("Residuals",0,"Res100[_RecoTracks]/F:Res101[_RecoTracks]/F:Res110[_RecoTracks]/F:Res111[_RecoTracks]/F:"
		      "Res120[_RecoTracks]/F:Res121[_RecoTracks]/F:Res200[_RecoTracks]/F:Res201[_RecoTracks]/F:Res210[_RecoTracks]/F:Res211[_RecoTracks]/F:Res220[_RecoTracks]/F:Res221[_RecoTracks]/F:Res300[_RecoTracks]/F:Res301[_RecoTracks]/F:"
		      "Res310[_RecoTracks]/F:Res311[_RecoTracks]/F:station1hits[_RecoTracks]/I:station2hits[_RecoTracks]/I:station3hits[_RecoTracks]/I:allstation1hits[_RecoTracks]/I:allstation2hits[_RecoTracks]/I:allstation3hits[_RecoTracks]/I:allhits[_RecoTracks]/I:"
		      "invmass1[_RecoTracks]/F:invmass2[_RecoTracks]/F:invmass3[_RecoTracks]/F:icharge1[_RecoTracks]/I:icharge2[_RecoTracks]/I:icharge3[_RecoTracks]/I:"
		      "thrust1[_RecoTracks]/F:thrust2[_RecoTracks]/F:thrust3[_RecoTracks]/F:"
  "RpcDCA[_RecoTracks]/F:RpcpT[_RecoTracks]/F:Rpctime[_RecoTracks]/F");

  //    cout << " booked tracks" << endl; 
 
  newsngmuons->Branch("_RpcHits",0,"_RpcHits/I");
  //  fMuonTree->Branch("RpcHits",0,"_begin[_RpcHits]/F:y_begin[_RpcHits]/F:z_begin[_RpcHits]/F:x_end[_RpcHits]/F:y_end[_RpcHits]/F:z_end[_RpcHits]/F:q[_RpcHits]/F:eq[_RpcHits]/F:t[_RpcHits]/F:et[_RpcHits]/F");


  newsngmuons->Branch("RpcHits",0,"rpc_t[_RpcHits]/F:rpc_c[_RpcHits]/F:arm[_RpcHits]/I:station[_RpcHits]/I:octant[_RpcHits]/I:halfoct[_RpcHits]/I:radsegment[_RpcHits]/I:strip[_RpcHits]/I");


  //    cout << " finished booking NewSngmuonsRun9" << endl;

  return;
}

//__________________________________________________________________
void MWGpico::BookNewSngmuonsEvtNtupleRun9(TNtuple*& newsngvtx, TString v_name, TString v_title )
{
  // define variable list for vertex ntuple
const  char* vtxvarlist=0;

  vtxvarlist= "Run_Number:SpinX_ID:Pol_Y:Pol_B:GL1X_ID:Evt_Z:bbcZ:zdcCentrality:bbcCentrality:BBCQN:"
    "BBCQS:BBCNN:BBCNS:ZDCN:ZDCS:Clock_trig:Evt_realtrig_MB:Evt_livetrigS_1S:Evt_livetrigS_1D:Evt_livetrigN_1S:Evt_livetrigN_1D:"
    "RPbbcrp10:RPbbcrp11:RPbbcrp12:delphi_RP:EPT1:"
    "RPbbcrp01:RPbbcrp02:RPsmdrp00:RPsmdrp01:RPsmdrp02"; // BBC SMD rp

  newsngvtx = new TNtuple( v_name, v_title, vtxvarlist );
  newsngvtx->SetAutoSave(160000000);

  return;
}

 
//__________________________________________________
int MWGpico::FillNewSngmuonsNtpRun9(PHMuoTracksOut* &muo, TTree* newsngmuons,  TNtuple* newsngvtx)
{

  //    cout <<  "in FillNewSngmuonsNtpRun9" << endl;
  //=== event information
  Float_t *bbcCentrality;
  Float_t *zdcCentrality;
  Float_t *ZdcEnergyN;
  Float_t *ZdcEnergyS;

  bbcCentrality= new Float_t[1];
  zdcCentrality= new Float_t[1];
  ZdcEnergyN= new Float_t[1];
  ZdcEnergyS= new Float_t[1];
  
  Float_t *delphi_RP;
  Float_t *RPbbcrp10;
  Float_t *RPbbcrp11;
  Float_t *RPbbcrp12;
  Float_t *RPbbcrp00;
  Float_t *RPbbcrp01;
  Float_t *RPbbcrp02;
  Float_t *RPsmdrp00;
  Float_t *RPsmdrp01;
  Float_t *RPsmdrp02;

  delphi_RP  = new Float_t[1];
  RPbbcrp10 = new Float_t[1];
  RPbbcrp11= new Float_t[1];
  RPbbcrp12= new Float_t[1];
  RPbbcrp00= new Float_t[1];
  RPbbcrp01= new Float_t[1];
  RPbbcrp02= new Float_t[1];
  RPsmdrp00= new Float_t[1];
  RPsmdrp01= new Float_t[1];
  RPsmdrp02= new Float_t[1];
  

  delphi_RP[0] = -9999;
  RPbbcrp10[0] = -9999;
  RPbbcrp11[0] = -9999;
  RPbbcrp12[0] = -9999;
  RPbbcrp00[0] = -9999;
  RPbbcrp01[0] = -9999;
  RPbbcrp02[0] = -9999;
  RPsmdrp00[0] = -9999;
  RPsmdrp01[0] = -9999;
  RPsmdrp02[0] = -9999;
  


  // z_vertex
  Float_t *BbcZVertex;
  Float_t *Evt_Z;

  BbcZVertex = new Float_t[1];
  Evt_Z = new Float_t[1];
   
  bbcCentrality[0]=-9999;
  zdcCentrality[0]=-9999;
  ZdcEnergyN[0]=-9999;
  ZdcEnergyS[0]=-9999;

  BbcZVertex[0] =-9999;
  Evt_Z[0]= -9999;




  if (evt) {
    BbcZVertex[0] = evt->getBbcZVertex();
    bbcCentrality[0] = (Float_t) evt->getCentrality();
    ZdcEnergyN[0]=evt->getZdcEnergyN();
    ZdcEnergyS[0]=evt->getZdcEnergyS();

  }
  
  // event/run number

  fRunNumber = new Int_t[1];
  fRunNumber[0] = ( run_header ) ?  run_header->get_RunNumber():0;


  EventNumber = new Int_t[1];
  EventNumber[0] = (event_header ) ? event_header->get_EvtSequence():0;
  
  // try load MC if failed from PHGlobal
  {
    bool error( false );
    if( !fRunNumber[0] ) fRunNumber[0] = Tools::runNumberMC( header, error );
    if( !EventNumber[0] ) EventNumber[0]  = Tools::eventNumberMC( header, error );
  }
  
  
  Int_t *realtrigS_1D; // real trigger South
  Int_t *realtrigN_1D; // real trigger North
  Int_t *realtrigS_1S; // real trigger South
  Int_t *realtrigN_1S; // real trigger North
  
  Int_t *livetrigS_1D; // live trigger South
  Int_t *livetrigN_1D; // live trigger North
  Int_t *livetrigS_1S; // live trigger South
  Int_t *livetrigN_1S; // live trigger North
  
  Int_t *realtrig_MB;
  Int_t *Clock_trig ;
  Int_t *Clock_live ;

  realtrigS_1D = new Int_t[1];
realtrigN_1D = new Int_t[1];
realtrigS_1S = new Int_t[1];
realtrigN_1S = new Int_t[1];
			     
livetrigS_1D = new Int_t[1];
livetrigN_1D = new Int_t[1];
livetrigS_1S = new Int_t[1];
livetrigN_1S = new Int_t[1];
			     
realtrig_MB = new Int_t[1] ;
Clock_trig = new Int_t[1]  ;
Clock_live = new Int_t[1]  ;
  

 Int_t *triggerbit;
 triggerbit = new Int_t[1];
triggerbit[0] = 0; 
 Int_t *triggerlive;
 triggerlive = new Int_t[1];
triggerlive[0] = 0; 
 Int_t *clockcross;
 clockcross = new Int_t[1];
clockcross[0] = 0; 


  realtrigS_1D[0]     =  0;
  realtrigN_1D[0]     =  0;
  realtrigS_1S[0]     =  0;
  realtrigN_1S[0]     =  0;
  
  livetrigS_1D[0]     =  0;
  livetrigN_1D[0]     =  0;
  livetrigS_1S[0]     =  0;
  livetrigN_1S[0]     =  0;
  
  realtrig_MB[0]      =  0;
  Clock_trig[0]       =  0;
  Clock_live[0]       =  0;
 

  
  // real triggers: 200GeV and 62 GeV runs
  // cout << "before triggers ini NEwSngmuonsRun9" << endl;  
  if(_trig_lvl1){ // Trigger Node exists in data file

    // for run8dAu, 246212 <= runnumber <= 247392, S1S and N1S
    // for run8dAu, 247397 <= runnumber <= 235710, S1H and N1H
    // use triggeR BITS: 10000(N1D), 20000(S1D), 40000(N1S, N1H), 80000(S1H, S1S)

    //    cout << " checking triggers scaled: " <<  _trig_lvl1->get_lvl1_trigscaled() << "  live " << _trig_lvl1->get_lvl1_triglive() << " " <<  _trig_lvl1->get_lvl1_clock_cross() 
    //<<endl;
    triggerbit[0] = _trig_lvl1->get_lvl1_trigscaled();
    triggerlive[0] = _trig_lvl1->get_lvl1_triglive();
    clockcross[0] = _trig_lvl1->get_lvl1_clock_cross(); 

    //    TrigHelp->dump_info();
    //real triggers
    if (TrigHelp->didLevel1TriggerGetScaled("MUIDLL1_S1D&BBCLL1")) {
      realtrigS_1D[0]=1;
    }
    if (TrigHelp->didLevel1TriggerGetScaled("(MUIDLL1_N1D||S1D)&BBCLL1(noVtx)")) {
      realtrigN_1D[0]=1;
    }
    if (TrigHelp->didLevel1TriggerGetScaled("(MUIDLL1_S1H)&BBCLL1(noVtx)")) {
      realtrigS_1S[0]=1;
    }
    if (TrigHelp->didLevel1TriggerGetScaled("(MUIDLL1_N1H)&BBCLL1(noVtx)")) {
      realtrigN_1S[0]=1;
    }
    
    // live triggers
    if (TrigHelp->didLevel1TriggerFire("MUIDLL1_S1D&BBCLL1(noVtx)")) {
      livetrigS_1D[0]=1;
      //  cout << "S1DDDD = livetrigS_1D" << endl; 
    }
    if (TrigHelp->didLevel1TriggerFire("(MUIDLL1_N1D||S1D)&BBCLL1(noVtx)")) {
      livetrigN_1D[0]=1;
      //cout << "N1SDDDD = livetrigN_1D" << endl; 
    }
    if (TrigHelp->didLevel1TriggerFire("(MUIDLL1_S1H)&BBCLL1(noVtx)")) {
      //cout << "SH = livetrigS_1H" << endl; 
      livetrigS_1S[0]=1;
    }
    if (TrigHelp->didLevel1TriggerFire("(MUIDLL1_N1H)&BBCLL1(noVtx)")) {
      livetrigN_1S[0]=1;
      //cout << "N1H = livetrigN_1H" << endl; 
    }
    

    //
    // --- use trigger bit for trigger selection to cover run6 200GeV and 62 GeV runs for MUIDLL1_trigger with and w/o BBCLL1
    // for low energy runs 
    // 08/18/2006  MXL
    //
    /*
    //scaled triggers
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00010000){
      realtrigN_1D[0]=1;
    }
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00020000){
      realtrigS_1D[0]=1;
    }
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00040000){
      realtrigN_1S[0]=1;
    }
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00080000){
      realtrigS_1S[0]=1;
    }
    
    //live triggers
    if (_trig_lvl1->get_lvl1_triglive()&0x00010000){
      livetrigN_1D[0]=1;
    }
    if (_trig_lvl1->get_lvl1_triglive()&0x00020000){
      livetrigS_1D[0]=1;
    }
    if (_trig_lvl1->get_lvl1_triglive()&0x00040000){
      livetrigN_1S[0]=1;
    }
    if (_trig_lvl1->get_lvl1_triglive()&0x00080000){
      livetrigS_1S[0]=1;
    }
    */


  
  if (TrigHelp->IsEventMinBias()) {
    realtrig_MB[0]=1;
  }
  
  //------------//////////////////////////////
  
  //Clock triggers
  if (TrigHelp->didLevel1TriggerGetScaled("Clock")) {
    Clock_trig[0]=1;
  }
  if (TrigHelp->didLevel1TriggerFire("Clock")) {
    Clock_live[0]=2;
  }
  } // end of muon trigger selection  
  /////  end of trigger selection ------

  //spin information
  Float_t *SpinX_ID; // beam crossing ID from spin DB
  Float_t *Pol_Y   ; // yellow beam polarization
  Float_t *Pol_B   ; // blue beam polarization
  Float_t *GL1X_ID ; // beam crossing ID from GL1
  
  SpinX_ID=new Float_t[1];
  Pol_Y=new Float_t[1]   ;
  Pol_B=new Float_t[1]   ;
  GL1X_ID=new Float_t[1] ;
  
  SpinX_ID[0] =  -999;
  Pol_Y[0]    =  -999;
  Pol_B[0]    =  -999;
  GL1X_ID[0]  =  -999;
  
  if (spin){
    //cout << " spin information found" << endl; 
    SpinX_ID[0]= spin->GetSpinGL1CrossingID();
    Pol_Y[0]   = spin->GetSpinDirectionYellowFromV124();
    Pol_B[0]   = spin->GetSpinDirectionBlueFromV124();
    GL1X_ID[0] = spin->GetGL1CrossingID();
    
  } // if (spin)
  //  else
    //cout << " no spin information found!" << endl; 
  
  float  *BBCQN;
  float  *BBCQS;
  float  *BBCNN;
  float  *BBCNS;  
  


  BBCQN =new float[1];
  BBCQS	=new float[1];
  BBCNN	=new float[1];
  BBCNS	=new float[1];

  if ( evt){
    BBCQN[0] = evt->getBbcChargeN();
    BBCQS[0] = evt->getBbcChargeS();
    BBCNN[0] = evt->getBbcMultN();  
    BBCNS[0] = evt->getBbcMultS();  
  }
  else{
    BBCQN[0] = -999;
    BBCQS[0] = -999;
    BBCNN[0] = -999;
    BBCNS[0] = -999;
    
    
  }

  Float_t Evt_Data[32];
  Evt_Data[0] = fRunNumber[0];
  Evt_Data[1] = SpinX_ID[0];
  Evt_Data[2] = Pol_Y[0];
  Evt_Data[3] = Pol_B[0];
  Evt_Data[4] = GL1X_ID[0];

  Evt_Data[5] = Evt_Z[0];
  Evt_Data[6] = BbcZVertex[0];
  Evt_Data[7] = zdcCentrality[0];
  Evt_Data[8] = bbcCentrality[0];
  Evt_Data[9] = BBCQN[0];

  Evt_Data[10] = BBCQS[0];
  Evt_Data[11] = BBCNN[0];
  Evt_Data[12] = BBCNS[0];
  Evt_Data[13] = ZdcEnergyN[0];
  Evt_Data[14] = ZdcEnergyS[0];
  Evt_Data[15] = Clock_trig[0];
  Evt_Data[16] = realtrig_MB[0];                    // live real   trigger for MB
  Evt_Data[17] = livetrigS_1S[0];                    // live real   trigger for MUID_LL1_S_1S
  Evt_Data[18] = livetrigS_1D[0];                    // live real   trigger for MUID_LL1_S_1D
  Evt_Data[19] = livetrigN_1S[0];                    // live real   trigger for MUID_LL1_N_1S
  Evt_Data[20] = livetrigN_1D[0];                    // live real   trigger for MUID_LL1_N_1D
  Evt_Data[21] = RPbbcrp10[0];                      // Reaction Plane
  Evt_Data[22] = RPbbcrp11[0];                      // Reaction Plane
  Evt_Data[23] = RPbbcrp12[0];                      // Reaction Plane
  Evt_Data[24] = delphi_RP[0];                      // Reaction Plane
  Evt_Data[25] = -9999;            // 
  Evt_Data[26] = RPbbcrp00[0];                      // Reaction Plane
  Evt_Data[27] = RPbbcrp01[0];                      // Reaction Plane
  Evt_Data[28] = RPbbcrp02[0];                      // Reaction Plane
  Evt_Data[29] = RPsmdrp00[0];                      // Reaction Plane
  Evt_Data[30] = RPsmdrp01[0];                      // Reaction Plane
  Evt_Data[31] = RPsmdrp02[0];                      // Reaction Plane

  newsngvtx->Fill(Evt_Data);


  // define the eventdata branch
  TBranch *fBranch = newsngmuons->GetBranch("Eventdata");
	
  //    cout << " got event branch" << endl;
  	
	//=== Fill branches
	((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(fRunNumber);                       // run number
	//cout << " after runnr" << endl;
	((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(EventNumber);                     // event number
	//cout << " after eventnr" << endl;
	((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(Evt_Z);                           // Global
((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(BbcZVertex);                      // BBC Zvertex
	((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(bbcCentrality);            
	((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(ZdcEnergyN);                      // zdcEnerN);
	((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(realtrigS_1D);                    // real   trigger 1 deep  South
	((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(realtrigS_1S);                    // real   trigger 1 sheep  South
	((TLeaf *) fBranch->GetListOfLeaves()->At(8))->SetAddress(realtrigN_1D);                    // real   trigger 1 deep  North
	((TLeaf *) fBranch->GetListOfLeaves()->At(9))->SetAddress(realtrigN_1S);                    // real   trigger 1 sheep North
	((TLeaf *) fBranch->GetListOfLeaves()->At(10))->SetAddress(livetrigS_1D);                    // live   trigger 1 deep  South
	
	((TLeaf *) fBranch->GetListOfLeaves()->At(11))->SetAddress(livetrigS_1S);                    // live    trigger 1 sheep  South
	((TLeaf *) fBranch->GetListOfLeaves()->At(12))->SetAddress(livetrigN_1D);                    // live   trigger 1 deep  North
	((TLeaf *) fBranch->GetListOfLeaves()->At(13))->SetAddress(livetrigN_1S);                    // live   trigger 1 sheep North
	((TLeaf *) fBranch->GetListOfLeaves()->At(14))->SetAddress(triggerbit);
	((TLeaf *) fBranch->GetListOfLeaves()->At(15))->SetAddress(triggerlive);
	((TLeaf *) fBranch->GetListOfLeaves()->At(16))->SetAddress(clockcross);
	((TLeaf *) fBranch->GetListOfLeaves()->At(17))->SetAddress(Clock_trig);
	((TLeaf *) fBranch->GetListOfLeaves()->At(18))->SetAddress(realtrig_MB);                     // live real   trigger for MB
	((TLeaf *) fBranch->GetListOfLeaves()->At(19))->SetAddress(SpinX_ID);                         // beam crossing ID
	((TLeaf *) fBranch->GetListOfLeaves()->At(20))->SetAddress(Pol_Y);
	((TLeaf *) fBranch->GetListOfLeaves()->At(21))->SetAddress(Pol_B);
	((TLeaf *) fBranch->GetListOfLeaves()->At(22))->SetAddress(GL1X_ID);
	((TLeaf *) fBranch->GetListOfLeaves()->At(23))->SetAddress(RPbbcrp10);                      // zdcEnerN);
	((TLeaf *) fBranch->GetListOfLeaves()->At(24))->SetAddress(RPbbcrp11);                      // zdcEnerN);
	((TLeaf *) fBranch->GetListOfLeaves()->At(25))->SetAddress(RPbbcrp12);                      // zdcEnerN);
	((TLeaf *) fBranch->GetListOfLeaves()->At(26))->SetAddress(delphi_RP);                      // zdcEnerN);
	((TLeaf *) fBranch->GetListOfLeaves()->At(27))->SetAddress(RPbbcrp00);                      // zdcEnerN);
	((TLeaf *) fBranch->GetListOfLeaves()->At(28))->SetAddress(RPbbcrp01);                      // zdcEnerN);
	((TLeaf *) fBranch->GetListOfLeaves()->At(29))->SetAddress(RPbbcrp02);                      // zdcEnerN);
	((TLeaf *) fBranch->GetListOfLeaves()->At(30))->SetAddress(RPsmdrp00);                      // zdcEnerN);
	((TLeaf *) fBranch->GetListOfLeaves()->At(31))->SetAddress(RPsmdrp01);                      // zdcEnerN);
	//	((TLeaf *) fBranch->GetListOfLeaves()->At(32))->SetAddress(RPsmdrp02);                      // zdcEnerN);



	//	cout << " after putting in addresses for sngvtx" << endl; 


  //_____________ end of sngvtx _____________


  Float_t *RPC2_x=NULL;
  Float_t *RPC2_y=NULL;
  Float_t *RPC3_x=NULL;
  Float_t *RPC3_y=NULL;
  Float_t *RPC3r_x=NULL;
  Float_t *RPC3r_y=NULL;
  Float_t *RPC2r_x=NULL;
  Float_t *RPC2r_y=NULL;

  Float_t *dS30=NULL;
  Float_t *dS3ctp0=NULL;
  Float_t *muIDchis0=NULL;
  Float_t *DG0=NULL;
  Float_t *DG4=NULL;
  Float_t *DCA_r=NULL;
  Float_t *DCA_z=NULL;
  Float_t *DDG0=NULL;
  Float_t *DS0=NULL;

  Float_t *pseudo_rapidity=NULL;
  Int_t   *muIDquad0=NULL;
  Int_t   *muTRhits0=NULL;
  Int_t   *lastGap=NULL;            

	Float_t *charge=NULL;
	Float_t *muchisquare=NULL;
	Int_t *status=NULL;
	Float_t *ghostflag=NULL;
	Float_t *newphi=NULL;
	Float_t *refit_z=NULL;
	Float_t *gap0x  =NULL;
	Float_t *gap0y  =NULL;
	Float_t *gap0z=NULL;  
	Float_t *dirX =NULL;  
	Float_t *dirY =NULL;  
	Float_t *refX =NULL;  
	Float_t *refY =NULL;  
	Float_t *muTRhits=NULL;
	Int_t *muIDhits=NULL;

	Float_t *mutr_nhits=NULL;
	Float_t *muid_nhits=NULL;
	
	Float_t *px=NULL;
	Float_t *py=NULL;
	Float_t *pz=NULL;
	
	Float_t *xSTI =NULL;
	Float_t *ySTI =NULL;
	Float_t *zSTI =NULL;
	Float_t *pxSTI=NULL;
	Float_t *pySTI=NULL;
	Float_t *pzSTI=NULL;
	
	Float_t *xSTII =NULL;
	Float_t *ySTII =NULL;
	Float_t *zSTII =NULL;
	
	Float_t *xSTIII =NULL;
	Float_t *ySTIII =NULL;
	Float_t *zSTIII =NULL;
	Float_t *pxSTIII=NULL;
	Float_t *pySTIII=NULL;
	Float_t *pzSTIII=NULL;
	
	Float_t *pT =NULL;
	Float_t *p =NULL;
	Float_t *pSTI =NULL;
	Float_t *ELoss =NULL;

	Float_t *dANGLE =NULL;
	Float_t *dANGLE_xyz=NULL;
	Float_t *dPHI=NULL;
	Float_t *dTHETA=NULL;
	Float_t *ROAD_SLOPE=NULL;

	 Float_t *rpc3z=NULL;
	 Float_t *rpc2z=NULL; 
		     
	 Float_t *rpc3st3=NULL;
	 Float_t *rpc2st3=NULL;
	 Float_t *rpc2g5=NULL; 
	 Float_t *rpc3g5=NULL; 
  

	Float_t *ref_vtx_rdca=NULL;
	Float_t *ref_vtx_r=NULL;
	Float_t *ref_vtx_z=NULL;
	Int_t *pseudotrigS_1D=NULL;
	Int_t *pseudotrigS_1S=NULL;
	Int_t *pseudotrigN_1D=NULL;
	Int_t *pseudotrigN_1S=NULL;
	Int_t *recoS_1D=NULL;
	Int_t *recoS_1S=NULL;
	Int_t *recoN_1D=NULL;
	Int_t *recoN_1S=NULL;

	Float_t *MC_N_PART=NULL;   // total # of MC particles associated with this reco'd track
	Float_t *MC_PX=NULL;        // the major MC particle's px
	Float_t *MC_PY=NULL;
	Float_t *MC_PZ=NULL;
	Float_t *MC_PTOT =NULL;      // the major MC particle's lastGap
	Float_t *MC_X=NULL;         // the major MC particle's X_orign
	Float_t *MC_Y=NULL;         // the major MC particle's Y_orign
	Float_t *MC_Z=NULL;         // the major MC particle's Z_orign
	Float_t *MC_PID =NULL;
	Float_t *MC_HITS=NULL;     // the major MC particle's total contribution to muTr hits
	
	Float_t *MC_P_PID =NULL;    // MC particle's parent information
	Float_t *MC_P_PX =NULL;     //
	Float_t *MC_P_PY =NULL;
	Float_t *MC_P_PZ =NULL;
	Float_t *MC_P_PTOT =NULL;
	Float_t *MC_P_Z =NULL;
	
	Float_t *MC_D_PID =NULL;    // MC particle's daughter information
	Float_t *MC_D_PX =NULL;     //
	Float_t *MC_D_PY =NULL;
	Float_t *MC_D_PZ =NULL;
	Float_t *MC_D_PTOT =NULL;
	Float_t *MC_D_Z =NULL;
	Float_t *MC_D_N =NULL;
	
	Float_t *MC_G_PID =NULL;    // MC particle's grandparent (ancestor, original, very primary) informationi
	Float_t *MC_G_PX =NULL;     //
	Float_t *MC_G_PY =NULL;
	Float_t *MC_G_PZ =NULL;
	Float_t *MC_G_PTOT =NULL;
	Float_t *MC_G_Z=NULL;
	
	Int_t  *MC_TRK  =NULL;
	Int_t  *MC_P_TRK=NULL;    // parent track ID: for debuggin now
	Int_t  *MC_D_TRK=NULL;    // daughter track ID: for debuggin now
	Int_t  *MC_G_TRK=NULL;    // grandparent track ID: for debuggin now


	Float_t *rpc_t=NULL; //RPC related variables
    Float_t *rpc_c=NULL;
    Int_t *arm=NULL;
    Int_t *station=NULL;
    Int_t *octant=NULL;
    Int_t *halfoctant=NULL;
    Int_t *radsegment=NULL;
    Int_t *strip=NULL;




    Float_t  *res100=NULL;
    Float_t  *res101=NULL;
    Float_t  *res110=NULL;
    Float_t  *res111=NULL;
    Float_t  *res120=NULL;
    Float_t  *res121=NULL;
    Float_t  *res200=NULL;
    Float_t  *res201=NULL;
    Float_t  *res210=NULL;
    Float_t  *res211=NULL;
    Float_t  *res220=NULL;
    Float_t  *res221=NULL;
    Float_t  *res300=NULL;
    Float_t  *res301=NULL;
    Float_t  *res310=NULL;
    Float_t  *res311=NULL;


    Int_t *station1hits=NULL;
    Int_t *station2hits=NULL;
    Int_t *station3hits=NULL;
    Int_t *allstation1hits=NULL;
    Int_t *allstation2hits=NULL;
    Int_t *allstation3hits=NULL;

    Int_t *allhits=NULL;


    Float_t *invmass1=NULL;
    Float_t *invmass2=NULL;
    Float_t *invmass3=NULL;


    Int_t *icharge1=NULL;
    Int_t *icharge2=NULL;
    Int_t *icharge3=NULL;

    Float_t *thrust1=NULL;
    Float_t *thrust2=NULL;
    Float_t *thrust3=NULL;
  
    Float_t *RpcDCA=NULL;
    Float_t *RpcpT=NULL;
    Float_t *Rpctime=NULL;


    int *npart=NULL;

    double rpcx=-9999.;
    double rpcy=-9999.;


  // get best RPC hit data also via Tools??
  /*      DG_Rpc3 =  ; 
	  DG_Rpc1;
	  t_rpc3;
	  t_rpc1;*/
  // for now only channel and timing	
    //cout << "before RPC part" << endl;  
    int test=0;
    if (test==1){
      
    }
    else {

      //      TMutVtxMap* hit_map1 =  TMutNode<TMutVtxMap>::find_node( _top_node,"TMutVtxMap" );
      TRpcTrkMap* hit_map1;
      try { hit_map1 = TMutNode<TRpcTrkMap>::find_node( _top_node, "TRpcTrkMap" ); }
      //MuonUtil::find_node<TRpcHit>( "TRpcHit" ); }
      catch (exception &e ) { MUTOO::TRACE( e.what() );
	hit_map1 = NULL;}
      //cout << "inb RPC trkmap, sizwe " << hit_map1->size() << endl;         	


      //            TRpcHit* hit_map1 =  TMutNode<TRpcHit>::find_node( _top_node,"TRpcHit" );
      //cout << "after RPC hit, before Map" << endl;
      
      TRpcHitMap* hit_map =  TMutNode<TRpcHitMap>::find_node( _top_node,"TRpcHitMap" );
      
      //cout << "before RPC hitmap defined" << endl;  
      if(hit_map1) {  // rpc hit map exists 
	
	//cout << "inb RPC hitmap, sizwe " << hit_map->size() << endl;         	
	
    TRpcHitMap::iterator hit_iter = hit_map->range();
 
    
    fNRpcHits=0;
    while(hit_iter.next()) { fNRpcHits++; }
 
    //   cout << "inb RPC hitmap, range " << fNRpcHits << endl;         
    if(fNRpcHits>0){    
    
    rpc_t  = new Float_t[fNRpcHits];
    
    rpc_c = new Float_t[fNRpcHits];
    
    arm        = new Int_t[fNRpcHits];
    station    = new Int_t[fNRpcHits];
    octant     = new Int_t[fNRpcHits];
    halfoctant = new Int_t[fNRpcHits];
    radsegment = new Int_t[fNRpcHits];
    strip      = new Int_t[fNRpcHits];
     int rp3oct[fNRpcHits];
     int rp3ho[fNRpcHits];
    
    for(int i=0; i<fNRpcHits; i++) {

rpc_t[i]=-9999;
rpc_c[i]=-9999;
arm[i]=-9999;
station[i]=-9999;
octant[i]=-9999;
halfoctant[i]=-9999;
radsegment[i]=-9999;
strip[i]=-9999;
rp3oct[i]=-9999;
rp3ho[i]=-9899;

    }    
    
    
    //  Int_t iHit=0;
    fNRpcHits = 0;
    hit_iter = hit_map->range();
    
    //     cout << " rpc range" << hit_map->size();
    
     int st3hits=0;
    while(TRpcHitMap::pointer hit_ptr = hit_iter.next()) {
      //	    if ( fNRpcHits<10){
      rpc_t[fNRpcHits] = (float)hit_ptr->get()->get_t();
      
      //      cout << fNRpcHits << " " << hit_ptr->get()->get_arm() << " station " << hit_ptr->get()->get_station()  << "  octant  " <<hit_ptr->get()->get_octant()  << "  half " <<hit_ptr->get()->get_half_octant()  << "  seg " <<hit_ptr->get()->get_rseg()  << "  st " <<hit_ptr->get()->get_strip()  << "  time " << (int)hit_ptr->get()->get_t() << endl ;
      rpc_c[fNRpcHits] = hit_ptr->get()->get_arm()*100000+ hit_ptr->get()->get_station()*100000+hit_ptr->get()->get_octant()*10000+hit_ptr->get()->get_half_octant()*1000+hit_ptr->get()->get_rseg()*100+hit_ptr->get()->get_strip() ;
      
      
      arm[fNRpcHits] = hit_ptr->get()->get_arm();      
      station[fNRpcHits] = hit_ptr->get()->get_station();         
      octant[fNRpcHits] = hit_ptr->get()->get_octant();          
      halfoctant[fNRpcHits] = hit_ptr->get()->get_half_octant();      
      radsegment[fNRpcHits] = hit_ptr->get()->get_rseg();      
      strip[fNRpcHits] = hit_ptr->get()->get_strip();           

      if(station[fNRpcHits]==2){
	rp3oct[st3hits] = hit_ptr->get()->get_octant();          
	rp3ho[st3hits] = hit_ptr->get()->get_half_octant();      
	st3hits++;

      }
      fNRpcHits++; 
      
    }

    if( st3hits>0){ //check coordinates only for st 3 hits
      rpcx=0;
      rpcy=0;
    

      TRpcCoordMap* coord =  TMutNode<TRpcCoordMap>::find_node( _top_node,"TRpcCoordMap" );
      TRpcCoordMap::iterator coord_iter = coord->range();
      while(TRpcCoordMap::pointer rpccord = coord_iter.next()) {
	//      TRpcCoord_v1 *rpccord =  new TRpcCoord_v1(hit_ptr->get()->get_key(),(UShort_t) arm[fNRpcHits], (UShort_t)station[fNRpcHits], (UShort_t)octant[fNRpcHits], (UShort_t)halfoctant[fNRpcHits], (UShort_t)radsegment[fNRpcHits], (UShort_t)strip[fNRpcHits] );



	//printf("xyz RPC \%4.2f %4.2f %4.2f\n",rpccord->get()->get_x(),rpccord->get()->get_y(),rpccord->get()->get_z());           

	if(fabs(rpccord->get()->get_z())>900){
	  rpcx+=rpccord->get()->get_x()/st3hits;
	  rpcy+=rpccord->get()->get_y()/st3hits;
	}
      }

    double dummyx=-1;double dummyy=-1;
    //printf("x %8.0f, y %8.0f radius %8.0f octant %d",rpcx,rpcy,sqrt(rpcx * rpcx + rpcy * rpcy), rp3oct[0]);
    dummyy = pow(-1,rp3ho[0]) * rpcy * cos(rp3oct[0] * 2 * TMath::Pi()/8) + rpcx*sin(rp3oct[0]*2*TMath::Pi()/8);
    dummyx = pow(-1,rp3ho[0]+1) * rpcy * sin(rp3oct[0] * 2 * TMath::Pi()/8) + rpcx * cos(rp3oct[0]*2*TMath::Pi()/8);

    rpcy= dummyy;
    rpcx= dummyx;
    //printf("and x %8.0f, y %8.0f radius %8.0f halfoct %d\n",rpcx,rpcy,sqrt(rpcx * rpcx + rpcy * rpcy), rp3ho[0]);


    } // if st3hits

    
    //cout << " Station 3 hist" << st3hits << endl;
    TBranch *fBranch2 = newsngmuons->GetBranch("_RpcHits");
    ((TLeaf *) fBranch2->GetListOfLeaves()->At(0))->SetAddress(&fNRpcHits);
    
    //cout << " got RPC branches" << endl;
    
    fBranch2 = newsngmuons->GetBranch("RpcHits");
    ((TLeaf *) fBranch2->GetListOfLeaves()->At(0))->SetAddress(rpc_t);
    ((TLeaf *) fBranch2->GetListOfLeaves()->At(1))->SetAddress(rpc_c);
    
    
    
    ((TLeaf *) fBranch2->GetListOfLeaves()->At(2))->SetAddress(arm);       
    ((TLeaf *) fBranch2->GetListOfLeaves()->At(3))->SetAddress(station);   
    ((TLeaf *) fBranch2->GetListOfLeaves()->At(4))->SetAddress(octant);    
    ((TLeaf *) fBranch2->GetListOfLeaves()->At(5))->SetAddress(halfoctant);
    ((TLeaf *) fBranch2->GetListOfLeaves()->At(6))->SetAddress(radsegment);
    ((TLeaf *) fBranch2->GetListOfLeaves()->At(7))->SetAddress(strip);     

}
      }
  
  
    }


  if (muo) { 
    //cout << " in muo";
    //  fNMU = (muo->get_npart()>0)?1:0;
  fNMU = muo->get_npart();
  if (fNMU>0) {


  RPC2_x= new Float_t[fNMU];
  RPC2_y= new Float_t[fNMU];
  RPC3_x= new Float_t[fNMU];
  RPC3_y= new Float_t[fNMU];
  RPC2r_x=new Float_t[fNMU];
  RPC2r_y=new Float_t[fNMU];
  RPC3r_x=new Float_t[fNMU];
  RPC3r_y=new Float_t[fNMU];

  RPC2_x[0]= -9999;
  RPC2_y[0]= -9999;
  RPC3_x[0]= -9999;
  RPC3_y[0]= -9999;
  RPC3r_x[0]= -9999;
  RPC3r_y[0]= -9999;
  RPC2r_x[0]= -9999;
  RPC2r_y[0]= -9999;











    res100=new Float_t[fNMU];
    res101=new Float_t[fNMU];
    res110=new Float_t[fNMU];
    res111=new Float_t[fNMU];
    res120=new Float_t[fNMU];
    res121=new Float_t[fNMU];
    res200=new Float_t[fNMU];
    res201=new Float_t[fNMU];
    res210=new Float_t[fNMU];
    res211=new Float_t[fNMU];
    res220=new Float_t[fNMU];
    res221=new Float_t[fNMU];
    res300=new Float_t[fNMU];
    res301=new Float_t[fNMU];
    res310=new Float_t[fNMU];
    res311=new Float_t[fNMU];


    station1hits=new Int_t[fNMU];
    station2hits=new Int_t[fNMU];
    station3hits=new Int_t[fNMU];



    allstation1hits=new Int_t[fNMU];
    allstation2hits=new Int_t[fNMU];
    allstation3hits=new Int_t[fNMU];

    allhits=new Int_t[fNMU];




    invmass1=new Float_t[fNMU];
    invmass2=new Float_t[fNMU];
    invmass3=new Float_t[fNMU];


    icharge1=new Int_t[fNMU];
    icharge2=new Int_t[fNMU];
    icharge3=new Int_t[fNMU];

    thrust1=new Float_t[fNMU];
    thrust2=new Float_t[fNMU];
    thrust3=new Float_t[fNMU];









  // muon variables

  
  dS30 = new Float_t[fNMU];     
  dS3ctp0 = new Float_t[fNMU];  
  muIDchis0 = new Float_t[fNMU];
  DG0 = new Float_t[fNMU];	     
  DG4 = new Float_t[fNMU];	     
  DCA_r = new Float_t[fNMU];	     
  DCA_z = new Float_t[fNMU];	     
  DDG0 = new Float_t[fNMU];          
  DS0  = new Float_t[fNMU];     

  dS30[0]=999; dS3ctp0[0]=999; muIDchis0[0]=999; DG0[0]=999; DDG0[0]=999; DS0[0]=999;

  pseudo_rapidity = new Float_t[fNMU];
  muIDquad0= new Int_t[fNMU];	 
  muTRhits0 = new Int_t[fNMU];		 
  lastGap= new Int_t[fNMU]; 
  
  pseudo_rapidity[0] = 999;
  muIDquad0[0]=-1;	 
  muTRhits0[0]=0;		 
  lastGap[0]=0;                       

	charge = new Float_t[fNMU];	
	muchisquare = new Float_t[fNMU];
	
	refit_z = new Float_t[fNMU];
	status = new Int_t[fNMU];
	newphi = new Float_t[fNMU];
	ghostflag = new Float_t[fNMU];


	gap0x = new Float_t[fNMU]; 
	gap0y = new Float_t[fNMU]; 
	gap0z = new Float_t[fNMU];			
	dirX = new Float_t[fNMU];
	dirY = new Float_t[fNMU];
	refX  = new Float_t[fNMU];
	refY = new Float_t[fNMU];
	muTRhits  = new Float_t[fNMU];
	muIDhits = new Int_t[fNMU];


	 mutr_nhits = new Float_t[fNMU];
	 muid_nhits = new Float_t[fNMU];
		           
	 px =         new Float_t[fNMU];
	 py =         new Float_t[fNMU];
	 pz =         new Float_t[fNMU];
		           
	 xSTI  =      new Float_t[fNMU];
	 ySTI  =      new Float_t[fNMU];
	 zSTI  =      new Float_t[fNMU];
	 pxSTI =      new Float_t[fNMU];
	 pySTI =      new Float_t[fNMU];
	 pzSTI =      new Float_t[fNMU];
	 
	 xSTII  =     new Float_t[fNMU];
	 ySTII  =     new Float_t[fNMU];
	 zSTII  =     new Float_t[fNMU];
	 
	 xSTIII  =    new Float_t[fNMU];
	 ySTIII  =    new Float_t[fNMU];
	 zSTIII  =    new Float_t[fNMU];
	 pxSTIII =    new Float_t[fNMU];
	 pySTIII =    new Float_t[fNMU];
	 pzSTIII =    new Float_t[fNMU];
	 
	 pT =         new Float_t[fNMU];
	 p =          new Float_t[fNMU];
	 pSTI =       new Float_t[fNMU];
	 ELoss =      new Float_t[fNMU];
	 

	 rpc3z = new  Float_t [fNMU];
	 rpc2z = new	 Float_t[fNMU]; 
	 
	 rpc3st3  = new	 Float_t[fNMU]; 
	 rpc2st3  = new	 Float_t[fNMU]; 
	 rpc2g5   = new	 Float_t[fNMU]; 
	 rpc3g5  = new	 Float_t[fNMU]; 

	Float_t costheta;
	Float_t costheta_xyz;
	Float_t xyz_St1;

	dANGLE = new Float_t[fNMU];
	  dANGLE_xyz= new Float_t[fNMU];
	  dPHI= new Float_t[fNMU];
	  dTHETA= new Float_t[fNMU];
	  ROAD_SLOPE= new Float_t[fNMU];


	ref_vtx_rdca = new Float_t[fNMU];
	ref_vtx_r = new Float_t[fNMU];
	ref_vtx_z = new Float_t[fNMU];


	pseudotrigS_1D   = new Int_t[fNMU];
	pseudotrigS_1S   = new Int_t[fNMU];
	pseudotrigN_1D   = new Int_t[fNMU];
	pseudotrigN_1S   = new Int_t[fNMU];
	recoS_1D = new Int_t[fNMU];
	recoS_1S = new Int_t[fNMU];
	recoN_1D = new Int_t[fNMU];
	recoN_1S = new Int_t[fNMU];	

	 MC_N_PART = new Float_t[fNMU];   // total # of MC particles associated with this reco'd track
	 MC_PX= new     Float_t[fNMU];        // the major MC particle's px
	 MC_PY= new     Float_t[fNMU];
	 MC_PZ= new     Float_t[fNMU];
	 MC_PTOT = new  Float_t[fNMU];      // the major MC particle's lastGap
	 MC_X= new      Float_t[fNMU];         // the major MC particle's X_orign
	 MC_Y= new      Float_t[fNMU];         // the major MC particle's Y_orign
	 MC_Z= new      Float_t[fNMU];         // the major MC particle's Z_orign
	 MC_PID = new   Float_t[fNMU];
	 MC_HITS = new  Float_t[fNMU];     // the major MC particle's total contribution to muTr hits
		           
	 MC_P_PID = new Float_t[fNMU];    // MC particle's parent information
	 MC_P_PX = new  Float_t[fNMU];     //
	 MC_P_PY = new  Float_t[fNMU];
	 MC_P_PZ = new  Float_t[fNMU];
	 MC_P_PTOT = new Float_t[fNMU] ;
	 MC_P_Z = new   Float_t[fNMU];
		           
	 MC_D_PID = new Float_t[fNMU];    // MC particle's daughter information
	 MC_D_PX = new  Float_t[fNMU];     //
	 MC_D_PY = new  Float_t[fNMU];
	 MC_D_PZ = new  Float_t[fNMU];
	 MC_D_PTOT = new Float_t[fNMU];
	 MC_D_Z = new   Float_t[fNMU] ;
	 MC_D_N = new   Float_t[fNMU] ;
		           
	 MC_G_PID = new Float_t[fNMU] ;    // MC particle's grandparent (ancestor, original, very primary) informationi
	 MC_G_PX = new  Float_t[fNMU] ;     //
	 MC_G_PY = new  Float_t[fNMU] ;
	 MC_G_PZ = new  Float_t[fNMU] ;
	 MC_G_PTOT = new Float_t[fNMU]  ;
	 MC_G_Z = new   Float_t[fNMU] ;
		           
	MC_TRK   = new  Int_t[fNMU]   ;
	MC_P_TRK = new  Int_t[fNMU]   ;    // parent track ID: for debuggin now
	MC_D_TRK = new  Int_t[fNMU]   ;    // daughter track ID: for debuggin now
	MC_G_TRK = new  Int_t[fNMU];    // grandparent track ID: for debuggin now


	RpcDCA  = new Float_t[fNMU];
	RpcpT   = new Float_t[fNMU];
	Rpctime = new Float_t[fNMU];



    npart = new int[fNMU];

for(int i=0; i<fNMU; i++){ 
  RPC2_x [i]=-9999;
  RPC2_y [i]=-9999;
  RPC3_x [i]=-9999;
  RPC3_y [i]=-9999;
  RPC2r_x [i]=-9999;
  RPC2r_y [i]=-9999;
  RPC3r_x [i]=-9999;
  RPC3r_y [i]=-9999;
  dS30  [i]=-9999;
  dS3ctp0  [i]=-9999;
  muIDchis0  [i]=-9999;
  DG0  [i]=-9999;
  DG4  [i]=-9999;
  DCA_r  [i]=-9999;
  DCA_z  [i]=-9999;
  DDG0  [i]=-9999;
  DS0   [i]=-9999;
  pseudo_rapidity  [i]=-9999;
  muIDquad0 [i]=-9999;
  muTRhits0  [i]=-9999;
  lastGap [i]=-9999;
        charge  [i]=-9999;
        muchisquare  [i]=-9999;
        status  [i]=-9999;
        ghostflag  [i]=-9999;
        newphi  [i]=-9999;
        refit_z  [i]=-9999;
        gap0x  [i]=-9999;
        gap0y  [i]=-9999;
        gap0z  [i]=-9999;
        dirX  [i]=-9999;
        dirY  [i]=-9999;
        refX   [i]=-9999;
        refY  [i]=-9999;
        muTRhits   [i]=-9999;
        muIDhits  [i]=-9999;
         mutr_nhits  [i]=-9999;
         muid_nhits  [i]=-9999;
         px  [i]=-9999;
         py  [i]=-9999;
         pz  [i]=-9999;
         xSTI   [i]=-9999;
         ySTI   [i]=-9999;
         zSTI   [i]=-9999;
         pxSTI  [i]=-9999;
         pySTI  [i]=-9999;
         pzSTI  [i]=-9999;
         xSTII   [i]=-9999;
         ySTII   [i]=-9999;
         zSTII   [i]=-9999;
         xSTIII   [i]=-9999;
         ySTIII   [i]=-9999;
         zSTIII   [i]=-9999;
         pxSTIII  [i]=-9999;
         pySTIII  [i]=-9999;
         pzSTIII  [i]=-9999;
         pT  [i]=-9999;
         p  [i]=-9999;
         pSTI  [i]=-9999;
         ELoss  [i]=-9999;
         rpc3z  [i]=-9999;
         rpc2z  [i]=-9999;
         rpc3st3   [i]=-9999;
         rpc2st3   [i]=-9999;
         rpc2g5    [i]=-9999;
         rpc3g5   [i]=-9999;
        dANGLE  [i]=-9999;
          dANGLE_xyz [i]=-9999;
          dPHI [i]=-9999;
          dTHETA [i]=-9999;
          ROAD_SLOPE [i]=-9999;
        ref_vtx_rdca  [i]=-9999;
        ref_vtx_r  [i]=-9999;
        ref_vtx_z  [i]=-9999;
        pseudotrigS_1D    [i]=-9999;
        pseudotrigS_1S    [i]=-9999;
        pseudotrigN_1D    [i]=-9999;
        pseudotrigN_1S    [i]=-9999;
        recoS_1D  [i]=-9999;
        recoS_1S  [i]=-9999;
        recoN_1D  [i]=-9999;
        recoN_1S  [i]=-9999;
         MC_N_PART  [i]=-9999;
         MC_PX [i]=-9999;
         MC_PY [i]=-9999;
         MC_PZ [i]=-9999;
         MC_PTOT  [i]=-9999;
         MC_X [i]=-9999;
         MC_Y [i]=-9999;
         MC_Z [i]=-9999;
         MC_PID  [i]=-9999;
         MC_HITS  [i]=-9999;
         MC_P_PID  [i]=-9999;
         MC_P_PX  [i]=-9999;
         MC_P_PY  [i]=-9999;
         MC_P_PZ  [i]=-9999;
         MC_P_PTOT  [i]=-9999;
         MC_P_Z  [i]=-9999;
         MC_D_PID  [i]=-9999;
         MC_D_PX  [i]=-9999;
         MC_D_PY  [i]=-9999;
         MC_D_PZ  [i]=-9999;
         MC_D_PTOT  [i]=-9999;
         MC_D_Z  [i]=-9999;
         MC_D_N  [i]=-9999;
         MC_G_PID  [i]=-9999;
         MC_G_PX  [i]=-9999;
         MC_G_PY  [i]=-9999;
         MC_G_PZ  [i]=-9999;
         MC_G_PTOT  [i]=-9999;
         MC_G_Z  [i]=-9999;
        MC_TRK    [i]=-9999;
        MC_P_TRK  [i]=-9999;
        MC_D_TRK  [i]=-9999;
        MC_G_TRK  [i]=-9999;
	RpcDCA  [i]=-9999;
	RpcpT   [i]=-9999;
	Rpctime [i]=-9999;
    npart  [i]=-9999;





    res100[i]=-9999;
    res101[i]=-9999;
    res110[i]=-9999;
    res111[i]=-9999;
    res120[i]=-9999;
    res121[i]=-9999;
    res200[i]=-9999;
    res201[i]=-9999;
    res210[i]=-9999;
    res211[i]=-9999;
    res220[i]=-9999;
    res221[i]=-9999;
    res300[i]=-9999;
    res301[i]=-9999;
    res310[i]=-9999;
    res311[i]=-9999;


    station1hits[i]=-9999;
    station2hits[i]=-9999;
    station3hits[i]=-9999;

    allstation1hits[i]=-9999;
    allstation2hits[i]=-9999;
    allstation3hits[i]=-9999;

    allhits[i]=-9999;
    invmass1[i]=-9999;
    invmass2[i]=-9999;
    invmass3[i]=-9999;


    icharge1[i]=-9999;
    icharge2[i]=-9999;
    icharge3[i]=-9999;

    thrust1[i]=-9999;
    thrust2[i]=-9999;
    thrust3[i]=-9999;


















 }

    
    npart[0] = muo->get_npart();
    totMU += npart[0];
    

    //   cout << npart[0] << " muon candidates, new res"<< endl;


      TRpcTrkMap* hit_map2;
      try { hit_map2 = TMutNode<TRpcTrkMap>::find_node( _top_node, "TRpcTrkMap" ); }
      //MuonUtil::find_node<TRpcHit>( "TRpcHit" ); }
      catch (exception &e ) { MUTOO::TRACE( e.what() );
	hit_map2 = NULL;}

      //	cout << "after hitmap2 " << endl;	

    for (int ipart=0; ipart<npart[0]; ipart++) 
      {
//	cout << "ipart  " << ipart << endl;	



//	Float_t fTracks_Rpcp ;
	Float_t fTracks_Rpcpx;
	Float_t fTracks_Rpcpy;
	Float_t fTracks_Rpcpz;
	Float_t fTracks_RpcDca  ;
	//	Float_t fTracks_RpcDcaVt;
	//	Float_t fTracks_Rpctime ;
	Float_t fTracks_RpcPt   ;

	TRpcTrkMap::iterator trk_iter = hit_map2->range();
	//cout << "rpctrack hitmap range " << hit_map2->size()<< endl;	
	while(TRpcTrkMap::pointer trk_ptr = trk_iter.next()) {
	  
	  TMutTrkPar *fTrkPar = (TMutTrkPar *) trk_ptr->get()->get_trk_par_vtx();
	  //TMutTrkPar *fTrkPar = (TMutTrkPar *) trk_ptr->get()->get_trk_par();
	  //int charge_r = fTrkPar->get_charge();
	
	Float_t p2 = fTrkPar->get_px()*fTrkPar->get_px();
	p2 += fTrkPar->get_py()*fTrkPar->get_py();
	p2 += fTrkPar->get_pz()*fTrkPar->get_pz();
	
	//	fTracks_Rpcp = sqrt(p2);
	fTracks_Rpcpx = fTrkPar->get_px();
	fTracks_Rpcpy = fTrkPar->get_py();
	fTracks_Rpcpz = fTrkPar->get_pz();


	//	printf("RPC track match? (%4.2f %4.2f %4.2f) (%4.2f,%4.2f,%4.2f)\n",fTracks_Rpcpx,fTracks_Rpcpy,fTracks_Rpcpz, muo->get_px(0,ipart), muo->get_py(0,ipart), muo->get_pz(0,ipart) );
	if ( fabs( muo->get_px(0,ipart) -  fTracks_Rpcpx ) < 0.1 && 
	     fabs( muo->get_py(0,ipart) -  fTracks_Rpcpy ) < 0.1 && 
	     fabs( muo->get_pz(0,ipart) -  fTracks_Rpcpz ) < 0.1)
	  {
	    //  printf("good RPC track match charges %d %d mom %4.1f\n",charge_r,muo->get_charge(ipart),p2 );
	    fTracks_RpcDca   = trk_ptr->get()->get_dca_trk();
	    //	    fTracks_RpcDcaVt = trk_ptr->get()->get_dca_trk_vtx();
	    //	    fTracks_Rpctime  = trk_ptr->get()->get_rpcclus3time();
	    fTracks_RpcPt    = sqrt((pow(fTracks_Rpcpx,2)+pow(fTracks_Rpcpy,2))/p2); //trk_ptr->get()->get_corr_pT();

	    RpcDCA[ipart]  = fTracks_RpcDca;
	    RpcpT[ipart]   = fTracks_RpcPt ;
	    Rpctime[ipart] = (float)trk_ptr->get()->get_rpcclus3time();//fTracks_Rpctime; 

	    //	    printf("good particles: DCA %4.2f pT %4.2f time %4.2f\n",RpcDCA[ipart],RpcpT[ipart],Rpctime[ipart]);	    
	  }
	
	}
	int combi=0;	
	// get best road
	int iroad = Cuts().get_best_road_oo( ipart, muo );
	
	//=== First track variables
	muIDquad0[ipart] = (muo->get_muIDOO_gap0( 0, iroad, ipart )>0) + 2*(muo->get_muIDOO_gap0( 1, iroad, ipart )<0);
	muIDchis0[ipart] = muo->get_muIDOOchi(iroad,ipart);
	muTRhits0 [ipart]= muo->get_muTRhits(ipart);
	




	dS30[ipart] = Tools::DS3(muo,ipart, iroad);
	DG0[ipart] = Tools::DG0(muo,ipart, iroad);

	DDG0[ipart] = Tools::DDG0(muo,ipart, iroad);
	dS3ctp0[ipart] = Tools::DS3ctp(muo,ipart, iroad );
	DS0[ipart] = Tools::DS0(muo,ipart, iroad );
	
	
	//		cout  << "DS0"<< DS0[ipart];// << endl;
	for(int igap=4; igap>0; igap--)
	  {
	    if (muo->is_muIDOOhit( iroad, ipart, igap, 0) || muo->is_muIDOOhit( iroad, ipart, igap, 1 )) 
	      {
		lastGap[ipart] = igap;
		break;
	      }
	  }
	
	// fit single track together with BBC vertex
	double px_vertex( muo->get_px(0,ipart) );
	double py_vertex( muo->get_py(0,ipart) );
	double pz_vertex( muo->get_pz(0,ipart) );
	



	// add J/psi,Z,cosmic track
	if (npart[0]>1){
	  for (int ipart2=0; ipart2<npart[0]; ipart2++) {
	    if(ipart2==ipart)
	      continue;

	    int iroad2 = Cuts().get_best_road_oo( ipart2, muo );
	    int lastGap2 = 0;
	    //		cout  << "DS0"<< DS0[ipart];// << endl;
	    for(int igap2=4; igap2>0; igap2--)
	      {
		if (muo->is_muIDOOhit( iroad2, ipart2, igap2, 0) || muo->is_muIDOOhit( iroad2, ipart2, igap2, 1 )) 
		  {
		    lastGap2 = igap2;
		    break;
		  }
	      }

	    if (lastGap2<4)
	      continue;
	    // printf("multip tracks %d ",combi); 
	    combi++;

	    //maybe needed for track cuts on jpsi candidates	 
	    /*   
	    // get best road





	    //=== First track variables
	    double 	muIDquad02 = (muo->get_muIDOO_gap0( 0, iroad2, ipart2 )>0) + 2*(muo->get_muIDOO_gap0( 1, iroad2, ipart2 )<0);
	    double muIDchis02 = muo->get_muIDOOchi(iroad2,ipart2);
	    double muTRhits02= muo->get_muTRhits(ipart2);
	    
	    
	    
	    
	    
	    double dS302 = Tools::DS3(muo,ipart2, iroad2);
	    double DG02 = Tools::DG0(muo,ipart2, iroad2);
	    
	    double DDG02 = Tools::DDG0(muo,ipart2, iroad2);
	    double dS3ctp02 = Tools::DS3ctp(muo,ipart2, iroad2 );
	    double DS02 = Tools::DS0(muo,ipart2, iroad2 );
	    
	    */
	    
	    
	    double px_vertex2( muo->get_px(0,ipart2) );
	    double py_vertex2( muo->get_py(0,ipart2) );
	    double pz_vertex2( muo->get_pz(0,ipart2) );
	    
	    double e = sqrt((px_vertex * px_vertex) + (py_vertex * py_vertex) +
			    (pz_vertex * pz_vertex) + 0.106 * 0.106);
	    
	    double e2 = sqrt((px_vertex2 * px_vertex2) + (py_vertex2 * py_vertex2) +
			     (pz_vertex2 * pz_vertex2) + 0.106 * 0.106);
	    
	    
	    double mass = sqrt( (e+e2)*(e+e2) -
				(px_vertex+px_vertex2) * (px_vertex+px_vertex2) -
(py_vertex+py_vertex2) * (py_vertex+py_vertex2) -
				(pz_vertex+pz_vertex2) * (pz_vertex+pz_vertex2)); 

	    double thrust = ((px_vertex * px_vertex2) 
			     + (py_vertex * py_vertex2) 
			     + (pz_vertex * pz_vertex2)) / 
	      (sqrt((px_vertex * px_vertex) + 
		    (py_vertex * py_vertex) +
		    (pz_vertex * pz_vertex)) * 
	       sqrt ((px_vertex2 * px_vertex2) + 
		     (py_vertex2 * py_vertex2) +			
		     (pz_vertex2 * pz_vertex2)));

	    
	    float sign = 1.0;
	    if (pz_vertex * pz_vertex2 < 0 ) 
	      sign =-1.0; 

	    if(combi==1){
	      invmass1[ipart]=sign * mass;
	      icharge1[ipart] = muo->get_charge(ipart) +  muo->get_charge(ipart2);	      
	      thrust1[ipart]=thrust;

	    } 
	    else if(combi==2){
	      invmass2[ipart]=sign * mass;	    
	      icharge2[ipart] = muo->get_charge(ipart) +  muo->get_charge(ipart2);
	      thrust2[ipart]=thrust;
	    }
	    else if(combi==3){
	      invmass3[ipart]=sign * mass;	    
	      icharge3[ipart] = muo->get_charge(ipart) +  muo->get_charge(ipart2);
	      thrust3[ipart]=thrust;
	    }
	    //printf("mass %4.2f \n",sign * mass);

	  }//ipart2
	  
	  
	  
	} //more than one track
	//printf("original DCA_r %4.2f, mass %4.2f\n",Tools::dist_zvtx(muo,ipart),invmass1[ipart]);


	
	muTRhits[ipart] = muo->get_muTRhits(ipart);
	muIDhits[ipart] = 999;






	

	//	cout << iroad << " roads " << endl;

	if( iroad >= 0 ){ muIDhits[ipart] = muo->get_muIDOOhits(iroad, ipart); }
	

	 mutr_nhits[ipart] = Tools::sumbit(muTRhits[ipart]);
	 muid_nhits[ipart] = Tools::sumbit(muIDhits[ipart]);
	
	 px[ipart] =  px_vertex;
	 py[ipart] =  py_vertex;
	 pz[ipart] =  pz_vertex;
	
	 xSTI[ipart]  = muo->get_xpos(1,ipart);
	 ySTI[ipart]  = muo->get_ypos(1,ipart);
	 zSTI[ipart]  = muo->get_zpos(1,ipart);
	 pxSTI[ipart] = muo->get_px(1,ipart);
	 pySTI[ipart] = muo->get_py(1,ipart);
	 pzSTI[ipart] = muo->get_pz(1,ipart);
	
	 xSTII[ipart]  = muo->get_xpos(2,ipart);
	 ySTII[ipart]  = muo->get_ypos(2,ipart);
	 zSTII[ipart]  = muo->get_zpos(2,ipart);
	
	 xSTIII[ipart]  = muo->get_xpos(3,ipart);
	 ySTIII[ipart]  = muo->get_ypos(3,ipart);
	 zSTIII[ipart]  = muo->get_zpos(3,ipart);
	 pxSTIII[ipart] = muo->get_px(3,ipart);
	 pySTIII[ipart] = muo->get_py(3,ipart);
	 pzSTIII[ipart] = muo->get_pz(3,ipart);
	
	 pT[ipart] = sqrt(px[ipart]*px[ipart]+py[ipart]*py[ipart]);
	 p[ipart] = sqrt(px[ipart]*px[ipart]+py[ipart]*py[ipart]+pz[ipart]*pz[ipart]);
	 pSTI[ipart] = sqrt(pxSTI[ipart]*pxSTI[ipart] + pySTI[ipart]*pySTI[ipart] + pzSTI[ipart]*pzSTI[ipart]);
	 ELoss[ipart] = p[ipart] - pSTI[ipart];


	

	// calculate track and road positions at rpcs 

	 if(pz[ipart]>0){
	   rpc3z[ipart] = 906.3;
	   rpc2z[ipart] = 690.2;
	 }
	 else{
	   rpc3z[ipart] = -906.3;
	   rpc2z[ipart] = -690.2;
	 }

	rpc3st3[ipart] = -zSTIII[ipart] + rpc3z[ipart];
	rpc2st3[ipart] = -zSTIII[ipart] + rpc2z[ipart];
	rpc2g5[ipart] = -gap0z[ipart] + rpc2z[ipart];
	rpc3g5[ipart] = -gap0z[ipart] + rpc3z[ipart];








	//	if ( fabs(rpc2g5[ipart]) < 250 ){
	if ( fabs(rpc2g5[ipart]) < 25000 ){

	  /*	  	  cout << "3st3 "
	       << rpc3st3[ipart] << "2st3 " 
	       << rpc2st3[ipart] << "2gap0 " 
	       << rpc2g5[ipart]  << "3gap3 " 
	       << rpc3g5[ipart] <<endl;
	  */  


		  //	RPC2_x[ipart] = ( pxSTIII[ipart]/pzSTIII[ipart]) * rpc2st3[ipart] + xSTIII[ipart];
		  //RPC2_y[ipart] = ( pySTIII[ipart]/pzSTIII[ipart]) * rpc2st3[ipart] + ySTIII[ipart];
	RPC3_x[ipart] = ( pxSTIII[ipart]/pzSTIII[ipart]) * rpc3st3[ipart] + xSTIII[ipart];
	RPC3_y[ipart] = ( pySTIII[ipart]/pzSTIII[ipart]) * rpc3st3[ipart] + ySTIII[ipart];


	RPC3r_x[ipart] = dirX[ipart] * rpc3g5[ipart] + gap0x[ipart];
	RPC3r_y[ipart] = dirY[ipart] * rpc3g5[ipart] + gap0y[ipart];
	//RPC2r_x[ipart] = dirX[ipart] * rpc2g5[ipart] + gap0x[ipart];
	//RPC2r_y[ipart] = dirY[ipart] * rpc2g5[ipart] + gap0y[ipart];


	//	cout <<" x" <<  RPC3_x[ipart] << "y "<< RPC3_y[ipart]<<endl;


	}


	
	// single muon v2
	//float phi0 = atan2(py[ipart],px[ipart]);
	//	float dphi = phi0 - RPbbcrp12[ipart];
	//dphi = 0.5*atan2(sin(2*dphi),cos(2*dphi));
	//	float v2_obs = cos(2*dphi);
	
	//float dphi_N = phi0 - RPbbcrp10[ipart];
	//dphi_N = 0.5*atan2(sin(2*dphi_N),cos(2*dphi_N));
	//	float v2_obs_N = cos(2*dphi_N);
	
	//float dphi_S = phi0 - RPbbcrp11[ipart];
	//dphi_S = 0.5*atan2(sin(2*dphi_S),cos(2*dphi_S));
	//	float v2_obs_S = cos(2*dphi_S);
	

	 pseudotrigS_1D[ipart]   = -1;
	 pseudotrigS_1S[ipart]   = -1;
	 pseudotrigN_1D[ipart]   = -1;
	 pseudotrigN_1S[ipart]   = -1;
	 recoS_1D[ipart] = -1;
	 recoS_1S[ipart] = -1;
	 recoN_1D[ipart] = -1;
	 recoN_1S[ipart] = -1;



	
	if (_choice == "simu" || _choice == "simu_file") {
	  pseudotrigN_1S[ipart] =  Tools::LL1_1S_Decision( MUTOO::North );
	  pseudotrigN_1D[ipart] =  Tools::LL1_1D_Decision( MUTOO::North );
	  
	  pseudotrigS_1S[ipart] =  Tools::LL1_1S_Decision( MUTOO::South );
	  pseudotrigS_1D[ipart] =  Tools::LL1_1D_Decision( MUTOO::South );
	}


	double prim_x=-90;
	//	double prim_y=-90;
	
	//for MC evaluation
	//We try to get the major MC particle's information
	//major MC particle == MC particle contributes the most to muTr hits
	
	//may need to rethink about this "major MC" approach for tracks coming from decay
	//inside muTr volume        03/05/2005  MXL
	
	
	MC_N_PART[ipart] =0;   // total # of MC particles associated with this reco'd track
	 MC_PX[ipart]=-999;        // the major MC particle's px
	 MC_PY[ipart]=-999;
	 MC_PZ[ipart]=-999;
	 MC_PTOT[ipart] =-999;      // the major MC particle's lastGap
	 MC_X[ipart]=-999;         // the major MC particle's X_orign
	 MC_Y[ipart]=-999;         // the major MC particle's Y_orign
	 MC_Z[ipart]=-999;         // the major MC particle's Z_orign
	 MC_PID[ipart] =-999;
	 MC_HITS[ipart] =0;     // the major MC particle's total contribution to muTr hits
	
	 MC_P_PID[ipart] =-999;    // MC particle's parent information
	 MC_P_PX[ipart] =-999;     //
	 MC_P_PY[ipart] =-999;
	 MC_P_PZ[ipart] =-999;
	 MC_P_PTOT[ipart] =-999;
	 MC_P_Z[ipart] =-999;
	
	 MC_D_PID[ipart] =-999;    // MC particle's daughter information
	 MC_D_PX[ipart] =-999;     //
	 MC_D_PY[ipart] =-999;
	 MC_D_PZ[ipart] =-999;
	 MC_D_PTOT[ipart] =-999;
	 MC_D_Z[ipart] =-999;
	 MC_D_N[ipart] =-999;
	
	 MC_G_PID[ipart] =-999;    // MC particle's grandparent (ancestor, original, very primary) informationi
	 MC_G_PX[ipart] =-999;     //
	 MC_G_PY[ipart] =-999;
	 MC_G_PZ[ipart] =-999;
	 MC_G_PTOT[ipart] =-999;
	 MC_G_Z[ipart] =-999;
	
	MC_TRK[ipart]   = -999;
	MC_P_TRK[ipart] = -999;    // parent track ID: for debuggin now
	MC_D_TRK[ipart] = -999;    // daughter track ID: for debuggin now
	MC_G_TRK[ipart] = -999;    // grandparent track ID: for debuggin now






	//	printf("now lets look  through tracks and hits and residuals\n");
	
	//how to associate a PHMuonTrack with a MutTrk? St1-P?
	//
	//	if (!mut_trk_map) cout << "no mut_trk_map " << endl;
	//if (!mut_mctrk_map) cout << "not mut_mctrk_map "  << endl;




	if (mut_trk_map) {

	  //	  printf(" in mut_trk map\n"); 
	  double res[16]={-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.};
	  int nhits1=0;
	  int nhits2=0;
	  int nhits3=0;
	  int allnhits1=0;
	  int allnhits2=0;
	  int allnhits3=0;
	  int armnumber=0;

	  int nhits=0;
	  
	  
	  TMutTrkMap::const_iterator trk_iter = mut_trk_map->range();
	  //printf(" starting mut_trk map loop\n"); 
	  
	  while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
	    {
	      //
	      //pick up the right reconstructed track
	      //
	      float	    p_xx  = trk_ptr->get()->get_trk_par_vtx()->get_px();
	      float	    p_yy  = trk_ptr->get()->get_trk_par_vtx()->get_py();
	      float	    p_zz  = trk_ptr->get()->get_trk_par_vtx()->get_pz();
	      //float	ptot_x  = trk_ptr->get()->get_trk_par_vtx()->get_ptot();
	      
	      float pxv =  muo->get_px(0,ipart);            // muon px at Vertex
	      float pyv =  muo->get_py(0,ipart);            // muon px at Vertex
	      float pzv =  muo->get_pz(0,ipart);            // muon px at Vertex
	      
	      float diff_x = 999;
	      diff_x = (pxv-p_xx)*(pxv-p_xx) + (pyv-p_yy)*(pyv-p_yy) + (pzv-p_zz)*(pzv-p_zz);
	      //cout << " >>>>>>   diff_x == " << diff_x << endl;
	      if (diff_x > 0.000001) continue; // not the same track


	      //try residual
	      //	      trk_ptr->get()->print();


	      TMutTrk *trk4 = trk_ptr->get();
	      //	      std::vector<TMutTrkRes> residual_list;
	      TMutHitMap* hit_map1 =  TMutNode<TMutHitMap>::find_node( _top_node,"TMutHitMap" );
	      //printf("chi2 %4.2f \n",	      trk4->get_w_chi_square());
	      //printf("test before residual, next line is print:\n");
	      //	      trk4->get_w_residual_list()->begin()->print();
	      //printf("test after residual, next line is w:\n");
	      //printf(" w and wmeas is %4.2f %4.2f diff %4.2f\n",trk4->get_w_residual_list()->begin()->get_w_trk(),trk4->get_w_residual_list()->begin()->get_w_meas(),trk4->get_w_residual_list()->begin()->get_w_trk()-trk4->get_w_residual_list()->begin()->get_w_meas());
	      //	      const residual_list  =   trk4->get_w_residual_list(); 

	      //	      residual_iter->print();
	      //printf("size .. %d\n",(trk4->get_w_residual_list())->size());
	      //	      int size = (trk4->get_w_residual_list())->size();
	      //	      int testt=0;
	      //while (size>0){
		//		printf(" w and wmeas is %4.2f %4.2f diff %4.2f\n",trk4->get_w_residual_list()[testt]->get_w_trk(),trk4->get_w_residual_list()[testt]->get_w_meas(),trk4->get_w_residual_list()[testt]->get_w_trk()-trk4->get_w_residual_list()[testt]->get_w_trk()->get_w_meas());      
	      //	size--;
	      //}
	      std::vector<TMutTrkRes>::const_iterator residual_iter = (trk4->get_w_residual_list())->begin();

	      int testt=0;
	      for(;residual_iter != (trk4->get_w_residual_list())->end(); ++residual_iter){ 
		//residual_iter->print();
		testt = residual_iter->get_station() * 6 +  residual_iter->get_gap() * 2 + residual_iter->get_cathode();
		res[testt]=residual_iter->get_w_trk()-residual_iter->get_w_meas();
		//  	printf("testt %d  a %d s %d o %d g %d c %d  w and wmeas is %4.2f %4.2f diff %4.2f\n",testt,residual_iter->get_arm(),residual_iter->get_station(),residual_iter->get_octant(),residual_iter->get_gap(),residual_iter->get_cathode(),residual_iter->get_w_trk(),residual_iter->get_w_meas(),residual_iter->get_w_trk()-residual_iter->get_w_meas());      
		


		//		TMutHitMap::key_iterator hit_iter = trk4->get_associated<TMutHit>();//(residual_iter->get_arm(),residual_iter->get_station(),residual_iter->get_octant(),residual_iter->get_gap(),residual_iter->get_cathode()); 



		
		
		TMutHitMap::iterator hit_iter = hit_map1->get(residual_iter->get_arm(),residual_iter->get_station(),residual_iter->get_octant(),residual_iter->get_half_octant(),residual_iter->get_gap(),residual_iter->get_cathode());//range();
		
		
		while(hit_iter.next()){ 
		  if(residual_iter->get_station()==0)
		    nhits1++;
		  if(residual_iter->get_station()==1)
		    nhits2++;
		  if(residual_iter->get_station()==2)
		    nhits3++;

		}

		//		printf("total hits are %d, %d, %d, range %d\n",nhits1,nhits2,nhits3,hit_map1->size());
		nhits=hit_map1->size();
		//		  trk4->get(residual_iter->get_arm(),residual_iter->get_station(),residual_iter->get_octant(),residual_iter->get_gap(),residual_iter->get_cathode()); 
		armnumber=  residual_iter->get_arm();

	      }

	      //	      printf("test after residuals, next line is hits\n");


	      // just testing both arms:
	      //    for (int arm=0;arm<2;arm++){
	      for(int sta =0; sta<3;sta++){
		
		for(int oct =0; oct<8;oct++){
		  for(int hoct =0; hoct<2;hoct++){
		  for(int gap =0; gap<((sta<2)?3:2);gap++){
		    for(int cath =0; cath<2;cath++){
		      //printf("sta %d oct %d hoct %d gap %d cat%d \n",sta,oct,hoct,gap,cath);		      
		      TMutHitMap::iterator hit_iter2 = hit_map1->get(armnumber,sta,oct,hoct,gap,cath);//range();
		      //TMutHitMap::iterator hit_iter2 = hit_map1->get(arm,sta,oct,hoct,gap,cath);//range();
		      
		      
		      while(hit_iter2.next()){ 
			if(sta==0)
			  allnhits1++;
			if(sta==1)
			  allnhits2++;
			if(sta==2)
			  allnhits3++;

			//printf(" %d %d %d %d\n", allnhits1,allnhits2,allnhits3,nhits);
			
		      } //while
		    } //cath
		  } //gap
		  } //hoct
		} //oct
	      } //sta
	      // }// arm




	      //finding all hits per plane of a track
	      //		TMutHitMap::key_iterator hit_iter = mc_trk_ptr->get()->get_associated<TMutHit>();
	      //while(hit_iter.next()) mchitx++;


	      /*	int octant =	trk_ptr->get()->get_octant();
	int arm =	trk_ptr->get()->get_arm();

	TMutTrkRes *residual = new TMutTrkRes();
	//arm,0,octant,0,0
	//	int octant =	trk_ptr->get()->get_half_octant();
	//	int station =	muo->get_station();
	//int gap =	muo->get_gap();
	residual->set_octant(octant);
	residual->set_octant(octant);
	residual->set_arm(arm);
	//	residual->set_half_octant(half_octant);
	residual->set_station(0);
	residual->set_gap(0);
	      */
	    //	      	printf(" w and wmeas is %4.2f %4.2f diff %4.2f\n",residual->get_w_trk(),residual->get_w_meas(),residual->get_w_trk()-residual->get_w_meas());

	      //	      printf("finished hits\n");
	      break;

	    }

    res100[ipart]=res[0];
    res101[ipart]=res[1];
    res110[ipart]=res[2];
    res111[ipart]=res[3];
    res120[ipart]=res[4];
    res121[ipart]=res[5];
    res200[ipart]=res[6];
    res201[ipart]=res[7];
    res210[ipart]=res[8];
    res211[ipart]=res[9];
    res220[ipart]=res[10];
    res221[ipart]=res[11];
    res300[ipart]=res[12];
    res301[ipart]=res[13];
    res310[ipart]=res[14];
    res311[ipart]=res[15];


    station1hits[ipart]=nhits1;
    station2hits[ipart]=nhits2;
    station3hits[ipart]=nhits3;

    allstation1hits[ipart]=allnhits1;
    allstation2hits[ipart]=allnhits2;
    allstation3hits[ipart]=allnhits3;

    allhits[ipart]=nhits;
    

	}
	
	if (mut_trk_map && mut_mctrk_map) {
	  
	  TMutTrkMap::const_iterator trk_iter = mut_trk_map->range();
	  
	  while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
	    {
	      //
	      //pick up the right reconstructed track
	      //
	      float	    p_xx  = trk_ptr->get()->get_trk_par_vtx()->get_px();
	      float	    p_yy  = trk_ptr->get()->get_trk_par_vtx()->get_py();
	      float	    p_zz  = trk_ptr->get()->get_trk_par_vtx()->get_pz();
	      //float	ptot_x  = trk_ptr->get()->get_trk_par_vtx()->get_ptot();
	      
	      float pxv =  muo->get_px(0,ipart);            // muon px at Vertex
	      float pyv =  muo->get_py(0,ipart);            // muon px at Vertex
	      float pzv =  muo->get_pz(0,ipart);            // muon px at Vertex
	      
	      float diff_x = 999;
	      diff_x = (pxv-p_xx)*(pxv-p_xx) + (pyv-p_yy)*(pyv-p_yy) + (pzv-p_zz)*(pzv-p_zz);
	      //	    cout << " >>>>>>   diff_x == " << diff_x << endl;
	      if (diff_x > 0.000001) continue; // not the same track



	      
	      //found the associated MutTrk
	      MC_N_PART[ipart]=-999;
	      MC_PX[ipart]=-999;        // the major MC particle's px
	      MC_PY[ipart]=-999;
	      MC_PZ[ipart]=-999;
	      MC_PTOT[ipart] =-999;      // the major MC particle's lastGap
	      MC_X[ipart]=-999;         // the major MC particle's X_orign
	      MC_Y[ipart]=-999;         // the major MC particle's Y_orign
	      MC_Z[ipart]=-999;         // the major MC particle's Z_orign
	      MC_PID[ipart] =-999;
	      MC_HITS[ipart] =-999;     // the major MC particle's total contribution to muTr hits
	      
	      MC_P_PID[ipart] =-999;    // MC particle's parent information
	      MC_P_PX[ipart] =-999;     //
	      MC_P_PY[ipart] =-999;
	      MC_P_PZ[ipart] =-999;
	      MC_P_PTOT[ipart] =-999;
	      MC_P_Z[ipart] =-999;
	      
	      MC_D_PID[ipart] =-999;    // MC particle's daughter information
	      MC_D_PX[ipart] =-999;     //
	      MC_D_PY[ipart] =-999;
	      MC_D_PZ[ipart] =-999;
	      MC_D_PTOT[ipart] =-999;
	      MC_D_Z[ipart] =-999;
	      MC_D_N[ipart] =-999;
	      
	      MC_G_PID[ipart] =-999;    // MC particle's parent information
	      MC_G_PX[ipart] =-999;     //
	      MC_G_PY[ipart] =-999;
	      MC_G_PZ[ipart] =-999;
	      MC_G_PTOT[ipart] =-999;
	      MC_G_Z[ipart] =-999;
	      
	      MC_TRK[ipart]   = -999;
	      MC_P_TRK[ipart] = -999;    // parent track ID: for debuggin now
	      MC_D_TRK[ipart] = -999;    // daughter track ID: for debuggin now
	      MC_G_TRK[ipart] = -999;    // grandparent track ID: for debuggin now
	      
	      //----------------
	      //get associated MC tracks information
	      //----------------
	      TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();
	      
	      // Loop over associated MCTracks
	      mc_trk_iter.reset(); // reset ???
	      while (TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()) {
		
		MC_N_PART[ipart]++;
		
		MC_PX[ipart]  = mc_trk_ptr->get()->get_px_orig();
		MC_PY[ipart]  = mc_trk_ptr->get()->get_py_orig();
		MC_PZ[ipart]  = mc_trk_ptr->get()->get_pz_orig();
		//	      MC_GAP = mc_trk_ptr->get()->get_depth();
		MC_PTOT[ipart] = sqrt(MC_PX[ipart]*MC_PX[ipart] + MC_PY[ipart]*MC_PY[ipart] + MC_PZ[ipart]*MC_PZ[ipart]);
		
		MC_X[ipart] = mc_trk_ptr->get()->get_x_orig();
		MC_Y[ipart] = mc_trk_ptr->get()->get_y_orig();
		MC_Z[ipart] = mc_trk_ptr->get()->get_z_orig();
		MC_PID[ipart] = mc_trk_ptr->get()->get_pid();
		MC_P_PID[ipart] = mc_trk_ptr->get()->get_parent_id();
		MC_TRK[ipart]   = mc_trk_ptr->get()->get_track_id();
		
		//
		//get associated MCHits information
		//
		TMutMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
		//how many hits from this MC particle ?
		MC_HITS[ipart] = mc_hit_iter.count();
		//cout << "thetr are \t " << MC_HITS << " \t MC hits from this MC particle" << endl;
		
		float mchitx = 0;
		while(mc_hit_iter.next()) mchitx++;
		//	      MC_HITS = mchitx;
		
		Int_t trk_IDx = -99;
		Int_t trk_IDxP = -99;
		
		//reset parent  MC information
		MC_P_PID[ipart] =-999;    // MC particle's parent information
		MC_P_PX[ipart]  =-999;     //
		MC_P_PY[ipart]  =-999;
		MC_P_PZ[ipart]  =-999;
		MC_P_PTOT[ipart]=-999;
		MC_P_Z[ipart]   =-999;
		MC_P_TRK[ipart] =-999;    // parent track ID: for debuggin now
		
		//
            //get 1st parent information - PID, P, charge etc.
            //
		MC_P_TRK[ipart] = mc_trk_ptr->get()->get_parent_track_id();
		
		TMutMCTrkMap::const_iterator mc_trk_iter_1p = mut_mctrk_map->range();
		
		while (TMutMCTrkMap::const_pointer mc_trk_ptr_1p = mc_trk_iter_1p.next()) {
		  //for debugging
		  //get the track id index
		  trk_IDx  = mc_trk_ptr_1p->get()->get_track_id();  // trk_id ==0 for the very primary particle
		  
		  /*
		    std::cout << "in the MCbank,  MC track id trk_IDx == " << trk_IDx;
		    cout << "  pz  ="     <<  mc_trk_ptr_1p->get()->get_pz_orig() ;
		    cout << "  z_orig  =" <<  mc_trk_ptr_1p->get()->get_z_orig() ;
		    cout << "  pID ="     <<  mc_trk_ptr_1p->get()->get_pid() << endl;
		  */
		  //end of debugging
		  
		  //
		  //currently  only MCtracks with hits deposited in muTr are kept in TMutMCTrkMap
		  //need to expand the map to include all MC particles   MXL 03/15/2005
		  //
		  
		  if ( MC_P_TRK[ipart] == trk_IDx ){ // found the parent MC track
		    // Float_t prnt_IDx = mc_trk_ptr_1p->get()->get_pid();
		    
		    MC_P_PID[ipart]= mc_trk_ptr_1p->get()->get_pid();
		    MC_P_PX[ipart] = mc_trk_ptr_1p->get()->get_px_orig();
		    MC_P_PY[ipart] = mc_trk_ptr_1p->get()->get_py_orig();
		    MC_P_PZ[ipart] = mc_trk_ptr_1p->get()->get_pz_orig();
		    MC_P_PTOT[ipart] = sqrt(MC_P_PX[ipart]*MC_P_PX[ipart] + MC_P_PY[ipart]*MC_P_PY[ipart] + MC_P_PZ[ipart]*MC_P_PZ[ipart]);
		    MC_P_Z[ipart] = mc_trk_ptr_1p->get()->get_z_orig();
		  } // found parent track ID
		  
		  //
		  //if no parent found, then the current MC is the primary particle
		  //set parent = current mc, but set mc_p_ptot = - value so we can tell it from others
	      //
		  if (MC_P_PID[ipart]==-999) {
		    MC_P_PID[ipart]= mc_trk_ptr->get()->get_pid();
		    MC_P_PX[ipart] = mc_trk_ptr->get()->get_px_orig();
		    MC_P_PY[ipart] = mc_trk_ptr->get()->get_py_orig();
		    MC_P_PZ[ipart] = mc_trk_ptr->get()->get_pz_orig();
		    MC_P_PTOT[ipart] = -sqrt(MC_P_PX[ipart]*MC_P_PX[ipart] + MC_P_PY[ipart]*MC_P_PY[ipart] + MC_P_PZ[ipart]*MC_P_PZ[ipart]);
		    MC_P_Z[ipart] = mc_trk_ptr->get()->get_z_orig();
		  }
		  
		  //
		  //daughter particle - exclude gamma(1), electrons(2+,3-), pi0(7);
		  //
		  
		  //reset daughter  MC information
		  MC_D_PID[ipart] =-999;    // MC particle's daughter information
		  MC_D_PX[ipart]  =-999;     //
		  MC_D_PY[ipart]  =-999;
		  MC_D_PZ[ipart]  =-999;
		  MC_D_PTOT[ipart]=-999;
		  MC_D_Z[ipart]   =-999;
		  MC_D_N[ipart]   =-999;
		  
		  MC_D_TRK[ipart] =-999;    // daughter track ID: for debuggin now
		  MC_D_TRK[ipart] = mc_trk_ptr->get()->get_track_id(); // current MC particle's trk_ID = daughter's parent trk_id
		  trk_IDxP = mc_trk_ptr_1p->get()->get_parent_track_id();  // trk_id ==0 for the very primary particle
		  
		  MC_D_N[ipart]=0;
		  if ( MC_D_TRK[ipart] == trk_IDxP ){ // found the daughter MC track
		    
		    MC_D_N[ipart]++; // count how many daughter particles we have
		    
		    Int_t MC_D_PIDx= mc_trk_ptr_1p->get()->get_pid();
		    
		    // always pick up the MC decay vtx
		    MC_D_Z[ipart] = mc_trk_ptr_1p->get()->get_z_orig();
		    
		    if (MC_D_PIDx!=1&&MC_D_PIDx!=2&&MC_D_PIDx!=3&&MC_D_PIDx!=7){
		      MC_D_PID[ipart]= mc_trk_ptr_1p->get()->get_pid();
		      MC_D_PX[ipart] = mc_trk_ptr_1p->get()->get_px_orig();
		      MC_D_PY[ipart] = mc_trk_ptr_1p->get()->get_py_orig();
		      MC_D_PZ[ipart] = mc_trk_ptr_1p->get()->get_pz_orig();
		      MC_D_PTOT[ipart] = sqrt(MC_D_PX[ipart]*MC_D_PX[ipart] + MC_D_PY[ipart]*MC_D_PY[ipart] + MC_D_PZ[ipart]*MC_D_PZ[ipart]);
		    }
		  } // found daughter track ID
		  
		  // get mc_primary track info.
		  if (_mc_primary_map) {
		    
		    //cout << "_mc_primary_map found, size " << _mc_primary_map->size() << endl;
		    TMCPrimaryMap::const_iterator prim_iter = _mc_primary_map->range();
		    
		    MC_G_TRK[ipart] = mc_trk_ptr->get()->get_grandparent_track_id();
		    //cout << "grandparent_track_id " << MC_G_TRK[ipart] << endl;
		    
		    while( TMCPrimaryMap::const_pointer prim_ptr = prim_iter.next() )
		      {
			Int_t trk_IDx = -99;
			trk_IDx  = prim_ptr->get()->get_trk_id();
			
			if ( MC_G_TRK[ipart] == trk_IDx ){ // found the parent MC track
			  
			  MC_G_PID[ipart]  = prim_ptr->get()->get_pid();
			  MC_G_PX[ipart]   = prim_ptr->get()->get_px_orig();
			  MC_G_PY[ipart]   = prim_ptr->get()->get_py_orig();
			  MC_G_PZ[ipart]   = prim_ptr->get()->get_pz_orig();
			  MC_G_PTOT[ipart] = prim_ptr->get()->get_ptot_orig();
			  MC_G_Z[ipart]    = prim_ptr->get()->get_z_orig();
			  prim_x = prim_ptr->get()->get_x_orig();
			  //			  prim_y = prim_ptr->get()->get_y_orig();
			  
			  /*  cout << "PRIMARY pID " << prim_ptr->get()->get_pid() << " tID " << prim_ptr->get()->get_trk_id() << endl
			      << "        vertex (" << prim_ptr->get()->get_x_orig() << ", " << prim_ptr->get()->get_y_orig()
			      << ", " << prim_ptr->get()->get_z_orig() << " )"<< endl
			      << "        mom    (" << prim_ptr->get()->get_px_orig() << ", " << prim_ptr->get()->get_py_orig()
			      << ", " << prim_ptr->get()->get_pz_orig() << " )" << endl;
			  */
			}
		      }
		  }
		  else {
		    //cout << "mc_primary_map not found " << endl;
		  }
		} // loop MC track bank for the 1st parent and daughter
	      } // loop MC trackMap bank for the reco associated track
	    } //end of TMutTrk loop
	} // end of if (mut_trk_map && mut_mctrk_map) loop
	
	

	bool do_refit( true );

	MWGVertex vertex;
	double z_vertex( 0 );
	if (_framework == MUTOO && do_refit )	try {
	  
	  vertex.set_verbosity( 0 );
	  
	  // add track
	  vertex.add_track( ipart, muo );
	  
	  // add vertex information
	  
	  double z_vertex_error( 0 );
	  //vertex always from gaussion here...
	  if( (_choice == "simu" || _choice == "simu_file") && ( abs (muo->get_zpos(0,0))<120) ) {
	    
	    // retrieve vertex from pisa header file or first track
	    if( header ) {
	      //cout << "in header for vertex new" << endl;
	      bool error( true );
	      z_vertex = Tools::zVertexMC( header, error );
	      if( error ) z_vertex = muo->get_zpos(0,0);
	    } else z_vertex = muo->get_zpos(0,0);
	    // vertex error (from MC) is hard coded to 2cm
	    z_vertex_error = 2;
	    
	  } 
	  else if( evt  && ( abs (muo->get_zpos(0,0))<120) ) 
	    {
	      //cout << "in evt for vertex" << endl;
	      // retrieve vertex from BBC
	      z_vertex = evt->getBbcZVertex();

	      //      z_vertex = (float)gRandom->Gaus(0.,30.); // just for the cosmics now...and the cheated fake hig
	      //cout << "vertex from bbc is  "<<z_vertex << endl;
	      /*
		up to now, found no way to retrieve bbc vertex error
		from PHGlobal. Assign a 2cm error
	      */
	      z_vertex_error = 2;
	    }
	  else if( fabs(MC_G_Z[ipart])>130) 
	    {
	      //cout << "in evt for vertex with cheated particles, z gen at "<< MC_G_Z[ipart] << endl;
	      // retrieve vertex from BBC
	      //z_vertex = evt->getBbcZVertex();
	      
	      z_vertex = prim_x * MC_G_PZ[ipart]/MC_G_PX[ipart] - MC_G_Z[ipart];  
	      //cout << "gives  "<<z_vertex << "check" << prim_y * MC_G_PZ[ipart]/MC_G_PY[ipart] - MC_G_Z[ipart] <<   endl;
	      /*
		up to now, found no way to retrieve bbc vertex error
		from PHGlobal. Assign a 2cm error
	      */
	      z_vertex_error = 2;
	    }
	  
	  vertex.add_vertex( z_vertex, z_vertex_error );
	  
	  // refit muon tracks with the correct event vtx (BBC_vtx for real one and PISA_vtx for sim ) fit
	  vertex.fit();
	  
	  // retrieve track momentum
	  //	  px_vertex = vertex.get_px( 0 );
	  //py_vertex = vertex.get_py( 0 );
	  //pz_vertex = vertex.get_pz( 0 );
	} catch( std::exception &e ) {
	  cout << e.what() << endl;
	}


	Float_t r_dca,x_dca,ydca,z_dca;
	Tools::DCA(muo,ipart,r_dca,x_dca,ydca,z_dca);
	DCA_r[ipart]=Tools::dist_zvtx(muo,ipart);
	DCA_z[ipart]=fabs(z_dca-z_vertex);
	//printf("refit DCA_r %4.2f  z_vertex %4.2f",Tools::dist_zvtx(muo,ipart),z_vertex);

	//cout << " DCA_r "<< DCA_r[ipart]<< " DCA_z "  << DCA_z[ipart];
  //Determine if kalman projection to gap4 is available.
  int lastIndex = 4;
  if (muo->get_xpos(4,ipart) == 0 && muo->get_ypos(4,ipart) == 0 && muo->get_zpos(4,ipart) == 0)
    lastIndex = 3;
  
  Float_t x_mut = muo->get_xpos(lastIndex,ipart);
  Float_t y_mut = muo->get_ypos(lastIndex,ipart);    
  Float_t z_mut = muo->get_zpos(lastIndex,ipart);
  
  Float_t dxdz_mut = muo->get_px(lastIndex,ipart)/muo->get_pz(lastIndex,ipart);
  Float_t dydz_mut = muo->get_py(lastIndex,ipart)/muo->get_pz(lastIndex,ipart);
    
	float mini_z = muo->get_zpos(4,ipart);	  

	gap0x[ipart] = muo->get_muIDOO_gap0(0, iroad, ipart);
	gap0y[ipart] = muo->get_muIDOO_gap0(1, iroad, ipart);
	gap0z[ipart] = muo->get_muIDOO_gap0(2, iroad,ipart);
	dirX[ipart]  = muo->get_muIDOO_gap0(3, iroad,ipart);
	dirY[ipart]  = muo->get_muIDOO_gap0(4, iroad,ipart);
	
	refX[ipart]  = -dirX[ipart]*gap0z[ipart] + gap0x[ipart];
	refY[ipart]  = -dirY[ipart]*gap0z[ipart] + gap0y[ipart];

	Float_t x_mui=gap0x[ipart] + (mini_z - gap0z[ipart]) * dirX[ipart];
	Float_t y_mui=gap0y[ipart] + (mini_z - gap0z[ipart]) * dirY[ipart];
	Float_t z_mui=mini_z;
	
	DG4[ipart] =    sqrt( 
			     MUTOO::SQUARE( x_mui - x_mut - dxdz_mut*(z_mui - z_mut) ) +
			     MUTOO::SQUARE( y_mui - y_mut - dydz_mut*(z_mui - z_mut) )
			     );
	
	//cout << " DG4 "<< DG4[ipart]<< endl;	


	//calculate deflection angle, delta theta, delta phi and road_slope.. put in to the X1,X2...
	//cout << "_choice" << _choice << endl;
	//	cout << "BbcZVertex " << BbcZVertex << endl;
	//cout << "refit_z " << vertex.get_vtx_z() << endl;
	
	if( _choice == "simu" || _choice == "simu_file" ) {
	  BbcZVertex[ipart] = vertex.get_vtx_z();
	}
	

	dANGLE[ipart] =-999;
	dANGLE_xyz[ipart]=-999;
	dPHI[ipart]=-999;
	dTHETA[ipart]=-999;
	ROAD_SLOPE[ipart]=-999;
	
	ROAD_SLOPE[ipart] = sqrt(dirX[ipart]*dirX[ipart]+dirY[ipart]*dirY[ipart]);
	
	xyz_St1 = sqrt(xSTI[ipart]*xSTI[ipart]+ySTI[ipart]*ySTI[ipart]+(zSTI[ipart]-BbcZVertex[ipart])*(zSTI[ipart]-BbcZVertex[ipart]) );
	costheta = (px[ipart]*pxSTI[ipart] + py[ipart]*pySTI[ipart] + pz[ipart]*pzSTI[ipart]) / ( p[ipart]*pSTI[ipart] ) ;
	
	costheta_xyz = (pxSTI[ipart]*xSTI[ipart] + pySTI[ipart]* ySTI[ipart] + pzSTI[ipart]*(zSTI[ipart]-BbcZVertex[ipart]) ) / (pSTI[ipart]*xyz_St1) ;
	
	if ( abs(costheta) < 1.0 ) {
	  dANGLE[ipart] = acos (costheta);
	}else{
	  dANGLE[ipart] = 0.0;
	}
	dANGLE[ipart] = dANGLE[ipart]*0.5*(p[ipart]+pSTI[ipart]);
	
	if ( abs(costheta_xyz) < 1.0 ) {
	  dANGLE_xyz[ipart] = acos (costheta_xyz);
	}else{
	  dANGLE_xyz[ipart] = 0.0;
	}
	dANGLE_xyz[ipart] = dANGLE_xyz[ipart]*0.5*(p[ipart]+pSTI[ipart]);
	
	//cout << "dangle = " << dANGLE << endl;
	//	 cout << "dangle_xyz = " << dANGLE_xyz << endl;
	
	dPHI[ipart] = (atan2(xSTI[ipart],ySTI[ipart])-atan2(xSTIII[ipart],ySTIII[ipart]));
	dTHETA[ipart] = (atan((sqrt((xSTI[ipart])*(xSTI[ipart])+(ySTI[ipart])*(ySTI[ipart])))/(sqrt((zSTI[ipart]-BbcZVertex[ipart])*(zSTI[ipart]-BbcZVertex[ipart]))))-atan((sqrt((xSTIII[ipart])*(xSTIII[ipart])+(ySTIII[ipart])*(ySTIII[ipart])))/(sqrt((zSTIII[ipart]-BbcZVertex[ipart])*(zSTIII[ipart]-BbcZVertex[ipart]))))) ;
	
	//-----------------------------------------------------------------------------
	
	// calculate reference vtx form track
	// the shortest distance between the trk at station 1 and beam line
	// two space points are (xSTI, ySTI, zSTI) and (0, 0, BbcZVertex)
	// two vetctors  are (pxSTI, pySTI, pzSTI) and (0,0,1)
	
	//	Float_t ref_vtx_xdca,ref_vtx_ydca,ref_vtx_zdca;
	

	//	printf("before old dca");

	// total momentum is p and pSTI
	/* x1[2],y1[2],z1[2]; position at station 1 and vtx point */
	
	Float_t a=0,b=0,c=0,d=0; /* equation du plan*/
	Float_t wx=0,wy=0,wz=0;
	Float_t X1=0,X2=0,Y1=0,Y2=0,Z1=0,Z2=0;
	Float_t P1[2],xp[2],yp[2];
	Float_t pp[2][3];
	Float_t x1[2]={0},y1[2]={0},z1[2]={0};
	
	x1[0] = 0.0;
	y1[0] = 0.0;
	z1[0] = BbcZVertex[0];
	x1[1] = muo->get_xpos(1,ipart);
	y1[1] = muo->get_ypos(1,ipart);
	z1[1] = muo->get_zpos(1,ipart);
	
	pp[0][0] = 0.0;
	pp[0][1] = 0.0;
	pp[0][2] = 1.0;
	pp[1][0] = muo->get_px(1,ipart);
	pp[1][1] = muo->get_py(1,ipart);
	pp[1][2] = muo->get_pz(1,ipart);
	/* calcul de z dca */
	for(Int_t k =0;k<2;k++)
	  {
	    P1[k]=pow(pp[k][0],2)+pow(pp[k][1],2)+pow(pp[k][2],2);
	    P1[k]=sqrt(P1[k]);
	    xp[k]=pp[k][0]/pp[k][2];
	    yp[k]=pp[k][1]/pp[k][2];
	  }
	
	/* calcul de w (produit vectoriel) */
	wx = yp[0]-yp[1];
	wy = xp[1]-xp[0];
	wz = xp[0]*yp[1]-xp[1]*yp[0];
	/*calcul des coeff pour eqn plan */
	a = wy-wz*yp[0];
	b = xp[0]*wz-wx;
	c = wx*yp[0]-xp[0]*wy;
	d = - x1[0]*a -y1[0]*b - z1[0]*c;
	
	/* sur la droite 2*/
	Z2 = a*(x1[1]-xp[1]*z1[1]) + b*(y1[1]-yp[1]*z1[1]) + d ;
	Z2 = -  Z2/(a*xp[1]+b*yp[1]+c);
	
	X2 = x1[1] + xp[1]*(Z2 - z1[1]);
	Y2 = y1[1] + yp[1]*(Z2 - z1[1]);
	
	
	Z1 = Z2 - xp[0]*(x1[0] - xp[0]*z1[0]- X2) - yp[0]*(y1[0] - yp[0]*z1[0] - Y2);
	Z1 = Z1/(1 +xp[0]*xp[0] + yp[0]*yp[0] );
	
	X1 = x1[0] + xp[0]*(Z1 - z1[0]);
	Y1 = y1[0] + yp[0]*(Z1 - z1[0]);
	
	//	ref_vtx_zdca =(Z1+Z2)/2; // z DCA vertex !
	//ref_vtx_ydca =(Y1+Y2)/2;
	//ref_vtx_xdca =(X1+X2)/2;
	ref_vtx_rdca[0] = sqrt( pow((X2-X1),2)+pow((Y2-Y1),2)+pow((Z2-Z1),2) );
	
	//distance between bbcZ and Z1;
	ref_vtx_z[ipart] = Z1;
	//  cout << "X1, Y1,Z1 = " << X1 << " " << Y1 << " " <<Z1 << endl;
	//  cout << "X2, Y2,Z2 = " << X2 << " " << Y2 << " " <<Z2 <<  " " << BbcZVertex << endl;
	
	//calculate reference vtx form track
	
	float z_ref = muo->get_zpos(0,ipart);
	
	float x_ref = muo->get_xpos(0,ipart)+( BbcZVertex[ipart]-z_ref)*(px[ipart]/pz[ipart]);
	float y_ref = muo->get_ypos(0,ipart) + ( BbcZVertex[ipart]-z_ref)* (py[ipart]/pz[ipart]);
	
        ref_vtx_r[ipart] = sqrt(x_ref*x_ref + y_ref*y_ref + z_ref*z_ref);
	
	// Calculating pseudo-rapidity
	if (p[ipart] == fabs(pz[ipart])){pseudo_rapidity[ipart] = -999.999;}
	else {pseudo_rapidity[ipart] = 0.5*log((p[ipart]+pz[ipart])/(p[ipart]-pz[ipart]));}


	

	// some of the leftover calls 
	

	charge[ipart] = muo->get_charge(ipart);
	

	muchisquare[ipart]=   muo->get_chisquare(ipart);


	status[ipart] = muo->get_TMutTrk_status(ipart);
	

	ghostflag[ipart] = muo->get_ghostflag(ipart);



	newphi[ipart] = atan2(py[ipart],px[ipart]);

	refit_z[ipart] = vertex.get_vtx_z();

	//cout << " shortly before filling branches" << endl;




  //cout << " done with branches" << endl;
//  fMuonTree->Fill();
  //cout << " filled" << endl;
	accMU++;




	RPC2r_x[ipart] = rpcx; 
	RPC2r_y[ipart] = rpcy;
	
	double aalpha = ( rpcx * RPC3_x[ipart] +  rpcy * RPC3_y[ipart])/(rpcx * rpcx + rpcy * rpcy);

	RPC2_x[ipart] = sqrt((aalpha * rpcx - RPC3_x[ipart]) * (aalpha * rpcx - RPC3_x[ipart])  + (aalpha * rpcy - RPC3_y[ipart]) * (aalpha * rpcy - RPC3_y[ipart]));
	RPC2_y[ipart] = (rpcx>-1000)?1:-9999;

	//	printf("rpcx is %4.2f and y %4.2f and RPC2r %4.2f %4.2f\n",rpcx,rpcy,RPC2r_x[ipart],RPC2r_y[ipart]);
      
      } // for

    //cout << " after tracks loop" << endl;

	TBranch *fBranch = newsngmuons->GetBranch("_RecoTracks");
	((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(&fNMU);
	
	fBranch = newsngmuons->GetBranch("RecoTracks");
	
	//cout << " got track branches" << endl;
  	
  //=== Fill branches
	//    ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(fRunNumber);                       // run number
  //cout << " after runnr" << endl;
	//  ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(EventNumber);                     // event number
  //cout << " after eventnr" << endl;
  ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(npart);                           // number of muons
  ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(charge);         // muon charge
  ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(px);     			  // muon px at Vertex
  ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(py);			          // muon py at Vertex
	
  ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(pz);			          // muon pz at Vertex
  ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(pxSTI);		          // muon px at St1
  ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(pySTI);		          // muon py at St1
  ((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(pzSTI);		          // muon pz at St1
  ((TLeaf *) fBranch->GetListOfLeaves()->At(8))->SetAddress(pxSTIII);		          // muon px at St3
  ((TLeaf *) fBranch->GetListOfLeaves()->At(9))->SetAddress(pySTIII);  		          // muon py at St3
  ((TLeaf *) fBranch->GetListOfLeaves()->At(10))->SetAddress(pzSTIII);            		  // muon pz at St3
  ((TLeaf *) fBranch->GetListOfLeaves()->At(11))->SetAddress(pT);				  // muon pT
  ((TLeaf *) fBranch->GetListOfLeaves()->At(12))->SetAddress(p);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(13))->SetAddress(pSTI);
  
  ((TLeaf *) fBranch->GetListOfLeaves()->At(14))->SetAddress(ELoss);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(15))->SetAddress(muchisquare);      // chi2 of 1st muon
  ((TLeaf *) fBranch->GetListOfLeaves()->At(16))->SetAddress(muIDhits); 		           // muid hit pattern of 1st muon
  ((TLeaf *) fBranch->GetListOfLeaves()->At(17))->SetAddress(muIDquad0);                       // muid quadrant at gap0 of 1st muon
  ((TLeaf *) fBranch->GetListOfLeaves()->At(18))->SetAddress(muTRhits);                       // mutr hit pattern of 1st muon
  ((TLeaf *) fBranch->GetListOfLeaves()->At(19))->SetAddress(dS30);                            // DS3 of 1st muon (à la Olivier)
  ((TLeaf *) fBranch->GetListOfLeaves()->At(20))->SetAddress(dS3ctp0);                         // DS3 of 1st muon (w const theta & phi)
  ((TLeaf *) fBranch->GetListOfLeaves()->At(21))->SetAddress(muIDchis0);                       // Muid Chi2
  ((TLeaf *) fBranch->GetListOfLeaves()->At(22))->SetAddress(gap0x);            // Muid Gap0 x
  ((TLeaf *) fBranch->GetListOfLeaves()->At(23))->SetAddress(gap0y);            // Muid Gap0 y
  
  ((TLeaf *) fBranch->GetListOfLeaves()->At(24))->SetAddress(gap0z);            // Muid Gap0 z
  ((TLeaf *) fBranch->GetListOfLeaves()->At(25))->SetAddress(dirX);             // Muid Road dxdz
  ((TLeaf *) fBranch->GetListOfLeaves()->At(26))->SetAddress(dirY);             // Muid Road  dydz
  ((TLeaf *) fBranch->GetListOfLeaves()->At(27))->SetAddress(status);       // Mutr track status
  ((TLeaf *) fBranch->GetListOfLeaves()->At(28))->SetAddress(ghostflag);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(29))->SetAddress(lastGap);
  //   cout << "pseudorap is " << pseudo_rapidity[0] << endl;
  ((TLeaf *) fBranch->GetListOfLeaves()->At(30))->SetAddress(pseudo_rapidity);		   // pseudo rapidity
  ((TLeaf *) fBranch->GetListOfLeaves()->At(31))->SetAddress(newphi);       // Reaction plane phi
  ((TLeaf *) fBranch->GetListOfLeaves()->At(32))->SetAddress(DG0);				   // DG0
  ((TLeaf *) fBranch->GetListOfLeaves()->At(33))->SetAddress(DDG0);				   // DDG0 - opening angle
  
  ((TLeaf *) fBranch->GetListOfLeaves()->At(34))->SetAddress(DS0);				   // DS0 - muID road doca @z=0
  ((TLeaf *) fBranch->GetListOfLeaves()->At(35))->SetAddress(refX);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(36))->SetAddress(refY);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(37))->SetAddress(mutr_nhits);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(38))->SetAddress(muid_nhits);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(39))->SetAddress(pseudotrigS_1D);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(40))->SetAddress(pseudotrigS_1S);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(41))->SetAddress(pseudotrigN_1D);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(42))->SetAddress(pseudotrigN_1S);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(43))->SetAddress(recoS_1D);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(44))->SetAddress(recoS_1S);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(45))->SetAddress(recoN_1D);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(46))->SetAddress(recoN_1S);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(47))->SetAddress(xSTI);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(48))->SetAddress(ySTI);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(49))->SetAddress(zSTI);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(50))->SetAddress(xSTII);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(51))->SetAddress(ySTII);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(52))->SetAddress(zSTII);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(53))->SetAddress(xSTIII);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(54))->SetAddress(ySTIII);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(55))->SetAddress(zSTIII);
  
  ((TLeaf *) fBranch->GetListOfLeaves()->At(56))->SetAddress(ref_vtx_rdca);           	   // muon px at Vertex
  ((TLeaf *) fBranch->GetListOfLeaves()->At(57))->SetAddress(ref_vtx_r);            	   // muon py at Vertex
  ((TLeaf *) fBranch->GetListOfLeaves()->At(58))->SetAddress(ref_vtx_z);                       // muon pz at Vertex
  ((TLeaf *) fBranch->GetListOfLeaves()->At(59))->SetAddress(refit_z);              // event Z-vtx from refit
  
  ((TLeaf *) fBranch->GetListOfLeaves()->At(60))->SetAddress(dANGLE);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(61))->SetAddress(dANGLE_xyz);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(62))->SetAddress(dTHETA);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(63))->SetAddress(dPHI);      
  ((TLeaf *) fBranch->GetListOfLeaves()->At(64))->SetAddress(ROAD_SLOPE);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(65))->SetAddress(MC_N_PART); //
  ((TLeaf *) fBranch->GetListOfLeaves()->At(66))->SetAddress(MC_PX); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(67))->SetAddress(MC_PY); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(68))->SetAddress(MC_PZ); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(69))->SetAddress(MC_PTOT); // from MC tracker_map
  
  ((TLeaf *) fBranch->GetListOfLeaves()->At(70))->SetAddress(MC_Z); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(71))->SetAddress(MC_PID); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(72))->SetAddress(MC_HITS); // hits of the major MC trk
  ((TLeaf *) fBranch->GetListOfLeaves()->At(73))->SetAddress(MC_P_PID); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(74))->SetAddress(MC_P_PX); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(75))->SetAddress(MC_P_PY); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(76))->SetAddress(MC_P_PZ); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(77))->SetAddress(MC_P_PTOT); //
  ((TLeaf *) fBranch->GetListOfLeaves()->At(78))->SetAddress(MC_P_Z); //
  ((TLeaf *) fBranch->GetListOfLeaves()->At(79))->SetAddress(MC_D_PID); // from MC tracker_map for daughter particle
  
  ((TLeaf *) fBranch->GetListOfLeaves()->At(80))->SetAddress(MC_D_PX); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(81))->SetAddress(MC_D_PY); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(82))->SetAddress(MC_D_PZ); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(83))->SetAddress(MC_D_PTOT); //
  ((TLeaf *) fBranch->GetListOfLeaves()->At(84))->SetAddress(MC_D_Z); // origin_z
  ((TLeaf *) fBranch->GetListOfLeaves()->At(85))->SetAddress(MC_D_N); // total # of daughter particles
  ((TLeaf *) fBranch->GetListOfLeaves()->At(86))->SetAddress(MC_X); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(87))->SetAddress(MC_Y); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(88))->SetAddress(MC_G_PID); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(89))->SetAddress(MC_G_PX); // from MC tracker_map
  
  ((TLeaf *) fBranch->GetListOfLeaves()->At(90))->SetAddress(MC_G_PY); // from MC tracker_map
   ( (TLeaf *) fBranch->GetListOfLeaves()->At(91))->SetAddress(MC_G_PZ); // from MC tracker_map
  ((TLeaf *) fBranch->GetListOfLeaves()->At(92))->SetAddress(MC_G_PTOT); //
((TLeaf *) fBranch->GetListOfLeaves()->At(93))->SetAddress(MC_G_Z); //
((TLeaf *) fBranch->GetListOfLeaves()->At(94))->SetAddress(MC_TRK);
((TLeaf *) fBranch->GetListOfLeaves()->At(95))->SetAddress(RPC2_x); 
((TLeaf *) fBranch->GetListOfLeaves()->At(96))->SetAddress(RPC2_y);
((TLeaf *) fBranch->GetListOfLeaves()->At(97))->SetAddress(RPC3_x);
((TLeaf *) fBranch->GetListOfLeaves()->At(98))->SetAddress(RPC3_y);
((TLeaf *) fBranch->GetListOfLeaves()->At(99))->SetAddress(RPC3r_x);

((TLeaf *) fBranch->GetListOfLeaves()->At(100))->SetAddress(RPC3r_y);
((TLeaf *) fBranch->GetListOfLeaves()->At(101))->SetAddress(RPC2r_x);
((TLeaf *) fBranch->GetListOfLeaves()->At(102))->SetAddress(RPC2r_y);
//cout << "pseudorap is " << pseudo_rapidity[0] << endl;
((TLeaf *) fBranch->GetListOfLeaves()->At(103))->SetAddress(DG4);
((TLeaf *) fBranch->GetListOfLeaves()->At(104))->SetAddress(DCA_r);
((TLeaf *) fBranch->GetListOfLeaves()->At(105))->SetAddress(DCA_z);


//cout << " after setting tack branch" << endl; 


 //	TBranch *fBranch = newsngmuons->GetBranch("_RecoTracks");
 //	((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(&fNMU);
	
	fBranch = newsngmuons->GetBranch("Residuals");
	

  ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(res100);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(res101);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(res110);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(res111);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(res120);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(res121);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(res200);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(res201);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(8))->SetAddress(res210);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(9))->SetAddress(res211);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(10))->SetAddress(res220);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(11))->SetAddress(res221);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(12))->SetAddress(res300);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(13))->SetAddress(res301);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(14))->SetAddress(res310);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(15))->SetAddress(res311);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(16))->SetAddress(station1hits);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(17))->SetAddress(station2hits);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(18))->SetAddress(station3hits);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(19))->SetAddress(allstation1hits);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(20))->SetAddress(allstation2hits);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(21))->SetAddress(allstation3hits); 
  ((TLeaf *) fBranch->GetListOfLeaves()->At(22))->SetAddress(allhits);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(23))->SetAddress(invmass1);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(24))->SetAddress(invmass2);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(25))->SetAddress(invmass3);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(26))->SetAddress(icharge1);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(27))->SetAddress(icharge2);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(28))->SetAddress(icharge3);
  ((TLeaf *) fBranch->GetListOfLeaves()->At(29))->SetAddress(thrust1);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(30))->SetAddress(thrust2);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(31))->SetAddress(thrust3);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(32))->SetAddress(RpcDCA);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(33))->SetAddress(RpcpT);    
  ((TLeaf *) fBranch->GetListOfLeaves()->At(34))->SetAddress(Rpctime);    
  


  }

  } // if
  else
    {
      //cout << "No muons found" << endl;

    }
  
  

  
  if ( fNMU >0  || fNRpcHits>0) {
//  if ( fNMU >0  && fNRpcHits>0) {
    
    newsngmuons->Fill();
  }

  if(fNMU>0){
delete [] RPC2_x;
delete [] RPC2_y;
delete [] RPC3_x;
delete [] RPC3_y;
delete [] RPC2r_x;
delete [] RPC2r_y;
delete [] RPC3r_x;
delete [] RPC3r_y;
delete [] dS30;
delete [] dS3ctp0;
delete [] muIDchis0;
delete [] DG0;
delete [] DG4;
delete [] DCA_r;
delete [] DCA_z;
delete [] DDG0;
delete [] DS0;
delete [] pseudo_rapidity;
delete [] muIDquad0;
delete [] muTRhits0;
delete [] lastGap;
delete [] charge;
delete [] muchisquare;
delete [] status;
delete [] ghostflag;
delete [] newphi;
delete [] refit_z;
delete [] gap0x;
delete [] gap0y;
delete [] gap0z;
delete [] dirX;
delete [] dirY;
delete [] refX;
delete [] refY;
delete [] muTRhits;
delete [] muIDhits;
delete [] mutr_nhits;
delete [] muid_nhits;
delete [] px;
delete [] py;
delete [] pz;
delete [] xSTI;
delete [] ySTI;
delete [] zSTI;
delete [] pxSTI;
delete [] pySTI;
delete [] pzSTI;
delete [] xSTII;
delete [] ySTII;
delete [] zSTII;
delete [] xSTIII;
delete [] ySTIII;
delete [] zSTIII;
delete [] pxSTIII;
delete [] pySTIII;
delete [] pzSTIII;
delete [] pT;
delete [] p;
delete [] pSTI;
delete [] ELoss;
delete [] rpc3z;
delete [] rpc2z;
delete [] rpc3st3;
delete [] rpc2st3;
delete [] rpc2g5;
delete [] rpc3g5;
delete [] dANGLE;
delete [] dANGLE_xyz;
delete [] dPHI;
delete [] dTHETA;
delete [] ROAD_SLOPE;
delete [] ref_vtx_rdca;
delete [] ref_vtx_r;
delete [] ref_vtx_z;
delete [] pseudotrigS_1D;
delete [] pseudotrigS_1S;
delete [] pseudotrigN_1D;
delete [] pseudotrigN_1S;
delete [] recoS_1D;
delete [] recoS_1S;
delete [] recoN_1D;
delete [] recoN_1S;
delete [] MC_N_PART;
delete [] MC_PX;
delete [] MC_PY;
delete [] MC_PZ;
delete [] MC_PTOT;
delete [] MC_X;
delete [] MC_Y;
delete [] MC_Z;
delete [] MC_PID;
delete [] MC_HITS;
delete [] MC_P_PID;
delete [] MC_P_PX;
delete [] MC_P_PY;
delete [] MC_P_PZ;
delete [] MC_P_PTOT;
delete [] MC_P_Z;
delete [] MC_D_PID;
delete [] MC_D_PX;
delete [] MC_D_PY;
delete [] MC_D_PZ;
delete [] MC_D_PTOT;
delete [] MC_D_Z;
delete [] MC_D_N;
delete [] MC_G_PID;
delete [] MC_G_PX;
delete [] MC_G_PY;
delete [] MC_G_PZ;
delete [] MC_G_PTOT;
delete [] MC_G_Z;
delete [] MC_TRK;
delete [] MC_P_TRK;
delete [] MC_D_TRK;
delete [] MC_G_TRK;

delete [] RpcDCA;
delete [] RpcpT;
delete [] Rpctime;
delete [] npart;




delete []    res100;
delete []    res101;
delete []    res110;
delete []    res111;
delete []    res120;
delete []    res121;
delete []    res200;
delete []    res201;
delete []    res210;
delete []    res211;
delete []    res220;
delete []    res221;
delete []    res300;
delete []    res301;
delete []    res310;
delete []    res311;
delete []    station1hits;
delete []    station2hits;
delete []    station3hits;
delete []    allstation1hits;
delete []    allstation2hits;
delete []    allstation3hits;
delete []    allhits;


 delete [] invmass1;
 delete [] invmass2;
 delete [] invmass3;


 delete [] icharge1;
 delete [] icharge2;
 delete [] icharge3;

 delete [] thrust1;
 delete [] thrust2;
 delete [] thrust3;


  }
  if(fNRpcHits>0){
delete [] rpc_t;
delete [] rpc_c;
delete [] arm;
delete [] station;
delete [] octant;
delete [] halfoctant;
delete [] radsegment;
delete [] strip;
  }



  accEVT++;
delete [] bbcCentrality;
delete [] zdcCentrality;
delete [] ZdcEnergyN;
delete [] ZdcEnergyS;
delete [] delphi_RP;
delete [] RPbbcrp10;
delete [] RPbbcrp11;
delete [] RPbbcrp12;
delete [] RPbbcrp00;
delete [] RPbbcrp01;
delete [] RPbbcrp02;
delete [] RPsmdrp00;
delete [] RPsmdrp01;
delete [] RPsmdrp02;
delete [] BbcZVertex;
delete [] Evt_Z;
delete [] fRunNumber;
delete [] EventNumber;
delete [] realtrigS_1D;
delete [] realtrigN_1D;
delete [] realtrigS_1S;
delete [] realtrigN_1S;
delete [] livetrigS_1D;
delete [] livetrigN_1D;
delete [] livetrigS_1S;
delete [] livetrigN_1S;
delete [] realtrig_MB;
delete [] Clock_trig;
delete [] Clock_live;
delete [] triggerbit;
delete [] triggerlive;
delete [] clockcross;
delete [] SpinX_ID;
delete [] Pol_Y;
delete [] Pol_B;
delete [] GL1X_ID;
delete [] BBCQN;
delete [] BBCQS;
delete [] BBCNN;
delete [] BBCNS;

  
  return 0;
}
