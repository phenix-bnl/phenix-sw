// $Id: NewSngmuonsRun12.C,v 1.19 2017/07/15 04:49:08 phnxbld Exp $

/*!
  \file SngmuonsRun11.C
  \brief single muon ntuple booking and filling
  \version $Revision: 1.19 $
  \date $Date: 2017/07/15 04:49:08 $
*/

#include <iostream>
#include <boost/array.hpp>
#include <EventHeader.h>
#include <PHGlobal.h>
#include <ReactionPlaneObject.h> // event information
#include <RpSumXYObject.h> // event information

#include <MWGVertex.h>
#include <PHMuoTracksOut.h>
//#include <PHMuoTracksv12.h>
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
#include<TMutCoordMap.h>
#include<TMutClusMap.h>

#include<TDataType.h>
#include<TMutCoordMap.h>
#include<TMutStubMap.h>
#include<TMutGapCoordMap.h>
#include<TMutGeo.h>
#include<MutrgTrk.hh>
#include<MutrgSelectHit_v1.hh> 
#include<MutrgTrkArray.hh>
#include<MutrgHitArray.hh>
#include<MutrgFindTrk_v2.hh>
#include <MutrgPar.hh>
#include <MutrgKey.hh>
#include<PHGeometry.h>
#include<algorithm>




#include <TRpcTrk.h>
#include <TRpcTrk_v3.h>
#include <TRpcTrkMap.h>
#include <TRpcHit.h>
#include <TRpcHodoHit.h>
#include <TRpcHodoHitMap.h>
#include <TRpcMCHit.h>
#include <TRpcClusMap.h>
#include <TRpcCoordMap.h>
#include <TRpcCoord_v1.h>
#include <TRpcHitMap.h>
#include <TRpcMCHitMap.h>
#include <RpcStrip.h>
// RPC Track Matching
#include <TRpcMuoTrk.h>
#include <TRpcMuoTrk_v1.h>
#include <TRpcMuoTrkMap.h>



#include <root_ptrk.h>
#include <TNtuple.h>
#include <TRandom.h>
#include <TriggerHelper.h>
#include <utiCentrality.h>
#include <BbcMultipleVtx.hh>
#include <BbcMultipleVtxList.hh>
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

#include <FvtxConeTracklets.h>

using namespace std;
using namespace PhUtilities;



//Int_t fNTracks;

Int_t  fNRpcHits12;
Int_t  fNRpchodoHits12;
Int_t fNMU12;

Int_t *fRunNumber12;
Int_t *EventNumber12;
const int MaxNHitClock = 7;


//__________________________________________________
void MWGpico::BookNewSngmuonsNtupleRun12(TTree*& newsngmuons, TString m_name, TString m_title)
{

  //        cout << " in booknewsngmuons" << endl;

   newsngmuons = new TTree(m_name,m_title); // maybe not needed to creat a new tree here anymore...
//   newsngmuons->Branch("Info",0,"");
  
  
   newsngmuons->Branch("_RecoTracks",0,"_RecoTracks/I");


   newsngmuons->Branch("Eventdata",0,"Run_Number/I:Evt_Number/I:Evt_Z/F:Evt_bbcZ/F:Evt_bbcCentrality/F:Evt_zdcEnerN/F:Evt_realtrigNS_1D/I:Evt_realtrigS_SG1D/I:Evt_realtrigNS_1H/I:Evt_realtrigN_SG1D/I:Evt_livetrigNS_1D/I:"
		       "Evt_livetrigS_SG1D/I:Evt_livetrigNS_1H/I:Evt_livetrigN_SG1D/I:triggerbit/I:triggerlive/I:clockcross/I:Clock_trig/I:Evt_realtrig_MB/I:SpinX_ID/F:Pol_Y/F:Pol_B/F:GL1X_ID/F:"
		       "triggerraw/I:McRunFlag/F");

   //  cout << " booked new eventdata" << endl; 

   newsngmuons->Branch("RecoTracks",0,"Evt_Nmu[_RecoTracks]/I:charge[_RecoTracks]/F:px[_RecoTracks]/F:py[_RecoTracks]/F:"
		       "pz[_RecoTracks]/F:pxSt1[_RecoTracks]/F:pySt1[_RecoTracks]/F:pzSt1[_RecoTracks]/F:pxSt3[_RecoTracks]/F:pySt3[_RecoTracks]/F:pzSt3[_RecoTracks]/F:pT[_RecoTracks]/F:p[_RecoTracks]/F:pSt1[_RecoTracks]/F:"
		       "ELoss[_RecoTracks]/F:chi2[_RecoTracks]/F:idhits[_RecoTracks]/I:idquad[_RecoTracks]/I:trhits[_RecoTracks]/I:DS3[_RecoTracks]/F:DS3ctp[_RecoTracks]/F:idchi2[_RecoTracks]/F:gap0x[_RecoTracks]/F:gap0y[_RecoTracks]/F:"
		       "gap0z[_RecoTracks]/F:dxdz[_RecoTracks]/F:dydz[_RecoTracks]/F:trstat[_RecoTracks]/I:ghost[_RecoTracks]/F:lastGap[_RecoTracks]/I:eta[_RecoTracks]/F:phi[_RecoTracks]/F:DG0[_RecoTracks]/F:DDG0[_RecoTracks]/F:"
		       "DS0[_RecoTracks]/F:refX[_RecoTracks]/F:refY[_RecoTracks]/F:mutr_nhits[_RecoTracks]/F:muid_nhits[_RecoTracks]/F:Evt_pseudotrigN_1S[_RecoTracks]/I:Evt_pseudotrigS_SG1D[_RecoTracks]/I:Evt_pseudotrigS_1S[_RecoTracks]/I:Evt_pseudotrigN_SG1D[_RecoTracks]/I:Evt_recoNS_1D[_RecoTracks]/I:Evt_recoS_SG1D[_RecoTracks]/I:Evt_recoNS_1H[_RecoTracks]/I:"
		       "Evt_recoN_SG1D[_RecoTracks]/I:xSt1[_RecoTracks]/F:ySt1[_RecoTracks]/F:zSt1[_RecoTracks]/F:xSt2[_RecoTracks]/F:ySt2[_RecoTracks]/F:zSt2[_RecoTracks]/F:xSt3[_RecoTracks]/F:ySt3[_RecoTracks]/F:zSt3[_RecoTracks]/F:"
		       "ref_vtx_rdca[_RecoTracks]/F:ref_vtx_r[_RecoTracks]/F:ref_vtx_z[_RecoTracks]/F:refit_zvtx[_RecoTracks]/F:"
		       "dAngle[_RecoTracks]/F:dAngle_xyz[_RecoTracks]/F:dTheta[_RecoTracks]/F:dPhi[_RecoTracks]/F:road_slope[_RecoTracks]/F:mc_n_part[_RecoTracks]/F:mc_px[_RecoTracks]/F:mc_py[_RecoTracks]/F:mc_pz[_RecoTracks]/F:mc_ptot[_RecoTracks]/F:"
		       "mc_z[_RecoTracks]/F:mc_pid[_RecoTracks]/F:mc_hits[_RecoTracks]/F:mc_p_pid[_RecoTracks]/F:mc_p_px[_RecoTracks]/F:mc_p_py[_RecoTracks]/F:mc_p_pz[_RecoTracks]/F:mc_p_ptot[_RecoTracks]/F:mc_p_z[_RecoTracks]/F:mc_d_pid[_RecoTracks]/F:"
		       "mc_d_px[_RecoTracks]/F:mc_d_py[_RecoTracks]/F:mc_d_pz[_RecoTracks]/F:mc_d_ptot[_RecoTracks]/F:mc_d_z[_RecoTracks]/F:mc_d_n[_RecoTracks]/F:mc_x[_RecoTracks]/F:mc_y[_RecoTracks]/F:mc_g_pid[_RecoTracks]/F:mc_g_px[_RecoTracks]/F:"
		       "mc_g_py[_RecoTracks]/F:mc_g_pz[_RecoTracks]/F:mc_g_ptot[_RecoTracks]/F:mc_g_z[_RecoTracks]/F:mc_trk[_RecoTracks]/I:RPC3_x[_RecoTracks]/F:RPC3_y[_RecoTracks]/F:RPC3r_x[_RecoTracks]/F:" 
		       "RPC3r_y[_RecoTracks]/F:"
		       "DG4[_RecoTracks]/F:DCA_r[_RecoTracks]/F:DCA_z[_RecoTracks]/F:pxSt2[_RecoTracks]/F:pySt2[_RecoTracks]/F:pzSt2[_RecoTracks]/F");
   /*		    */


   newsngmuons->Branch("Residuals",0,"Res100[_RecoTracks]/F:Res101[_RecoTracks]/F:Res110[_RecoTracks]/F:Res111[_RecoTracks]/F:"
		       "Res120[_RecoTracks]/F:Res121[_RecoTracks]/F:Res200[_RecoTracks]/F:Res201[_RecoTracks]/F:Res210[_RecoTracks]/F:Res211[_RecoTracks]/F:Res220[_RecoTracks]/F:Res221[_RecoTracks]/F:Res300[_RecoTracks]/F:Res301[_RecoTracks]/F:"
		       "Res310[_RecoTracks]/F:Res311[_RecoTracks]/F:station1hits[_RecoTracks]/I:station2hits[_RecoTracks]/I:station3hits[_RecoTracks]/I:allstation1hits[_RecoTracks]/I:allstation2hits[_RecoTracks]/I:allstation3hits[_RecoTracks]/I:allhits[_RecoTracks]/I:"
		       "invmass1[_RecoTracks]/F:invmass2[_RecoTracks]/F:invmass3[_RecoTracks]/F:icharge1[_RecoTracks]/I:icharge2[_RecoTracks]/I:icharge3[_RecoTracks]/I:"
		       "thrust1[_RecoTracks]/F:thrust2[_RecoTracks]/F:thrust3[_RecoTracks]/F:"
		       "RpcDCA[_RecoTracks]/F:RpcpT[_RecoTracks]/F:Rpctime[_RecoTracks]/F:"
		       "DCA_zmult0[_RecoTracks]/F:DCA_zmult1[_RecoTracks]/F:DCA_zmult2[_RecoTracks]/F:DCA_zmult3[_RecoTracks]/F:DCA_zmult4[_RecoTracks]/F:DCA_zmult5[_RecoTracks]/F:DCA_zmult6[_RecoTracks]/F:DCA_zmult7[_RecoTracks]/F:DCA_zmult8[_RecoTracks]/F:zmult0[_RecoTracks]/F:zmult1[_RecoTracks]/F:zmult2[_RecoTracks]/F:zmult3[_RecoTracks]/F:zmult4[_RecoTracks]/F:zmult5[_RecoTracks]/F:zmult6[_RecoTracks]/F:zmult7[_RecoTracks]/F:zmult8[_RecoTracks]/F:"
		       "Rpc1DCA[_RecoTracks]/F:Rpc1time[_RecoTracks]/F:"
"Qtot100[_RecoTracks]/F:Qtot101[_RecoTracks]/F:Qtot110[_RecoTracks]/F:Qtot111[_RecoTracks]/F:"
		       "Qtot120[_RecoTracks]/F:Qtot121[_RecoTracks]/F:Qtot200[_RecoTracks]/F:Qtot201[_RecoTracks]/F:Qtot210[_RecoTracks]/F:Qtot211[_RecoTracks]/F:Qtot220[_RecoTracks]/F:Qtot221[_RecoTracks]/F:Qtot300[_RecoTracks]/F:Qtot301[_RecoTracks]/F:"
		       "Qtot310[_RecoTracks]/F:Qtot311[_RecoTracks]/F:Nstr100[_RecoTracks]/I:Nstr101[_RecoTracks]/I:Nstr110[_RecoTracks]/I:Nstr111[_RecoTracks]/I:"
		       "Nstr120[_RecoTracks]/I:Nstr121[_RecoTracks]/I:Nstr200[_RecoTracks]/I:Nstr201[_RecoTracks]/I:Nstr210[_RecoTracks]/I:Nstr211[_RecoTracks]/I:Nstr220[_RecoTracks]/I:Nstr221[_RecoTracks]/I:Nstr300[_RecoTracks]/I:Nstr301[_RecoTracks]/I:"
		       "Nstr310[_RecoTracks]/I:Nstr311[_RecoTracks]/I");

//       cout << " booked tracks" << endl; 


 newsngmuons->Branch("RpcMatchVtx",0,"Rpc3dca[_RecoTracks]/F:Rpc3time[_RecoTracks]/F:Rpc3x[_RecoTracks]/F:Rpc3y[_RecoTracks]/F:Rpc1dca[_RecoTracks]/F:Rpc1time[_RecoTracks]/F:Rpc1x[_RecoTracks]/F:Rpc1y[_RecoTracks]/F");
   newsngmuons->Branch("RpcMatchSt1",0,"Rpc3dca[_RecoTracks]/F:Rpc3time[_RecoTracks]/F:Rpc3x[_RecoTracks]/F:Rpc3y[_RecoTracks]/F:Rpc1dca[_RecoTracks]/F:Rpc1time[_RecoTracks]/F:Rpc1x[_RecoTracks]/F:Rpc1y[_RecoTracks]/F");
   newsngmuons->Branch("RpcMatchSt3",0,"Rpc3dca[_RecoTracks]/F:Rpc3time[_RecoTracks]/F:Rpc3x[_RecoTracks]/F:Rpc3y[_RecoTracks]/F:Rpc1dca[_RecoTracks]/F:Rpc1time[_RecoTracks]/F:Rpc1x[_RecoTracks]/F:Rpc1y[_RecoTracks]/F");
   newsngmuons->Branch("RpcMatchMuID",0,"Rpc3dca[_RecoTracks]/F:Rpc3time[_RecoTracks]/F:Rpc3x[_RecoTracks]/F:Rpc3y[_RecoTracks]/F:Rpc1dca[_RecoTracks]/F:Rpc1time[_RecoTracks]/F:Rpc1x[_RecoTracks]/F:Rpc1y[_RecoTracks]/F");



 newsngmuons->Branch("FvtxMatch",0,"fvtx_px[_RecoTracks]/F:fvtx_py[_RecoTracks]/F:fvtx_pz[_RecoTracks]/F:fvtx_dphi[_RecoTracks]/F:fvtx_dtheta[_RecoTracks]/F:fvtx_chi[_RecoTracks]/F:fvtx_conebits[_RecoTracks]/i:fvtx_mpx[_RecoTracks]/F:fvtx_mpy[_RecoTracks]/F:fvtx_mpz[_RecoTracks]/F:fvtx_mdcaz[_RecoTracks]/F:fvtx_dr[_RecoTracks]/F:fvtx_tracklconebits[_RecoTracks]/i:fvtx_vtx_x[_RecoTracks]/F:fvtx_vtx_y[_RecoTracks]/F:fvtx_vtx_z[_RecoTracks]/F");


 
   newsngmuons->Branch("_RpcHits",0,"_RpcHits/I");


   newsngmuons->Branch("RpcHits",0,"rpc_t[_RpcHits]/F:rpc_c[_RpcHits]/F:arm[_RpcHits]/I:station[_RpcHits]/I:octant[_RpcHits]/I:halfoct[_RpcHits]/I:radsegment[_RpcHits]/I:strip[_RpcHits]/I");



   newsngmuons->Branch("_RpchodoHits",0,"_RpchodoHits/I");


   newsngmuons->Branch("RpchodoHits",0,"rpchodo_t[_RpchodoHits]/I:hodoarm[_RpchodoHits]/I:hodostation[_RpchodoHits]/I:hodostrip[_RpchodoHits]/I");



//   cout << " finished booking NewSngmuonsRun12" << endl;
   return;
}

//__________________________________________________________________
void MWGpico::BookNewSngmuonsEvtNtupleRun12(TNtuple*& newsngvtx, TString v_name, TString v_title )
{
   // define variable list for vertex ntuple
   const  char* vtxvarlist=0;

   vtxvarlist= "Run_Number:SpinX_ID:Pol_Y:Pol_B:GL1X_ID:Evt_Z:bbcZ:zdcCentrality:bbcCentrality:BBCQN:"
      "BBCQS:BBCNN:BBCNS:ZDCN:ZDCS:Clock_trig:Evt_realtrig_MB:Evt_livetrigS_SG1D:Evt_livetrigNS_1D:Evt_livetrigN_SG1D:Evt_livetrigNS_1H:"
      "RPbbcrp10:RPbbcrp11:RPbbcrp12:delphi_RP:EPT1:"
      "RPbbcrp01:RPbbcrp02:RPsmdrp00:RPsmdrp01:RPsmdrp02"; // BBC SMD rp

   newsngvtx = new TNtuple( v_name, v_title, vtxvarlist );
   newsngvtx->SetAutoSave(160000000);

   return;
}

 
//__________________________________________________
int MWGpico::FillNewSngmuonsNtpRun12(PHMuoTracksOut* &muo, TTree* newsngmuons,  TNtuple* newsngvtx)
{








  //     cout <<  "in FillNewSngmuonsNtpRun12" << endl;
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
   RPsmdrp00[0] = (Float_t)_run_flag;
   RPsmdrp01[0] = (Float_t)_run_flag;
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

   fRunNumber12 = new Int_t[1];
   fRunNumber12[0] = ( run_header ) ?  run_header->get_RunNumber():0;


   EventNumber12 = new Int_t[1];
   EventNumber12[0] = (event_header ) ? event_header->get_EvtSequence():0;

  
// try load MC if failed from PHGlobal
   {
      bool error( false );
      if( !fRunNumber12[0] ) fRunNumber12[0] = Tools::runNumberMC( header, error );
      if( !EventNumber12[0] ) EventNumber12[0]  = Tools::eventNumberMC( header, error );
   }
  
   Int_t *realtrigNS_1D; // real trigger South
   Int_t *realtrigNS_1H; // real trigger North
   Int_t *realtrigS_SG1D; // real trigger South
   Int_t *realtrigN_SG1D; // real trigger North
  
   Int_t *livetrigNS_1D; // live trigger South
   Int_t *livetrigNS_1H; // live trigger North
   Int_t *livetrigS_SG1D; // live trigger South
   Int_t *livetrigN_SG1D; // live trigger North
  
   Int_t *realtrig_MB;
   Int_t *Clock_trig ;
   Int_t *Clock_live ;

   realtrigNS_1D = new Int_t[1];
   realtrigNS_1H = new Int_t[1];
   realtrigS_SG1D = new Int_t[1];
   realtrigN_SG1D = new Int_t[1];
			     
   livetrigNS_1D = new Int_t[1];
   livetrigNS_1H = new Int_t[1];
   livetrigS_SG1D = new Int_t[1];
   livetrigN_SG1D = new Int_t[1];
			     
   realtrig_MB = new Int_t[1] ;
   Clock_trig = new Int_t[1]  ;
   Clock_live = new Int_t[1]  ;
  

   Int_t *triggerbit;
   triggerbit = new Int_t[1];
   triggerbit[0] = 0; 
   Int_t *triggerlive;
   triggerlive = new Int_t[1];
   triggerlive[0] = 0; 
   Int_t *triggerraw;
   triggerraw = new Int_t[1];
   triggerraw[0] = 0; 
   Int_t *clockcross;
   clockcross = new Int_t[1];
   clockcross[0] = 0; 


   realtrigNS_1D[0]     =  0;
   realtrigNS_1H[0]     =  0;
   realtrigS_SG1D[0]     =  0;
   realtrigN_SG1D[0]     =  0;
  
   livetrigNS_1D[0]     =  0;
   livetrigNS_1H[0]     =  0;
   livetrigS_SG1D[0]     =  0;
   livetrigN_SG1D[0]     =  0;
  
   realtrig_MB[0]      =  0;
   Clock_trig[0]       =  0;
   Clock_live[0]       =  0;
 

  
// real triggers: 200GeV and 62 GeV runs
// cout << "before triggers in NewSngmuonsRun12" << endl;  
   if(_trig_lvl1){ // Trigger Node exists in data file


//for run 12 almost all of the run with the following triggers:
/* ((MUIDLL1_N2D||S2D)||(N1D&S1D))&BBCLL1(noVtx) 	0x00001000 
   (MUIDLL1_N1H||S1H)&BBCLL1(noVtx) 	0x00002000
   (MUIDLL1_N1D||S1D)&BBCLL1(noVtx) 	0x00004000
   (NRPCA||NRPCB||NRPCC)&BBCLL1 	        0x00010000 
   MUON_N_RPCA 	                        0x00020000
   MUON_N_RPCB                           	0x00040000
   MUON_N_RPCC                      	0x00080000
   MUON_N_SG1&BBCLL1(NoVtx)&MUIDLL1_N1D 	0x00100000 
   MUON_N_SG1                        	0x00200000 
   (SRPCA||SRPCB||SRPCC)&BBCLL1 	        0x00400000 
   MUON_S_RPCA                        	0x00800000 
   MUON_S_RPCB                         	0x01000000 
   MUON_S_RPCC                      	0x02000000 
   MUON_S_SG1&BBCLL1(NoVtx)&MUIDLL1_S1D 	0x04000000 
   MUON_S_SG1                       	0x08000000
*/


//    cout << " checking triggers scaled: " <<  _trig_lvl1->get_lvl1_trigscaled() << "  live " << _trig_lvl1->get_lvl1_triglive() << " " <<  _trig_lvl1->get_lvl1_clock_cross() 
//<<endl;
      triggerbit[0] = _trig_lvl1->get_lvl1_trigscaled();
      triggerlive[0] = _trig_lvl1->get_lvl1_triglive();
      triggerraw[0] = _trig_lvl1->get_lvl1_trigraw();
      clockcross[0] = _trig_lvl1->get_lvl1_clock_cross(); 

//    TrigHelp->dump_info();
//real triggers
/*    if (TrigHelp->didLevel1TriggerGetScaled("MUIDLL1_S1D&BBCLL1")) {
      realtrigNS_1D[0]=1;
      }
      if (TrigHelp->didLevel1TriggerGetScaled("(MUIDLL1_N1D||S1D)&BBCLL1(noVtx)")) {
      realtrigNS_1H[0]=1;
      }
      if (TrigHelp->didLevel1TriggerGetScaled("(MUIDLL1_S1H)&BBCLL1(noVtx)")) {
      realtrigS_SG1D[0]=1;
      }
      if (TrigHelp->didLevel1TriggerGetScaled("(MUIDLL1_N1H)&BBCLL1(noVtx)")) {
      realtrigN_SG1D[0]=1;
      }
    
// live triggers
if (TrigHelp->didLevel1TriggerFire("MUIDLL1_S1D&BBCLL1(noVtx)")) {
livetrigNS_1D[0]=1;
//  cout << "S1DDDD = livetrigNS_1D" << endl; 
}
if (TrigHelp->didLevel1TriggerFire("(MUIDLL1_N1D||S1D)&BBCLL1(noVtx)")) {
livetrigNS_1H[0]=1;
//cout << "N1SDDDD = livetrigNS_1H" << endl; 
}
if (TrigHelp->didLevel1TriggerFire("(MUIDLL1_S1H)&BBCLL1(noVtx)")) {
//cout << "SH = livetrigS_1H" << endl; 
livetrigS_SG1D[0]=1;
}
if (TrigHelp->didLevel1TriggerFire("(MUIDLL1_N1H)&BBCLL1(noVtx)")) {
livetrigN_SG1D[0]=1;
//cout << "N1H = livetrigN_1H" << endl; 
}
*/

//
// RCS use bits for run12 for the moment, only deep, halted, and SG1Ds
//scaled triggers
    
      if ( _trig_lvl1->get_lvl1_trigscaled()&0x00002000){
	 realtrigNS_1H[0]=1;
      }
      if ( _trig_lvl1->get_lvl1_trigscaled()&0x00004000){
	 realtrigNS_1D[0]=1;
      }
      if ( _trig_lvl1->get_lvl1_trigscaled()&0x00100000){
	 realtrigN_SG1D[0]=1;
      }
      if ( _trig_lvl1->get_lvl1_trigscaled()&0x04000000){
	 realtrigS_SG1D[0]=1;
      }
    
//live triggers
      if (_trig_lvl1->get_lvl1_triglive()&0x00002000){
	 livetrigNS_1H[0]=1;
      }
      if (_trig_lvl1->get_lvl1_triglive()&0x00004000){
	 livetrigNS_1D[0]=1;
      }
      if (_trig_lvl1->get_lvl1_triglive()&0x00100000){
	 livetrigN_SG1D[0]=1;
      }
      if (_trig_lvl1->get_lvl1_triglive()&0x04000000){
	 livetrigS_SG1D[0]=1;
      }



  
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
      // we do get the spin information from the data base you only need the crossing information from variable clockcross and the runnumber  
  
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
      Evt_Data[0] = fRunNumber12[0];
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
      Evt_Data[17] = livetrigS_SG1D[0];                    // live real   trigger for MUID_LL1_S_SG1D
      Evt_Data[18] = livetrigNS_1D[0];                    // live real   trigger for MUID_LL1_NS_1D
      Evt_Data[19] = livetrigN_SG1D[0];                    // live real   trigger for MUID_LL1_N_SG1D
      Evt_Data[20] = livetrigNS_1H[0];                    // live real   trigger for MUID_LL1_NS_1H
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
	
//      cout << " got event branch" << endl;
  	
//=== Fill branches
      ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(fRunNumber12);                       // run number
//cout << " after runnr" << endl;
      ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(EventNumber12);                     // event number
//cout << " after eventnr" << endl;
      ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(Evt_Z);                           // Global
      ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(BbcZVertex);                      // BBC Zvertex
      ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(bbcCentrality);            
      ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(ZdcEnergyN);                      // zdcEnerN);
      ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(realtrigNS_1D);                    // real   trigger 1 deep  South
      ((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(realtrigS_SG1D);                    // real   trigger 1 sheep  South
      ((TLeaf *) fBranch->GetListOfLeaves()->At(8))->SetAddress(realtrigNS_1H);                    // real   trigger 1 deep  North
      ((TLeaf *) fBranch->GetListOfLeaves()->At(9))->SetAddress(realtrigN_SG1D);                    // real   trigger 1 sheep North
      ((TLeaf *) fBranch->GetListOfLeaves()->At(10))->SetAddress(livetrigNS_1D);                    // live   trigger 1 deep  South
	
      ((TLeaf *) fBranch->GetListOfLeaves()->At(11))->SetAddress(livetrigS_SG1D);                    // live    trigger 1 sheep  South
      ((TLeaf *) fBranch->GetListOfLeaves()->At(12))->SetAddress(livetrigNS_1H);                    // live   trigger 1 deep  North
      ((TLeaf *) fBranch->GetListOfLeaves()->At(13))->SetAddress(livetrigN_SG1D);                    // live   trigger 1 sheep North
      ((TLeaf *) fBranch->GetListOfLeaves()->At(14))->SetAddress(triggerbit);
      ((TLeaf *) fBranch->GetListOfLeaves()->At(15))->SetAddress(triggerlive);
      ((TLeaf *) fBranch->GetListOfLeaves()->At(16))->SetAddress(clockcross);
      ((TLeaf *) fBranch->GetListOfLeaves()->At(17))->SetAddress(Clock_trig);
      ((TLeaf *) fBranch->GetListOfLeaves()->At(18))->SetAddress(realtrig_MB);                     // live real   trigger for MB
      ((TLeaf *) fBranch->GetListOfLeaves()->At(19))->SetAddress(SpinX_ID);                         // beam crossing ID
      ((TLeaf *) fBranch->GetListOfLeaves()->At(20))->SetAddress(Pol_Y);
      ((TLeaf *) fBranch->GetListOfLeaves()->At(21))->SetAddress(Pol_B);
      ((TLeaf *) fBranch->GetListOfLeaves()->At(22))->SetAddress(GL1X_ID);
      ((TLeaf *) fBranch->GetListOfLeaves()->At(23))->SetAddress(triggerraw);       ((TLeaf *) fBranch->GetListOfLeaves()->At(24))->SetAddress(RPsmdrp00);                      // zdcEnerN);
//      ((TLeaf *) fBranch->GetListOfLeaves()->At(31))->SetAddress(RPsmdrp01);                      // zdcEnerN);
//	((TLeaf *) fBranch->GetListOfLeaves()->At(32))->SetAddress(RPsmdrp02);                      // zdcEnerN);



//	cout << " after putting in addresses for sngvtx" << endl; 


//_____________ end of sngvtx _____________


      Float_t *RPC3_x=NULL;
      Float_t *RPC3_y=NULL;
      Float_t *RPC3r_x=NULL;
      Float_t *RPC3r_y=NULL;

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

      Float_t *pxSTII=NULL;
      Float_t *pySTII=NULL;
      Float_t *pzSTII=NULL;
	
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
		     
      Float_t *rpc3st3=NULL;
      Float_t *rpc3g5=NULL; 
  

      Float_t *ref_vtx_rdca=NULL;
      Float_t *ref_vtx_r=NULL;
      Float_t *ref_vtx_z=NULL;
      Int_t *pseudotrigN_1S=NULL;
      Int_t *pseudotrigS_SG1D=NULL;
      Int_t *pseudotrigS_1S=NULL;
      Int_t *pseudotrigN_SG1D=NULL;
      Int_t *recoNS_1D=NULL;
      Int_t *recoS_SG1D=NULL;
      Int_t *recoNS_1H=NULL;
      Int_t *recoN_SG1D=NULL;

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

     Int_t *rpchodo_t=NULL; //RPC related variables
      Int_t *hodoarm=NULL;
      Int_t *hodostation=NULL;
      Int_t *hodostrip=NULL;




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


      Float_t  *Qtot100=NULL;
      Float_t  *Qtot101=NULL;
      Float_t  *Qtot110=NULL;
      Float_t  *Qtot111=NULL;
      Float_t  *Qtot120=NULL;
      Float_t  *Qtot121=NULL;
      Float_t  *Qtot200=NULL;
      Float_t  *Qtot201=NULL;
      Float_t  *Qtot210=NULL;
      Float_t  *Qtot211=NULL;
      Float_t  *Qtot220=NULL;
      Float_t  *Qtot221=NULL;
      Float_t  *Qtot300=NULL;
      Float_t  *Qtot301=NULL;
      Float_t  *Qtot310=NULL;
      Float_t  *Qtot311=NULL;

      Int_t  *Nstr100=NULL;
      Int_t  *Nstr101=NULL;
      Int_t  *Nstr110=NULL;
      Int_t  *Nstr111=NULL;
      Int_t  *Nstr120=NULL;
      Int_t  *Nstr121=NULL;
      Int_t  *Nstr200=NULL;
      Int_t  *Nstr201=NULL;
      Int_t  *Nstr210=NULL;
      Int_t  *Nstr211=NULL;
      Int_t  *Nstr220=NULL;
      Int_t  *Nstr221=NULL;
      Int_t  *Nstr300=NULL;
      Int_t  *Nstr301=NULL;
      Int_t  *Nstr310=NULL;
      Int_t  *Nstr311=NULL;


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

      Float_t *Rpc1DCA=NULL;
      Float_t *Rpc1time=NULL;


      Float_t *DCA_zmult0=NULL;
      Float_t *DCA_zmult1=NULL;
      Float_t *DCA_zmult2=NULL;
      Float_t *DCA_zmult3=NULL;
      Float_t *DCA_zmult4=NULL;
      Float_t *DCA_zmult5=NULL;
      Float_t *DCA_zmult6=NULL;
      Float_t *DCA_zmult7=NULL;
      Float_t *DCA_zmult8=NULL;


      Float_t *zmult0=NULL;
      Float_t *zmult1=NULL;
      Float_t *zmult2=NULL;
      Float_t *zmult3=NULL;
      Float_t *zmult4=NULL;
      Float_t *zmult5=NULL;
      Float_t *zmult6=NULL;
      Float_t *zmult7=NULL;
      Float_t *zmult8=NULL;



    Float_t *fRpc3VtxDCA=NULL;
      Float_t *fRpc3VtxTime=NULL;
      Float_t *fRpc3VtxX=NULL;
      Float_t *fRpc3VtxY=NULL;
      Float_t *fRpc1VtxDCA=NULL;
      Float_t *fRpc1VtxTime=NULL;
      Float_t *fRpc1VtxX=NULL;
      Float_t *fRpc1VtxY=NULL;

      Float_t *fRpc3St1DCA=NULL;
      Float_t *fRpc3St1Time=NULL;
      Float_t *fRpc3St1X=NULL;
      Float_t *fRpc3St1Y=NULL;
      Float_t *fRpc1St1DCA=NULL;
      Float_t *fRpc1St1Time=NULL;
      Float_t *fRpc1St1X=NULL;
      Float_t *fRpc1St1Y=NULL;

      Float_t *fRpc3St3DCA=NULL;
      Float_t *fRpc3St3Time=NULL;
      Float_t *fRpc3St3X=NULL;
      Float_t *fRpc3St3Y=NULL;
      Float_t *fRpc1St3DCA=NULL;
      Float_t *fRpc1St3Time=NULL;
      Float_t *fRpc1St3X=NULL;
      Float_t *fRpc1St3Y=NULL;

     Float_t *fRpc3MuIDDCA=NULL;
      Float_t *fRpc3MuIDTime=NULL;
      Float_t *fRpc3MuIDX=NULL;
      Float_t *fRpc3MuIDY=NULL;
      Float_t *fRpc1MuIDDCA=NULL;
      Float_t *fRpc1MuIDTime=NULL;
      Float_t *fRpc1MuIDX=NULL;
      Float_t *fRpc1MuIDY=NULL;


      Float_t *fvtx_px=NULL; 
      Float_t *fvtx_py=NULL; 
      Float_t *fvtx_pz=NULL; 
      Float_t *fvtx_dphi=NULL; 
      Float_t *fvtx_dtheta=NULL; 
      Float_t *fvtx_chi=NULL; 
      UInt_t *fvtx_conebits=NULL;
      Float_t *fvtx_mpx=NULL; 
      Float_t *fvtx_mpy=NULL; 
      Float_t *fvtx_mpz=NULL; 
      Float_t *fvtx_mdcaz=NULL; 
      Float_t *fvtx_dr=NULL; 
      UInt_t *fvtx_tracklconebits= NULL;
      Float_t *fvtx_vtx_x= NULL;
      Float_t *fvtx_vtx_y= NULL;
      Float_t *fvtx_vtx_z= NULL;




      int *npart=NULL;



//      cout << "before RPC part" << endl;  
      int test=0;
      if (test==1){
      
      }
      else {

//commented out the next few lines as not really needed
//	 TRpcTrkMap* rpctrk_map1;
//	 try { rpctrk_map1 = TMutNode<TRpcTrkMap>::find_node( _top_node, "TRpcTrkMap" ); }
//	 catch (exception &e ) { MUTOO::TRACE( e.what() );
//	    rpctrk_map1 = NULL;}
      
//cout << "before RPC hitmap defined" << endl;  
	 if(rpchit_map) {  // rpc hit map exists 	
//cout << "inb RPC hitmap, sizwe " << hit_map->size() << endl;         	
	
	    TRpcHitMap::iterator hit_iter = rpchit_map->range();
	
	
	    fNRpcHits12=0;
	    while(hit_iter.next()) { fNRpcHits12++; }
	
//   cout << "inb RPC hitmap, range " << fNRpcHits12 << endl;         
	    if(fNRpcHits12>0){    
	  
	       rpc_t  = new Float_t[fNRpcHits12];
	  
	       rpc_c = new Float_t[fNRpcHits12];
	  
	       arm        = new Int_t[fNRpcHits12];
	       station    = new Int_t[fNRpcHits12];
	       octant     = new Int_t[fNRpcHits12];
	       halfoctant = new Int_t[fNRpcHits12];
	       radsegment = new Int_t[fNRpcHits12];
	       strip      = new Int_t[fNRpcHits12];
	  
	       for(int i=0; i<fNRpcHits12; i++) {
	    
		  rpc_t[i]=-9999;
		  rpc_c[i]=-9999;
		  arm[i]=-9999;
		  station[i]=-9999;
		  octant[i]=-9999;
		  halfoctant[i]=-9999;
		  radsegment[i]=-9999;
		  strip[i]=-9999;
	    
	       }    
	  
	  
//  Int_t iHit=0;
	       fNRpcHits12 = 0;
	       hit_iter = rpchit_map->range();
	  
//     cout << " rpc range" << hit_map->size();
    
	       int st3hits=0;
	       while(TRpcHitMap::pointer hit_ptr = hit_iter.next()) {
//	    if ( fNRpcHits12<10){
		  rpc_t[fNRpcHits12] = (float)hit_ptr->get()->get_t();
	    
//      cout << fNRpcHits12 << " " << hit_ptr->get()->get_arm() << " station " << hit_ptr->get()->get_station()  << "  octant  " <<hit_ptr->get()->get_octant()  << "  half " <<hit_ptr->get()->get_half_octant()  << "  seg " <<hit_ptr->get()->get_rseg()  << "  st " <<hit_ptr->get()->get_strip()  << "  time " << (int)hit_ptr->get()->get_t() << endl ;
		  rpc_c[fNRpcHits12] = hit_ptr->get()->get_arm()*100000+ hit_ptr->get()->get_station()*100000+hit_ptr->get()->get_octant()*10000+hit_ptr->get()->get_half_octant()*1000+hit_ptr->get()->get_rseg()*100+hit_ptr->get()->get_strip() ;
      
	    
		  arm[fNRpcHits12] = hit_ptr->get()->get_arm();      
		  station[fNRpcHits12] = hit_ptr->get()->get_station();         
		  octant[fNRpcHits12] = hit_ptr->get()->get_octant();          
		  halfoctant[fNRpcHits12] = hit_ptr->get()->get_half_octant();      
		  radsegment[fNRpcHits12] = hit_ptr->get()->get_rseg();      
		  strip[fNRpcHits12] = hit_ptr->get()->get_strip();           
	    
		  if(station[fNRpcHits12]==2){
 
		     st3hits++;
	      
		  }
		  fNRpcHits12++; 
	    
	       }
	  
	  
//cout << " Station 3 hist" << st3hits << endl;
	       TBranch *fBranch2 = newsngmuons->GetBranch("_RpcHits");
	       ((TLeaf *) fBranch2->GetListOfLeaves()->At(0))->SetAddress(&fNRpcHits12);
	  
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
	  
	    } //RPC hit loop
	 } //hitmap1 exits
			      
	    
      }



//cout << "before RPCHODO hitmap defined" << endl;  
	 if(rpchodohit_map) {  // rpchodo hit map exists 	
//cout << "inb RPCHODO hitmap, sizwe " << hodohit_map->size() << endl;         	
	
	    TRpcHodoHitMap::iterator hit_iter = rpchodohit_map->range();
	
	
	    fNRpchodoHits12=0;
	    while(hit_iter.next()) { fNRpchodoHits12++; }
	
//   cout << "inb RPCHODO hitmap, range " << fNRpchodoHits12 << endl;         
	    if(fNRpchodoHits12>0){    
	  
	       rpchodo_t  = new Int_t[fNRpchodoHits12];
	  
	  
	       hodoarm        = new Int_t[fNRpchodoHits12];
	       hodostation    = new Int_t[fNRpchodoHits12];
	       hodostrip      = new Int_t[fNRpchodoHits12];
	  
	       for(int i=0; i<fNRpchodoHits12; i++) {	    
		  rpchodo_t[i]=-9999;
		  hodoarm[i]=-9999;
		  hodostation[i]=-9999;
		  hodostrip[i]=-9999;
	    
	       }    
	  
	  
//  Int_t iHit=0;
	       fNRpchodoHits12 = 0;
	       hit_iter = rpchodohit_map->range();
	  
//     cout << " rpchodo range" << hodohit_map->size();
    
	       while(TRpcHodoHitMap::pointer hit_ptr = hit_iter.next()) {
		  rpchodo_t[fNRpchodoHits12] = (int)hit_ptr->get()->get_t();
		  hodoarm[fNRpchodoHits12] = hit_ptr->get()->get_arm();      
		  hodostation[fNRpchodoHits12] = hit_ptr->get()->get_station();         
		  hodostrip[fNRpchodoHits12] = hit_ptr->get()->get_strip();           
	    	  fNRpchodoHits12++; 
	       }
	  
	  
//cout << " Station 3 hist" << st3hits << endl;
	       TBranch *fBranch2 = newsngmuons->GetBranch("_RpchodoHits");
	       ((TLeaf *) fBranch2->GetListOfLeaves()->At(0))->SetAddress(&fNRpchodoHits12);
	  
//cout << " got RPC branches" << endl;
	  
	       fBranch2 = newsngmuons->GetBranch("RpchodoHits");
	       ((TLeaf *) fBranch2->GetListOfLeaves()->At(0))->SetAddress(rpchodo_t);
	  
	  
	       ((TLeaf *) fBranch2->GetListOfLeaves()->At(1))->SetAddress(hodoarm);       
	       ((TLeaf *) fBranch2->GetListOfLeaves()->At(2))->SetAddress(hodostation);   
	       ((TLeaf *) fBranch2->GetListOfLeaves()->At(3))->SetAddress(hodostrip);     
	  
	    } //RPC hit loop
	 } //hodohitmap exits
			      
	 

	
      if (muo) { 
//cout << " in muo \n";
//  fNMU12 = (muo->get_npart()>0)?1:0;


	 fNMU12 = muo->get_npart();
	 if (fNMU12>0) {
		  
		  
	    RPC3_x= new Float_t[fNMU12];
	    RPC3_y= new Float_t[fNMU12];
	    RPC3r_x=new Float_t[fNMU12];
	    RPC3r_y=new Float_t[fNMU12];
		  
	    RPC3_x[0]= -9999;
	    RPC3_y[0]= -9999;
	    RPC3r_x[0]= -9999;
	    RPC3r_y[0]= -9999;
	     
		  
	    res100=new Float_t[fNMU12];
	    res101=new Float_t[fNMU12];
	    res110=new Float_t[fNMU12];
	    res111=new Float_t[fNMU12];
	    res120=new Float_t[fNMU12];
	    res121=new Float_t[fNMU12];
	    res200=new Float_t[fNMU12];
	    res201=new Float_t[fNMU12];
	    res210=new Float_t[fNMU12];
	    res211=new Float_t[fNMU12];
	    res220=new Float_t[fNMU12];
	    res221=new Float_t[fNMU12];
	    res300=new Float_t[fNMU12];
	    res301=new Float_t[fNMU12];
	    res310=new Float_t[fNMU12];
	    res311=new Float_t[fNMU12];


	    Qtot100=new Float_t[fNMU12];
	    Qtot101=new Float_t[fNMU12];
	    Qtot110=new Float_t[fNMU12];
	    Qtot111=new Float_t[fNMU12];
	    Qtot120=new Float_t[fNMU12];
	    Qtot121=new Float_t[fNMU12];
	    Qtot200=new Float_t[fNMU12];
	    Qtot201=new Float_t[fNMU12];
	    Qtot210=new Float_t[fNMU12];
	    Qtot211=new Float_t[fNMU12];
	    Qtot220=new Float_t[fNMU12];
	    Qtot221=new Float_t[fNMU12];
	    Qtot300=new Float_t[fNMU12];
	    Qtot301=new Float_t[fNMU12];
	    Qtot310=new Float_t[fNMU12];
	    Qtot311=new Float_t[fNMU12];


	    Nstr100=new Int_t[fNMU12];
	    Nstr101=new Int_t[fNMU12];
	    Nstr110=new Int_t[fNMU12];
	    Nstr111=new Int_t[fNMU12];
	    Nstr120=new Int_t[fNMU12];
	    Nstr121=new Int_t[fNMU12];
	    Nstr200=new Int_t[fNMU12];
	    Nstr201=new Int_t[fNMU12];
	    Nstr210=new Int_t[fNMU12];
	    Nstr211=new Int_t[fNMU12];
	    Nstr220=new Int_t[fNMU12];
	    Nstr221=new Int_t[fNMU12];
	    Nstr300=new Int_t[fNMU12];
	    Nstr301=new Int_t[fNMU12];
	    Nstr310=new Int_t[fNMU12];
	    Nstr311=new Int_t[fNMU12];


	    fRpc3VtxDCA = new Float_t[fNMU12];
	    fRpc3VtxTime = new Float_t[fNMU12];
	    fRpc3VtxX = new Float_t[fNMU12];
	    fRpc3VtxY = new Float_t[fNMU12];
	    fRpc1VtxDCA = new Float_t[fNMU12];
	    fRpc1VtxTime = new Float_t[fNMU12];
	    fRpc1VtxX = new Float_t[fNMU12];
	    fRpc1VtxY = new Float_t[fNMU12];

	    fRpc3St1DCA = new Float_t[fNMU12];
	    fRpc3St1Time = new Float_t[fNMU12];
	    fRpc3St1X = new Float_t[fNMU12];
	    fRpc3St1Y = new Float_t[fNMU12];
	    fRpc1St1DCA = new Float_t[fNMU12];
	    fRpc1St1Time = new Float_t[fNMU12];
	    fRpc1St1X = new Float_t[fNMU12];
	    fRpc1St1Y = new Float_t[fNMU12];

	    fRpc3St3DCA = new Float_t[fNMU12];
	    fRpc3St3Time = new Float_t[fNMU12];
	    fRpc3St3X = new Float_t[fNMU12];
	    fRpc3St3Y = new Float_t[fNMU12];
	    fRpc1St3DCA = new Float_t[fNMU12];
	    fRpc1St3Time = new Float_t[fNMU12];
	    fRpc1St3X = new Float_t[fNMU12];
	    fRpc1St3Y = new Float_t[fNMU12];

	    fRpc3MuIDDCA = new Float_t[fNMU12];
	    fRpc3MuIDTime = new Float_t[fNMU12];
	    fRpc3MuIDX = new Float_t[fNMU12];
	    fRpc3MuIDY = new Float_t[fNMU12];
	    fRpc1MuIDDCA = new Float_t[fNMU12];
	    fRpc1MuIDTime = new Float_t[fNMU12];
	    fRpc1MuIDX = new Float_t[fNMU12];
	    fRpc1MuIDY = new Float_t[fNMU12];	
	
	    station1hits=new Int_t[fNMU12];
	    station2hits=new Int_t[fNMU12];
	    station3hits=new Int_t[fNMU12];
	

	
	    allstation1hits=new Int_t[fNMU12];
	    allstation2hits=new Int_t[fNMU12];
	    allstation3hits=new Int_t[fNMU12];

	    allhits=new Int_t[fNMU12];
	
	    invmass1=new Float_t[fNMU12];
	    invmass2=new Float_t[fNMU12];
	    invmass3=new Float_t[fNMU12];
	
	
	    icharge1=new Int_t[fNMU12];
	    icharge2=new Int_t[fNMU12];
	    icharge3=new Int_t[fNMU12];
	
	    thrust1=new Float_t[fNMU12];
	    thrust2=new Float_t[fNMU12];
	    thrust3=new Float_t[fNMU12];
	

// muon variables
	
	
	    dS30 = new Float_t[fNMU12];     
	    dS3ctp0 = new Float_t[fNMU12];  
	    muIDchis0 = new Float_t[fNMU12];
	    DG0 = new Float_t[fNMU12];	     
	    DG4 = new Float_t[fNMU12];	     
	    DCA_r = new Float_t[fNMU12];	     
	    DCA_z = new Float_t[fNMU12];	     
	    DDG0 = new Float_t[fNMU12];          
	    DS0  = new Float_t[fNMU12];     

	    dS30[0]=999; dS3ctp0[0]=999; muIDchis0[0]=999; DG0[0]=999; DDG0[0]=999; DS0[0]=999;
	
	    pseudo_rapidity = new Float_t[fNMU12];
	    muIDquad0= new Int_t[fNMU12];	 
	    muTRhits0 = new Int_t[fNMU12];		 
	    lastGap= new Int_t[fNMU12]; 
	
	    pseudo_rapidity[0] = 999;
	    muIDquad0[0]=-1;	 
	    muTRhits0[0]=0;		 
	    lastGap[0]=0;                       

	    charge = new Float_t[fNMU12];	
	    muchisquare = new Float_t[fNMU12];
	
	    refit_z = new Float_t[fNMU12];
	    status = new Int_t[fNMU12];
	    newphi = new Float_t[fNMU12];
	    ghostflag = new Float_t[fNMU12];

	
	    gap0x = new Float_t[fNMU12]; 
	    gap0y = new Float_t[fNMU12]; 
	    gap0z = new Float_t[fNMU12];			
	    dirX = new Float_t[fNMU12];
	    dirY = new Float_t[fNMU12];
	    refX  = new Float_t[fNMU12];
	    refY = new Float_t[fNMU12];
	    muTRhits  = new Float_t[fNMU12];
	    muIDhits = new Int_t[fNMU12];


	    mutr_nhits = new Float_t[fNMU12];
	    muid_nhits = new Float_t[fNMU12];
	
	    px =         new Float_t[fNMU12];
	    py =         new Float_t[fNMU12];
	    pz =         new Float_t[fNMU12];
	
	    xSTI  =      new Float_t[fNMU12];
	    ySTI  =      new Float_t[fNMU12];
	    zSTI  =      new Float_t[fNMU12];
	    pxSTI =      new Float_t[fNMU12];
	    pySTI =      new Float_t[fNMU12];
	    pzSTI =      new Float_t[fNMU12];
	
	    pxSTII =      new Float_t[fNMU12];
	    pySTII =      new Float_t[fNMU12];
	    pzSTII =      new Float_t[fNMU12];
	
	    xSTII  =     new Float_t[fNMU12];
	    ySTII  =     new Float_t[fNMU12];
	    zSTII  =     new Float_t[fNMU12];
	 
	    xSTIII  =    new Float_t[fNMU12];
	    ySTIII  =    new Float_t[fNMU12];
	    zSTIII  =    new Float_t[fNMU12];
	    pxSTIII =    new Float_t[fNMU12];
	    pySTIII =    new Float_t[fNMU12];
	    pzSTIII =    new Float_t[fNMU12];
	 
	    pT =         new Float_t[fNMU12];
	    p =          new Float_t[fNMU12];
	    pSTI =       new Float_t[fNMU12];
	    ELoss =      new Float_t[fNMU12];
	
	
	    rpc3z = new  Float_t [fNMU12];
	    	
	    rpc3st3  = new	 Float_t[fNMU12]; 
	    rpc3g5  = new	 Float_t[fNMU12]; 

	    Float_t costheta;
	    Float_t costheta_xyz;
	    Float_t xyz_St1;
	
	    dANGLE = new Float_t[fNMU12];
	    dANGLE_xyz= new Float_t[fNMU12];
	    dPHI= new Float_t[fNMU12];
	    dTHETA= new Float_t[fNMU12];
	    ROAD_SLOPE= new Float_t[fNMU12];
	
	
	    ref_vtx_rdca = new Float_t[fNMU12];
	    ref_vtx_r = new Float_t[fNMU12];
	    ref_vtx_z = new Float_t[fNMU12];
	
	
	    pseudotrigN_1S   = new Int_t[fNMU12];
	    pseudotrigS_SG1D   = new Int_t[fNMU12];
	    pseudotrigS_1S   = new Int_t[fNMU12];
	    pseudotrigN_SG1D   = new Int_t[fNMU12];
	    recoNS_1D = new Int_t[fNMU12];
	    recoS_SG1D = new Int_t[fNMU12];
	    recoNS_1H = new Int_t[fNMU12];
	    recoN_SG1D = new Int_t[fNMU12];	

	    MC_N_PART = new Float_t[fNMU12];   // total # of MC particles associated with this reco'd track
	    MC_PX= new     Float_t[fNMU12];        // the major MC particle's px
	    MC_PY= new     Float_t[fNMU12];
	    MC_PZ= new     Float_t[fNMU12];
	    MC_PTOT = new  Float_t[fNMU12];      // the major MC particle's lastGap
	    MC_X= new      Float_t[fNMU12];         // the major MC particle's X_orign
	    MC_Y= new      Float_t[fNMU12];         // the major MC particle's Y_orign
	    MC_Z= new      Float_t[fNMU12];         // the major MC particle's Z_orign
	    MC_PID = new   Float_t[fNMU12];
	    MC_HITS = new  Float_t[fNMU12];     // the major MC particle's total contribution to muTr hits
	
	    MC_P_PID = new Float_t[fNMU12];    // MC particle's parent information
	    MC_P_PX = new  Float_t[fNMU12];     //
	    MC_P_PY = new  Float_t[fNMU12];
	    MC_P_PZ = new  Float_t[fNMU12];
	    MC_P_PTOT = new Float_t[fNMU12] ;
	    MC_P_Z = new   Float_t[fNMU12];
	
	    MC_D_PID = new Float_t[fNMU12];    // MC particle's daughter information
	    MC_D_PX = new  Float_t[fNMU12];     //
	    MC_D_PY = new  Float_t[fNMU12];
	    MC_D_PZ = new  Float_t[fNMU12];
	    MC_D_PTOT = new Float_t[fNMU12];
	    MC_D_Z = new   Float_t[fNMU12] ;
	    MC_D_N = new   Float_t[fNMU12] ;
	
	    MC_G_PID = new Float_t[fNMU12] ;    // MC particle's grandparent (ancestor, original, very primary) informationi
	    MC_G_PX = new  Float_t[fNMU12] ;     //
	    MC_G_PY = new  Float_t[fNMU12] ;
	    MC_G_PZ = new  Float_t[fNMU12] ;
	    MC_G_PTOT = new Float_t[fNMU12]  ;
	    MC_G_Z = new   Float_t[fNMU12] ;
	
	    MC_TRK   = new  Int_t[fNMU12]   ;
	    MC_P_TRK = new  Int_t[fNMU12]   ;    // parent track ID: for debuggin now
	    MC_D_TRK = new  Int_t[fNMU12]   ;    // daughter track ID: for debuggin now
	    MC_G_TRK = new  Int_t[fNMU12];    // grandparent track ID: for debuggin now

	    RpcDCA  = new Float_t[fNMU12];
	    RpcpT   = new Float_t[fNMU12];
	    Rpctime = new Float_t[fNMU12];

	    Rpc1DCA  = new Float_t[fNMU12];
	    Rpc1time = new Float_t[fNMU12];

	    //	    printf("before defining DCA zmult\n");
	    DCA_zmult0 = new Float_t[fNMU12];	     	
	    DCA_zmult1 = new Float_t[fNMU12];	     
	    DCA_zmult2 = new Float_t[fNMU12];	     	
	    DCA_zmult3 = new Float_t[fNMU12];	     	
	    DCA_zmult4 = new Float_t[fNMU12];	     	
	    DCA_zmult5 = new Float_t[fNMU12];	     
	    DCA_zmult6 = new Float_t[fNMU12];	     	
	    DCA_zmult7 = new Float_t[fNMU12];	     	
	    DCA_zmult8 = new Float_t[fNMU12];	     	
	    zmult0 = new Float_t[fNMU12];	     	
	    zmult1 = new Float_t[fNMU12];	     
	    zmult2 = new Float_t[fNMU12];	     	
	    zmult3 = new Float_t[fNMU12];	     	
	    zmult4 = new Float_t[fNMU12];	     	
	    zmult5 = new Float_t[fNMU12];	     
	    zmult6 = new Float_t[fNMU12];	     	
	    zmult7 = new Float_t[fNMU12];	     	
	    zmult8 = new Float_t[fNMU12];	     	
	

	    fvtx_px= new Float_t[fNMU12];
	    fvtx_py= new Float_t[fNMU12];
	    fvtx_pz= new Float_t[fNMU12];
	    fvtx_dphi= new Float_t[fNMU12];
	    fvtx_dtheta= new Float_t[fNMU12];
	    fvtx_chi= new Float_t[fNMU12];
	    fvtx_conebits = new UInt_t[fNMU12];
	    fvtx_mpx= new Float_t[fNMU12];
	    fvtx_mpy= new Float_t[fNMU12];
	    fvtx_mpz= new Float_t[fNMU12];
	    fvtx_mdcaz= new Float_t[fNMU12];
	    fvtx_dr= new Float_t[fNMU12];


	    fvtx_tracklconebits= new UInt_t[fNMU12];
	    fvtx_vtx_x= new Float_t[fNMU12];
	    fvtx_vtx_y= new Float_t[fNMU12];
	    fvtx_vtx_z= new Float_t[fNMU12];




	
	    npart = new int[fNMU12];

	    for(int i=0; i<fNMU12; i++){ 
	       RPC3_x [i]=-9999;
	       RPC3_y [i]=-9999;
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
	       pxSTII  [i]=-9999;
	       pySTII  [i]=-9999;
	       pzSTII  [i]=-9999;
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
	       rpc3st3   [i]=-9999;
	       rpc3g5   [i]=-9999;
	       dANGLE  [i]=-9999;
	       dANGLE_xyz [i]=-9999;
	       dPHI [i]=-9999;
	       dTHETA [i]=-9999;
	       ROAD_SLOPE [i]=-9999;
	       ref_vtx_rdca  [i]=-9999;
	       ref_vtx_r  [i]=-9999;
	       ref_vtx_z  [i]=-9999;
	       pseudotrigN_1S    [i]=-9999;
	       pseudotrigS_SG1D    [i]=-9999;
	       pseudotrigS_1S    [i]=-9999;
	       pseudotrigN_SG1D    [i]=-9999;
	       recoNS_1D  [i]=-9999;
	       recoS_SG1D  [i]=-9999;
	       recoNS_1H  [i]=-9999;
	       recoN_SG1D  [i]=-9999;
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
	       Rpc1DCA  [i]=-9999;
	       Rpc1time [i]=-9999;
	       npart  [i]=-9999;
	       DCA_zmult0[i]=-9999;
	       DCA_zmult1[i]=-9999;
	       DCA_zmult2[i]=-9999;
	       DCA_zmult3[i]=-9999;
	       DCA_zmult4[i]=-9999;
	       DCA_zmult5[i]=-9999;
	       DCA_zmult6[i]=-9999;
	       DCA_zmult7[i]=-9999;
	       DCA_zmult8[i]=-9999;
	       zmult0[i]=-9999;
	       zmult1[i]=-9999;
	       zmult2[i]=-9999;
	       zmult3[i]=-9999;
	       zmult4[i]=-9999;
	       zmult5[i]=-9999;
	       zmult6[i]=-9999;
	       zmult7[i]=-9999;
	       zmult8[i]=-9999;




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

	       Qtot100[i]=-9999;
	       Qtot101[i]=-9999;
	       Qtot110[i]=-9999;
	       Qtot111[i]=-9999;
	       Qtot120[i]=-9999;
	       Qtot121[i]=-9999;
	       Qtot200[i]=-9999;
	       Qtot201[i]=-9999;
	       Qtot210[i]=-9999;
	       Qtot211[i]=-9999;
	       Qtot220[i]=-9999;
	       Qtot221[i]=-9999;
	       Qtot300[i]=-9999;
	       Qtot301[i]=-9999;
	       Qtot310[i]=-9999;
	       Qtot311[i]=-9999;

	       Nstr100[i]=-9999;
	       Nstr101[i]=-9999;
	       Nstr110[i]=-9999;
	       Nstr111[i]=-9999;
	       Nstr120[i]=-9999;
	       Nstr121[i]=-9999;
	       Nstr200[i]=-9999;
	       Nstr201[i]=-9999;
	       Nstr210[i]=-9999;
	       Nstr211[i]=-9999;
	       Nstr220[i]=-9999;
	       Nstr221[i]=-9999;
	       Nstr300[i]=-9999;
	       Nstr301[i]=-9999;
	       Nstr310[i]=-9999;
	       Nstr311[i]=-9999;


	       fRpc3VtxDCA[i] = -9998;
	       fRpc3VtxTime[i] = -9998;
	       fRpc3VtxX[i] = -9998;
	       fRpc3VtxY[i] = -9998;
	       fRpc1VtxDCA[i] = -9998;
	       fRpc1VtxTime[i] = -9998;
	       fRpc1VtxX[i] = -9998;
	       fRpc1VtxY[i] = -9998;

	       fRpc3St1DCA[i] = -9998;
	       fRpc3St1Time[i] = -9998;
	       fRpc3St1X[i] = -9998;
	       fRpc3St1Y[i] = -9998;
	       fRpc1St1DCA[i] = -9998;
	       fRpc1St1Time[i] = -9998;
	       fRpc1St1X[i] = -9998;
	       fRpc1St1Y[i] = -9998;

	       fRpc3St3DCA[i] = -9998;
	       fRpc3St3Time[i] = -9998;
	       fRpc3St3X[i] = -9998;
	       fRpc3St3Y[i] = -9998;
	       fRpc1St3DCA[i] = -9998;
	       fRpc1St3Time[i] = -9998;
	       fRpc1St3X[i] = -9998;
	       fRpc1St3Y[i] = -9998;

	       fRpc3MuIDDCA[i] = -9998;
	       fRpc3MuIDTime[i] = -9998;
	       fRpc3MuIDX[i] = -9998;
	       fRpc3MuIDY[i] = -9998;
	       fRpc1MuIDDCA[i] = -9998;
	       fRpc1MuIDTime[i] = -9998;
	       fRpc1MuIDX[i] = -9998;
	       fRpc1MuIDY[i] = -9998;


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


	       fvtx_px[i]=-9999;
	       fvtx_py[i]=-9999;
	       fvtx_pz[i]=-9999;
	       fvtx_dphi[i]=-9999;
	       fvtx_dtheta[i]=-9999;
	       fvtx_chi[i]=-9999;
	       fvtx_conebits[i]=0;
	       fvtx_mpx[i]=-9999;
	       fvtx_mpy[i]=-9999;
	       fvtx_mpz[i]=-9999;
	       fvtx_mdcaz[i]=-9999;
	       fvtx_dr[i]=-9999;


	       fvtx_tracklconebits[i]=0;
	       fvtx_vtx_x[i]=-9999;
	       fvtx_vtx_y[i]=-9999;
	       fvtx_vtx_z[i]=-9999;



	    } // initialization

    
	    //	    npart[0] = muo->get_npart();
	    totMU += fNMU12;
    

//	    cout << npart[0] << " muon candidates, new res"<< endl;




	    for (int ipart=0; ipart<fNMU12; ipart++) 
	    {
	       //cout << "ipart  " << ipart << endl;	

	      npart[ipart] = ipart;

	      //	       Float_t fTracks_Rpcp ;
	       Float_t fTracks_Rpcpx;
	       Float_t fTracks_Rpcpy;
	       Float_t fTracks_Rpcpz;
	       Float_t fTracks_RpcDca  ;
	       //	       Float_t fTracks_RpcDcaVt;
	       //     Float_t fTracks_Rpctime ;
	       Float_t fTracks_RpcPt   ;

	       //	       Float_t fTracks_RpcDca_1  ;
	       Float_t fTracks_RpcDcaVt_1;
	       Float_t fTracks_Rpctime_1 ;
	    if (rpctrk_map2){
//	cout << "after hitmap2 " << endl;	

	       TRpcTrkMap::iterator trk_iter = rpctrk_map2->range();
//cout << "rpctrack hitmap range " << rpctrk_map2->size()<< endl;	
	       while(TRpcTrkMap::pointer trk_ptr = trk_iter.next()) {
	  
		  TMutTrkPar *fTrkPar = (TMutTrkPar *) trk_ptr->get()->get_trk_par_vtx();
//TMutTrkPar *fTrkPar = (TMutTrkPar *) trk_ptr->get()->get_trk_par();
//int charge_r = fTrkPar->get_charge();
	
		  Float_t p2 = fTrkPar->get_px()*fTrkPar->get_px();
		  p2 += fTrkPar->get_py()*fTrkPar->get_py();
		  p2 += fTrkPar->get_pz()*fTrkPar->get_pz();
	
		  //  fTracks_Rpcp = sqrt(p2);
		  fTracks_Rpcpx = fTrkPar->get_px();
		  fTracks_Rpcpy = fTrkPar->get_py();
		  fTracks_Rpcpz = fTrkPar->get_pz();


		  if ( fabs( muo->get_px(0,ipart) -  fTracks_Rpcpx ) < 0.1 && 
		       fabs( muo->get_py(0,ipart) -  fTracks_Rpcpy ) < 0.1 && 
		       fabs( muo->get_pz(0,ipart) -  fTracks_Rpcpz ) < 0.1)
		  {
		     fTracks_RpcDca   = trk_ptr->get()->get_dca_trk();
		     //     fTracks_RpcDcaVt = trk_ptr->get()->get_dca_trk_vtx();
		     // fTracks_Rpctime  = trk_ptr->get()->get_rpcclus3time();
		     fTracks_RpcPt    = sqrt((pow(fTracks_Rpcpx,2)+pow(fTracks_Rpcpy,2))/p2); //trk_ptr->get()->get_corr_pT();


		     //		     fTracks_RpcDca_1   = trk_ptr->get()->get_dca_trk();
		     fTracks_RpcDcaVt_1 = trk_ptr->get()->get_dca_trk_vtx1();
		     fTracks_Rpctime_1  = trk_ptr->get()->get_rpcclus1time_vtx();
		   

		     RpcDCA[ipart]  = fTracks_RpcDca;
		     RpcpT[ipart]   = fTracks_RpcPt ;
		     Rpctime[ipart] = (float)trk_ptr->get()->get_rpcclus3time();//fTracks_Rpctime; 
		     Rpc1DCA[ipart]  = fTracks_RpcDcaVt_1;
		     Rpc1time[ipart] = fTracks_Rpctime_1;


		  }
	
	       }
	    } // rpctrk_map2
	   
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
	



// add simple J/psi,Z,cosmic track match
	       if (fNMU12>1){
		  for (int ipart2=0; ipart2<fNMU12; ipart2++) {
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
		     combi++;

	    
	    
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

		  }//ipart2
	  
	  
	  
	       } //more than one track


	
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
	
	       pxSTII[ipart] = muo->get_px(2,ipart);
	       pySTII[ipart] = muo->get_py(2,ipart);
	       pzSTII[ipart] = muo->get_pz(2,ipart);
	
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


	



	
// single muon v2
	       float phi0 = atan2(py[ipart],px[ipart]);
	       //	       float dphi = phi0 - RPbbcrp12[0];
	       //	       dphi = 0.5*atan2(sin(2*dphi),cos(2*dphi));
	       //	float v2_obs = cos(2*dphi);
	       
	       //	       float dphi_N = phi0 - RPbbcrp10[0];
	       //	       dphi_N = 0.5*atan2(sin(2*dphi_N),cos(2*dphi_N));
	       //	float v2_obs_N = cos(2*dphi_N);
	       
	       //	       float dphi_S = phi0 - RPbbcrp11[0];
	       //	       dphi_S = 0.5*atan2(sin(2*dphi_S),cos(2*dphi_S));
	       //	float v2_obs_S = cos(2*dphi_S);
	       

	       pseudotrigN_1S[ipart]   = -1;
	       pseudotrigS_SG1D[ipart]   = -1;
	       pseudotrigS_1S[ipart]   = -1;
	       pseudotrigN_SG1D[ipart]   = -1;
	       recoNS_1D[ipart] = -1;
	       recoS_SG1D[ipart] = -1;
	       recoNS_1H[ipart] = -1;
	       recoN_SG1D[ipart] = -1;



//	       cout << "before pseudotrigger \n";	
//	       if (_choice == "simu" || _choice == "simu_file") {
		  pseudotrigN_SG1D[ipart] =  Tools::LL1_1D_Decision( MUTOO::North );
		  pseudotrigN_1S[ipart] =  Tools::LL1_1S_Decision( MUTOO::North );
	  
		  pseudotrigS_SG1D[ipart] =  Tools::LL1_1D_Decision( MUTOO::South );
		  pseudotrigS_1S[ipart] =  Tools::LL1_1S_Decision( MUTOO::South );
//	       }


	       double prim_x=-90;
	       //	       double prim_y=-90;
	
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
//	       if (!mut_trk_map) cout << "no mut_trk_map " << endl;
//	       if (!mut_mctrk_map) cout << "not mut_mctrk_map "  << endl;



	       int id = -1;	  // track index, will be used later 
	       if (mut_trk_map) {

//	  printf(" in mut_trk map\n"); 
		  double res[16]={-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.};
		  double qtot_[16]={-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.,-999.};
		  int clsz_[16]={-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999};
		  int nhits1=0;
		  int nhits2=0;
		  int nhits3=0;
		  int allnhits1=0;
		  int allnhits2=0;
		  int allnhits3=0;
		  int armnumber=0;

		  int nhits=0;
	  
		  

		  TMutTrkMap::const_iterator trk_iter = mut_trk_map->range();
	  
		  int itrktest=0;
//		  int range = mut_trk_map->size();
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
		     itrktest++;
		     if (diff_x > 0.000001) continue; // not the same track


//try residual
//	      trk_ptr->get()->print();
	       

		     TMutTrk *trk4 = trk_ptr->get();
		     id = trk4->get_index(); 
//		     cout << " just got id " << id ;
//		     cout << " size " << range;
//		     cout <<  " trk number " << itrktest << endl;
//		     TMutHitMap* muthit_map1 =  TMutNode<TMutHitMap>::find_node( _top_node,"TMutHitMap" );
		     std::vector<TMutTrkRes>::const_iterator residual_iter = (trk4->get_w_residual_list())->begin();
 
		     int testt=0;
		     for(;residual_iter != (trk4->get_w_residual_list())->end(); ++residual_iter){ 
//residual_iter->print();
			testt = residual_iter->get_station() * 6 +  residual_iter->get_gap() * 2 + residual_iter->get_cathode();
			res[testt]=residual_iter->get_w_trk()-residual_iter->get_w_meas();
		
			TMutHitMap::iterator hit_iter = muthit_map1->get(residual_iter->get_arm(),residual_iter->get_station(),residual_iter->get_octant(),residual_iter->get_half_octant(),residual_iter->get_gap(),residual_iter->get_cathode());//range();
		
		
			while(hit_iter.next()){ 
			   if(residual_iter->get_station()==0)
			      nhits1++;
			   if(residual_iter->get_station()==1)
			      nhits2++;
			   if(residual_iter->get_station()==2)
			      nhits3++;

			}

//		printf("total hits are %d, %d, %d, range %d\n",nhits1,nhits2,nhits3,hit_map1->size());
			nhits=muthit_map1->size();
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
				    TMutHitMap::iterator hit_iter2 = muthit_map1->get(armnumber,sta,oct,hoct,gap,cath);//range();
				    while(hit_iter2.next()){ 
				       if(sta==0)
					  allnhits1++;
				       if(sta==1)
					  allnhits2++;
				       if(sta==2)
					  allnhits3++;
				    } //while
				 } //cath
			      } //gap
			   } //hoct
			} //oct
		     } //sta
// }// arm
		   
  TMutCoordMap::key_iterator kit_coord
    = trk4->get_associated< TMutCoord >();

  while( TMutCoordMap::pointer ptr_coord = kit_coord.next() ){
     // cout << "in coordinate map " << endl;
TMutCoord* mut_coord = ptr_coord->get();

    int st = mut_coord->get_station();
    int gap = mut_coord->get_gap();
    int cath = mut_coord->get_cathode();
    int pid_ = cath + 2 * gap + 6 * st;

    //    pkstrp_[ pid_ ] = mut_coord->get_peak_strip();
    qtot_[ pid_ ] = mut_coord->get_q_tot();
    //pkw_[ pid_ ] = mut_coord->get_w();
    //qpk_[ pid_ ] = mut_coord->get_q_peak();

    TMutClusMap::key_iterator kit_clus
      = mut_coord->get_associated< TMutClus >();
    while( TMutClusMap::pointer ptr_clus = kit_clus.next() ){
      TMutClus* mut_clus = ptr_clus->get();

      clsz_[ pid_ ] = mut_clus->get_n_strip();
      //clchi2_[ pid_ ] = mut_clus->get_chi_square();
      //clncent_[ pid_ ] = mut_clus->get_n_centroid();

    }

  }

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



		  Qtot100[ipart]=qtot_[0];
		  Qtot101[ipart]=qtot_[1];
		  Qtot110[ipart]=qtot_[2];
		  Qtot111[ipart]=qtot_[3];
		  Qtot120[ipart]=qtot_[4];
		  Qtot121[ipart]=qtot_[5];
		  Qtot200[ipart]=qtot_[6];
		  Qtot201[ipart]=qtot_[7];
		  Qtot210[ipart]=qtot_[8];
		  Qtot211[ipart]=qtot_[9];
		  Qtot220[ipart]=qtot_[10];
		  Qtot221[ipart]=qtot_[11];
		  Qtot300[ipart]=qtot_[12];
		  Qtot301[ipart]=qtot_[13];
		  Qtot310[ipart]=qtot_[14];
		  Qtot311[ipart]=qtot_[15];



		  Nstr100[ipart]=clsz_[0];
		  Nstr101[ipart]=clsz_[1];
		  Nstr110[ipart]=clsz_[2];
		  Nstr111[ipart]=clsz_[3];
		  Nstr120[ipart]=clsz_[4];
		  Nstr121[ipart]=clsz_[5];
		  Nstr200[ipart]=clsz_[6];
		  Nstr201[ipart]=clsz_[7];
		  Nstr210[ipart]=clsz_[8];
		  Nstr211[ipart]=clsz_[9];
		  Nstr220[ipart]=clsz_[10];
		  Nstr221[ipart]=clsz_[11];
		  Nstr300[ipart]=clsz_[12];
		  Nstr301[ipart]=clsz_[13];
		  Nstr310[ipart]=clsz_[14];
		  Nstr311[ipart]=clsz_[15];

		  station1hits[ipart]=nhits1;
		  station2hits[ipart]=nhits2;
		  station3hits[ipart]=nhits3;

		  allstation1hits[ipart]=allnhits1;
		  allstation2hits[ipart]=allnhits2;
		  allstation3hits[ipart]=allnhits3;

		  allhits[ipart]=nhits;
    

	       }//muttrkmap 
	


		  
		  if(rpc_muotrk_map)
		    {
		      TRpcMuoTrkMap::iterator trk_iter = rpc_muotrk_map->range();
//		      cout << "rpcmuotrack map range " << rpc_muotrk_map->size()<< endl;	
		      while(TRpcMuoTrkMap::pointer trk_ptr = trk_iter.next())
			{
			  if ( fabs(px[ipart] - trk_ptr->get()->get_muo_trk_momentum(0) ) < 0.1 && 
			       fabs(py[ipart] - trk_ptr->get()->get_muo_trk_momentum(1) ) < 0.1 && 
			       fabs(pz[ipart] - trk_ptr->get()->get_muo_trk_momentum(2) ) < 0.1)
			    {


//			      cout << "found matched track " << trk_ptr->get()->get_muo_trk_number() << " " << ipart << endl;
			      fRpc3VtxDCA[ipart]  = trk_ptr->get()->get_dca_vtx(1);
			      fRpc3VtxTime[ipart] = trk_ptr->get()->get_rpcclus_time_vtx(1);
			      fRpc3VtxX[ipart]    = trk_ptr->get()->get_vtx_extrapolated_hit_x(1);
			      fRpc3VtxY[ipart]    = trk_ptr->get()->get_vtx_extrapolated_hit_y(1);
			      fRpc1VtxDCA[ipart]  = trk_ptr->get()->get_dca_vtx(0);
			      fRpc1VtxTime[ipart] = trk_ptr->get()->get_rpcclus_time_vtx(0);
			      fRpc1VtxX[ipart]    = trk_ptr->get()->get_vtx_extrapolated_hit_x(0);
			      fRpc1VtxY[ipart]    = trk_ptr->get()->get_vtx_extrapolated_hit_y(0);

			      fRpc3St3DCA[ipart]  = trk_ptr->get()->get_dca_trk(1,1);
			      fRpc3St3Time[ipart] = trk_ptr->get()->get_rpcclus_time(1,1);
			      fRpc3St3X[ipart]    = trk_ptr->get()->get_trk_extrapolated_hit_x(1,1);
			      fRpc3St3Y[ipart]    = trk_ptr->get()->get_trk_extrapolated_hit_y(1,1);
			      fRpc1St3DCA[ipart]  = trk_ptr->get()->get_dca_trk(0,1);
			      fRpc1St3Time[ipart] = trk_ptr->get()->get_rpcclus_time(0,1);
			      fRpc1St3X[ipart]    = trk_ptr->get()->get_trk_extrapolated_hit_x(0,1);
			      fRpc1St3Y[ipart]    = trk_ptr->get()->get_trk_extrapolated_hit_y(0,1);

			      fRpc3St1DCA[ipart]  = trk_ptr->get()->get_dca_trk(1,0);
			      fRpc3St1Time[ipart] = trk_ptr->get()->get_rpcclus_time(1,0);
			      fRpc3St1X[ipart]    = trk_ptr->get()->get_trk_extrapolated_hit_x(1,0);
			      fRpc3St1Y[ipart]    = trk_ptr->get()->get_trk_extrapolated_hit_y(1,0);
			      fRpc1St1DCA[ipart]  = trk_ptr->get()->get_dca_trk(0,0);
			      fRpc1St1Time[ipart] = trk_ptr->get()->get_rpcclus_time(0,0);
			      fRpc1St1X[ipart]    = trk_ptr->get()->get_trk_extrapolated_hit_x(0,0);
			      fRpc1St1Y[ipart]    = trk_ptr->get()->get_trk_extrapolated_hit_y(0,0);

			      fRpc3MuIDDCA[ipart]  = trk_ptr->get()->get_dca_muid(1);
			      fRpc3MuIDTime[ipart] = trk_ptr->get()->get_rpcclus_time_muid(1);
			      fRpc3MuIDX[ipart]    = trk_ptr->get()->get_muid_extrapolated_hit_x(1);
			      fRpc3MuIDY[ipart]    = trk_ptr->get()->get_muid_extrapolated_hit_y(1);
			      fRpc1MuIDDCA[ipart]  = trk_ptr->get()->get_dca_muid(0);
			      fRpc1MuIDTime[ipart] = trk_ptr->get()->get_rpcclus_time_muid(0);
			      fRpc1MuIDX[ipart]    = trk_ptr->get()->get_muid_extrapolated_hit_x(0);

	    }//matched
			}//trk_ptr
		    }//rpc_trk_map
		  
		  

	       if (mut_trk_map && mut_mctrk_map) {
		  //printf("in mutmctrack loop size: %d\n",mut_trk_map->size()); 	  
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
		     MC_N_PART[ipart]=0;
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
		     // printf("in mctrack loop\n");
// Loop over associated MCTracks
		     mc_trk_iter.reset(); // reset ???
		     while (TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()) {
		
			MC_N_PART[ipart]++;
			//printf("in mctrack loop %d \n",(int)MC_N_PART[ipart]);
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
				    // prim_y = prim_ptr->get()->get_y_orig();
			  
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

//      z_vertex = (float)gRandom->Gaus(0.,30.); // just for the cosmics now...and the cheated fake high pt tracks
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
		     z_vertex_error = 2;
		  }
	  
		  vertex.add_vertex( z_vertex, z_vertex_error );
	  
// refit muon tracks with the correct event vtx (BBC_vtx for real one and PISA_vtx for sim ) fit
		  vertex.fit();
	  
// retrieve track momentum
//		  px_vertex = vertex.get_px( 0 );
		  //		  py_vertex = vertex.get_py( 0 );
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



// calculate track and road positions at rpcs 
	       if(pz[ipart]>0){
		  rpc3z[ipart] = 906.3;
		 	       }
	       else{
		  rpc3z[ipart] = -906.3;
		 	       }
	       rpc3st3[ipart] = -zSTIII[ipart] + rpc3z[ipart];
	       rpc3g5[ipart] = -gap0z[ipart] + rpc3z[ipart];
	       if ( fabs(rpc3g5[ipart]) < 25000 ){
		  RPC3_x[ipart] = ( pxSTIII[ipart]/pzSTIII[ipart]) * rpc3st3[ipart] + xSTIII[ipart];
		  RPC3_y[ipart] = ( pySTIII[ipart]/pzSTIII[ipart]) * rpc3st3[ipart] + ySTIII[ipart];

		  RPC3r_x[ipart] = dirX[ipart] * rpc3g5[ipart] + gap0x[ipart];
		  RPC3r_y[ipart] = dirY[ipart] * rpc3g5[ipart] + gap0y[ipart];

//	cout <<" x" <<  RPC3_x[ipart] << "y "<< RPC3_y[ipart]<<endl;
	       }





	       int moct =( (int)(fmod((phi0+2*TMath::Pi()+TMath::Pi()/8),2*TMath::Pi()) * 8 / (2 * TMath::Pi())));

//	       MutrgHitArray* fMutrgHitArray;

//	       if(fMutrgHitArray){
//fMutrgHitArray->print();
//	       }

//	       printf("pseudotrigs are now %d %d\n",pseudotrigS_SG1D[ipart],pseudotrigN_SG1D[ipart]);
//	       MutrgTrkArray* fMutrgtrkArray;
	       if(fMutrgtrkArray){
		  //printf(" in mutrgtrkarray, size %d",fMutrgtrkArray->GetSize());
		  for(unsigned int itrk=0; itrk<fMutrgtrkArray->GetSize(); itrk++){
		     //printf(" in itrk %d\n",itrk);
		     MutrgTrk *trk=fMutrgtrkArray->Get(itrk);
		     unsigned int key=trk->GetHit(0);

		     int arm,st,oct,hoct,strip;
		     MutrgKey::KeyToLoc(key,arm,st,oct,hoct,strip);
		     //	  printf("oct %d %d  pseudtriggerhit\n",oct,moct);
		     if(moct == oct){
			for(int iclk=0; iclk<MaxNHitClock; iclk++){
			   if(trk->GetHitClock(iclk)){
			      //printf("arm %d pseudtriggerhit, clock %d \n",arm,iclk);
			      if ( arm == 1){
				 pseudotrigN_SG1D[ipart]+=iclk*10 +2 ;
//				 printf("indeed north\n");
			      }
			      if ( arm == 0)
				 pseudotrigS_SG1D[ipart]+=iclk*10 +2 ;
			   }
			}
		     }

		  }


//Imazu's pseudotrig, shoudl be the same as above
//		  cout << "trk id is " << id << " size " << fMutrgtrkArray->GetSize() <<  endl; 
		  if (id >-1){
//	       unsigned int trk_uid( muo->get_uid( id ) );
	       unsigned int trk_uid( muo->get_uid( ipart ) );
//		  cout << "trk uid is " << trk_uid << endl; 
	       int clock_id = 4;
	       for(unsigned int itrk=0; itrk<fMutrgtrkArray->GetSize(); itrk++){
//		  printf(" in itrk %d\n",itrk);
		     MutrgTrk *trk=fMutrgtrkArray->Get(itrk);
//		     cout << "clock " << trk->GetHitClock() << endl;
//		  if( !( trk->GetHitClock()&(0x1<<clock_id ) ) ) continue;
		  if( !( trk->GetHitClock() == clock_id ) ) continue;
//		  cout << "clock ok itrk " << itrk << endl; 
		  for( int j = 0; j < trk->GetMuTrNUid(); j++ )
		     if( trk->GetMuTrUid( j ) == trk_uid ){

//			cout << "did fire " << j << endl; 
		     unsigned int key=trk->GetHit(0);

		     int arm,st,oct,hoct,strip;
		     MutrgKey::KeyToLoc(key,arm,st,oct,hoct,strip);
			      if ( arm == 1){
				 pseudotrigN_SG1D[ipart]+=100 ;
//				 printf("indeed north\n");
			      }
			      if ( arm == 0)
				 pseudotrigS_SG1D[ipart]+=100 ;
//				 printf("indeed south\n");
			   }

		     }

		  }
	       }
	       else{
//		  printf(" somehow not in array...\n");
	       }


//	       printf("pseudotrigs are now %d %d\n",pseudotrigS_SG1D[ipart],pseudotrigN_SG1D[ipart]);
//cout << " DG4 "<< DG4[ipart]<< endl;	
//	       cout << "trying BbcMultipleVtx next" << endl;
//trying to add Oidesans multiple bbc vertices

	       if(bbcm){
//	       cout << "multvtxsize " << bbcm->get_size()<< endl;	
		  int vtxsize = bbcm->get_size();
		  for (int tune =0; tune<vtxsize;tune++){
		     BbcMultipleVtxList* list = bbcm->get_vtxlist(tune);
		     //cout << "got BbcMultipleVtxList "<< tune << endl;
		     //bbcm->print();
		     double vertexdca = 9999.;
		     double z_vertex_new=  999;
		     for(int nvert= 0; nvert<  list->get_vertex_number(); nvert++){
			//cout << "trying to get vertex" << endl;
			float newvertex = list->get_vertex_z( nvert);
			//	  printf("newvertex nr %d bining %d is %4.2f\n",nvert,tune,newvertex );
			MWGVertex vertex;

			double z_vertex_error( 0 );
			if (_framework == MUTOO && do_refit )	try {
			   vertex.set_verbosity( 0 );
			   vertex.add_track( ipart, muo );
			   z_vertex_error = 2;
			   vertex.add_vertex( newvertex, z_vertex_error );
			   vertex.fit();
			   //px_vertex = vertex.get_px( 0 );
			   //py_vertex = vertex.get_py( 0 );
			   //pz_vertex = vertex.get_pz( 0 );
			} catch( std::exception &e ) {
			   cout << e.what() << endl;
			}


			Float_t r_dca,x_dca,ydca,z_dca;
			Tools::DCA(muo,ipart,r_dca,x_dca,ydca,z_dca);
			if(fabs(z_dca-newvertex)<vertexdca){
			   vertexdca=(z_dca-newvertex);
			   z_vertex_new = newvertex;
			}
		     }//loop over reconstructed vertices
		     if(tune==0){
			DCA_zmult0[ipart]=vertexdca;
			zmult0[ipart]=z_vertex_new;	
		     }
		     else		  if(tune==1){
			DCA_zmult1[ipart]=vertexdca;
			zmult1[ipart]=z_vertex_new;	
		     }
		     else 	  if(tune==2){
			DCA_zmult2[ipart]=vertexdca;
			zmult2[ipart]=z_vertex_new;	
		     }
		     else 	  if(tune==3){
			DCA_zmult3[ipart]=vertexdca;
			zmult3[ipart]=z_vertex_new;	
		     }
		     else 	  if(tune==4){
			DCA_zmult4[ipart]=vertexdca;
			zmult4[ipart]=z_vertex_new;	
		     }
		     else 	  if(tune==5){
			DCA_zmult5[ipart]=vertexdca;
			zmult5[ipart]=z_vertex_new;	
		     }
		     else 	  if(tune==6){
			DCA_zmult6[ipart]=vertexdca;
			zmult6[ipart]=z_vertex_new;	
		     }
		     else 	  if(tune==7){
			DCA_zmult7[ipart]=vertexdca;
			zmult7[ipart]=z_vertex_new;	
		     }
		     else 	  if(tune==8){
			DCA_zmult8[ipart]=vertexdca;
			zmult8[ipart]=z_vertex_new;	
		     }
		     //	     delete list;
		  }//loopover binnings

	       }
//	       delete bbcm;

//	       cout << "after vertex" << endl;
//calculate deflection angle, delta theta, delta phi and road_slope.. put in to the X1,X2...
//cout << "_choice" << _choice << endl;
//	cout << "BbcZVertex " << BbcZVertex << endl;
//cout << "refit_z " << vertex.get_vtx_z() << endl;
	
	       if( _choice == "simu" || _choice == "simu_file" ) {
		  BbcZVertex[0] = vertex.get_vtx_z();
	       }
	

	       dANGLE[ipart] =-999;
	       dANGLE_xyz[ipart]=-999;
	       dPHI[ipart]=-999;
	       dTHETA[ipart]=-999;
	       ROAD_SLOPE[ipart]=-999;
	
	       ROAD_SLOPE[ipart] = sqrt(dirX[ipart]*dirX[ipart]+dirY[ipart]*dirY[ipart]);
	
	       xyz_St1 = sqrt(xSTI[ipart]*xSTI[ipart]+ySTI[ipart]*ySTI[ipart]+(zSTI[ipart]-BbcZVertex[0])*(zSTI[ipart]-BbcZVertex[0]) );
	       costheta = (px[ipart]*pxSTI[ipart] + py[ipart]*pySTI[ipart] + pz[ipart]*pzSTI[ipart]) / ( p[ipart]*pSTI[ipart] ) ;
	
	       costheta_xyz = (pxSTI[ipart]*xSTI[ipart] + pySTI[ipart]* ySTI[ipart] + pzSTI[ipart]*(zSTI[ipart]-BbcZVertex[0]) ) / (pSTI[ipart]*xyz_St1) ;
	
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
	       dTHETA[ipart] = (atan((sqrt((xSTI[ipart])*(xSTI[ipart])+(ySTI[ipart])*(ySTI[ipart])))/(sqrt((zSTI[ipart]-BbcZVertex[0])*(zSTI[ipart]-BbcZVertex[0]))))-atan((sqrt((xSTIII[ipart])*(xSTIII[ipart])+(ySTIII[ipart])*(ySTIII[ipart])))/(sqrt((zSTIII[ipart]-BbcZVertex[0])*(zSTIII[ipart]-BbcZVertex[0]))))) ;
	
//-----------------------------------------------------------------------------
	
// calculate reference vtx form track
// the shortest distance between the trk at station 1 and beam line
// two space points are (xSTI, ySTI, zSTI) and (0, 0, BbcZVertex)
// two vetctors  are (pxSTI, pySTI, pzSTI) and (0,0,1)
	
//	       Float_t ref_vtx_zdca;
	

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
	
	       //	       ref_vtx_zdca =(Z1+Z2)/2; // z DCA vertex !
	       //	       ref_vtx_ydca =(Y1+Y2)/2;
	       //	       ref_vtx_xdca =(X1+X2)/2;
	       ref_vtx_rdca[0] = sqrt( pow((X2-X1),2)+pow((Y2-Y1),2)+pow((Z2-Z1),2) );
	
//distance between bbcZ and Z1;
	       ref_vtx_z[ipart] = Z1;
//  cout << "X1, Y1,Z1 = " << X1 << " " << Y1 << " " <<Z1 << endl;
//  cout << "X2, Y2,Z2 = " << X2 << " " << Y2 << " " <<Z2 <<  " " << BbcZVertex << endl;
	
//calculate reference vtx form track
	
	       float z_ref = muo->get_zpos(0,ipart);
	
	       float x_ref = muo->get_xpos(0,ipart)+( BbcZVertex[0]-z_ref)*(px[ipart]/pz[ipart]);
	       float y_ref = muo->get_ypos(0,ipart) + ( BbcZVertex[0]-z_ref)* (py[ipart]/pz[ipart]);
	
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
	       accMU++;
	       const unsigned int muo_version = MWGVersion::get(muo->ClassName());
	       if (muo_version >= 13){
		 // fvtx related variables 	     
		 fvtx_px[ipart]=muo->get_fvtx_p(ipart,0);
		 fvtx_py[ipart]=muo->get_fvtx_p(ipart,1);
		 fvtx_pz[ipart]=muo->get_fvtx_p(ipart,2);
		 fvtx_dphi[ipart]=muo->get_fvtx_dphi(ipart);
		 fvtx_dtheta[ipart]=muo->get_fvtx_dtheta(ipart);
		 fvtx_chi[ipart]=muo->get_fvtx_chi2(ipart);
		 fvtx_mpx[ipart]=muo->get_fvtxmutr_p(ipart,0);
		 fvtx_mpy[ipart]=muo->get_fvtxmutr_p(ipart,1);
		 fvtx_mpz[ipart]=muo->get_fvtxmutr_p(ipart,2);
		 fvtx_mdcaz[ipart]=BbcZVertex[0]-muo->get_fvtxmutr_vtx(ipart,2);
		 fvtx_dr[ipart]=muo->get_fvtx_dr(ipart);
		 
		 fvtx_vtx_x[ipart]=muo->get_fvtxmutr_vtx(ipart,0);
		 fvtx_vtx_y[ipart]=muo->get_fvtxmutr_vtx(ipart,1);
		 fvtx_vtx_z[ipart]=muo->get_fvtxmutr_vtx(ipart,2);
	       }


	       //following Cesars mFillSingleMuonContainer.cc
	       if (muo_version >= 15)
		 {
		   fvtx_conebits[ipart]=muo->get_fvtx_clusters_cone(ipart);
		   fvtx_tracklconebits[ipart]=muo->get_fvtx_tracklets_cone(ipart);
		   //		   printf(" cone variables should  be %lu %lu \n",muo->get_fvtx_clusters_cone(ipart),muo->get_fvtx_tracklets_cone(ipart));
		 }
	       else  if (muo_version >= 13)
		 {
		   if (fvtx_cone)
		     {
		       const PHMuoTracksOut * muo_cone = fvtx_cone->get_muo_local();
		       
		       if (!muo_cone)
			 {
			   cout <<"mFillSingleMuonContainer::process_event - Error - "
				<<"cannot read local PHMuoTracksOut from FvtxConeTracklets"<<endl;
			 }
		       else
			 {
			   fvtx_conebits[ipart]=muo_cone->get_fvtx_clusters_cone(ipart);
			   fvtx_tracklconebits[ipart]=muo_cone->get_fvtx_tracklets_cone(ipart);
			   // 	muon->set_nfvtx_tracklets_cone(muo_cone->get_fvtx_tracklets_cone(imu));
			   //	muon->set_nfvtx_tracklets(muo_cone->get_nfvtx_tracklets(imu));
			   //muon->set_nfvtx_clusters_cone(muo_cone->get_fvtx_clusters_cone(imu));
			 }
		     }

		 }

	    } // for

//cout << " after tracks loop" << endl;



	    TBranch *fBranch = newsngmuons->GetBranch("_RecoTracks");
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(&fNMU12);
	
	    fBranch = newsngmuons->GetBranch("RecoTracks");
	
//cout << " got track branches" << endl;
  	
//=== Fill branches
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
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(39))->SetAddress(pseudotrigN_1S);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(40))->SetAddress(pseudotrigS_SG1D);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(41))->SetAddress(pseudotrigS_1S);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(42))->SetAddress(pseudotrigN_SG1D);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(43))->SetAddress(recoNS_1D);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(44))->SetAddress(recoS_SG1D);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(45))->SetAddress(recoNS_1H);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(46))->SetAddress(recoN_SG1D);
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
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(95))->SetAddress(RPC3_x);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(96))->SetAddress(RPC3_y);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(97))->SetAddress(RPC3r_x);

	    ((TLeaf *) fBranch->GetListOfLeaves()->At(98))->SetAddress(RPC3r_y);
//cout << "pseudorap is " << pseudo_rapidity[0] << endl;
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(99))->SetAddress(DG4);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(100))->SetAddress(DCA_r);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(101))->SetAddress(DCA_z);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(102))->SetAddress(pxSTII);		          // muon px at St1
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(103))->SetAddress(pySTII);		          // muon py at St1
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(104))->SetAddress(pzSTII);		          // muon pz at St1

//cout << " after setting tack branch" << endl; 
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
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(35))->SetAddress(DCA_zmult0);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(36))->SetAddress(DCA_zmult1);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(37))->SetAddress(DCA_zmult2);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(38))->SetAddress(DCA_zmult3);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(39))->SetAddress(DCA_zmult4);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(40))->SetAddress(DCA_zmult5);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(41))->SetAddress(DCA_zmult6);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(42))->SetAddress(DCA_zmult7);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(43))->SetAddress(DCA_zmult8);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(44))->SetAddress(zmult0);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(45))->SetAddress(zmult1);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(46))->SetAddress(zmult2);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(47))->SetAddress(zmult3);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(48))->SetAddress(zmult4);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(49))->SetAddress(zmult5);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(50))->SetAddress(zmult6);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(51))->SetAddress(zmult7);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(52))->SetAddress(zmult8);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(53))->SetAddress(Rpc1DCA);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(54))->SetAddress(Rpc1time);    
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(55))->SetAddress(Qtot100);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(56))->SetAddress(Qtot101);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(57))->SetAddress(Qtot110);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(58))->SetAddress(Qtot111);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(59))->SetAddress(Qtot120);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(60))->SetAddress(Qtot121);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(61))->SetAddress(Qtot200);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(62))->SetAddress(Qtot201);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(63))->SetAddress(Qtot210);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(64))->SetAddress(Qtot211);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(65))->SetAddress(Qtot220);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(66))->SetAddress(Qtot221);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(67))->SetAddress(Qtot300);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(68))->SetAddress(Qtot301);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(69))->SetAddress(Qtot310);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(70))->SetAddress(Qtot311);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(71))->SetAddress(Nstr100);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(72))->SetAddress(Nstr101);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(73))->SetAddress(Nstr110);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(74))->SetAddress(Nstr111);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(75))->SetAddress(Nstr120);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(76))->SetAddress(Nstr121);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(77))->SetAddress(Nstr200);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(78))->SetAddress(Nstr201);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(79))->SetAddress(Nstr210);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(80))->SetAddress(Nstr211);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(81))->SetAddress(Nstr220);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(82))->SetAddress(Nstr221);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(83))->SetAddress(Nstr300);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(84))->SetAddress(Nstr301);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(85))->SetAddress(Nstr310);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(86))->SetAddress(Nstr311);
    

	    fBranch = newsngmuons->GetBranch("RpcMatchVtx");
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(fRpc3VtxDCA);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(fRpc3VtxTime);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(fRpc3VtxX);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(fRpc3VtxY);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(fRpc1VtxDCA);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(fRpc1VtxTime);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(fRpc1VtxX);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(fRpc1VtxY);


  	    fBranch = newsngmuons->GetBranch("RpcMatchSt1");
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(fRpc3St1DCA);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(fRpc3St1Time);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(fRpc3St1X);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(fRpc3St1Y);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(fRpc1St1DCA);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(fRpc1St1Time);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(fRpc1St1X);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(fRpc1St1Y);

	    fBranch = newsngmuons->GetBranch("RpcMatchSt3");
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(fRpc3St3DCA);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(fRpc3St3Time);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(fRpc3St3X);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(fRpc3St3Y);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(fRpc1St3DCA);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(fRpc1St3Time);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(fRpc1St3X);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(fRpc1St3Y);

	    fBranch = newsngmuons->GetBranch("RpcMatchMuID");
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(fRpc3MuIDDCA);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(fRpc3MuIDTime);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(fRpc3MuIDX);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(fRpc3MuIDY);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(fRpc1MuIDDCA);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(fRpc1MuIDTime);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(fRpc1MuIDX);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(fRpc1MuIDY);

  	    fBranch = newsngmuons->GetBranch("FvtxMatch");
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(0))->SetAddress(fvtx_px);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(1))->SetAddress(fvtx_py);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(2))->SetAddress(fvtx_pz);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(3))->SetAddress(fvtx_dphi);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(4))->SetAddress(fvtx_dtheta);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(5))->SetAddress(fvtx_chi);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(6))->SetAddress(fvtx_conebits);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(7))->SetAddress(fvtx_mpx);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(8))->SetAddress(fvtx_mpy);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(9))->SetAddress(fvtx_mpz);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(10))->SetAddress(fvtx_mdcaz);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(11))->SetAddress(fvtx_dr);

	    ((TLeaf *) fBranch->GetListOfLeaves()->At(12))->SetAddress(fvtx_vtx_x);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(13))->SetAddress(fvtx_vtx_y);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(14))->SetAddress(fvtx_vtx_z);
	    ((TLeaf *) fBranch->GetListOfLeaves()->At(15))->SetAddress(fvtx_tracklconebits);


	 }

      } // if
      else
      {
//cout << "No muons found" << endl;

      }
      if ( fNMU12 >0  || ( _rpcnotracks_flag && ( fNRpcHits12>0)) ) { //when flag is set also all rpc hits w/o track are written out - important only for timing BG in fast production
//      if ( fNMU12 >0 ) {  //used for only good muon tracks
	 newsngmuons->Fill();
      }

      if(fNMU12>0){
	 delete [] RPC3_x;
	 delete [] RPC3_y;
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
	 delete [] pxSTII;
	 delete [] pySTII;
	 delete [] pzSTII;
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
	 delete [] rpc3st3;
	 delete [] rpc3g5;
	 delete [] dANGLE;
	 delete [] dANGLE_xyz;
	 delete [] dPHI;
	 delete [] dTHETA;
	 delete [] ROAD_SLOPE;
	 delete [] ref_vtx_rdca;
	 delete [] ref_vtx_r;
	 delete [] ref_vtx_z;
	 delete [] pseudotrigN_1S;
	 delete [] pseudotrigS_SG1D;
	 delete [] pseudotrigS_1S;
	 delete [] pseudotrigN_SG1D;
	 delete [] recoNS_1D;
	 delete [] recoS_SG1D;
	 delete [] recoNS_1H;
	 delete [] recoN_SG1D;
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
	 delete [] Rpc1DCA;
	 delete [] Rpc1time;


	 delete [] DCA_zmult0;
	 delete [] DCA_zmult1;
	 delete [] DCA_zmult2;
	 delete [] DCA_zmult3;
	 delete [] DCA_zmult4;
	 delete [] DCA_zmult5;
	 delete [] DCA_zmult6;
	 delete [] DCA_zmult7;
	 delete [] DCA_zmult8;
	 delete [] zmult0;
	 delete [] zmult1;
	 delete [] zmult2;
	 delete [] zmult3;
	 delete [] zmult4;
	 delete [] zmult5;
	 delete [] zmult6;
	 delete [] zmult7;
	 delete [] zmult8;



	 delete [] fRpc3VtxDCA;
	 delete [] fRpc3VtxTime;
	 delete [] fRpc3VtxX;
	 delete [] fRpc3VtxY;
	 delete [] fRpc1VtxDCA;
	 delete [] fRpc1VtxTime;
	 delete [] fRpc1VtxX;
	 delete [] fRpc1VtxY;

	 delete [] fRpc3St1DCA;
	 delete [] fRpc3St1Time;
	 delete [] fRpc3St1X;
	 delete [] fRpc3St1Y;
	 delete [] fRpc1St1DCA;
	 delete [] fRpc1St1Time;
	 delete [] fRpc1St1X;
	 delete [] fRpc1St1Y;

	 delete [] fRpc3St3DCA;
	 delete [] fRpc3St3Time;
	 delete [] fRpc3St3X;
	 delete [] fRpc3St3Y;
	 delete [] fRpc1St3DCA;
	 delete [] fRpc1St3Time;
	 delete [] fRpc1St3X;
	 delete [] fRpc1St3Y;



	 delete [] fRpc3MuIDDCA;
	 delete [] fRpc3MuIDTime;
	 delete [] fRpc3MuIDX;
	 delete [] fRpc3MuIDY;
	 delete [] fRpc1MuIDDCA;
	 delete [] fRpc1MuIDTime;
	 delete [] fRpc1MuIDX;
	 delete [] fRpc1MuIDY;






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


	 delete []    Qtot100;
	 delete []    Qtot101;
	 delete []    Qtot110;
	 delete []    Qtot111;
	 delete []    Qtot120;
	 delete []    Qtot121;
	 delete []    Qtot200;
	 delete []    Qtot201;
	 delete []    Qtot210;
	 delete []    Qtot211;
	 delete []    Qtot220;
	 delete []    Qtot221;
	 delete []    Qtot300;
	 delete []    Qtot301;
	 delete []    Qtot310;
	 delete []    Qtot311;

	 delete []    Nstr100;
	 delete []    Nstr101;
	 delete []    Nstr110;
	 delete []    Nstr111;
	 delete []    Nstr120;
	 delete []    Nstr121;
	 delete []    Nstr200;
	 delete []    Nstr201;
	 delete []    Nstr210;
	 delete []    Nstr211;
	 delete []    Nstr220;
	 delete []    Nstr221;
	 delete []    Nstr300;
	 delete []    Nstr301;
	 delete []    Nstr310;
	 delete []    Nstr311;

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


	 delete [] fvtx_px;
	 delete [] fvtx_py;
	 delete [] fvtx_pz;
	 delete [] fvtx_dphi;
	 delete [] fvtx_dtheta;
	 delete [] fvtx_chi;
	 delete [] fvtx_conebits;
	 delete [] fvtx_mpx;
	 delete [] fvtx_mpy;
	 delete [] fvtx_mpz;
	 delete [] fvtx_mdcaz;
	 delete [] fvtx_dr;

	 delete [] fvtx_vtx_x;
	 delete [] fvtx_vtx_y;
	 delete [] fvtx_vtx_z;
	 delete [] fvtx_tracklconebits;




      }
      if(fNRpcHits12>0){
	 delete [] rpc_t;
	 delete [] rpc_c;
	 delete [] arm;
	 delete [] station;
	 delete [] octant;
	 delete [] halfoctant;
	 delete [] radsegment;
	 delete [] strip;
	 }

      if(fNRpchodoHits12>0){
	 delete [] rpchodo_t;
	 delete [] hodoarm;
	 delete [] hodostation;
	 delete [] hodostrip;
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
      delete [] fRunNumber12;
      delete [] EventNumber12;
      delete [] realtrigNS_1D;
      delete [] realtrigNS_1H;
      delete [] realtrigS_SG1D;
      delete [] realtrigN_SG1D;
      delete [] livetrigNS_1D;
      delete [] livetrigNS_1H;
      delete [] livetrigS_SG1D;
      delete [] livetrigN_SG1D;
      delete [] realtrig_MB;
      delete [] Clock_trig;
      delete [] Clock_live;
      delete [] triggerbit;
      delete [] triggerlive;
      delete [] triggerraw;
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
