// $Id: SngmuonsRun4.C,v 1.11 2014/12/09 06:28:10 rseidl Exp $

/*!
   \file SngmuonsRun4.C
   \brief single muon ntuple booking and feeling
   \version $Revision: 1.11 $
   \date $Date: 2014/12/09 06:28:10 $
*/

#include <iostream>
#include <boost/array.hpp>
#include <EventHeader.h>
#include <PHGlobal.h>
#include <ReactionPlaneObject.h> 
#include <RpSumXYObject.h>

#include <MWGVertex.h>
#include <PHMuoTracksOut.h>
#include <RunHeader.h>
#include <SpinDataEventOut.h>
#include <stdexcept>
#include <string>
#include <TChain.h>
#include <TH1.h>

#include <TMutTrkMap.h>
#include <TMutMCTrkMap.h>
#include <TMutMCHitMap.h>
#include <TMuiRoadMapO.h>
#include <TMCPrimaryMap.h>

#include <root_ptrk.h>
#include <TNtuple.h>
#include <TriggerHelper.h>
#include <utiCentrality.h>
#include <vector>

#include <MUTOO.h>
#include "../MWGpico.h"
#include <Tools.h>
#include <MWGConsts.h>

#include "TrigLvl1v1.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"


using namespace std;
using namespace PhUtilities;

//__________________________________________________
void MWGpico::BookSngmuonsNtupleRun4(TNtuple*& sngmuons, TString m_name, TString m_title)
{

  // define variable list for single muons ntuple
  /*
    IMPORTANT NOTE:
    here we write only 10 variables/line to make counting easier when filling the ntuple
    please follow this policy in later modifications to avoid stupid bugs
  */
  const char* sngvarlist=0;
  
  sngvarlist=
    "Run_Number:Evt_Number:Evt_Nmu:Evt_Z:Evt_bbcZ:Evt_bbcCentralityByPerp:Evt_bbcCentralityByClock:Evt_zdcCentrality:Evt_zdcEnerN:Evt_fclGreyN:charge:"
    "px:py:pz:pxSt1:pySt1:pzSt1:pxSt3:pySt3:pzSt3:pT:"
    "p:pSt1:ELoss:chi2:idhits:idquad:trhits:DS3:DS3ctp:idchi2:"
    "gap0x:gap0y:gap0z:dxdz:dydz:trstat:ghost:lastGap:eta:phi:"
    "DG0:DDG0:DS0:refX:refY:mutr_nhits:muid_nhits:Evt_realtrigS_1D:Evt_realtrigS_1S:Evt_realtrigN_1D:"
    "Evt_realtrigN_1S:Evt_livetrigS_1D:Evt_livetrigS_1S:Evt_livetrigN_1D:Evt_livetrigN_1S:Evt_pseudotrigS_1D:Evt_pseudotrigS_1S:Evt_pseudotrigN_1D:Evt_pseudotrigN_1S:"
    "Evt_recoS_1D:Evt_recoS_1S:Evt_recoN_1D:Evt_recoN_1S:xSt1:ySt1:zSt1:xSt2:ySt2:zSt2:"
    "xSt3:ySt3:zSt3:ref_vtx_rdca:ref_vtx_r:ref_vtx_z:refit_zvtx:Clock_trig:Evt_realtrig_MB:"
    "SpinX_ID:Pol_Y:Pol_B:GL1X_ID:dAngle:dAngle_xyz:dTheta:dPhi:road_slope:X6:"
    "mc_n_part:mc_px:mc_py:mc_pz:mc_ptot:mc_z:mc_pid:mc_hits:mc_p_pid:mc_p_px:"
    "mc_p_py:mc_p_pz:mc_p_ptot:mc_p_z:mc_d_pid:mc_d_px:mc_d_py:mc_d_pz:mc_d_ptot:mc_d_z:"
    "mc_d_n:mc_x:mc_y:mc_g_pid:mc_g_px:mc_g_py:mc_g_pz:mc_g_ptot:mc_g_z:"
    "RPbbcrp10:RPbbcrp11:RPbbcrp12:RPbbcrp00:RPbbcrp01:RPbbcrp02:RPsmdrp00:RPsmdrp01:RPsmdrp02"; // BBC SMD rp
 
  sngmuons = new TNtuple( m_name, m_title, sngvarlist );
  sngmuons->SetAutoSave(160000000);
  
  return;
}

//__________________________________________________________________
void MWGpico::BookSngmuonsEvtNtupleRun4(TNtuple*& sngvtx, TString v_name, TString v_title )
{
  // define variable list for vertex ntuple
  const char* vtxvarlist=0;

  vtxvarlist= "Run_Number:SpinX_ID:Pol_Y:Pol_B:GL1X_ID:Evt_Z:bbcZ:bbcCentralityByPerp:bbcCentralityByClock:BBCQN:"
    "BBCQS:BBCNN:BBCNS:ZDCN:ZDCS:Clock_trig:Evt_realtrig_MB:Evt_livetrigS_1S:Evt_livetrigS_1D:Evt_livetrigN_1S:Evt_livetrigN_1D:"
    "RPbbcrp10:RPbbcrp11:RPbbcrp12:RPbbcrp00:RPbbcrp01:RPbbcrp02:RPsmdrp00:RPsmdrp01:RPsmdrp02"; // BBC SMD rp
  
  sngvtx = new TNtuple( v_name, v_title, vtxvarlist );
  sngvtx->SetAutoSave(160000000);

  return;
}


//__________________________________________________
int MWGpico::FillSngmuonsNtpRun4(PHMuoTracksOut* &muo, TNtuple* sngmuons,  TNtuple* sngvtx)
{
  //=== event selection
  //    if (!PassCuts(evt)) return 1; // see ../PassCuts.h

  //=== event information
  Float_t bbcCentralityByPerp=-9999;
  Float_t bbcCentralityByClock=-9999;
  // Float_t bbcCentrality=-9999;
  Float_t zdcCentrality=-9999;
  Float_t FclGreyN=-9999;
  Float_t ZdcEnergyN=-9999;
  Float_t ZdcEnergyS=-9999;

  // R.P variables
  double RPbbcrp10 = -9999;
  double RPbbcrp11 = -9999;
  double RPbbcrp12 = -9999;
  double RPbbcrp00 = -9999;
  double RPbbcrp01 = -9999;
  double RPbbcrp02 = -9999;
  double RPsmdrp00 = -9999;
  double RPsmdrp01 = -9999;
  double RPsmdrp02 = -9999;


  //  if ( _choice=="simu" || _choice == "simu_file") BbcZVertex = muo->get_zpos(0,0);
  //for full pythia type simulations, we still want to use the "BBC hits" for the event vertex
  //instead of the PISA event vtx; the vtx used for muon reco is stored in "refit_zvtx" in pDST  02/21/2005  MXL
  //
  //  if ( _choice=="simu" || _choice == "simu_file") BbcZVertex = evt->getBbcZVertex();
  //  else if (evt) BbcZVertex = evt->getBbcZVertex();

  // z_vertex
  float BbcZVertex( -9999 );
  float Evt_Z( -9999 );
  if (evt) {
    BbcZVertex = evt->getBbcZVertex();
    //Evt_Z = evt->getZVertex();
  }
  
  // event/run number
  int RunNumber = ( run_header ) ?  run_header->get_RunNumber():0;
  int EventNumber = (event_header ) ? event_header->get_EvtSequence():0;
  
  // try load MC if failed from PHGlobal
  {
    bool error( false );
    if( !RunNumber ) RunNumber = Tools::runNumberMC( header, error );
    if( !EventNumber ) EventNumber  = Tools::eventNumberMC( header, error );
  }
  
  if (evt) {
    if (_type=="dAu") {
      //bbcCentrality = evt->get_dAuBbcCentrality();
      zdcCentrality = evt->get_dAuZdcCentrality();
      bbcCentralityByPerp = 0;
      bbcCentralityByClock = 0;
    }
    else if (_type=="AuAu") {
      bbcCentralityByPerp = (Float_t) PhUtilities::getCentralityByPerpRun4(_top_node); // bbc centrality for AuAu by perpendicular method
      bbcCentralityByClock = (Float_t) PhUtilities::getCentralityByClockRun4(_top_node); // bbc centrality for AuAu by perpendicular method
    }
    if (rp) {  // Load R.P variables
      RPbbcrp10 = rp->getBBCrp10();
      RPbbcrp11 = rp->getBBCrp11();
      RPbbcrp12 = rp->getBBCrp12();
      RPbbcrp00 = rp->getBBCrp00();
      RPbbcrp01 = rp->getBBCrp01();
      RPbbcrp02 = rp->getBBCrp02();
      RPsmdrp00 = rp->getSMDrp00();
      RPsmdrp01 = rp->getSMDrp01();
      RPsmdrp02 = rp->getSMDrp02();
    }

    ZdcEnergyN=evt->getZdcEnergyN();
    ZdcEnergyS=evt->getZdcEnergyS();
  } // evt

  Float_t realtrigS_1D     =  0; // real trigger South
  Float_t realtrigN_1D     =  0; // real trigger North
  Float_t realtrigS_1S     =  0; // real trigger South
  Float_t realtrigN_1S     =  0; // real trigger North

  Float_t livetrigS_1D     =  0; // live trigger South
  Float_t livetrigN_1D     =  0; // live trigger North
  Float_t livetrigS_1S     =  0; // live trigger South
  Float_t livetrigN_1S     =  0; // live trigger North

  Float_t realtrig_MB      =  0;
  Float_t Clock_trig       =  0;
  //  Float_t Clock_live       =  0;

  //
  // real triggers -> require to have triger node
  // old BLT triggers

  if(_trig_lvl1){ // Trigger Node exists in data file
    
    if (TrigHelp->didLevel1TriggerGetScaled("MUIDS_1D&BBCLL1")) {
      realtrigS_1D=1;
    }
    if (TrigHelp->didLevel1TriggerGetScaled("MUIDN_1D&BBCLL1")) {
      realtrigN_1D=1;
    }
    if (TrigHelp->didLevel1TriggerGetScaled("MUIDS_1S&BBCLL1")) {
      realtrigS_1S=1;
    }
    if (TrigHelp->didLevel1TriggerGetScaled("MUIDN_1S&BBCLL1")) {
      realtrigN_1S=1;
    }
    
    // live triggers
    if (TrigHelp->didLevel1TriggerFire("MUIDS_1D&BBCLL1")) {
      livetrigS_1D=1;
    }
    if (TrigHelp->didLevel1TriggerFire("MUIDN_1D&BBCLL1")) {
      livetrigN_1D=1;
    }
    if (TrigHelp->didLevel1TriggerFire("MUIDS_1S&BBCLL1")) {
      livetrigS_1S=1;
    }
    if (TrigHelp->didLevel1TriggerFire("MUIDN_1S&BBCLL1")) {
      livetrigN_1S=1;
    }
    
    /*
    //
    // --- use trigger bit for trigger selection to cover run6 MUIDLL1_trigger with and w/o BBCLL1
    // for low energy runs 
    // 08/18/2006  MXL
    //
      
    //scaled triggers
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00004000){
    realtrigN_1D=1;
    }
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00008000){
    realtrigS_1D=1;
    }
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00010000){
    realtrigN_1S=1;
    }
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00020000){
    realtrigS_1S=1;
    }
    
    //live triggers
    if (_trig_lvl1->get_lvl1_triglive()&0x00004000){
    livetrigN_1D=1;
    }
    if (_trig_lvl1->get_lvl1_triglive()&0x00008000){
    livetrigS_1D=1;
    }
    if (_trig_lvl1->get_lvl1_triglive()&0x00010000){
    livetrigN_1S=1;
    }
    if (_trig_lvl1->get_lvl1_triglive()&0x00020000){
    livetrigS_1S=1;
    }
    */
      
    if (TrigHelp->IsEventMinBias()) {
      realtrig_MB=1;
    }

    //Clock triggers
    if (TrigHelp->didLevel1TriggerGetScaled("Clock")) {
      Clock_trig=1;
    }
    //    if (TrigHelp->didLevel1TriggerFire("Clock")) {
    // Clock_live=2;
    //}
    
  } //   if(_trig_lvl1) // Trigger Node exists in data

  
  //spin information
  Float_t SpinX_ID =  -999; // beam crossing ID from spin DB
  Float_t Pol_Y    =  -999; // yellow beam polarization
  Float_t Pol_B    =  -999; // blue beam polarization
  Float_t GL1X_ID  =  -999; // beam crossing ID from GL1
  
  if (spin){
    SpinX_ID= spin->GetSpinGL1CrossingID();
    Pol_Y   = spin->GetSpinDirectionYellowFromV124();
    Pol_B   = spin->GetSpinDirectionBlueFromV124();
    GL1X_ID = spin->GetGL1CrossingID();
    
  } // if (spin)
  
  Float_t  BBCQN= -999;
  Float_t  BBCQS= -999;
  Float_t  BBCNN= -999;
  Float_t  BBCNS= -999;  
  
  if ( evt){
    BBCQN = evt->getBbcChargeN();
    BBCQS = evt->getBbcChargeS();
    BBCNN = evt->getBbcMultN();  
    BBCNS = evt->getBbcMultS();  
  }


  Float_t Evt_Data[30];
  Evt_Data[0] = RunNumber;
  Evt_Data[1] = SpinX_ID;
  Evt_Data[2] = Pol_Y;
  Evt_Data[3] = Pol_B;
  Evt_Data[4] = GL1X_ID;

  Evt_Data[5] = Evt_Z;
  Evt_Data[6] = BbcZVertex;
  Evt_Data[7] = bbcCentralityByPerp;
  Evt_Data[8] = bbcCentralityByClock;
  Evt_Data[9] = BBCQN;

  Evt_Data[10] = BBCQS;
  Evt_Data[11] = BBCNN;
  Evt_Data[12] = BBCNS;
  Evt_Data[13] = ZdcEnergyN;
  Evt_Data[14] = ZdcEnergyS;
  Evt_Data[15] = Clock_trig;
  Evt_Data[16] = realtrig_MB;                    // live real   trigger for MB
  Evt_Data[17] = livetrigS_1S;                    // live real   trigger for MUID_LL1_S_1S
  Evt_Data[18] = livetrigS_1D;                    // live real   trigger for MUID_LL1_S_1D
  Evt_Data[19] = livetrigN_1S;                    // live real   trigger for MUID_LL1_N_1S
  Evt_Data[20] = livetrigN_1D;                    // live real   trigger for MUID_LL1_N_1D

  Evt_Data[21] = RPbbcrp10;                      // R.P v2
  Evt_Data[22] = RPbbcrp11;
  Evt_Data[23] = RPbbcrp12;
  Evt_Data[24] = RPbbcrp00;                      // R.P v2
  Evt_Data[25] = RPbbcrp01;
  Evt_Data[26] = RPbbcrp02;
  Evt_Data[27] = RPsmdrp00;                       // R.P v1
  Evt_Data[28] = RPsmdrp01;
  Evt_Data[29] = RPsmdrp02;

  sngvtx->Fill(Evt_Data);
  
  //_____________ end of sngvtx _____________

  // muon variables
  Float_t dS30=999, dS3ctp0=999, muIDchis0=999, DG0=999, DDG0=999, DS0=999;
  Float_t pseudo_rapidity = 999;
  Int_t muIDquad0=-1, lastGap=0;

  if (muo) { // check PHMuoTracks or PHMuoTracksOO exist

    int npart = muo->get_npart();
    totMU += npart;
    
    for (int ipart=0; ipart<npart; ipart++) 
    {
      
      // get best road
      int iroad = Cuts().get_best_road_oo( ipart, muo );
      
      //=== First track variables
      muIDquad0 = (muo->get_muIDOO_gap0( 0, iroad, ipart )>0) + 2*(muo->get_muIDOO_gap0( 1, iroad, ipart )<0);
      muIDchis0 = muo->get_muIDOOchi(iroad,ipart);
      //      muTRhits0 = muo->get_muTRhits(ipart);

      dS30 = Tools::DS3(muo,ipart, iroad);
      DG0 = Tools::DG0(muo,ipart, iroad);
      DDG0 = Tools::DDG0(muo,ipart, iroad);
      dS3ctp0 = Tools::DS3ctp(muo,ipart, iroad );
      DS0 = Tools::DS0(muo,ipart, iroad );

      for(int igap=4; igap>0; igap--)
      {
        if (muo->is_muIDOOhit( iroad, ipart, igap, 0) || muo->is_muIDOOhit( iroad, ipart, igap, 1 )) 
        {
          lastGap = igap;
          break;
        }
      }

      // fit single track together with BBC vertex
      double px_vertex( muo->get_px(0,ipart) );
      double py_vertex( muo->get_py(0,ipart) );
      double pz_vertex( muo->get_pz(0,ipart) );
      
      bool do_refit( true );
      
      MWGVertex vertex;
      if (_framework == MUTOO && do_refit )	try {
	  
        vertex.set_verbosity( 0 );
        
        // add track
        vertex.add_track( ipart, muo );
        
        // add vertex information
        double z_vertex( 0 );
        double z_vertex_error( 0 );
        
        if( _choice == "simu" || _choice == "simu_file" ) {

          // retrieve vertex from pisa header file or first track
          if( header ) {
            bool error( true );
            z_vertex = Tools::zVertexMC( header, error );
            if( error ) z_vertex = muo->get_zpos(0,0);
          } else z_vertex = muo->get_zpos(0,0);
          // vertex error (from MC) is hard coded to 2cm
          z_vertex_error = 2;
          
        } else if( evt ) {
          
          // retrieve vertex from BBC
          z_vertex = evt->getBbcZVertex();
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
        px_vertex = vertex.get_px( 0 );
        py_vertex = vertex.get_py( 0 );
        pz_vertex = vertex.get_pz( 0 );
        
        //       // some dump
        //                cout << "MWGpico::FillSngmuonsNtp -"
        //         << " momentum=("
        //                        << vertex.get_px( 0 ) << ","
        //                        << vertex.get_py( 0 ) << ","
        //                        << vertex.get_pz( 0 ) << ")"
        //         << " position=("
        //                        << vertex.get_trk_x( 0 ) << ","
        //                        << vertex.get_trk_y( 0 ) << ","
        //                        << vertex.get_trk_z( 0 ) << ")"
        //         << " chisquare=" << vertex.get_chisquare()
        //                      << endl;
        
      }
      catch( std::exception &e ) {
        cout << e.what() << endl;
      }
      
      float gap0x = muo->get_muIDOO_gap0(0, iroad, ipart);
      float gap0y = muo->get_muIDOO_gap0(1, iroad, ipart);
      float gap0z = muo->get_muIDOO_gap0(2, iroad,ipart);
      float dirX  = muo->get_muIDOO_gap0(3, iroad,ipart);
      float dirY  = muo->get_muIDOO_gap0(4, iroad,ipart);
      float refX  = -dirX*gap0z + gap0x;
      float refY  = -dirY*gap0z + gap0y;
      float muTRhits = muo->get_muTRhits(ipart);
      float muIDhits = 999;
      if( iroad >= 0 ) muIDhits = muo->get_muIDOOhits(iroad, ipart);
      
      float mutr_nhits = Tools::sumbit(muTRhits);
      float muid_nhits = Tools::sumbit(muIDhits);
      
      float px =  px_vertex;
      float py =  py_vertex;
      float pz =  pz_vertex;

      float xSTI  = muo->get_xpos(1,ipart);
      float ySTI  = muo->get_ypos(1,ipart);
      float zSTI  = muo->get_zpos(1,ipart);
      float pxSTI = muo->get_px(1,ipart);
      float pySTI = muo->get_py(1,ipart);
      float pzSTI = muo->get_pz(1,ipart);

      float xSTII  = muo->get_xpos(2,ipart);
      float ySTII  = muo->get_ypos(2,ipart);
      float zSTII  = muo->get_zpos(2,ipart);

      float xSTIII  = muo->get_xpos(3,ipart);
      float ySTIII  = muo->get_ypos(3,ipart);
      float zSTIII  = muo->get_zpos(3,ipart);
      float pxSTIII = muo->get_px(3,ipart);
      float pySTIII = muo->get_py(3,ipart);
      float pzSTIII = muo->get_pz(3,ipart);

      float pT = sqrt(px*px+py*py);
      float p = sqrt(px*px+py*py+pz*pz);
      float pSTI = sqrt(pxSTI*pxSTI + pySTI*pySTI + pzSTI*pzSTI);
      float ELoss = p - pSTI;

      //-------------------------------
      //calculate deflection angle, delta theta, delta phi and road_slope.. put in to the X1,X2...

      //cout << "_choice" << _choice << endl;
      //cout << "BbcZVertex " << BbcZVertex << endl;
      //cout << "refit_z " << vertex.get_vtx_z() << endl;

      if( _choice == "simu" || _choice == "simu_file" ) {
        BbcZVertex = vertex.get_vtx_z();
      }
      float costheta, costheta_xyz, xyz_St1;
      float dANGLE =-999;
      float dANGLE_xyz=-999;
      float dPHI=-999;
      float dTHETA=-999;
      float ROAD_SLOPE=-999;

      ROAD_SLOPE = sqrt(dirX*dirX+dirY*dirY); // bug fixed on 6/6/2007 MXL

      xyz_St1 = sqrt(xSTI*xSTI+ySTI*ySTI+(zSTI-BbcZVertex)*(zSTI-BbcZVertex) );
      costheta = (px*pxSTI + py*pySTI + pz*pzSTI) / ( p*pSTI ) ;

      costheta_xyz = (pxSTI*xSTI + pySTI*ySTI + pzSTI*(zSTI-BbcZVertex) ) / (pSTI*xyz_St1) ;

      if ( abs(costheta) < 1.0 ) {
        dANGLE = acos (costheta);
      }else{
        dANGLE = 0.0;
      }
      dANGLE = dANGLE*0.5*(p+pSTI);

      if ( abs(costheta_xyz) < 1.0 ) {
        dANGLE_xyz = acos (costheta_xyz);
      }else{
        dANGLE_xyz = 0.0;
      }
      dANGLE_xyz = dANGLE_xyz*0.5*(p+pSTI);

      //cout << "dangle = " << dANGLE << endl;
      // cout << "dangle_xyz = " << dANGLE_xyz << endl;

      dPHI = (atan2(ySTI,xSTI)-atan2(ySTIII,xSTIII));  //ST1 - ST3  // (x,y) ->(y,x) bug fix  12/03/2009  MXL 
      dTHETA = (atan((sqrt(xSTI*xSTI+ySTI*ySTI))/(sqrt((zSTI-BbcZVertex)*(zSTI-BbcZVertex))))-atan((sqrt(xSTIII*xSTIII+ySTIII*ySTIII))/(sqrt((zSTIII-BbcZVertex)*(zSTIII-BbcZVertex))))) ; //ST1 - ST3

      //-----------------------------------------------------------------------------

      //
      // calculate reference vtx from track
      // the shortest distance between the trk at station 1 and beam line
      // two space points are (xSTI, ySTI, zSTI) and (0, 0, BbcZVertex)
      // two momentum are (pxSTI, pySTI, pzSTI) and (0,0,1)

      Float_t ref_vtx_rdca;//, ref_vtx_xdca, ref_vtx_ydca, ref_vtx_zdca;

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
      z1[0] = BbcZVertex;
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

      //      ref_vtx_zdca =(Z1+Z2)/2; // z DCA vertex !
      //ref_vtx_ydca =(Y1+Y2)/2;
      //ref_vtx_xdca =(X1+X2)/2;
      ref_vtx_rdca = sqrt( pow((X2-X1),2)+pow((Y2-Y1),2)+pow((Z2-Z1),2) );

      //distance between bbcZ and Z1;
      Float_t ref_vtx_z = Z1;
      //  cout << "X1, Y1,Z1 = " << X1 << " " << Y1 << " " <<Z1 << endl;
      //  cout << "X2, Y2,Z2 = " << X2 << " " << Y2 << " " <<Z2 <<  " " << BbcZVertex << endl;

      //calculate reference vtx from track

      float z_ref = muo->get_zpos(0,ipart);
      float x_ref = muo->get_xpos(0,ipart)+( BbcZVertex-z_ref)*(px/pz);
      float y_ref = muo->get_ypos(0,ipart) + ( BbcZVertex-z_ref)* (py/pz);

      float ref_vtx_r = sqrt(x_ref*x_ref + y_ref*y_ref + z_ref*z_ref);


      // Calculate pseudo-rapidity
      if (p == pz){pseudo_rapidity = -999.999;}
      else {pseudo_rapidity = 0.5*log((p+pz)/(p-pz));}

      // single muon v2
      //float phi0 = atan2(py,px);
      //      float dphi = phi0 - RPbbcrp12;
      //dphi = 0.5*atan2(sin(2*dphi),cos(2*dphi));

      //float dphi_N = phi0 - RPbbcrp10;
      //dphi_N = 0.5*atan2(sin(2*dphi_N),cos(2*dphi_N));

      //float dphi_S = phi0 - RPbbcrp11;
      //dphi_S = 0.5*atan2(sin(2*dphi_S),cos(2*dphi_S));

      //BLT trigger simulators
      Float_t pseudotrigS_1D   = -1;
      Float_t pseudotrigS_1S   = -1;
      Float_t pseudotrigN_1D   = -1;
      Float_t pseudotrigN_1S   = -1;
      Float_t recoS_1D = -1;
      Float_t recoS_1S = -1;
      Float_t recoN_1D = -1;
      Float_t recoN_1S = -1;

      pseudotrigN_1S = Tools::BLT_1S_Decision( MUTOO::North );
      pseudotrigN_1D = Tools::BLT_1D_Decision( MUTOO::North );

      pseudotrigS_1S = Tools::BLT_1S_Decision( MUTOO::South );
      pseudotrigS_1D = Tools::BLT_1D_Decision( MUTOO::South );

      recoN_1S = Tools::BLT_1S_Decision( MUTOO::North, true );
      recoN_1D = Tools::BLT_1D_Decision( MUTOO::North, true );

      recoS_1S = Tools::BLT_1S_Decision( MUTOO::South, true );
      recoS_1D = Tools::BLT_1D_Decision( MUTOO::South, true );

      
      //for MC evaluation
      //We try to get the major MC particle's information
      //major MC particle == MC particle contributes the most to muTr hits
      //
      //may need to rethink about this "major MC" approach for tracks coming from decay
      //inside muTr volume 03/05/2005  MXL

      Float_t MC_N_PART =0;   // total # of MC particles associated with this reco'd track
      Float_t MC_PX=-999;        // the major MC particle's px
      Float_t MC_PY=-999;
      Float_t MC_PZ=-999;
      Float_t MC_PTOT =-999;      // the major MC particle's lastGap
      Float_t MC_X=-999;         // the major MC particle's X_orign
      Float_t MC_Y=-999;         // the major MC particle's Y_orign
      Float_t MC_Z=-999;         // the major MC particle's Z_orign
      Float_t MC_PID =-999;
      Float_t MC_HITS =0;     // the major MC particle's total contribution to muTr hits

      Float_t MC_P_PID =-999;    // MC particle's parent information
      Float_t MC_P_PX =-999;     //
      Float_t MC_P_PY =-999;
      Float_t MC_P_PZ =-999;
      Float_t MC_P_PTOT =-999;
      Float_t MC_P_Z =-999;

      Float_t MC_D_PID =-999;    // MC particle's daughter information
      Float_t MC_D_PX =-999;     //
      Float_t MC_D_PY =-999;
      Float_t MC_D_PZ =-999;
      Float_t MC_D_PTOT =-999;
      Float_t MC_D_Z =-999;
      Float_t MC_D_N =-999;

      Int_t  MC_P_TRK = -999;    // parent track ID: for debuggin now
      Int_t  MC_D_TRK = -999;    // daughter track ID: for debuggin now

      Float_t MC_G_PID =-999;    // MC particle's grandparent (ancestor, original, very primary) information, only work for single particle now
      Float_t MC_G_PX =-999;     //
      Float_t MC_G_PY =-999;
      Float_t MC_G_PZ =-999;
      Float_t MC_G_PTOT =-999;
      Float_t MC_G_Z =-999;

      // get mc_primary track info.
      if (_mc_primary_map) {

	//cout << "_mc_primary_map found, size " << _mc_primary_map->size() << endl;
	TMCPrimaryMap::const_iterator prim_iter = _mc_primary_map->range();

	while( TMCPrimaryMap::const_pointer prim_ptr = prim_iter.next() )
	  {
	    MC_G_PID  = prim_ptr->get()->get_pid();
	    MC_G_PX   = prim_ptr->get()->get_px_orig();
	    MC_G_PY   = prim_ptr->get()->get_py_orig();
	    MC_G_PZ   = prim_ptr->get()->get_pz_orig();
	    MC_G_PTOT = prim_ptr->get()->get_ptot_orig();
	    MC_G_Z    = prim_ptr->get()->get_z_orig();
	    
	    // cout << "PRIMARY pID " << prim_ptr->get()->get_pid() << " tID " << prim_ptr->get()->get_trk_id() << endl
	    //<< "        vertex (" << prim_ptr->get()->get_x_orig() << ", " << prim_ptr->get()->get_y_orig() << ", " << prim_ptr->get()->get_z_orig() << " )"<< endl
	    // << "        mom    (" << prim_ptr->get()->get_px_orig() << ", " << prim_ptr->get()->get_py_orig() << ", " << prim_ptr->get()->get_pz_orig() << " )" << endl;
	  }
      }
      else {
	cout << "_mc_primary_map could not be found " << endl;
      }
      
      //
      //how to associate a PHMuonTrack with a MutTrk? St1-P?
      //
      if (mut_trk_map && mut_mctrk_map) {

	TMutTrkMap::const_iterator trk_iter = mut_trk_map->range();

	//	int   n_mut_tracks = trk_iter.count();
	//	cout << " >>>>> there are \t = " << n_mut_tracks << "\t mut tracks!" << endl;
	
	while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
	  {
	    //
	    //pick up the right reconstructed track
	    //
	    float	    p_xx  = trk_ptr->get()->get_trk_par_vtx()->get_px();
	    float	    p_yy  = trk_ptr->get()->get_trk_par_vtx()->get_py();
	    float	    p_zz  = trk_ptr->get()->get_trk_par_vtx()->get_pz();
	    //   float	    ptot_x  = trk_ptr->get()->get_trk_par_vtx()->get_ptot();
	    
	    float pxv =  muo->get_px(0,ipart);            // muon px at Vertex
	    float pyv =  muo->get_py(0,ipart);            // muon px at Vertex
	    float pzv =  muo->get_pz(0,ipart);            // muon px at Vertex
	    

	    float diff_x = 999;
	    
	    diff_x = (pxv-p_xx)*(pxv-p_xx) + (pyv-p_yy)*(pyv-p_yy) + (pzv-p_zz)*(pzv-p_zz);
	    
	    //	    cout << " >>>>>>   diff_x == " << diff_x << endl;
	    if (diff_x > 0.000001) continue; // not the same track


	    //found the associated MutTrk
	    
	    MC_N_PART=-999;
	    MC_PX=-999;        // the major MC particle's px
	    MC_PY=-999;
	    MC_PZ=-999;
	    MC_PTOT =-999;      // the major MC particle's lastGap
	    MC_X=-999;         // the major MC particle's X_orign
	    MC_Y=-999;         // the major MC particle's Y_orign
	    MC_Z=-999;         // the major MC particle's Z_orign
	    MC_PID =-999;
	    MC_HITS =-999;     // the major MC particle's total contribution to muTr hits
	    
	    MC_P_PID =-999;    // MC particle's parent information
	    MC_P_PX =-999;     //
	    MC_P_PY =-999;
	    MC_P_PZ =-999;
	    MC_P_PTOT =-999;
	    MC_P_Z =-999;
	    
	    MC_D_PID =-999;    // MC particle's daughter information
	    MC_D_PX =-999;     //
	    MC_D_PY =-999;
	    MC_D_PZ =-999;
	    MC_D_PTOT =-999;
	    MC_D_Z =-999;
            MC_D_N =-999;
	    
	    //	    MC_P_TRK = -999;    // parent track ID: for debuggin now
	    //MC_D_TRK = -999;    // daughter track ID: for debuggin now

	    //----------------
	    //get associated MC tracks information
	    //----------------
	    TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();

	    // How many associated MCTracks are there for this reco_trk?
	    //  float       n_mc_tracks = mc_trk_iter.count();
	    //	    cout << " >>>>> there are \t = " << n_mc_tracks << "\t associated tracks!" << endl;
	    //	    MC_N_PART = n_mc_tracks;
	    
	    // Loop over associated MCTracks
	    mc_trk_iter.reset(); // reset ???
	    while (TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()) {
	      
	      MC_N_PART++;
	      
	      MC_PX  = mc_trk_ptr->get()->get_px_orig();
	      MC_PY  = mc_trk_ptr->get()->get_py_orig();
	      MC_PZ  = mc_trk_ptr->get()->get_pz_orig();
	      //	      MC_GAP = mc_trk_ptr->get()->get_depth();
	      MC_PTOT = sqrt(MC_PX*MC_PX + MC_PY*MC_PY + MC_PZ*MC_PZ);
	      
	      MC_X = mc_trk_ptr->get()->get_x_orig();
	      MC_Y = mc_trk_ptr->get()->get_y_orig();
	      MC_Z = mc_trk_ptr->get()->get_z_orig();
	      MC_PID = mc_trk_ptr->get()->get_pid();
	      //	      MC_P_PID = mc_trk_ptr->get()->get_parent_id();
	      
	      /* functions for finding primary track of a TMutMCTrk, need to add PISAHits info
		 int true_track = mc_trk_ptr->get()->get_track_id();
		 int nfile;
		 int error;
		 float ptotpri;
		 float pthetpri;
		 float pphipri;
		 float r_vertex;
		 float z0vertex;
		 float theta_vertex;
		 float phi_vertex;
		 int idpart;
		 int itorigin;
		 int idorigin;
		 
		 cout << "true_track " << true_track << endl;
		 dio_ptrkorigin(&true_track, &nfile, &error,
		 &ptotpri, &pthetpri, &pphipri,
		 &r_vertex, &z0vertex, &theta_vertex, &phi_vertex,
		 &itorigin, &idorigin, &idpart);
		 cout << "true_track " << true_track << " IT origin " << itorigin << " ID origin " << idorigin << endl;
	      */
	      
	      //
	      //get associated MCHits information
	      //
	      TMutMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
	      //how many hits from this MC particle ?
	      MC_HITS = mc_hit_iter.count();
	      //cout << "thetr are \t " << MC_HITS << " \t MC hits from this MC particle" << endl;
	      
	      float mchitx = 0;
	      while(mc_hit_iter.next()) mchitx++;
	      
	      //	      MC_HITS = mchitx;
	      
	      Int_t trk_IDx = -99;
	      Int_t trk_IDxP = -99;
	      
	      //reset parent  MC information
	      MC_P_PID =-999;    // MC particle's parent information
	      MC_P_PX  =-999;     //
	      MC_P_PY  =-999;
	      MC_P_PZ  =-999;
	      MC_P_PTOT=-999;
	      MC_P_Z   =-999;
	      
	      //	      MC_P_TRK =-999;    // parent track ID: for debuggin now
	      //
	      //get 1st parent information - PID, P, charge etc.
	      //
	      MC_P_TRK = mc_trk_ptr->get()->get_parent_track_id();
	      
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
		

		if ( MC_P_TRK == trk_IDx ){ // found the parent MC track
		  //		  Float_t prnt_IDx = mc_trk_ptr_1p->get()->get_pid();
		  
		  MC_P_PID= mc_trk_ptr_1p->get()->get_pid();
		  MC_P_PX = mc_trk_ptr_1p->get()->get_px_orig();
		  MC_P_PY = mc_trk_ptr_1p->get()->get_py_orig();
		  MC_P_PZ = mc_trk_ptr_1p->get()->get_pz_orig();
		  MC_P_PTOT = sqrt(MC_P_PX*MC_P_PX + MC_P_PY*MC_P_PY + MC_P_PZ*MC_P_PZ);
		  MC_P_Z = mc_trk_ptr_1p->get()->get_z_orig();
		  

		} // found parent track ID
		
		//
		//if no parent found, them the current MC is the primary particle
		//set parent = current mc, but set mc_p_ptot = - value so we can tell it from others
		//
		if (MC_P_PID==-999) {
		  MC_P_PID= mc_trk_ptr->get()->get_pid();
		  MC_P_PX = mc_trk_ptr->get()->get_px_orig();
		  MC_P_PY = mc_trk_ptr->get()->get_py_orig();
		  MC_P_PZ = mc_trk_ptr->get()->get_pz_orig();
		  MC_P_PTOT = -sqrt(MC_P_PX*MC_P_PX + MC_P_PY*MC_P_PY + MC_P_PZ*MC_P_PZ);
		  MC_P_Z = mc_trk_ptr->get()->get_z_orig();
		}
		
		
		//
		//daughter particle - exclude gamma(1), electrons(2+,3-), pi0(7);
		//
		
		//reset daughter  MC information
		MC_D_PID =-999;    // MC particle's daughter information
		MC_D_PX  =-999;     //
		MC_D_PY  =-999;
		MC_D_PZ  =-999;
		MC_D_PTOT=-999;
		MC_D_Z   =-999;
		//		MC_D_N   =-999;
		
		//		MC_D_TRK =-999;    // daughter track ID: for debuggin now
		
		MC_D_TRK = mc_trk_ptr->get()->get_track_id(); // current MC particle's trk_ID = daughter's parent trk_id
		
		trk_IDxP = mc_trk_ptr_1p->get()->get_parent_track_id();  // trk_id ==0 for the very primary particle
		
		MC_D_N=0;
		if ( MC_D_TRK == trk_IDxP ){ // found the daughter MC track
		  
		  MC_D_N++; // count how many daughter particles we have
		  
		  Int_t MC_D_PIDx= mc_trk_ptr_1p->get()->get_pid();

		  // always pick up the MC decay vtx
		  MC_D_Z = mc_trk_ptr_1p->get()->get_z_orig();
		  
		  if (MC_D_PIDx!=1&&MC_D_PIDx!=2&&MC_D_PIDx!=3&&MC_D_PIDx!=7){
		    MC_D_PID= mc_trk_ptr_1p->get()->get_pid();
		    MC_D_PX = mc_trk_ptr_1p->get()->get_px_orig();
		    MC_D_PY = mc_trk_ptr_1p->get()->get_py_orig();
		    MC_D_PZ = mc_trk_ptr_1p->get()->get_pz_orig();
		    MC_D_PTOT = sqrt(MC_D_PX*MC_D_PX + MC_D_PY*MC_D_PY + MC_D_PZ*MC_D_PZ);
		  }

		} // found daughter track ID
		
	      } // loop MC track bank for the 1st parent and daughter
	      
	    } // loop MC trackMap bank for the reco associated track
	  } //end of TMutTrk loop

      }    // end of if (mut_trk_map && mut_mctrk_map) loop


      Float_t varNT[127];

      //=== Fill ntuple
      varNT[0]  = RunNumber;                       // run number
      varNT[1]  = EventNumber;                     // event number
      varNT[2]  = npart;                           // number of muons
      varNT[3]  = Evt_Z;                           // Global
      varNT[4]  = BbcZVertex;                      // BBC Zvertex
      varNT[5]  = bbcCentralityByPerp;             // bbcCentrality by perpendicular method
      varNT[6]  = bbcCentralityByClock;            // bbcCentrality by clock method
      varNT[7]  = zdcCentrality;                   // zdcCentrality
      varNT[8]  = ZdcEnergyN;                      // zdcEnerN;
      varNT[9]  = FclGreyN;                        // fclGreyN;

      varNT[10] = muo->get_charge(ipart);         // muon charge
      varNT[11] = px;     			  // muon px at Vertex
      varNT[12] = py;			          // muon py at Vertex
      varNT[13] = pz;			          // muon pz at Vertex
      varNT[14] = pxSTI;		          // muon px at St1
      varNT[15] = pySTI;		          // muon py at St1
      varNT[16] = pzSTI;		          // muon pz at St1
      varNT[17] = pxSTIII;		          // muon px at St3
      varNT[18] = pySTIII;  		          // muon py at St3
      varNT[19] = pzSTIII;;            		  // muon pz at St3

      varNT[20] = pT;				  // muon pT
      varNT[21] = p;
      varNT[22] = pSTI;
      varNT[23] = ELoss;
      varNT[24] = muo->get_chisquare(ipart);      // chi2 of 1st muon
      varNT[25] = muIDhits; 		           // muid hit pattern of 1st muon
      varNT[26] = muIDquad0;                       // muid quadrant at gap0 of 1st muon
      varNT[27] = muTRhits;                       // mutr hit pattern of 1st muon
      varNT[28] = dS30;                            // DS3 of 1st muon (à la Olivier)
      varNT[29] = dS3ctp0;                         // DS3 of 1st muon (w const theta & phi)
      
      varNT[30] = muIDchis0;                       // Muid Chi2
      varNT[31] = gap0x;          // Muid Gap0 x
      varNT[32] = gap0y;          // Muid Gap0 y
      varNT[33] = gap0z;          // Muid Gap0 z
      varNT[34] = dirX;           // Muid Road dxdz
      varNT[35] = dirY;           // Muid Road  dydz
      varNT[36] = muo->get_TMutTrk_status(ipart);       // Mutr track status
      varNT[37] = muo->get_ghostflag(ipart);
      varNT[38] = lastGap;
      varNT[39] = pseudo_rapidity;		   // pseudo rapidity
      
      varNT[40] = atan(py/px);       // Reaction plane phi
      varNT[41] = DG0;				   // DG0
      varNT[42] = DDG0;				   // DDG0 - opening angle
      varNT[43] = DS0;				   // DS0 - muID road doca @z=0
      varNT[44] = refX;
      varNT[45] = refY;
      varNT[46] = mutr_nhits;
      varNT[47] = muid_nhits;
      varNT[48] = realtrigS_1D;                    // real   trigger 1 deep  South
      varNT[49] = realtrigS_1S;                    // real   trigger 1 sheep  South
      
      varNT[50] = realtrigN_1D;                    // real   trigger 1 deep  North
      varNT[51] = realtrigN_1S;                    // real   trigger 1 sheep North
      varNT[52] = livetrigS_1D;                    // live   trigger 1 deep  South
      varNT[53] = livetrigS_1S;                    // live    trigger 1 sheep  South
      varNT[54] = livetrigN_1D;                    // live   trigger 1 deep  North
      varNT[55] = livetrigN_1S;                    // live   trigger 1 sheep North
      varNT[56] = pseudotrigS_1D;
      varNT[57] = pseudotrigS_1S;
      varNT[58] = pseudotrigN_1D;
      varNT[59] = pseudotrigN_1S;
      
      varNT[60] = recoS_1D;                        // offline BLT trigger from PRDF 
      varNT[61] = recoS_1S;
      varNT[62] = recoN_1D;
      varNT[63] = recoN_1S;
      varNT[64] = xSTI;
      varNT[65] = ySTI;
      varNT[66] = zSTI;
      varNT[67] = xSTII;
      varNT[68] = ySTII;
      varNT[69] = zSTII;

      varNT[70] = xSTIII;
      varNT[71] = ySTIII;
      varNT[72] = zSTIII;
      varNT[73] = ref_vtx_rdca;           	   // muon px at Vertex
      varNT[74] = ref_vtx_r;            	   // muon py at Vertex
      varNT[75] = ref_vtx_z;                       // muon pz at Vertex
      varNT[76] = vertex.get_vtx_z();
      varNT[77] = Clock_trig;
      varNT[78] = realtrig_MB;                     // live real   trigger for MB
      varNT[79] = SpinX_ID;                         // beam crossing ID

      varNT[80] = Pol_Y;
      varNT[81] = Pol_B;
      varNT[82] = GL1X_ID;                          // 
      varNT[83] = dANGLE;                           // multiple-scattering angle form P_0 & P1
      varNT[84] = dANGLE_xyz;                       // multiple-scattering angle form P_0 & X1
      varNT[85] = dPHI;                             // bending angle = St1 -St3 
      varNT[86] = dTHETA;                           // bending angle = St1 -St3 
      varNT[87] = ROAD_SLOPE;                       // muID road slope
      varNT[88] = -999;                             // X6, a place holder
      varNT[89] = MC_N_PART;    //

      varNT[90] = MC_PX;        // from MC tracker_map
      varNT[91] = MC_PY;        // from MC tracker_map
      varNT[92] = MC_PZ;        // from MC tracker_map
      varNT[93] = MC_PTOT;      // from MC tracker_map
      varNT[94] = MC_Z;         // from MC tracker_map
      varNT[95] = MC_PID;       // from MC tracker_map
      varNT[96] = MC_HITS;      // hits of the major MC trk
      varNT[97] = MC_P_PID;     // from MC tracker_map
      varNT[98] = MC_P_PX;      // from MC tracker_map
      varNT[99] = MC_P_PY;      // from MC tracker_map

      varNT[100] = MC_P_PZ;     // from MC tracker_map
      varNT[101] = MC_P_PTOT;   //
      varNT[102] = MC_P_Z;      //
      varNT[103] = MC_D_PID;    // from MC tracker_map for daughter particle
      varNT[104] = MC_D_PX;     // from MC tracker_map
      varNT[105] = MC_D_PY;     // from MC tracker_map
      varNT[106] = MC_D_PZ;     // from MC tracker_map
      varNT[107] = MC_D_PTOT;   //
      varNT[108] = MC_D_Z;      // origin_z
      varNT[109] = MC_D_N;      // total # of daughter particles

      varNT[110] = MC_X;        // from MC tracker_map
      varNT[111] = MC_Y;        // from MC tracker_map
      varNT[112] = MC_G_PID;    // from MC tracker_map
      varNT[113] = MC_G_PX;     // from MC tracker_map
      varNT[114] = MC_G_PY;     // from MC tracker_map
      varNT[115] = MC_G_PZ;     // from MC tracker_map
      varNT[116] = MC_G_PTOT;   //
      varNT[117] = MC_G_Z;      //

      varNT[118] = RPbbcrp10;                      // Reaction Plane
      varNT[119] = RPbbcrp11;                      // Reaction Plane
      varNT[120] = RPbbcrp12;                      // Reaction Plane
      varNT[121] = RPbbcrp00;                      // Reaction Plane
      varNT[122] = RPbbcrp01;                      // Reaction Plane
      varNT[123] = RPbbcrp02;                      // Reaction Plane
      varNT[124] = RPsmdrp00;
      varNT[125] = RPsmdrp01;
      varNT[126] = RPsmdrp02;

      sngmuons->Fill(varNT);
      
    }
  } 
  return 0;
}

