// $Id: DimuonsRun12.C,v 1.5 2013/02/05 15:12:37 rseidl Exp $

/*!
   \file DimuonsRun4.C
   \brief dimuons ntuple booking and feeling
   \author Frederic Fleuret/Hugo Pereira
   \version $Revision: 1.5 $
   \date $Date: 2013/02/05 15:12:37 $
*/

#include <boost/array.hpp>
#include <EventHeader.h>
#include <PHGlobal.h>
#include <PHMuoTracksOut.h>
#include <RunHeader.h>
#include <SpinDataEventOut.h>
#include <string>
#include <TChain.h>
#include <TH1.h>
#include <TNtuple.h>
#include <TVector3.h>
#include <TriggerHelper.h>
#include <utiCentrality.h>
#include <vector>
#include <MUTOO.h>
#include <getClass.h>

// reaction plane
#include <ReactionPlaneObject.h>

#include <MUTOO.h>
#include "../MWGpico.h"
#include <Tools.h>
#include <MWGConsts.h>

using namespace std;
using namespace PhUtilities;

//_________________________________________________________________
void MWGpico::BookDimuonsNtupleRun12( TNtuple*& dimuons, TString name, TString title )
{

  MUTOO::PRINT( cout, "MWGpico::BookDimuonsNtupleRun12" );

  /* to make counting of variables easier, please try to write only ten variables/lines */
  const char* varlist  =
    "Run_Number:Evt_Number:Evt_Nmu:Evt_bbcZ:Evt_bbcCentrality:Evt_bbcChargeS:Evt_bbcChargeN:trigger:clockcross:mass:charge:rapidity:phi:"
    "pT:p:xF:x1:x2:costhCS:Tr0_chi2:Tr1_chi2:Tr0_px:Tr0_py:"
    "Tr0_pz:Tr1_px:Tr1_py:Tr1_pz:Tr0_idhits:Tr1_idhits:Tr0_idquad:Tr1_idquad:Tr0_trhits:Tr1_trhits:"
    "Tr0_DS3:Tr1_DS3:Tr0_DS3ctp:Tr1_DS3ctp:Evt_vtxchi2:Evt_vtxooz:Tr0_idchi2:Tr1_idchi2:Tr0_DG0:Tr1_DG0:"
    "Tr0_DDG0:Tr1_DDG0:Evt_vtxoor:Evt_l2S:Evt_l2N:dca:xvtxbp:yvtxbp:zvtxbp:Tr0_DG0x:"
    "Tr0_DG0y:Tr1_DG0x:Tr1_DG0y:MuIDlevel2ok:MuIDl2primitiveok:mutr_hits_south:mutr_hits_north:Evt_RPS:Evt_RPN:Evt_RP:"
    "Tr0_lastgap:Tr1_lastgap:Tr0_nmuidhits:Tr1_nmuidhits:"
    "Evt_RPBBC:Evt_RPBBCN:Evt_RPBBCS:Evt_RP1BBCN:Evt_RP1BBCS:"
    "Evt_RPMPC:Evt_RPMPCN:Evt_RPMPCS:Evt_RP1MPCN:Evt_RP1MPCS:"
    "Evt_RPVTX:Evt_RPVTXN:Evt_RPVTXS:Evt_RP1VTXN:Evt_RP1VTXS:"
    "Tr0_DG4:Tr1_DG4:"
    "Tr0_DCAr:Tr1_DCAr:"
    "Tr0_DCAz:Tr1_DCAz:"
    "Tr0_dphi12:Tr1_dphi12:"
    "Tr0_dphi23:Tr1_dphi23:"
    "Tr0_RPC3DCA:Tr1_RPC3DCA:Tr0_RPC3time:Tr1_RPC3time:"
    "Tr0_RPC1DCA:Tr1_RPC1DCA:Tr0_RPC1time:Tr1_RPC1time:"
    "Tr0_mutradc:Tr1_mutradc:"
    "Zdc_EnergyS:Zdc_EnergyN";
  
  //! create the ntuple
  dimuons = new TNtuple( name, title, varlist);
  dimuons->SetMarkerStyle(7);

  return;

}


//_________________________________________________________________
int MWGpico::FillDimuonsRun12(PHMuoTracksOut* &muo, TNtuple* dimuons)
{
  if( verbosity >= 2 ) cout << "MWGpico::FillDimuonsRun12" << endl;

  // Global variables
  Float_t E_CMS=200;

  // event selection
  totDIMU+=muo->get_ndimu();
  if( !Cuts().pass_event_cuts( _top_node ) ) return 1;

  // z_vertex
  float BbcZVertex( -9999 );
  if( _choice == "simu" || _choice == "simu_file" )
  {
    bool error( false );
    BbcZVertex = Tools::zVertexMC( header, error );
    if( error ) BbcZVertex = muo->get_zpos(0,0);
  } else if(evt) BbcZVertex = evt->getBbcZVertex();

  // reaction plane
  float rp_south( 0 );
  float rp_north( 0 );
  float rp_both( 0 );
  if( rp )
  {
    //   Removed in CuAu for production July 2012
    /*rp_south = rp->getRXNrp12();
    rp_north = rp->getRXNrp15();
    rp_both = rp->getRXNrp18();*/

  }

  // BBC Reaction Plane
  float rpBBC_south( 0 );
  float rpBBC_north( 0 );
  float rpBBC_both( 0 );
  float rp1BBC_south( 0 );
  float rp1BBC_north( 0 );
  if( rp )
  {
    //v2
    rpBBC_south = rp->getBBCrp11();
    rpBBC_north = rp->getBBCrp12();
    rpBBC_both = rp->getBBCrp10();
    //v1
    rp1BBC_south = rp->getBBCrp01();
    rp1BBC_north = rp->getBBCrp02();

  }

  // VTX Reaction Plane
  float rpVTX_south( 0 );
  float rpVTX_north( 0 );
  float rpVTX_both( 0 );
  float rp1VTX_south( 0 );
  float rp1VTX_north( 0 );
  if( rp )
  {
    //v2
    rpVTX_south = -999;//rp->getVTXrp12();
    rpVTX_north = -999;//rp->getVTXrp15();
    rpVTX_both = -999;//rp->getVTXrp18();
    //v1
    rp1VTX_south = -999;//rp->getVTXrp12();//?
    rp1VTX_north = -999;//rp->getVTXrp15();//?
  }

  // MPC Reaction Plane
  float rpMPC_south( 0 );
  float rpMPC_north( 0 );
  float rpMPC_both( 0 );
  float rp1MPC_south( 0 );
  float rp1MPC_north( 0 );
  if( rp )
  {
    //v2
    rpMPC_south = rp->getMPCrp11();
    rpMPC_north = rp->getMPCrp12();
    rpMPC_both = rp->getMPCrp10();
    //v1
    rp1MPC_south = rp->getMPCrp01();
    rp1MPC_north = rp->getMPCrp02();

  }
  

  // event/run number
  int RunNumber = ( run_header ) ? run_header->get_RunNumber():0;
  int EventNumber = (event_header ) ? event_header->get_EvtSequence():0;

  // try load MC if failed from Run/EventHeader
  {
    bool error( false );
    if( !RunNumber ) RunNumber = Tools::runNumberMC( header, error );
    if( !EventNumber ) EventNumber  = Tools::eventNumberMC( header, error );
  }

  // # hits
  int mutr_hits_south = 0;
  int mutr_hits_north= 0;
  if(evt)
  for(int i =0; i< MUTOO::NumberOfStations; i++)
  {
    mutr_hits_south += evt->get_nMutrHits(0,i);
    mutr_hits_north += evt->get_nMutrHits(1,i);

    //cout << "mutr_hits_north: " << evt->get_nMutrHits(0,i) << endl;
    //cout << "mutr_hits_south: " << evt->get_nMutrHits(1,i) << endl;
  }

  // centrality
  Float_t bbcCentrality = (Float_t) evt->getCentrality();

  // BBC charge
  Float_t bbcChargeSouth = evt->getBbcChargeS();
  Float_t bbcChargeNorth = evt->getBbcChargeN();
  Float_t zdcEnergySouth = evt->getZdcEnergyS();
  Float_t zdcEnergyNorth = evt->getZdcEnergyN();

  // level2 decision
  Float_t l2_south = -1;//Tools::L2MuidDecision( MUTOO::South );
  Float_t l2_north = -1;//Tools::L2MuidDecision( MUTOO::North );

  // get trigger bit word
  TrigLvl1* _trig_lvl1 = findNode::getClass<TrigLvl1> (_top_node, "TrigLvl1");
  int trigger = 9999;
  int clockcross = 9999;
  if ( _trig_lvl1 ) {
    trigger =_trig_lvl1->get_lvl1_triglive();
    clockcross =_trig_lvl1->get_lvl1_clock_cross(); }

  if (muo) {

    // check PHMuoTracks or PHMuoTracksOO exist
    int ndimu = muo->get_ndimu();
    int npart = muo->get_npart();

    // Loop over dimuons
    for (int idimu=0; idimu<ndimu; idimu++)
    {

      // Remove dimuons with unassociated tracks (negative index)
      if(muo->get_ditrkIndex(0,idimu) < 0 || muo->get_ditrkIndex(1,idimu) < 0 ) {
        if( verbosity ) cout << "MWGpico::FillDimuonsNtuple - wrong track index. Vertex skipped.\n";
        negDIMU++;
        continue;
      }

      // See if dimuon pass dimuon cuts (see ../PassCuts.C)
      if( !Cuts().pass_dimuon_cuts( evt, idimu, muo, _framework ) ) continue;

      // track ids
      Int_t idx0(0);
      Int_t idx1(0);

      // first muon variables
      boost::array< Float_t, 3> P0;
      boost::array< Float_t, 3> P1;

      P0.assign(0);
      P1.assign(0);

      // get dimuon's tracks index (for +- pairs the + track will go to first index)
      if (muo->get_vtx_chrg_1(idimu)>0) {

        P0[0] = muo->get_vtx_px_1(idimu);
        P0[1] = muo->get_vtx_py_1(idimu);
        P0[2] = muo->get_vtx_pz_1(idimu);
        idx0  = muo->get_ditrkIndex(0,idimu) ;

        P1[0] = muo->get_vtx_px_2(idimu);
        P1[1] = muo->get_vtx_py_2(idimu);
        P1[2] = muo->get_vtx_pz_2(idimu);
        idx1  = muo->get_ditrkIndex(1,idimu) ;

      } else {

        P0[0] = muo->get_vtx_px_2(idimu);
        P0[1] = muo->get_vtx_py_2(idimu);
        P0[2] = muo->get_vtx_pz_2(idimu);
        idx0  = muo->get_ditrkIndex(1,idimu) ;

        P1[0] = muo->get_vtx_px_1(idimu);
        P1[1] = muo->get_vtx_py_1(idimu);
        P1[2] = muo->get_vtx_pz_1(idimu);
        idx1  = muo->get_ditrkIndex(0,idimu) ;

      }

      // dimuon selection (see ../PassCuts.C)
      if (evt && (!Cuts().pass_single_muon_cuts(evt,idx0,muo, _framework) || !Cuts().pass_single_muon_cuts(evt,idx1,muo, _framework)) ) continue;

      Float_t chi0( muo->get_chisquare(idx0) );
      Int_t muTRhits0( muo->get_muTRhits(idx0) );

      // First track muid variables
      Float_t muIDchis0(0);
      Int_t muIDhits0(0);
      Int_t muIDquad0(-1);
      Int_t muLastGap0( 0 );
      
      Float_t dS30( 0 );
      Float_t dS3ctp0( 0 );
      Float_t dG00( 0 );
      Float_t ddG00( 0 );
      Float_t dG40( 0 );
      int iroad0 = Cuts().get_best_road_oo( idx0, muo );

      if( iroad0 >= 0 )
      {
        muIDhits0 = muo->get_muIDOOhits( iroad0, idx0);
	//muIDhits0 = Tools::get_muid_hit_pattern( muo->get_muIDOOhits(iroad0,idx0 ) );
        muIDquad0 = (muo->get_muIDOO_gap0(0, iroad0, idx0 ) >0) + 2*(muo->get_muIDOO_gap0( 1, iroad0, idx0)<0);
        muIDchis0 = muo->get_muIDOOchi( iroad0, idx0 );

        dS30 = Tools::DS3( muo, idx0, iroad0 );
        dS3ctp0 = Tools::DS3ctp( muo, idx0, iroad0 );
        dG00 = Tools::DG0( muo, idx0, iroad0 );

        ddG00 = Tools::DDG0( muo, idx0, iroad0 );
      }
      
      Float_t r_dca=-999,x_dca=-999,y_dca=-999,z_dca=-999;
      Tools::DCA(muo,idx0,r_dca,x_dca,y_dca,z_dca);
      Float_t fDCAr0 = Tools::dist_zvtx(muo,idx0);
      Float_t fDCAz0 = fabs(z_dca-BbcZVertex);
      
      Float_t fdphi12_0 = atan2(muo->get_ypos(2,idx0),muo->get_xpos(2,idx0));
      fdphi12_0 -= atan2(muo->get_ypos(1,idx0),muo->get_xpos(1,idx0));
      fdphi12_0 = fabs(fdphi12_0);
      if(fdphi12_0>3) { fdphi12_0 -= 2*3.14159; }
      fdphi12_0 = fabs(fdphi12_0);
      Float_t fdphi23_0 = atan2(muo->get_ypos(3,idx0),muo->get_xpos(3,idx0));
      fdphi23_0 -= atan2(muo->get_ypos(2,idx0),muo->get_xpos(2,idx0));
      fdphi23_0 = fabs(fdphi23_0);
      if(fdphi23_0>3) { fdphi23_0 -= 2*3.14159; }
      fdphi23_0 = fabs(fdphi23_0);
      
      for(int igap0=4; igap0>0; igap0--)
	if (muo->is_muIDOOhit( iroad0, idx0, igap0, 0)
	    || muo->is_muIDOOhit( iroad0, idx0, igap0, 1 ))
	  {
	    muLastGap0 = igap0;
	    break;
	  }
      
      //Determine if kalman projection to gap4 is available. 
      int lastIndex = 4;
      if (muo->get_xpos(4,idx0) == 0 && 
	  muo->get_ypos(4,idx0) == 0 && 
	  muo->get_zpos(4,idx0) == 0) 
	{
	  lastIndex = 3;
	}
      Float_t x_mut = muo->get_xpos(lastIndex,idx0);
      Float_t y_mut = muo->get_ypos(lastIndex,idx0);
      Float_t z_mut = muo->get_zpos(lastIndex,idx0);
	
      Float_t dxdz_mut = muo->get_px(lastIndex,idx0)/muo->get_pz(lastIndex,idx0);
      Float_t dydz_mut = muo->get_py(lastIndex,idx0)/muo->get_pz(lastIndex,idx0);

      Float_t x_mui=muo->get_muIDOO_gap0(0,iroad0,idx0) + (muo->get_zpos(4,idx0)-muo->get_muIDOO_gap0(2,iroad0,idx0))*muo->get_muIDOO_gap0(3, iroad0,idx0);
      Float_t y_mui=muo->get_muIDOO_gap0(1,iroad0,idx0) + (muo->get_zpos(4,idx0)-muo->get_muIDOO_gap0(2,iroad0,idx0))*muo->get_muIDOO_gap0(4, iroad0,idx0);
      Float_t z_mui=muo->get_zpos(4,idx0);
      
      dG40 = sqrt(
	MUTOO::SQUARE( x_mui - x_mut - dxdz_mut*(z_mui - z_mut) ) +
	MUTOO::SQUARE( y_mui - y_mut - dydz_mut*(z_mui - z_mut) )
	);
      
      
      
      // if (muLastGap0 < lastgap_cut) continue;//set t
      
      
      // second muon variables
      Float_t chi1( muo->get_chisquare(idx1) );
      Int_t muTRhits1( muo->get_muTRhits(idx1) );

      // second track muid variables
      Float_t muIDchis1(0);
      Int_t muIDhits1(0);
      Int_t muIDquad1(-1);
      Int_t muLastGap1( 0 );
      
      Float_t dS31( 0 );
      Float_t dS3ctp1( 0 );
      Float_t dG01( 0 );
      Float_t ddG01( 0 );
      Float_t dG41( 0 );
      
      int iroad1 = Cuts().get_best_road_oo( idx1, muo );
      if( iroad1 >= 0 )
      {
        muIDhits1 = muo->get_muIDOOhits( iroad1, idx1);
        muIDquad1 = (muo->get_muIDOO_gap0(0, iroad1, idx1 ) >0) + 2*(muo->get_muIDOO_gap0( 1, iroad1, idx1)<0);
        muIDchis1 = muo->get_muIDOOchi( iroad1, idx1 );

        dS31 = Tools::DS3( muo, idx1, iroad1 );
        dS3ctp1 = Tools::DS3ctp( muo, idx1, iroad1 );
        dG01 = Tools::DG0( muo, idx1, iroad1 );
        ddG01 = Tools::DDG0( muo, idx1, iroad1 );
      }

      r_dca=-999; x_dca=-999; y_dca=-999; z_dca=-999;
      Tools::DCA(muo,idx1,r_dca,x_dca,y_dca,z_dca);
      Float_t fDCAr1 = Tools::dist_zvtx(muo,idx1);
      Float_t fDCAz1 = fabs(z_dca-BbcZVertex);

      Float_t fdphi12_1 = atan2(muo->get_ypos(2,idx1),muo->get_xpos(2,idx1));
      fdphi12_1 -= atan2(muo->get_ypos(1,idx1),muo->get_xpos(1,idx1));
      fdphi12_1 = fabs(fdphi12_1);
      if(fdphi12_1>3) { fdphi12_1 -= 2*3.14159; }
      fdphi12_1 = fabs(fdphi12_1);
      Float_t fdphi23_1 = atan2(muo->get_ypos(3,idx1),muo->get_xpos(3,idx1));
      fdphi23_1 -= atan2(muo->get_ypos(2,idx1),muo->get_xpos(2,idx1));
      fdphi23_1 = fabs(fdphi23_1);
      if(fdphi23_1>3) { fdphi23_1 -= 2*3.14159; }
      fdphi23_1 = fabs(fdphi23_1);

      for(int igap1=4; igap1>0; igap1--)
	if (muo->is_muIDOOhit( iroad1, idx1, igap1, 0)
	    || muo->is_muIDOOhit( iroad1, idx1, igap1, 1 ))
	  {
	    muLastGap1 = igap1;
	    break;
	  }
      // if (muLastGap1 < lastgap_cut) continue;//set to 0 for single

      //Determine if kalman projection to gap4 is available. 
      lastIndex = 4;
      if (muo->get_xpos(4,idx1) == 0 && 
	  muo->get_ypos(4,idx1) == 0 && 
	  muo->get_zpos(4,idx1) == 0) 
	{
	  lastIndex = 3;
	}
      x_mut = muo->get_xpos(lastIndex,idx1);
      y_mut = muo->get_ypos(lastIndex,idx1);
      z_mut = muo->get_zpos(lastIndex,idx1);
	
      dxdz_mut = muo->get_px(lastIndex,idx1)/muo->get_pz(lastIndex,idx1);
      dydz_mut = muo->get_py(lastIndex,idx1)/muo->get_pz(lastIndex,idx1);

      x_mui=muo->get_muIDOO_gap0(0,iroad0,idx1) + (muo->get_zpos(4,idx1)-muo->get_muIDOO_gap0(2,iroad0,idx1))*muo->get_muIDOO_gap0(3, iroad1,idx1);
      y_mui=muo->get_muIDOO_gap0(1,iroad0,idx1) + (muo->get_zpos(4,idx1)-muo->get_muIDOO_gap0(2,iroad0,idx1))*muo->get_muIDOO_gap0(4, iroad1,idx1);
      z_mui=muo->get_zpos(4,idx1);
      
      dG41 = sqrt(
	MUTOO::SQUARE( x_mui - x_mut - dxdz_mut*(z_mui - z_mut) ) +
	MUTOO::SQUARE( y_mui - y_mut - dydz_mut*(z_mui - z_mut) )
	);

      // dimuon variables
      boost::array<Float_t,3> diP;
      Float_t dimass(0);
      Float_t dicharge(0);

      // dimuon's variables
      diP[0] = P0[0] + P1[0];
      diP[1] = P0[1] + P1[1];
      diP[2] = P0[2] + P1[2];

      // bend plane vertex
      Float_t dca_bp( muo->get_vtx_bp_dca( idimu ) );
      Float_t xvtx_bp( muo->get_vtx_bp_xpos( idimu ) );
      Float_t yvtx_bp( muo->get_vtx_bp_ypos( idimu ) );
      Float_t zvtx_bp( muo->get_vtx_bp_zpos( idimu ) );

      dimass = muo->get_dimass(idimu);
      dicharge = muo->get_dicharge(idimu);

      // Fill ntuple
      boost::array< Float_t, 104 > varNT;
      varNT[0] = RunNumber;
      varNT[1] = EventNumber;
      varNT[2] = npart;
      varNT[3] = BbcZVertex;
      varNT[4] = bbcCentrality;
      varNT[5] = bbcChargeSouth;
      varNT[6] = bbcChargeNorth;
      varNT[7] = trigger;
      varNT[8] = clockcross;
      varNT[9] = dimass;
      varNT[10] = dicharge;
      varNT[11] = Tools::rapidity( dimass, &diP[0] );
      TVector3 *v3 = new TVector3(diP[0],diP[1],diP[2]);
      varNT[12] = v3->Phi();
      delete v3;

      varNT[13] = Tools::pT( &diP[0] );
      varNT[14] = Tools::p( &diP[0] );
      varNT[15] = Tools::xF( dimass, &diP[0], E_CMS );
      varNT[16] = Tools::x1( dimass, &diP[0], E_CMS );
      varNT[17] = Tools::x2( dimass, &diP[0], E_CMS );
      varNT[18] = Tools::costhetaCS(Const::MUMASS, &P0[0], &P1[0] );
      varNT[19] = chi0;
      varNT[20] = chi1;
      varNT[21] = P0[0];
      varNT[22] = P0[1];

      varNT[23] = P0[2];
      varNT[24] = P1[0];
      varNT[25] = P1[1];
      varNT[26] = P1[2];
      varNT[27] = muIDhits0;
      varNT[28] = muIDhits1;
      varNT[29] = muIDquad0;
      varNT[30] = muIDquad1;
      varNT[31] = muTRhits0;
      varNT[32] = muTRhits1;

      varNT[33] = dS30;
      varNT[34] = dS31;
      varNT[35] = dS3ctp0;
      varNT[36] = dS3ctp1;
      varNT[37] = muo->get_vtx_chisquare(idimu);
      varNT[38] = muo->get_vtx_zpos(idimu);
      varNT[39] = muIDchis0;
      varNT[40] = muIDchis1;
      varNT[41] = dG00;
      varNT[42] = dG01;

      varNT[43] = ddG00;
      varNT[44] = ddG01;
      varNT[45] =  sqrt( MUTOO::SQUARE( muo->get_vtx_xpos(idimu) ) + MUTOO::SQUARE( muo->get_vtx_ypos(idimu) ) );
      varNT[46] = l2_south;
      varNT[47] = l2_north;
      varNT[48] = dca_bp;
      varNT[49] = xvtx_bp;
      varNT[50] = yvtx_bp;
      varNT[51] = zvtx_bp;
      varNT[52] = Tools::DG0x(muo,idx0, iroad0 );
      varNT[53] = Tools::DG0y(muo,idx0, iroad0 );
      varNT[54] = Tools::DG0x(muo,idx1, iroad1 );
      varNT[55] = Tools::DG0y(muo,idx1, iroad1 );
      varNT[56] = -1;//Tools::L2MuID_dimuOK( idx0, iroad0, idx1, iroad1, muo, MWGpico::RUN4);
      varNT[57] = -1;//Tools::L2MuidPairOK(idimu,muo);
      varNT[58] = mutr_hits_south;
      varNT[59] = mutr_hits_north;
      varNT[60] = rp_north;//Switched for CuAu
      varNT[61] = rp_south;//Switched for CuAu
      varNT[62] = rp_both;
      //New for CuAu
      varNT[63] = muLastGap0;
      varNT[64] = muLastGap1;
      varNT[65] = Tools::sumbit(muIDhits0);
      varNT[66] = Tools::sumbit(muIDhits1);
      varNT[67] = rpBBC_north;
      varNT[68] = rpBBC_south;
      varNT[69] = rpBBC_both;
      varNT[70] = rp1BBC_north;
      varNT[71] = rp1BBC_south;
      varNT[72] = rpMPC_north;
      varNT[73] = rpMPC_south;
      varNT[74] = rpMPC_both;
      varNT[75] = rp1MPC_north;
      varNT[76] = rp1MPC_south;
      varNT[77] = rpVTX_north;
      varNT[78] = rpVTX_south;
      varNT[79] = rpVTX_both;
      varNT[80] = rp1VTX_north;
      varNT[81] = rp1VTX_south;
      //Need to be populated
      varNT[82] = dG40;//Tr0_DG4
      varNT[83] = dG41;//Tr1_DG4
      varNT[84] = fDCAr0;//Tr0_DCAr
      varNT[85] = fDCAr1;//Tr1_DCAr
      varNT[86] = fDCAz0;//Tr0_DCAz
      varNT[87] = fDCAz1;//Tr1_DCAz
      varNT[88] = fdphi12_0;//Tr0_phi12
      varNT[89] = fdphi12_1;//Tr1_phi12
      varNT[90] = fdphi23_0;//Tr0_phi23
      varNT[91] = fdphi23_1;//Tr1_phi23
      varNT[92] = -999;//Tr0_RPC3DCA
      varNT[93] = -999;//Tr1_RPC3DCA
      varNT[94] = -999;//Tr0_RPC3time
      varNT[95] = -999;//Tr1_RPC3time
      varNT[96] = -999;//Tr0_RPC1DCA
      varNT[97] = -999;//Tr1_RPC1DCA
      varNT[98] = -999;//Tr0_RPC1time
      varNT[99] = -999;//Tr1_RPC1time
      varNT[100] = -999;//Tr0_mutradc
      varNT[101] = -999;//Tr1_mutradc
      varNT[102] = zdcEnergySouth;//ZDC Energy South
      varNT[103] = zdcEnergyNorth;//ZDC Energy North
      accDIMU++;
      dimuons->Fill( &varNT[0] );

    }
  }
  accEVT++;
  if( verbosity >= 2 ) cout << "MWGpico::FillDimuonsRun12 - done." << endl;
  return 0;
}
