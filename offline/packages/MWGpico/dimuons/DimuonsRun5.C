// $Id: DimuonsRun5.C,v 1.18 2017/07/15 04:49:08 phnxbld Exp $

/*!
   \file DimuonsRun5.C
   \brief dimuons ntuple booking and feeling
   \author Frederic Fleuret/Hugo Pereira
   \version $Revision: 1.18 $
   \date $Date: 2017/07/15 04:49:08 $
*/

#include <boost/array.hpp>
#include <EventHeader.h>
#include <PHGlobal.h>
#include <PHMuoTracksOut.h>
#include <RunHeader.h>
#include <string>
#include <TChain.h>
#include <TH1.h>
#include <TNtuple.h>
#include <TriggerHelper.h>
#include <utiCentrality.h>
#include <vector>
#include <MWGVertex.h>

#include <MUTOO.h>
#include "../MWGpico.h"
#include "Tools.h"
#include "MWGConsts.h"

using namespace std;
using namespace PhUtilities;

//_________________________________________________________________
void MWGpico::BookDimuonsNtupleRun5( TNtuple*& dimuons, TString name, TString title )
{
  // define variable lists
  /* to make counting of variables easier, please try to write only ten variables/lines */
  const char* varlist =
    "Run_Number:Evt_Number:Evt_Nmu:Evt_bbcCentrality:Evt_bbcZ:Evt_vtxchi2:Evt_vtxoor:Evt_vtxooz:Tr0_DDG0:Tr0_DG0:"
    "Tr0_DS3:Tr0_DS3ctp:Tr0_chi2:Tr0_idchi2:Tr0_idhits:Tr0_idquad:Tr0_px:Tr0_py:Tr0_pz:Tr0_trhits:"
    "Tr0_DG0x:Tr0_DG0y:Tr1_DDG0:Tr1_DG0:Tr1_DS3:Tr1_DS3ctp:Tr1_chi2:Tr1_idchi2:Tr1_idhits:Tr1_idquad:"
    "Tr1_px:Tr1_py:Tr1_pz:Tr1_trhits:Tr1_DG0x:Tr1_DG0y:charge:costhCS:dca:mass:"
    "p:pT:rapidity:x1:x2:xF:xvtxbp:yvtxbp:zvtxbp:MuIDlevel2ok:"
    "MuIDl2primitiveok:Evt_ll1_S2D:Evt_ll1_N2D:Evt_l2S:Evt_l2N:Evt_l2MutrS:Evt_l2MutrN:SpinX_ID:Pol_Y:Pol_B:"
    "GL1X_ID:Evt_ll1_S1D1S:Evt_ll1_N1D1S:Evt_pseudoblt_S1D1S:Evt_pseudoblt_N1D1S:Evt_pseudoblt_S2D:Evt_pseudoblt_N2D:Evt_pseudoblt_reco_S1D1S:Evt_pseudoblt_reco_N1D1S:Evt_pseudoblt_reco_S2D:"
    "Evt_pseudoblt_reco_N2D:Evt_blt_S1D1S:Evt_blt_N1D1S:Evt_blt_S2D:Evt_blt_N2D:Tr0_dAx:Tr1_dAx";

  //! create the ntuple
  dimuons = new TNtuple( name, title, varlist);

  return;

}

//_________________________________________________________________
int MWGpico::FillDimuonsRun5(PHMuoTracksOut* &muo, TNtuple* dimuons)
{

  if( verbosity >= 2 ) cout << "MWGpico::FillDimuonsRun5" << endl;

  // Global variables
  Float_t E_CMS=200;

  // event selection
  totDIMU+=muo->get_ndimu();
  if( !Cuts().pass_event_cuts( _top_node ) ) return 1; // see ../PassCuts.C

  // z_vertex and centrality
  float BbcZVertex( -9999 );
  Float_t bbcCentrality= -9999;
  if( _choice == "simu" || _choice == "simu_file" ) {
    bool error( false );
    BbcZVertex = Tools::zVertexMC( header, error );
    if( error ) BbcZVertex = muo->get_zpos(0,0);
  }
  else if (evt) {
    BbcZVertex = evt->getBbcZVertex();
    if( _choice != "pp05" ) bbcCentrality = evt->getCentrality();
  }


  // event/run number
  int RunNumber = ( run_header ) ? run_header->get_RunNumber():0;
  int EventNumber = (event_header ) ? event_header->get_EvtSequence():0;

  // try load MC if failed from Run/EventHeader
  {
    bool error( false );
    if( !RunNumber ) RunNumber = Tools::runNumberMC( header, error );
    if( !EventNumber ) EventNumber = Tools::eventNumberMC( header, error );
  }

  // pseudoLL1 1D1S and 2D decision
  Float_t ll1_2D_south = Tools::LL1_2D_Decision( MUTOO::South );
  Float_t ll1_2D_north = Tools::LL1_2D_Decision( MUTOO::North );
  Float_t ll1_1D1S_south = Tools::LL1_1D1S_Decision( MUTOO::South );
  Float_t ll1_1D1S_north = Tools::LL1_1D1S_Decision( MUTOO::North );

  // BLT decision
  Float_t blt_2D_south = -1;
  Float_t blt_2D_north = -1;
  Float_t blt_1D1S_south = -1;
  Float_t blt_1D1S_north = -1;
  if( !(_choice == "simu" || _choice == "simu_file") )
  {

    TriggerHelper trigger_helper( _top_node );
    blt_2D_south = trigger_helper.trigScaled( "MUIDS_2D&BBCLL1" );
    blt_2D_north = trigger_helper.trigScaled( "MUIDN_2D&BBCLL1" );
    blt_1D1S_south = trigger_helper.trigScaled( "MUIDS_1D1S*BBCLL1" );
    blt_1D1S_north = trigger_helper.trigScaled( "MUIDN_1D1S*BBCLL1" );
  }

  // BLT emulator
  Float_t pseudoblt_2D_south = Tools::BLT_2D_Decision( MUTOO::South );
  Float_t pseudoblt_2D_north = Tools::BLT_2D_Decision( MUTOO::North );
  Float_t pseudoblt_1D1S_south = Tools::BLT_1D1S_Decision( MUTOO::South );
  Float_t pseudoblt_1D1S_north = Tools::BLT_1D1S_Decision( MUTOO::North );

  Float_t pseudoblt_reco_2D_south = Tools::BLT_2D_Decision( MUTOO::South, true );
  Float_t pseudoblt_reco_2D_north = Tools::BLT_2D_Decision( MUTOO::North, true );
  Float_t pseudoblt_reco_1D1S_south = Tools::BLT_1D1S_Decision( MUTOO::South, true );
  Float_t pseudoblt_reco_1D1S_north = Tools::BLT_1D1S_Decision( MUTOO::North, true );

  // level2 decision
  Float_t l2_south = Tools::L2MuidDecision( MUTOO::South );
  Float_t l2_north = Tools::L2MuidDecision( MUTOO::North );
  Float_t l2_mutr_south = Tools::L2MutrDecision( MUTOO::South );
  Float_t l2_mutr_north = Tools::L2MutrDecision( MUTOO::North );

  //spin information
  Float_t SpinX_ID = -999; // beam crossing ID from spin DB
  Float_t Pol_Y = -999; // yellow beam polarization
  Float_t Pol_B = -999; // blue beam polarization
  Float_t GL1X_ID =  -999; // beam crossing ID from GL1

  if (spin){
    SpinX_ID = spin->GetSpinGL1CrossingID();
    Pol_Y = spin->GetSpinDirectionYellowFromV124();
    Pol_B = spin->GetSpinDirectionBlueFromV124();
    GL1X_ID = spin->GetGL1CrossingID();
  }

  Float_t dA0 = -99; // deflection angle vtx-STI
  Float_t dA1 = -99;

  if (muo) {
    int ndimu = muo->get_ndimu();
    int npart = muo->get_npart();

    // Loop over dimuons
    for (int idimu=0; idimu<ndimu; idimu++)
    {

      // Remove dimuons with unassociated tracks (negative index)
      if(muo->get_ditrkIndex(0,idimu) < 0 || muo->get_ditrkIndex(1,idimu) < 0 )
      {
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

      // First track variables
      Float_t chi0( muo->get_chisquare(idx0) );
      Float_t muTRhits0( muo->get_muTRhits(idx0) );

      // First track muid variables
      Float_t muIDchis0(0);
      Int_t muIDhits0(0);
      Int_t muIDquad0(-1);

      Float_t dS30( 0 );
      Float_t dS3ctp0( 0 );
      Float_t dG00( 0 );
      Float_t ddG00( 0 );
      Float_t dG0x0( 0 );
      Float_t dG0y0( 0 );

      int iroad0 = Cuts().get_best_road_oo( idx0, muo );
      if( iroad0 >= 0 )
      {
        muIDhits0 = muo->get_muIDOOhits( iroad0, idx0);
        muIDquad0 = (muo->get_muIDOO_gap0(0, iroad0, idx0 ) >0) + 2*(muo->get_muIDOO_gap0( 1, iroad0, idx0)<0);
        muIDchis0 = muo->get_muIDOOchi( iroad0, idx0 );

        dS30 = Tools::DS3( muo, idx0, iroad0 );
        dS3ctp0 = Tools::DS3ctp( muo, idx0, iroad0 );
        dG00 = Tools::DG0( muo, idx0, iroad0 );
        dG0x0 = Tools::DG0x( muo, idx0, iroad0 );
        dG0y0 = Tools::DG0y( muo, idx0, iroad0 );

        ddG00 = Tools::DDG0( muo, idx0, iroad0 );
      }

      // Second track variables
      Float_t muTRhits1 = muo->get_muTRhits(idx1);
      Float_t chi1 = muo->get_chisquare(idx1);

      // second track muid variables
      Float_t muIDchis1(0);
      Int_t muIDhits1(0);
      Int_t muIDquad1(-1);

      Float_t dS31( 0 );
      Float_t dS3ctp1( 0 );
      Float_t dG01( 0 );
      Float_t ddG01( 0 );
      //Float_t dG0x1( 0 );
      //      Float_t dG0y1( 0 );

      int iroad1 = Cuts().get_best_road_oo( idx1, muo );
      if( iroad1 >= 0 )
      {
        muIDhits1 = muo->get_muIDOOhits( iroad1, idx1);
        muIDquad1 = (muo->get_muIDOO_gap0(0, iroad1, idx1 ) >0) + 2*(muo->get_muIDOO_gap0( 1, iroad1, idx1)<0);
        muIDchis1 = muo->get_muIDOOchi( iroad1, idx1 );

        dS31 = Tools::DS3( muo, idx1, iroad1 );
        dS3ctp1 = Tools::DS3ctp( muo, idx1, iroad1 );
        dG01 = Tools::DG0( muo, idx1, iroad1 );
	//        dG0x1 = Tools::DG0x( muo, idx1, iroad1 );
        //dG0y1 = Tools::DG0y( muo, idx1, iroad1 );

        ddG01 = Tools::DDG0( muo, idx1, iroad1 );
     }

      // dimuon's variables
      boost::array<Float_t,3> diP;
      diP[0] = P0[0] + P1[0];
      diP[1] = P0[1] + P1[1];
      diP[2] = P0[2] + P1[2];

      // bend plane vertex
      Float_t dca_bp( muo->get_vtx_bp_dca( idimu ) );
      Float_t xvtx_bp( muo->get_vtx_bp_xpos( idimu ) );
      Float_t yvtx_bp( muo->get_vtx_bp_ypos( idimu ) );
      Float_t zvtx_bp( muo->get_vtx_bp_zpos( idimu ) );

      Float_t dimass = muo->get_dimass(idimu);
      Float_t dicharge = muo->get_dicharge(idimu);

      // Fill ntuple
      //  boost::array< Float_t, 75 > varNT;
      boost::array< Float_t, 77 > varNT;

      //== run
      varNT[0]  = RunNumber; // run number
      //== event
      varNT[1] = EventNumber; // event number
      varNT[2] = npart; // number of muons
      varNT[3] = bbcCentrality; // bbcCentrality
      varNT[4] = BbcZVertex; // BBC Zvertex
      varNT[5] = muo->get_vtx_chisquare(idimu);   // Vertex Chi2
      varNT[6] =  sqrt( // mutoo vertex distance to the beam axis
      MUTOO::SQUARE( muo->get_vtx_xpos(idimu) ) +
      MUTOO::SQUARE( muo->get_vtx_ypos(idimu) ) );
      varNT[7] = muo->get_vtx_zpos(idimu); // Vertex Z
      //== first muon
      varNT[8] = ddG00;  // road to track angular distance
      varNT[9] = dG00;   // road to track distance at gap0
      varNT[10] = dS30; // DS3 of 1st muon
      varNT[11] = dS3ctp0; // DS3 of 1st muon (w const theta & phi)
      varNT[12] = chi0; // chi2 of 1st muon
      varNT[13] = muIDchis0; // Muid Chi2
      varNT[14] = muIDhits0; // muid hit pattern of 1st muon
      varNT[15] = muIDquad0; // muid quadrant at gap0 of 1st muon
      varNT[16] = P0[0]; // px of 1st muon
      varNT[17] = P0[1]; // py of 1st muon
      varNT[18] = P0[2]; // pz of 1st muon
      varNT[19] = muTRhits0; // mutr hit pattern of 1st muon
      varNT[20] = dG0x0;
      varNT[21] = dG0y0;
      //== second muon
      varNT[22] = ddG01; // road to track angular distance
      varNT[23] = dG01; // road to track distance at gap0
      varNT[24] = dS31; // DS3 of 2nd muon
      varNT[25] = dS3ctp1; // DS3 of 2nd muon (w const theta & phi)
      varNT[26] = chi1; // chi2 of 2nd muon
      varNT[27] = muIDchis1; // Muid Chi2
      varNT[28] = muIDhits1; // muid hit pattern of 2nd muon
      varNT[29] = muIDquad1; // muid quadrant at gap0 of 2nd muon
      varNT[30] = P1[0]; // px of 2nd muon
      varNT[31] = P1[1]; // py of 2nd muon
      varNT[32] = P1[2]; // pz of 2nd muon
      varNT[33] = muTRhits1; // mutr hit pattern of 2nd muon
      varNT[34] = Tools::DG0x(muo,idx1);
      varNT[35] = Tools::DG0y(muo,idx1);
      //== dimuon
      varNT[36] = dicharge; // dimuon charge
      varNT[37] = Tools::costhetaCS(Const::MUMASS,&P0[0],&P1[0]); // costhetaCS
      varNT[38] = dca_bp;
      varNT[39] = dimass; // dimuon mass
      varNT[40] = Tools::p(&diP[0]); // dimuon p
      varNT[41] = Tools::pT(&diP[0]); // dimuon pT
      varNT[42] = Tools::rapidity(dimass,&diP[0]); // dimuon rapidity
      varNT[43] = Tools::x1(dimass,&diP[0],E_CMS); // dimuon x1
      varNT[44] = Tools::x2(dimass,&diP[0],E_CMS); // dimuon x2
      varNT[45] = Tools::xF(dimass,&diP[0],E_CMS); // dimuon xF
      varNT[46] = xvtx_bp;
      varNT[47] = yvtx_bp;
      varNT[48] = zvtx_bp;
      varNT[49] = Tools::L2MuID_dimuOK( idx0, iroad0, idx1, iroad1, muo, get_run_type() );
      varNT[50] = Tools::L2MuidPairOK(idimu,muo);

      // pseudoLL1 2D
      varNT[51] = ll1_2D_south;
      varNT[52] = ll1_2D_north;
      // level2 decision [muid dimuon trigger]
      varNT[53] = l2_south;
      varNT[54] = l2_north;
      // level2 decision [mutr dimuon trigger]
      varNT[55] = l2_mutr_south;
      varNT[56] = l2_mutr_north;

      //spin information
      varNT[57] = SpinX_ID; // corrected beam Xing ID
      varNT[58] = Pol_Y; // corrected beam Xing ID
      varNT[59] = Pol_B; // corrected beam Xing ID
      varNT[60] = GL1X_ID; // corrected beam Xing ID

      // pseudoLL1 1D1S
      varNT[61] = ll1_1D1S_south;
      varNT[62] = ll1_1D1S_north;

      // blt decisions
      varNT[63] = pseudoblt_1D1S_south;
      varNT[64] = pseudoblt_1D1S_north;
      varNT[65] = pseudoblt_2D_south;
      varNT[66] = pseudoblt_2D_north;
      varNT[67] = pseudoblt_reco_1D1S_south;
      varNT[68] = pseudoblt_reco_1D1S_north;
      varNT[69] = pseudoblt_reco_2D_south;
      varNT[70] = pseudoblt_reco_2D_north;

      varNT[71] = blt_1D1S_south;
      varNT[72] = blt_1D1S_north;
      varNT[73] = blt_2D_south;
      varNT[74] = blt_2D_north;
      varNT[75] = dA0;
      varNT[76] = dA1;

      accDIMU++;
      dimuons->Fill( &varNT[0] );

    }
  }
  accEVT++;
  return 0;
}

//_________________________________________________________________
int MWGpico::FillDimuonsRun5BackToBack(PHMuoTracksOut* &muo, TNtuple* dimuons)
{

  if( verbosity >= 2 ) cout << "MWGpico::FillDimuonsRun5BackToBack" << endl;

  // Global variables
  Float_t E_CMS=200;

  // event selection
  totDIMU+=muo->get_ndimu();
  if( !Cuts().pass_event_cuts( _top_node ) ) return 1; // see ../PassCuts.C

  // z_vertex and centrality
  float BbcZVertex( -9999 );
  Float_t bbcCentrality= -9999;
  if( _choice == "simu" || _choice == "simu_file" ) {
    bool error( false );
    BbcZVertex = Tools::zVertexMC( header, error );
    if( error ) BbcZVertex = muo->get_zpos(0,0);
  }
  else if (evt) {
    BbcZVertex = evt->getBbcZVertex();
    if( _choice != "pp05" ) bbcCentrality = evt->getCentrality();
  }


  // event/run number
  int RunNumber = ( run_header ) ? run_header->get_RunNumber():0;
  int EventNumber = (event_header ) ? event_header->get_EvtSequence():0;

  // try load MC if failed from Run/EventHeader
  {
    bool error( false );
    if( !RunNumber ) RunNumber = Tools::runNumberMC( header, error );
    if( !EventNumber ) EventNumber = Tools::eventNumberMC( header, error );
  }

  // pseudoLL1 1D1S and 2D decision
  Float_t ll1_2D_south   = Tools::LL1_2D_Decision( MUTOO::South );
  Float_t ll1_2D_north   = Tools::LL1_2D_Decision( MUTOO::North );
  Float_t ll1_1D1S_south = Tools::LL1_1D1S_Decision( MUTOO::South );
  Float_t ll1_1D1S_north = Tools::LL1_1D1S_Decision( MUTOO::North );

  // level2 decision
  Float_t l2_south      = Tools::L2MuidDecision( MUTOO::South );
  Float_t l2_north      = Tools::L2MuidDecision( MUTOO::North );
  Float_t l2_mutr_south = Tools::L2MutrDecision( MUTOO::South );
  Float_t l2_mutr_north = Tools::L2MutrDecision( MUTOO::North );

  //spin information
  Float_t SpinX_ID = -999; // beam crossing ID from spin DB
  Float_t Pol_Y    = -999; // yellow beam polarization
  Float_t Pol_B    = -999; // blue beam polarization
  Float_t GL1X_ID  =  -999; // beam crossing ID from GL1

  if (spin){
    SpinX_ID = spin->GetSpinGL1CrossingID();
    Pol_Y = spin->GetSpinDirectionYellowFromV124();
    Pol_B = spin->GetSpinDirectionBlueFromV124();
    GL1X_ID = spin->GetGL1CrossingID();
  }

  if (muo) {


    // check PHMuoTracks or PHMuoTracksOO exist
    int npart = muo->get_npart();

    if( verbosity >= 2 ) cout << "MWGPico::FillDimuonsRun5BackToBack - totEVT: " << totEVT << " npart: " << npart << endl;

    // Loop over south arm muons
    for( int idx0 = 0; idx0 < npart; idx0 ++ )
    {

      // make sure muon is in south arm
      if( !( muo->get_pz( 0, idx0 ) < 0 ) ) continue;

      // make sure muon pass cuts
      if (evt && !Cuts().pass_single_muon_cuts(evt,idx0,muo, _framework) ) continue;

      // First track variables
      Int_t muTRhits0( muo->get_muTRhits(idx0) );
      Float_t chi0( muo->get_chisquare(idx0) );

      // first track muid variables
      Float_t dA0 = Tools::DA(muo,idx0,BbcZVertex);
      Int_t muIDhits0(0);
      Int_t muIDquad0(0);
      Float_t muIDchis0(0);

      Float_t dS30(0);
      Float_t dS3ctp0(0);
      Float_t dG00(0);
      Float_t ddG00(0);

      int iroad0 = Cuts().get_best_road_oo( idx0, muo );
      if( iroad0 >= 0 )
      {
        muIDhits0 = muo->get_muIDOOhits( iroad0, idx0);
        muIDquad0 = (muo->get_muIDOO_gap0(0, iroad0, idx0 ) >0) + 2*(muo->get_muIDOO_gap0( 1, iroad0, idx0)<0);
        muIDchis0 = muo->get_muIDOOchi( iroad0, idx0 );

        dS30 = Tools::DS3( muo, idx0, iroad0 );
        dS3ctp0 = Tools::DS3ctp( muo, idx0, iroad0 );
        dG00 = Tools::DG0( muo, idx0, iroad0 );
        ddG00 = Tools::DDG0( muo, idx0, iroad0 );
      }

      // loop over north arm muons
      for( int idx1 = 0; idx1 < npart; idx1++ )
      {

        // make sure muon is in south arm
        if( !( muo->get_pz( 0, idx1 ) > 0 ) ) continue;

        // make sure muon pass cuts
        if (evt && !Cuts().pass_single_muon_cuts(evt,idx1,muo, _framework) ) continue;

        // Second track variables
        Int_t muTRhits1 = muo->get_muTRhits(idx1);
        Float_t chi1 = muo->get_chisquare(idx1);

        Float_t dA1 = Tools::DA(muo,idx1, BbcZVertex);
        Float_t muIDchis1(0);
        Int_t muIDhits1(0);
        Int_t muIDquad1(-1);

        Float_t dS31( 0 );
        Float_t dS3ctp1( 0 );
        Float_t dG01( 0 );
        Float_t ddG01( 0 );

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

        // make vertex fit
        MWGVertex vertex;
        try {

          vertex.set_verbosity( 0 );

          // add tracks
          vertex.add_track( idx0, muo );
          vertex.add_track( idx1, muo );

          // add vertex information
          double z_vertex( 0 );
          double z_vertex_error( 0 );

          if( _choice == "simu" || _choice == "simu_file" ) {

            // retrieve vertex from pisa header file or first track
            if( header ) {
              bool error( true );
              z_vertex = Tools::zVertexMC( header, error );
              if( error ) z_vertex = muo->get_zpos(0,0);
            }
            else {
              z_vertex = muo->get_zpos(0,0);
              z_vertex_error = 2;
            }
          }
          else if( evt ) {  // real data
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
          Float_t P0[3] = {
            (float) vertex.get_px( 0 ),
            (float) vertex.get_py( 0 ),
            (float) vertex.get_pz( 0 )
          };

          Float_t P1[3] = {
            (float) vertex.get_px( 1 ),
            (float) vertex.get_py( 1 ),
            (float) vertex.get_pz( 1 )
          };

          // dimuon's variables
          Float_t diP[3] = {
            P0[0] + P1[0],
            P0[1] + P1[1],
            P0[2] + P1[2]
          };

          Float_t dimass = vertex.get_mass();
          Float_t dicharge = ( muo->get_charge(idx0) != muo->get_charge(idx1 ) ) ? 0:muo->get_charge( idx0 );

          // Fill ntuple
          boost::array< Float_t, 77 > varNT;

          //== run
          varNT[0]  = RunNumber; // run number

          //== event
          varNT[1] = EventNumber; // event number
          varNT[2] = npart; // number of muons
          varNT[3] = bbcCentrality; // bbcCentrality
          varNT[4] = BbcZVertex;                                     // BBC Zvertex
          varNT[5] = vertex.get_chisquare();                         // Vertex Chi2
          // mutoo vertex distance to the beam axis
          varNT[6] =  sqrt(MUTOO::SQUARE( vertex.get_vtx_x() ) +
            MUTOO::SQUARE( vertex.get_vtx_y() ) );
          varNT[7] = vertex.get_vtx_z(); // Vertex Z

          //== first muon
          varNT[8]  = ddG00;  // road to track angular distance
          varNT[9]  = dG00;   // road to track distance at gap0
          varNT[10] = dS30; // DS3 of 1st muon
          varNT[11] = dS3ctp0; // DS3 of 1st muon (w const theta & phi)
          varNT[12] = chi0; // chi2 of 1st muon
          varNT[13] = muIDchis0; // Muid Chi2
          varNT[14] = muIDhits0; // muid hit pattern of 1st muon
          varNT[15] = muIDquad0; // muid quadrant at gap0 of 1st muon
          varNT[16] = P0[0]; // px of 1st muon
          varNT[17] = P0[1]; // py of 1st muon
          varNT[18] = P0[2]; // pz of 1st muon
          varNT[19] = muTRhits0; // mutr hit pattern of 1st muon
          varNT[20] = Tools::DG0x(muo,idx0);
          varNT[21] = Tools::DG0y(muo,idx0);

          //== second muon
          varNT[22] = ddG01; // road to track angular distance
          varNT[23] = dG01; // road to track distance at gap0
          varNT[24] = dS31; // DS3 of 2nd muon
          varNT[25] = dS3ctp1; // DS3 of 2nd muon (w const theta & phi)
          varNT[26] = chi1; // chi2 of 2nd muon
          varNT[27] = muIDchis1; // Muid Chi2
          varNT[28] = muIDhits1; // muid hit pattern of 2nd muon
          varNT[29] = muIDquad1; // muid quadrant at gap0 of 2nd muon
          varNT[30] = P1[0]; // px of 2nd muon
          varNT[31] = P1[1]; // py of 2nd muon
          varNT[32] = P1[2]; // pz of 2nd muon
          varNT[33] = muTRhits1; // mutr hit pattern of 2nd muon
          varNT[34] = Tools::DG0x(muo,idx1);
          varNT[35] = Tools::DG0y(muo,idx1);

          //== dimuon
          varNT[36] = dicharge; // dimuon charge
          varNT[37] = Tools::costhetaCS(Const::MUMASS,P0,P1); // costhetaCS
          varNT[38] = 0;
          varNT[39] = dimass; // dimuon mass
          varNT[40] = Tools::p(diP);   // dimuon p
          varNT[41] = Tools::pT(diP);   // dimuon pT
          varNT[42] = Tools::rapidity(dimass,diP); // dimuon rapidity
          varNT[43] = Tools::x1(dimass,diP,E_CMS); // dimuon x1
          varNT[44] = Tools::x2(dimass,diP,E_CMS); // dimuon x2
          varNT[45] = Tools::xF(dimass,diP,E_CMS); // dimuon xF
          varNT[46] = 0;
          varNT[47] = 0;
          varNT[48] = 0;
          varNT[49] = 0;
          varNT[50] = 0;

          // pseudoLL1 2D
          varNT[51] = ll1_2D_south;
          varNT[52] = ll1_2D_north;

          // level2 decision [muid dimuon trigger]
          varNT[53] = l2_south;
          varNT[54] = l2_north;

          // level2 decision [mutr dimuon trigger]
          varNT[55] = l2_mutr_south;
          varNT[56] = l2_mutr_north;

          //spin information
          varNT[57] = SpinX_ID;   // corrected beam Xing ID
          varNT[58] = Pol_Y;   // corrected beam Xing ID
          varNT[59] = Pol_B;   // corrected beam Xing ID
          varNT[60] = GL1X_ID;   // corrected beam Xing ID

          // pseudoLL1 1D1S
          varNT[61] = ll1_1D1S_south;
          varNT[62] = ll1_1D1S_north;


          // blt decisions
          varNT[63] =0; // pseudoblt_1D1S_south;
          varNT[64] =0; // pseudoblt_1D1S_north;
          varNT[65] =0; // pseudoblt_2D_south;
          varNT[66] =0; // pseudoblt_2D_north;
          varNT[67] =0; // pseudoblt_reco_1D1S_south;
          varNT[68] =0; // pseudoblt_reco_1D1S_north;
          varNT[69] =0; // pseudoblt_reco_2D_south;
          varNT[70] =0; // pseudoblt_reco_2D_north;

          varNT[71] =0; // blt_1D1S_south;
          varNT[72] =0; // blt_1D1S_north;
          varNT[73] =0; // blt_2D_south;
          varNT[74] =0; // blt_2D_north;
          varNT[75] = dA0;
          varNT[76] = dA1;

          accDIMU++;
          dimuons->Fill( &varNT[0] );

        } catch( std::exception &e ) { cout << e.what() << endl; }

      }
    }

  }

  accEVT++;
  return 0;
}

