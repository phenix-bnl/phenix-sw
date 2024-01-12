// $Id: DimuonsRun4.C,v 1.11 2014/12/09 06:28:28 rseidl Exp $

/*!
   \file DimuonsRun4.C
   \brief dimuons ntuple booking and feeling
   \author Frederic Fleuret/Hugo Pereira
   \version $Revision: 1.11 $
   \date $Date: 2014/12/09 06:28:28 $
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
#include <TriggerHelper.h>
#include <utiCentrality.h>
#include <vector>
#include <MUTOO.h>
#include <MUTOO.h>

#include <MUTOO.h>
#include "../MWGpico.h"
#include "Tools.h"
#include "MWGConsts.h"

using namespace std;
using namespace PhUtilities;

//_________________________________________________________________
void MWGpico::BookDimuonsNtupleRun4( TNtuple*& dimuons, TString name, TString title )
{

  /* to make counting of variables easier, please try to write only ten variables/lines */
  const char* varlist  =
    "Run_Number:Evt_Number:Evt_Nmu:Evt_bbcZ:Evt_bbcCentralityByPerp:Evt_bbcCentralityByClock:mass:charge:rapidity:pT:"
    "p:xF:x1:x2:costhCS:Tr0_chi2:Tr1_chi2:Tr0_px:Tr0_py:Tr0_pz:"
    "Tr1_px:Tr1_py:Tr1_pz:Tr0_idhits:Tr1_idhits:Tr0_idquad:Tr1_idquad:Tr0_trhits:Tr1_trhits:Tr0_DS3:"
    "Tr1_DS3:Tr0_DS3ctp:Tr1_DS3ctp:Evt_vtxchi2:Evt_vtxooz:Tr0_idchi2:Tr1_idchi2:Tr0_DG0:Tr1_DG0:Tr0_DDG0:"
    "Tr1_DDG0:Evt_vtxoor:Evt_pseudotrigS_2D:Evt_pseudotrigS_1D1S:Evt_pseudotrigN_2D:Evt_pseudotrigN_1D1S:Evt_recoS_2D:Evt_recoS_1D1S:Evt_recoN_2D:Evt_recoN_1D1S:"
    "Evt_l2S:Evt_l2N:Evt_l2MutrS:Evt_l2MutrN:dca:xvtxbp:yvtxbp:zvtxbp:Tr0_DG0x:Tr0_DG0y:"
    "Tr1_DG0x:Tr1_DG0y:MuIDlevel2ok:MuIDl2primitiveok:SpinX_ID:Pol_Y:Pol_B:GL1X_ID:mutr_hits_south:mutr_hits_north";

  //! create the ntuple
  dimuons = new TNtuple( name, title, varlist);

  return;

}


//_________________________________________________________________
int MWGpico::FillDimuonsRun4(PHMuoTracksOut* &muo, TNtuple* dimuons)
{

  if( verbosity >= 2 )
  { cout << "MWGpico::FillDimuonsRun4" << endl; }

  // Global variables
  Float_t E_CMS=200;

  // event selection
  totDIMU+=muo->get_ndimu();
  if( !Cuts().pass_event_cuts( _top_node ) ) return 1; // see ../PassCuts.C

  // z_vertex
  float BbcZVertex( -9999 );
  if( _choice == "simu" || _choice == "simu_file" ) {
    bool error( false );
    BbcZVertex = Tools::zVertexMC( header, error );
    if( error ) BbcZVertex = muo->get_zpos(0,0);
  } else if(evt) BbcZVertex = evt->getBbcZVertex();

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
  }

  // centrality
  Float_t bbcCentralityByPerp = (Float_t) PhUtilities::getCentralityByPerpRun4( _top_node );
  Float_t bbcCentralityByClock = (Float_t) PhUtilities::getCentralityByClockRun4( _top_node );

  //  Float_t ZdcEnergyN=-9999;
  //if(evt) ZdcEnergyN=evt->getZdcEnergyN();

  Float_t pseudotrigN_1D1S = Tools::BLT_1D1S_Decision( MUTOO::North );
  Float_t pseudotrigN_2D = Tools::BLT_2D_Decision( MUTOO::North );
  Float_t pseudotrigS_1D1S = Tools::BLT_1D1S_Decision( MUTOO::South );
  Float_t pseudotrigS_2D = Tools::BLT_2D_Decision( MUTOO::South );
  Float_t recoN_1D1S = Tools::BLT_1D1S_Decision( MUTOO::North, true );
  Float_t recoN_2D = Tools::BLT_2D_Decision( MUTOO::North, true );
  Float_t recoS_1D1S = Tools::BLT_1D1S_Decision( MUTOO::South, true );
  Float_t recoS_2D = Tools::BLT_2D_Decision( MUTOO::South, true );

  // level2 decision
  Float_t l2_south = Tools::L2MuidDecision( MUTOO::South );
  Float_t l2_north = Tools::L2MuidDecision( MUTOO::North );
  Float_t l2_mutr_south = Tools::L2MutrDecision( MUTOO::South );
  Float_t l2_mutr_north = Tools::L2MutrDecision( MUTOO::North );

  //spin information
  Float_t SpinX_ID = -999; // beam crossing ID from spin DB
  Float_t Pol_Y =  -999; // yellow beam polarization
  Float_t Pol_B =  -999; // blue beam polarization
  Float_t GL1X_ID =  -999; // beam crossing ID from GL1

  if (spin){
    SpinX_ID = spin->GetSpinGL1CrossingID();
    Pol_Y = spin->GetSpinDirectionYellowFromV124();
    Pol_B = spin->GetSpinDirectionBlueFromV124();
    GL1X_ID = spin->GetGL1CrossingID();

  }

  if (muo)
  {
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

//       // dump positions
//       for( int i=0; i<5; i++ )
//       cout << "MWGpico::FillDimuonsRun4 - x["<<i<<"]=("
//           << muo->get_xpos(i,muo->get_ditrkIndex(1,idimu)) << ","
//           << muo->get_ypos(i,muo->get_ditrkIndex(1,idimu)) << ","
//           << muo->get_zpos(i,muo->get_ditrkIndex(1,idimu)) << ")"
//           << endl;

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

      // First track variables
      Float_t chi0( muo->get_chisquare(idx0) );
      Int_t muTRhits0( muo->get_muTRhits(idx0) );

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

      // First track variables
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

      // second muon variables
      Float_t chi1( muo->get_chisquare(idx1) );
      Int_t muTRhits1;

      // second track muid variables
      Float_t muIDchis1(0);
      Int_t muIDhits1(0);
      Int_t muIDquad1(-1);

      Float_t dS31( 0 );
      Float_t dS3ctp1( 0 );
      Float_t dG01( 0 );
      Float_t ddG01( 0 );
      Float_t dG0x1( 0 );
      Float_t dG0y1( 0 );

      muTRhits1 = muo->get_muTRhits(idx1);
      int iroad1 = Cuts().get_best_road_oo( idx1, muo );
      if( iroad1 >= 0 )
      {
        muIDhits1 = muo->get_muIDOOhits( iroad1, idx1);
        muIDquad1 = (muo->get_muIDOO_gap0(0, iroad1, idx1 ) >0) + 2*(muo->get_muIDOO_gap0( 1, iroad1, idx1)<0);
        muIDchis1 = muo->get_muIDOOchi( iroad1, idx1 );

        dS31 = Tools::DS3( muo, idx1, iroad1 );
        dS3ctp1 = Tools::DS3ctp( muo, idx1, iroad1 );
        dG01 = Tools::DG0( muo, idx1, iroad1 );
        dG0x1 = Tools::DG0x( muo, idx1, iroad1 );
        dG0y1 = Tools::DG0y( muo, idx1, iroad1 );
        ddG01 = Tools::DDG0( muo, idx1, iroad1 );
      }

      // dimuon variables
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
      boost::array< Float_t, 70 > varNT;
      varNT[0]  = RunNumber; // run number
      varNT[1]  = EventNumber; // event number
      varNT[2]  = npart; // number of muons
      varNT[3]  = BbcZVertex; // BBC Zvertex
      varNT[4]  = bbcCentralityByPerp; // bbcCentralityByPerp
      varNT[5]  = bbcCentralityByClock; // bbcCentralityByClock
      varNT[6] = dimass; // dimuon mass
      varNT[7] = dicharge; // dimuon charge
      varNT[8] = Tools::rapidity(dimass,&diP[0]); // dimuon rapidity
      varNT[9] = Tools::pT(&diP[0]); // dimuon pT

      varNT[10] = Tools::p(&diP[0]); // dimuon p
      varNT[11] = Tools::xF(dimass,&diP[0],E_CMS); // dimuon xF
      varNT[12] = Tools::x1(dimass,&diP[0],E_CMS); // dimuon x1
      varNT[13] = Tools::x2(dimass,&diP[0],E_CMS); // dimuon x2
      varNT[14] = Tools::costhetaCS(Const::MUMASS,&P0[0],&P1[1]); // costhetaCS
      varNT[15] = chi0; // chi2 of 1st muon
      varNT[16] = chi1; // chi2 of 2nd muon
      varNT[17] = P0[0]; // px of 1st muon
      varNT[18] = P0[1]; // py of 1st muon
      varNT[19] = P0[2]; // pz of 1st muon

      varNT[20] = P1[0]; // px of 2nd muon
      varNT[21] = P1[1]; // py of 2nd muon
      varNT[22] = P1[2]; // pz of 2nd muon
      varNT[23] = muIDhits0; // muid hit pattern of 1st muon
      varNT[24] = muIDhits1; // muid hit pattern of 2nd muon
      varNT[25] = muIDquad0; // muid quadrant at gap0 of 1st muon
      varNT[26] = muIDquad1; // muid quadrant at gap0 of 2nd muon
      varNT[27] = muTRhits0; // mutr hit pattern of 1st muon
      varNT[28] = muTRhits1; // mutr hit pattern of 2nd muon
      varNT[29] = dS30; // DS3 of 1st muon

      varNT[30] = dS31; // DS3 of 2nd muon
      varNT[31] = dS3ctp0; // DS3 of 1st muon (w const theta & phi)
      varNT[32] = dS3ctp1; // DS3 of 2nd muon (w const theta & phi)
      varNT[33] = muo->get_vtx_chisquare(idimu); // Vertex Chi2
      varNT[34] = muo->get_vtx_zpos(idimu); // Vertex Z
      varNT[35] = muIDchis0; // Muid Chi2
      varNT[36] = muIDchis1; // Muid Chi2
      varNT[37] = dG00;   // road to track distance at gap0
      varNT[38] = dG01;   // road to track distance at gap0
      varNT[39] = ddG00; // road to track angular distance

      varNT[40] = ddG01; // road to track angular distance
      varNT[41] =  sqrt(
      MUTOO::SQUARE( muo->get_vtx_xpos(idimu) ) +
      MUTOO::SQUARE( muo->get_vtx_ypos(idimu) ) );
      varNT[42] = pseudotrigS_2D; // pseudo trigger 2 deeps South
      varNT[43] = pseudotrigS_1D1S; // pseudo trigger 1 deep 1 shallow South
      varNT[44] = pseudotrigN_2D; // pseudo trigger 2 deeps North
      varNT[45] = pseudotrigN_1D1S; // pseudo trigger 1 deep 1 shallow North
      varNT[46] = recoS_2D; // 2 deeps South according to muid reco hits
      varNT[47] = recoS_1D1S; // 1 deep 1 shallow South according to muid reco hits
      varNT[48] = recoN_2D; // 2 deeps North according to muid reco hits
      varNT[49] = recoN_1D1S; // 1 deep 1 shallow North according to muid reco hits

      varNT[50] = l2_south;
      varNT[51] = l2_north;
      varNT[52] = l2_mutr_south;
      varNT[53] = l2_mutr_north;
      varNT[54] = dca_bp;
      varNT[55] = xvtx_bp;
      varNT[56] = yvtx_bp;
      varNT[57] = zvtx_bp;

      varNT[58] = dG0x0;
      varNT[59] = dG0y0;

      varNT[60] = dG0x1;
      varNT[61] = dG0y1;

      varNT[62] = Tools::L2MuID_dimuOK( idx0, iroad0, idx1, iroad1, muo, get_run_type() );
      varNT[63] = Tools::L2MuidPairOK(idimu,muo);

      varNT[64] = SpinX_ID;  // corrected beam Xing ID
      varNT[65] = Pol_Y;  // corrected beam Xing ID
      varNT[66] = Pol_B;  // corrected beam Xing ID
      varNT[67] = GL1X_ID;  // corrected beam Xing ID
      varNT[68] = mutr_hits_south; // # hits in south arm
      varNT[69] = mutr_hits_north;  // # hits in north arm

      accDIMU++;
      dimuons->Fill( &varNT[0] );

    }
  }
  accEVT++;
  return 0;
}
