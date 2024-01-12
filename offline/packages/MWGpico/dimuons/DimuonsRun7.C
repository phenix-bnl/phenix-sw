// $Id: DimuonsRun7.C,v 1.19 2010/02/23 20:03:13 hpereira Exp $

/*!
   \file DimuonsRun4.C
   \brief dimuons ntuple booking and feeling
   \author Frederic Fleuret/Hugo Pereira
   \version $Revision: 1.19 $
   \date $Date: 2010/02/23 20:03:13 $
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
void MWGpico::BookDimuonsNtupleRun7( TNtuple*& dimuons, TString name, TString title )
{

  MUTOO::PRINT( cout, "MWGpico::BookDimuonsNtupleRun7" );

  /* to make counting of variables easier, please try to write only ten variables/lines */
  const char* varlist  =
    "Run_Number:Evt_Number:Evt_Nmu:Evt_bbcZ:Evt_bbcCentrality:Evt_bbcChargeS:Evt_bbcChargeN:mass:charge:rapidity:"
    "pT:p:xF:x1:x2:costhCS:Tr0_chi2:Tr1_chi2:Tr0_px:Tr0_py:"
    "Tr0_pz:Tr1_px:Tr1_py:Tr1_pz:Tr0_idhits:Tr1_idhits:Tr0_idquad:Tr1_idquad:Tr0_trhits:Tr1_trhits:"
    "Tr0_DS3:Tr1_DS3:Tr0_DS3ctp:Tr1_DS3ctp:Evt_vtxchi2:Evt_vtxooz:Tr0_idchi2:Tr1_idchi2:Tr0_DG0:Tr1_DG0:"
    "Tr0_DDG0:Tr1_DDG0:Evt_vtxoor:Evt_l2S:Evt_l2N:dca:xvtxbp:yvtxbp:zvtxbp:Tr0_DG0x:"
    "Tr0_DG0y:Tr1_DG0x:Tr1_DG0y:MuIDlevel2ok:MuIDl2primitiveok:mutr_hits_south:mutr_hits_north:Evt_RPS:Evt_RPN:Evt_RP";

  //! create the ntuple
  dimuons = new TNtuple( name, title, varlist);

  return;

}


//_________________________________________________________________
int MWGpico::FillDimuonsRun7(PHMuoTracksOut* &muo, TNtuple* dimuons)
{
  if( verbosity >= 2 ) cout << "MWGpico::FillDimuonsRun7" << endl;

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

    rp_south = rp->getRXNrp12();
    rp_north = rp->getRXNrp15();
    rp_both = rp->getRXNrp18();

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

  // level2 decision
  Float_t l2_south = Tools::L2MuidDecision( MUTOO::South );
  Float_t l2_north = Tools::L2MuidDecision( MUTOO::North );

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

      Float_t dS30( 0 );
      Float_t dS3ctp0( 0 );
      Float_t dG00( 0 );
      Float_t ddG00( 0 );

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


      // second muon variables
      Float_t chi1( muo->get_chisquare(idx1) );
      Int_t muTRhits1( muo->get_muTRhits(idx1) );

      // second track muid variables
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
      boost::array< Float_t, 60 > varNT;
      varNT[0] = RunNumber;
      varNT[1] = EventNumber;
      varNT[2] = npart;
      varNT[3] = BbcZVertex;
      varNT[4] = bbcCentrality;
      varNT[5] = bbcChargeSouth;
      varNT[6] = bbcChargeNorth;
      varNT[7] = dimass;
      varNT[8] = dicharge;
      varNT[9] = Tools::rapidity( dimass, &diP[0] );

      varNT[10] = Tools::pT( &diP[0] );
      varNT[11] = Tools::p( &diP[0] );
      varNT[12] = Tools::xF( dimass, &diP[0], E_CMS );
      varNT[13] = Tools::x1( dimass, &diP[0], E_CMS );
      varNT[14] = Tools::x2( dimass, &diP[0], E_CMS );
      varNT[15] = Tools::costhetaCS(Const::MUMASS, &P0[0], &P1[0] );
      varNT[16] = chi0;
      varNT[17] = chi1;
      varNT[18] = P0[0];
      varNT[19] = P0[1];

      varNT[20] = P0[2];
      varNT[21] = P1[0];
      varNT[22] = P1[1];
      varNT[23] = P1[2];
      varNT[24] = muIDhits0;
      varNT[25] = muIDhits1;
      varNT[26] = muIDquad0;
      varNT[27] = muIDquad1;
      varNT[28] = muTRhits0;
      varNT[29] = muTRhits1;

      varNT[30] = dS30;
      varNT[31] = dS31;
      varNT[32] = dS3ctp0;
      varNT[33] = dS3ctp1;
      varNT[34] = muo->get_vtx_chisquare(idimu);
      varNT[35] = muo->get_vtx_zpos(idimu);
      varNT[36] = muIDchis0;
      varNT[37] = muIDchis1;
      varNT[38] = dG00;
      varNT[39] = dG01;

      varNT[40] = ddG00;
      varNT[41] = ddG01;
      varNT[42] =  sqrt( MUTOO::SQUARE( muo->get_vtx_xpos(idimu) ) + MUTOO::SQUARE( muo->get_vtx_ypos(idimu) ) );
      varNT[43] = l2_south;
      varNT[44] = l2_north;
      varNT[45] = dca_bp;
      varNT[46] = xvtx_bp;
      varNT[47] = yvtx_bp;
      varNT[48] = zvtx_bp;
      varNT[49] = Tools::DG0x(muo,idx0, iroad0 );

      varNT[50] = Tools::DG0y(muo,idx0, iroad0 );
      varNT[51] = Tools::DG0x(muo,idx1, iroad1 );
      varNT[52] = Tools::DG0y(muo,idx1, iroad1 );
      varNT[53] = Tools::L2MuID_dimuOK( idx0, iroad0, idx1, iroad1, muo, MWGpico::RUN4);
      varNT[54] = Tools::L2MuidPairOK(idimu,muo);
      varNT[55] = mutr_hits_south;
      varNT[56] = mutr_hits_north;
      varNT[57] = rp_south;
      varNT[58] = rp_north;
      varNT[59] = rp_both;

      accDIMU++;
      dimuons->Fill( &varNT[0] );

    }
  }
  accEVT++;
  if( verbosity >= 2 ) cout << "MWGpico::FillDimuonsRun7 - done." << endl;
  return 0;
}
