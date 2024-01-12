
// $Id: DimuonsRun3.C,v 1.9 2010/02/23 20:03:13 hpereira Exp $

/*!
   \file DimuonsRun3.C
   \brief dimuons ntuple booking and feeling
   \author Frederic Fleuret/Hugo Pereira
   \version $Revision: 1.9 $
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

#include <MUTOO.h>
#include "../MWGpico.h"
#include "Tools.h"
#include "MWGConsts.h"

using namespace std;
using namespace PhUtilities;

//_________________________________________________________________
void MWGpico::BookDimuonsNtupleRun3( TNtuple*& dimuons, TString name, TString title )
{
  // define variable lists
  /* to make counting of variables easier, please try to write only ten variables/lines */
  const char* varlist=  "Run_Number:Evt_Number:Evt_Nmu:Evt_bbcZ:Evt_bbcCentrality:Evt_fclCentrality:Evt_zdcCentrality:Evt_zdcEnerN:Evt_fclGreyN:Evt_pseudotrigS_2D:"
        "Evt_pseudotrigS_1D1S:Evt_pseudotrigN_2D:Evt_pseudotrigN_1D1S:mass:charge:rapidity:pT:p:xF:x1:"
        "x2:costhCS:dca:xvtxbp:yvtxbp:zvtxbp:Tr0_chi2:Tr1_chi2:Tr0_px:Tr0_py:"
        "Tr0_pz:Tr1_px:Tr1_py:Tr1_pz:Tr0_idhits:Tr1_idhits:Tr0_idquad:Tr1_idquad:Tr0_trhits:Tr1_trhits:"
        "Tr0_DS3:Tr1_DS3:Tr0_DS3ctp:Tr1_DS3ctp:Evt_vtxchi2:Evt_vtxooz:Tr0_idchi2:Tr1_idchi2:Tr0_DG0:Tr1_DG0:"
        "Tr0_DDG0:Tr1_DDG0:Evt_vtxoor:SpinX_ID:Pol_Y:Pol_B:GL1X_ID";

  //! create the ntuple
  dimuons = new TNtuple( name, title, varlist);

  return;

}

//_________________________________________________________________
int MWGpico::FillDimuonsRun3(PHMuoTracksOut* &muo, TNtuple* dimuons)
{

  if( verbosity >= 2 ) cout << "MWGpico::FillDimuonsRun3" << endl;

  // Done for dAu, tricked for pp
  // Global variables
  Float_t E_CMS=200;

  // event selection
  totDIMU+=muo->get_ndimu();
  if( !Cuts().pass_event_cuts( _top_node ) ) return 1;

  // z_vertex
  float BbcZVertex( -9999 );
  if( _choice == "simu" || _choice == "simu_file" ) {
    bool error( false );
    BbcZVertex = Tools::zVertexMC( header, error );
    if( error ) BbcZVertex = muo->get_zpos(0,0);
  } else if (evt) BbcZVertex = evt->getBbcZVertex();

  // event/run number
  int RunNumber = ( run_header ) ? run_header->get_RunNumber():0;
  int EventNumber = (event_header ) ? event_header->get_EvtSequence():0;

  // try load MC if failed from PHGlobal
  {
    bool error( false );
    if( !RunNumber ) RunNumber = Tools::runNumberMC( header, error );
    if( !EventNumber ) EventNumber  = Tools::eventNumberMC( header, error );
  }

  // centrality
  Float_t bbcCentrality=-9999;
  Float_t zdcCentrality=-9999;
  Float_t fclCentrality=-9999;
  Float_t FclGreyN=-9999;
  Float_t ZdcEnergyN=-9999;
  if (evt  && _type == "dAu" ) {
    bbcCentrality = evt->get_dAuBbcCentrality();
    zdcCentrality = evt->get_dAuZdcCentrality();
    fclCentrality = evt->get_dAuFclCentrality();
    ZdcEnergyN=evt->getZdcEnergyN();
  }

  int pseudotrigS_2D   = -1;
  int pseudotrigS_1D1S = -1;
  int pseudotrigN_2D   = -1;
  int pseudotrigN_1D1S = -1;

//  if (_framework == MUT) {
//  // pseudotrig does not exist anymore
//    if (pseudotrig) {
//      pseudotrigS_2D   = pseudotrig->DoubleDeep();
//      pseudotrigS_1D1S = pseudotrig->DeepShallow();
//      pseudotrigN_2D   = pseudotrig->DoubleDeepN();
//      pseudotrigN_1D1S = pseudotrig->DeepShallowN();
//    }
//  }

  if (_framework == MUTOO )
  {
    pseudotrigN_1D1S = Tools::BLT_1D1S_Decision( MUTOO::North );
    pseudotrigN_2D = Tools::BLT_2D_Decision( MUTOO::North );

    pseudotrigS_1D1S = Tools::BLT_1D1S_Decision( MUTOO::South );
    pseudotrigS_2D = Tools::BLT_2D_Decision( MUTOO::South );
  }

  // first muon variables
  Float_t P0[3]={0,0,0};
  Float_t chi0=0;
  Float_t muIDchis0=0;

  Int_t idx0=0;
  Int_t muIDhits0=0;
  Int_t muIDquad0=-1;
  Int_t muTRhits0=0;

  // second muon variables
  Float_t P1[3]={0,0,0};
  Float_t chi1=0;
  Float_t muIDchis1=0;

  Int_t idx1=0;
  Int_t muIDhits1=0;
  Int_t muIDquad1=-1;
  Int_t muTRhits1=0;

  // dimuon variables
  Float_t diP[3]={0,0,0};
  Float_t dimass=0;
  Float_t dicharge=0;
  Float_t mydca=0;
  Float_t xvtxbp=0;
  Float_t yvtxbp=0;
  Float_t zvtxbp=0;

  //spin information
  Float_t SpinX_ID =  -999; // beam crossing ID from spin DB
  Float_t Pol_Y    =  -999; // yellow beam polarization
  Float_t Pol_B    =  -999; // blue beam polarization
  Float_t GL1X_ID =  -999; // beam crossing ID from GL1

  if (spin){
    SpinX_ID = spin->GetSpinGL1CrossingID();
    Pol_Y    = spin->GetSpinDirectionYellowFromV124();
    Pol_B    = spin->GetSpinDirectionBlueFromV124();
    GL1X_ID  = spin->GetGL1CrossingID();

  } // if (spin)



  if (muo) { // check PHMuoTracks or PHMuoTracksOO exist
    int ndimu = muo->get_ndimu();
    int npart = muo->get_npart();

    // Loop over dimuons
    for (int idimu=0; idimu<ndimu; idimu++) {

      // Remove dimuons with unassociated tracks (negative index)
      if(muo->get_ditrkIndex(0,idimu) < 0 || muo->get_ditrkIndex(1,idimu) < 0 ) {
        if( verbosity ) cout << "MWGpico::FillDimuonsNtuple - wrong track index. Vertex skipped.\n";
        negDIMU++;
        continue;
      }

      // See if dimuon pass dimuon cuts (see ../PassCuts.C)
      if (!Cuts().pass_dimuon_cuts( evt, idimu, muo, _framework )) continue;

      // get dimuon's tracks index (for +- pairs the + track will go to first index)
      if (_framework == MUTOO) {
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
      }

      if( _framework == MUT ) {
        int idx=muo->get_ditrkIndex(0,idimu);
        if (muo->get_charge(idx)>0) {

          idx0 = idx;
          P0[0] = muo->get_px(0,idx0);
          P0[1] = muo->get_py(0,idx0);
          P0[2] = muo->get_pz(0,idx0);

          idx1 = muo->get_ditrkIndex(1,idimu);
          P1[0] = muo->get_px(0,idx1);
          P1[1] = muo->get_py(0,idx1);
          P1[2] = muo->get_pz(0,idx1);

        } else {

          idx1 = idx;
          P1[0] = muo->get_px(0,idx1);
          P1[1] = muo->get_py(0,idx1);
          P1[2] = muo->get_pz(0,idx1);

          idx0 = muo->get_ditrkIndex(1,idimu);
          P0[0] = muo->get_px(0,idx0);
          P0[1] = muo->get_py(0,idx0);
          P0[2] = muo->get_pz(0,idx0);

        }

      }

      // dimuon selection (see ../PassCuts.C)
      if (evt && ( !Cuts().pass_single_muon_cuts( evt, idx0, muo, _framework ) || !Cuts().pass_single_muon_cuts(evt,idx1,muo, _framework )) ) continue;

      // First track variables
      muIDhits0 = muo->get_muIDhits(idx0);
      muIDquad0 = (muo->get_muID_gap0(0,idx0)>0) + 2*(muo->get_muID_gap0(1,idx0)<0);
      muIDchis0 = muo->get_muIDOOchi(0,idx0); // value for best, or only, road is stored in slot 0
      muTRhits0 = muo->get_muTRhits(idx0);
      chi0 = muo->get_chisquare(idx0);
      Float_t dS30 = Tools::DS3(muo,idx0);
      Float_t dS3ctp0 = Tools::DS3ctp(muo,idx0);
      Float_t dG00 = Tools::DG0(muo,idx0);
      Float_t ddG00 = Tools::DDG0(muo,idx0);

      // Second track variables
      muIDhits1 = muo->get_muIDhits(idx1);
      muIDquad1 = (muo->get_muID_gap0(0,idx1)>0) + 2*(muo->get_muID_gap0(1,idx1)<0);
      muIDchis1 = muo->get_muIDOOchi(0,idx1); // value for best, or only, road is stored in slot 0
      muTRhits1 = muo->get_muTRhits(idx1);
      chi1 = muo->get_chisquare(idx1);
      Float_t dS31 = Tools::DS3(muo,idx1);
      Float_t dS3ctp1 = Tools::DS3ctp(muo,idx1);
      Float_t dG01 = Tools::DG0(muo,idx1);
      Float_t ddG01 = Tools::DDG0(muo,idx1);

      // dimuon's variables
      diP[0] = P0[0] + P1[0];
      diP[1] = P0[1] + P1[1];
      diP[2] = P0[2] + P1[2];
      dimass = muo->get_dimass(idimu);
      dicharge = muo->get_dicharge(idimu);
      Tools::vtxBP(muo, idx0, idx1, mydca, xvtxbp, yvtxbp, zvtxbp);


      // Fill ntuple
      //      boost::array< Float_t, 53> varNT;
      boost::array< Float_t, 57> varNT;
      varNT[0] = RunNumber; // run number
      varNT[1] = EventNumber; // event number
      varNT[2] = npart; // number of muons
      varNT[3] = BbcZVertex; // BBC Zvertex
      varNT[4] = bbcCentrality; // bbcCentrality
      varNT[5] = fclCentrality; // fclCentrality
      varNT[6] = zdcCentrality; // zdcCentrality
      varNT[7] = ZdcEnergyN; // zdcEnerN;
      varNT[8] = FclGreyN; // fclGreyN;
      varNT[9] = pseudotrigS_2D; // pseudo trigger 2 deeps South
      varNT[10] = pseudotrigS_1D1S; // pseudo trigger 1 deep 1 shallow South
      varNT[11] = pseudotrigN_2D; // pseudo trigger 2 deeps North
      varNT[12] = pseudotrigN_1D1S; // pseudo trigger 1 deep 1 shallow North
      varNT[13] = dimass; // dimuon mass
      varNT[14] = dicharge; // dimuon charge
      varNT[15] = Tools::rapidity(dimass,diP); // dimuon rapidity
      varNT[16] = Tools::pT(diP); // dimuon pT
      varNT[17] = Tools::p(diP); // dimuon p
      varNT[18] = Tools::xF(dimass,diP,E_CMS); // dimuon xF
      varNT[19] = Tools::x1(dimass,diP,E_CMS); // dimuon x1
      varNT[20] = Tools::x2(dimass,diP,E_CMS); // dimuon x2
      varNT[21] = Tools::costhetaCS(Const::MUMASS,P0,P1); // costhetaCS
      varNT[22] = mydca;  // DCA between the 2 tracks (à la Mike)
      varNT[23] = xvtxbp; // Bend plane x vertex (à la Mike)
      varNT[24] = yvtxbp; // Bend plane y vertex (à la Mike)
      varNT[25] = zvtxbp; // Bend plane z vertex (à la Mike)
      varNT[26] = chi0;    // chi2 of 1st muon
      varNT[27] = chi1;    // chi2 of 2nd muon
      varNT[28] = P0[0];  // px of 1st muon
      varNT[29] = P0[1];  // py of 1st muon
      varNT[30] = P0[2];  // pz of 1st muon
      varNT[31] = P1[0];  // px of 2nd muon
      varNT[32] = P1[1];  // py of 2nd muon
      varNT[33] = P1[2];  // pz of 2nd muon
      varNT[34] = muIDhits0; // muid hit pattern of 1st muon
      varNT[35] = muIDhits1; // muid hit pattern of 2nd muon
      varNT[36] = muIDquad0; // muid quadrant at gap0 of 1st muon
      varNT[37] = muIDquad1; // muid quadrant at gap0 of 2nd muon
      varNT[38] = muTRhits0; // mutr hit pattern of 1st muon
      varNT[39] = muTRhits1; // mutr hit pattern of 2nd muon
      varNT[40] = dS30; // DS3 of 1st muon (à la Olivier)
      varNT[41] = dS31; // DS3 of 2nd muon (à la Olivier)
      varNT[42] = dS3ctp0;  // DS3 of 1st muon (w const theta & phi)
      varNT[43] = dS3ctp1;  // DS3 of 2nd muon (w const theta & phi)
      varNT[44] = muo->get_vtx_chisquare(idimu); // Vertex Chi2
      varNT[45] = muo->get_vtx_zpos(idimu); // Vertex Z
      varNT[46] = muIDchis0; // Muid Chi2
      varNT[47] = muIDchis1; // Muid Chi2

      /* new variables from matt/hugo/vi-nham */
      varNT[48] = dG00;   // road to track distance at gap0
      varNT[49] = dG01;   // road to track distance at gap0

      varNT[50] = ddG00; // road to track angular distance
      varNT[51] = ddG01; // road to track angular distance

      // mutoo vertex distance to the beam axis
      varNT[52] =  sqrt(
      MUTOO::SQUARE( muo->get_vtx_xpos(idimu) ) +
      MUTOO::SQUARE( muo->get_vtx_ypos(idimu) ) );


      //spin information
      varNT[53] = SpinX_ID; // corrected beam Xing ID
      varNT[54] = Pol_Y; // corrected beam Xing ID
      varNT[55] = Pol_B; // corrected beam Xing ID
      varNT[56] = GL1X_ID; // corrected beam Xing ID


      accDIMU++;

      dimuons->Fill( &varNT[0] );
    }
  }

  accEVT++;
  return 0;
}

