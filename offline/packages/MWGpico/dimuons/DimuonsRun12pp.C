// $Id: DimuonsRun12pp.C,v 1.2 2017/07/15 04:49:08 phnxbld Exp $
/*!
\file DimuonsRun5.C
\brief dimuons ntuple booking and feeling
\author Frederic Fleuret/Hugo Pereira
\version $Revision: 1.2 $
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
#include <MUTOO.h>
#include <MWGVertex.h>

#include <MUTOO.h>
#include "../MWGpico.h"
#include "Tools.h"
#include "MWGConsts.h"

using namespace std;
using namespace PhUtilities;

static const int default_verbosity = 2;

//_________________________________________________________________
void MWGpico::BookDimuonsNtupleRun12pp( TNtuple*& dimuons, TString name, TString title )
{

  // define variable lists
  /* to make counting of variables easier, please try to write only ten variables/lines */
  const char* varlist =
    "Run_Number:Evt_Number:Evt_Nmu:Evt_bbcCentrality:Evt_bbcZ:Evt_vtxchi2:Evt_vtxoor:Evt_vtxooz:Tr0_DDG0:Tr0_DG0:"
    "Tr0_DS3:Tr0_DS3ctp:Tr0_chi2:Tr0_idchi2:Tr0_idhits:Tr0_idquad:Tr0_px:Tr0_py:Tr0_pz:Tr0_trhits:"
    "Tr0_DG0x:Tr0_DG0y:Tr1_DDG0:Tr1_DG0:Tr1_DS3:Tr1_DS3ctp:Tr1_chi2:Tr1_idchi2:Tr1_idhits:Tr1_idquad:"
    "Tr1_px:Tr1_py:Tr1_pz:Tr1_trhits:Tr1_DG0x:Tr1_DG0y:charge:costhCS:dca:mass:"
    "p:pT:rapidity:x1:x2:xF:xvtxbp:yvtxbp:zvtxbp:SpinX_ID:"
    "Pol_Y:Pol_B:GL1X_ID:Evt_ll1_S2DN2D:Evt_ll1_SN1D:Evt_ll1_SN1H:Mu_TrigBit:Tr0_dAx:Tr1_dAx:zvtx_dimu"; // 60

  //! create the ntuple
  dimuons = new TNtuple( name, title, varlist);

  return;

}

//_________________________________________________________________
void MWGpico::BookDimuonsNtupleRun12ppBackToBack( TNtuple*& dimuonsb2b, TString name, TString title )
{
  // define variable lists
  /* to make counting of variables easier, please try to write only ten variables/lines */
  const char* varlist =
    "Run_Number:Evt_Number:Evt_Nmu:Evt_bbcCentrality:Evt_bbcZ:Evt_vtxchi2:Evt_vtxoor:Evt_vtxooz:x1:x2:"
    "Tr0_eta:Tr0_phi:Tr0_DG0:Tr0_DDG0:Tr0_DS0:Tr0_DS3ctp:Tr0_chi2:Tr0_idchi2:Tr0_idhits:Tr0_trhits:"
    "Tr0_dA:Tr0_px:Tr0_py:Tr0_pz:Tr0_dPhi:Tr0_dTh:Tr0_lastGap:Tr0_rdca:Tr0_vr:Tr0_vz:"
    "Tr1_eta:Tr1_phi:Tr1_DG0:Tr1_DDG0:Tr1_DS0:Tr1_DS3ctp:Tr1_chi2:Tr1_idchi2:Tr1_idhits:Tr1_trhits:"
    "Tr1_dA:Tr1_px:Tr1_py:Tr1_pz:Tr1_dPhi:Tr1_dTh:Tr1_lastGap:Tr1_rdca:Tr1_vr:Tr1_vz:"
    "charge:costhCS:dca:mass:p:pT:rapidity:xF:SpinX_ID:Pol_Y:"
    "Pol_B:GL1X_ID:Evt_ll1_S2DN2D:Evt_ll1_SN1D:Evt_ll1_SN1H:Mu_TrigBit";  // 66

  //! create the ntuple
  dimuonsb2b = new TNtuple( name, title, varlist);

  return;
}

//_________________________________________________________________
int MWGpico::FillDimuonsRun12pp(PHMuoTracksOut* &muo, TNtuple* dimuons)
{

  if( verbosity >= default_verbosity ) cout << "MWGpico::FillDimuonsRun12pp" << endl;

  // Global variables
  Float_t E_CMS=200;

  // event selection
  totDIMU+=muo->get_ndimu();
  if( !Cuts().pass_event_cuts( _top_node ) ) return 1; // see ../PassCuts.C
  accEVT++;

  // z_vertex and centrality
  float BbcZVertex( -9999 );
  Float_t bbcCentrality= -9999;
  if( _choice == "simu" || _choice == "simu_file" )
  {

    bool error( false );
    BbcZVertex = Tools::zVertexMC( header, error );
    if( error ) BbcZVertex = muo->get_zpos(0,0);

  } else if (evt) {

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

  // LL1 decision
  Float_t ll1_2D_SN = 0;
  Float_t ll1_SN1D = 0;
  Float_t ll1_SN1H = 0;
  Float_t ll1_trigbits = 0;

  //change to use trigger bit to select the trigger
  //  cout << "_choice = " << _choice << endl;
  //  cout << "_trig_lvl1 = " << _trig_lvl1 << endl;

  if( !(_choice == "simu" || _choice == "simu_file") )
  {

    if(_trig_lvl1){

      ll1_trigbits = _trig_lvl1->get_lvl1_trigscaled();
      if ( _trig_lvl1->get_lvl1_trigscaled()&0x00001000) ll1_2D_SN = 1;
      if ( _trig_lvl1->get_lvl1_trigscaled()&0x00002000) ll1_SN1H = 1;
      if ( _trig_lvl1->get_lvl1_trigscaled()&0x00004000) ll1_SN1D = 1;
    }
  }

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

  if (muo)
  {

    int ndimu = muo->get_ndimu();
    int npart = muo->get_npart();

    // Loop over dimuons
    for (int idimu=0; idimu<ndimu; idimu++)
    {

      // Remove dimuons with unassociated tracks (negative index)
      if(muo->get_ditrkIndex(0,idimu) < 0 || muo->get_ditrkIndex(1,idimu) < 0 )
      {
        if( verbosity >= default_verbosity ) cout << "MWGpico::FillDimuonsNtuple - wrong track index. Vertex skipped.\n";
        negDIMU++;
        continue;
      }

      // See if dimuon pass dimuon cuts (see ../PassCuts.C)
      if( !Cuts().pass_dimuon_cuts( evt, idimu, muo, _framework ) ) continue;

      // track ids
      Int_t idx0(0);
      Int_t idx1(0);

      // get dimuon's tracks index (for +- pairs the + track will go to first index)
      if (muo->get_vtx_chrg_1(idimu)>0) {

        idx0  = muo->get_ditrkIndex(0,idimu) ;
        idx1  = muo->get_ditrkIndex(1,idimu) ;

      } else {

        idx0  = muo->get_ditrkIndex(1,idimu) ;
        idx1  = muo->get_ditrkIndex(0,idimu) ;

      }

      // redo the vertex fit. This is needed because the original
      // fit was done with wrong BBC z vertex error
      MWGVertex vertex;
      vertex.set_verbosity( 1 );

      // add tracks
      vertex.add_track( idx0, muo );
      vertex.add_track( idx1, muo );

      // add vertex information
      double z_vertex( 0 );
      double z_vertex_error( 0 );

      if( _choice == "simu" || _choice == "simu_file" )
      {

        // retrieve vertex from pisa header file or first track
        if( header )
        {

          bool error( true );
          z_vertex = Tools::zVertexMC( header, error );
          if( error ) z_vertex = muo->get_zpos(0,0);

        } else {

          z_vertex = muo->get_zpos(0,0);
          z_vertex_error = 2;

        }

      } else if( evt ) {

        // real data
        // retrieve vertex from BBC
        z_vertex = evt->getBbcZVertex();

        /*
        up to now, found no way to retrieve bbc vertex error
        from PHGlobal. Assign a 2cm error
        */
        z_vertex_error = 2;

      }

//        cout << "MWGpico::FillDimuonsRun12pp - zvtx: " << z_vertex  << endl;
//      z_vertex = 0;
      vertex.add_vertex( z_vertex, z_vertex_error );

      try {

        // refit muon tracks with the correct event vtx (BBC_vtx for real one and PISA_vtx for sim ) fit
        vertex.fit();

        // some output
//        cout << "MWGpico::FillDimuonsRun12pp - mass: " << vertex.get_mass() << endl;

      } catch( exception &e ) { cout << e.what() << endl; }
//      cout << "MWGpico::FillDimuonsRun12pp - p1: "<< vertex.get_px(0) << "  " << vertex.get_py(0)<< " " << vertex.get_pz(0) << endl;

      // first muon variables
      Float_t P0[3] =
      {
        (float) vertex.get_px( 0 ),
        (float) vertex.get_py( 0 ),
        (float) vertex.get_pz( 0 )
      };

      // first muon variables
      Float_t P1[3] =
      {
        (float) vertex.get_px( 1 ),
        (float) vertex.get_py( 1 ),
        (float) vertex.get_pz( 1 )
      };


      // deflection angle vtx-STI
      Float_t dA0 = Tools::DA(muo,idx0,z_vertex);
      Float_t dA1 = Tools::DA(muo,idx1,z_vertex);

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
      Float_t dG0x1( 0 );
      Float_t dG0y1( 0 );

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

      Float_t dimass = vertex.get_mass();
      Float_t dicharge = ( muo->get_charge(idx0) != muo->get_charge(idx1 ) ) ? 0:muo->get_charge( idx0 );

      // Fill ntuple
      boost::array< Float_t, 60 > varNT;

      varNT[0]  = RunNumber;
      varNT[1] = EventNumber;
      varNT[2] = npart;
      varNT[3] = bbcCentrality;
      varNT[4] = BbcZVertex;
      varNT[5] = vertex.get_chisquare()/vertex.get_ndf();
      varNT[6] = sqrt(

        // mutoo vertex distance to the beam axis
        MUTOO::SQUARE( vertex.get_vtx_x() ) +
        MUTOO::SQUARE( vertex.get_vtx_y() ) );

      varNT[7] = vertex.get_vtx_z(); // Vertex Z

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
      varNT[34] = dG0x1;
      varNT[35] = dG0y1;

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

      //spin information
      varNT[49] = SpinX_ID; // corrected beam Xing ID
      varNT[50] = Pol_Y; // corrected beam Xing ID
      varNT[51] = Pol_B; // corrected beam Xing ID
      varNT[52] = GL1X_ID; // corrected beam Xing ID

      // LL1 1D1S
      varNT[53] = ll1_2D_SN;

      varNT[54] = ll1_SN1D;
      varNT[55] = ll1_SN1H;

      varNT[56] = ll1_trigbits;
      varNT[57] = dA0;
      varNT[58] = dA1;
      varNT[59] = z_vertex;

      accDIMU++;
      dimuons->Fill( &varNT[0] );
    }
  }

  if( verbosity >= default_verbosity ) cout << "MWGpico::FillDimuonsRun12pp - done." << endl;
  return 0;
}

//_________________________________________________________________
int MWGpico::FillDimuonsRun12ppBackToBack(PHMuoTracksOut* &muo, TNtuple* dimuonsb2b)
{

  if( verbosity >= default_verbosity ) cout << "MWGpico::FillDimuonsRun12ppBackToBack" << endl;

  // Global variables
  Float_t E_CMS=200;

  // event selection
  totDIMU+=muo->get_ndimu();
  if( !Cuts().pass_event_cuts( _top_node ) ) return 1; // see ../PassCuts.C
  accEVT_backToBack++;

  // z_vertex and centrality
  float BbcZVertex( -9999 );
  Float_t bbcCentrality= -9999;
  if( _choice == "simu" || _choice == "simu_file" )
  {

    bool error( false );
    BbcZVertex = Tools::zVertexMC( header, error );
    if( error ) BbcZVertex = muo->get_zpos(0,0);

  } else if (evt) {

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

  // LL1 decision
  Float_t ll1_2D_SN = 0;
  Float_t ll1_SN1D = 0;
  Float_t ll1_SN1H = 0;
  Float_t ll1_trigbits = 0;
  
  if(_trig_lvl1){
    
    ll1_trigbits = _trig_lvl1->get_lvl1_trigscaled();
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00001000) ll1_2D_SN = 1;
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00002000) ll1_SN1H = 1;
    if ( _trig_lvl1->get_lvl1_trigscaled()&0x00004000) ll1_SN1D = 1;
  }
  
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
    if( verbosity >= default_verbosity ) cout << "MWGPico::FillDimuonsRun12ppBackToBack - totEVT: " << totEVT << " npart: " << npart << endl;

    // Loop over for the 1st muon
    for( int idx0 = 0; idx0 < npart; idx0 ++ )
    {

      // the 1st muon candidate
      // make sure muon pass cuts
      if (evt && !Cuts().pass_single_muon_cuts(evt,idx0,muo, _framework) ) continue;

      // First track variables
      Int_t muTRhits0   = muo->get_muTRhits(idx0);
      Float_t chi0      = muo->get_chisquare(idx0);

      Int_t muIDhits0(0);
      Float_t muIDchis0(0);

      Float_t dS00(0);
      Float_t dS3ctp0(0);
      Float_t dG00(0);
      Float_t ddG00(0);

      int iroad0 = Cuts().get_best_road_oo( idx0, muo );
      if( iroad0 >= 0 )
      {
        muIDhits0 = muo->get_muIDOOhits( iroad0, idx0);
        muIDchis0 = muo->get_muIDOOchi( iroad0, idx0 );

        dS3ctp0 = Tools::DS3ctp( muo, idx0, iroad0 );
        dS00 = Tools::DS0( muo, idx0, iroad0 );
        dG00 = Tools::DG0( muo, idx0, iroad0 );
        ddG00 = Tools::DDG0( muo, idx0, iroad0 );
      }

      // single muon DCA (?)
      Float_t r_dca0,x_dca0,y_dca0,z_dca0;
      Tools::DCA(muo,idx0,r_dca0,x_dca0,y_dca0,z_dca0);

      Float_t rdca0 = r_dca0;
      Float_t vr0 = sqrt( MUTOO::SQUARE( x_dca0 ) + MUTOO::SQUARE( y_dca0 ) );
      Float_t vz0 = z_dca0;

      Float_t  xSTI0  = muo->get_xpos(1,idx0);
      Float_t  ySTI0  = muo->get_ypos(1,idx0);
      Float_t  zSTI0  = muo->get_zpos(1,idx0);

      Float_t  xSTIII0  = muo->get_xpos(3,idx0);
      Float_t  ySTIII0  = muo->get_ypos(3,idx0);
      Float_t  zSTIII0  = muo->get_zpos(3,idx0);

      Float_t dPhi0 = (atan2(xSTI0,ySTI0)-atan2(xSTIII0,ySTIII0));

      // loop over for the 2nd  muon
      for( int idx1 = idx0+1; idx1 < npart; idx1++ )
      {

        // make sure muon pass cuts
        if (evt && !Cuts().pass_single_muon_cuts(evt,idx1,muo, _framework) ) continue;

        // Second track variables
        Int_t muTRhits1 = muo->get_muTRhits(idx1);
        Float_t chi1 = muo->get_chisquare(idx1);

        Float_t muIDchis1(0);
        Int_t muIDhits1(0);

        Float_t dS01( 0 );
        Float_t dS3ctp1( 0 );
        Float_t dG01( 0 );
        Float_t ddG01( 0 );

        int iroad1 = Cuts().get_best_road_oo( idx1, muo );
        if( iroad1 >= 0 )
        {
          muIDhits1 = muo->get_muIDOOhits( iroad1, idx1);
          muIDchis1 = muo->get_muIDOOchi( iroad1, idx1 );

          dS3ctp1 = Tools::DS3ctp( muo, idx1, iroad1 );
          dS01 = Tools::DS0( muo, idx1, iroad1 );
          dG01 = Tools::DG0( muo, idx1, iroad1 );

          ddG01 = Tools::DDG0( muo, idx1, iroad1 );

        }

        // single muon dca
        Float_t r_dca1,x_dca1,y_dca1,z_dca1;
        Tools::DCA(muo,idx1,r_dca1,x_dca1,y_dca1,z_dca1);

        // dimuon dca
        Float_t dca, x_dca,y_dca,z_dca;
        Tools::dca(muo,idx0,idx1,dca,x_dca,y_dca,z_dca);

        // dimuon vertex fit
        MWGVertex vertex;
        vertex.set_verbosity( 0 );

        // add tracks
        vertex.add_track( idx0, muo );
        vertex.add_track( idx1, muo );

        // add vertex information
        double z_vertex( 0 );
        double z_vertex_error( 0 );

        if( _choice == "simu" || _choice == "simu_file" )
        {

          // retrieve vertex from pisa header file or first track
          if( header )
          {

            bool error( true );
            z_vertex = Tools::zVertexMC( header, error );
            if( error ) z_vertex = muo->get_zpos(0,0);

          } else {

            z_vertex = muo->get_zpos(0,0);
            z_vertex_error = 2;

          }

        } else if( evt ) {

          // real data
          // retrieve vertex from BBC
          z_vertex = evt->getBbcZVertex();

          /*
          up to now, found no way to retrieve bbc vertex error
          from PHGlobal. Assign a 2cm error
          */
          z_vertex_error = 2;

        }

        vertex.add_vertex( z_vertex, z_vertex_error );

        try {

          // refit muon tracks with the correct event vtx (BBC_vtx for real one and PISA_vtx for sim ) fit
          vertex.fit();

        } catch( exception &e ) { cout << e.what() << endl; }

        // first muon variables
        Float_t P0[3] =
        {
          (float) vertex.get_px( 0 ),
          (float) vertex.get_py( 0 ),
          (float) vertex.get_pz( 0 )
        };

        // pseudo rapidity
        Float_t eta0( 0 );
        Float_t p0_tot( sqrt( MUTOO::SQUARE( P0[0] ) + MUTOO::SQUARE( P0[1] ) + MUTOO::SQUARE( P0[2] ) ) );
        if( p0_tot == P0[2] ) { eta0 = -999.999; }
        else { eta0 = 0.5*log( (p0_tot+P0[2])/(p0_tot-P0[2])); }

        // azimuth
        Float_t phi0 = atan2(P0[1], P0[0]);
        Float_t BbcZVertex0( z_vertex );
        Float_t dA0 = Tools::DA(muo,idx0,BbcZVertex0);
        Float_t dTh0 = atan((sqrt( MUTOO::SQUARE(xSTI0)+MUTOO::SQUARE(ySTI0)))/abs(zSTI0-BbcZVertex0))-atan((sqrt( MUTOO::SQUARE(xSTIII0)+MUTOO::SQUARE(ySTIII0)))/abs(zSTIII0-BbcZVertex0));

        Float_t lastGap0( -99 );
        for(int igap=4; igap>0; igap--)
        {

          if (muo->is_muIDOOhit(idx0, igap, 0) || muo->is_muIDOOhit(idx0, igap, 1))
          {
            lastGap0 = igap;
            break;
          }
        }

        // second muon variables
        Float_t P1[3] = {
          (float) vertex.get_px( 1 ),
          (float) vertex.get_py( 1 ),
          (float) vertex.get_pz( 1 )
        };

        // pseudo rapidity
        Float_t eta1( 0 );
        Float_t p1_tot( sqrt( MUTOO::SQUARE( P1[0] ) + MUTOO::SQUARE( P1[1] ) + MUTOO::SQUARE( P1[2] )) );
        if( p1_tot == P1[2] ) { eta1 = -999.999; }
        else { eta1 = 0.5*log( (p1_tot+P1[2])/(p1_tot-P1[2])); }

        // azimuth
        Float_t phi1 = atan2(P1[1], P1[0]);
        Double_t BbcZVertex1( z_vertex );
        Float_t dA1 = Tools::DA(muo,idx1, BbcZVertex1);
        Float_t rdca1 = r_dca1;

        Float_t vr1 = sqrt( MUTOO::SQUARE( x_dca1 ) + MUTOO::SQUARE( y_dca1 ) );
        Float_t vz1 = z_dca1;

        Float_t  xSTI1  = muo->get_xpos(1,idx1);
        Float_t  ySTI1  = muo->get_ypos(1,idx1);
        Float_t  zSTI1  = muo->get_zpos(1,idx1);

        Float_t  xSTIII1  = muo->get_xpos(3,idx1);
        Float_t  ySTIII1  = muo->get_ypos(3,idx1);
        Float_t  zSTIII1  = muo->get_zpos(3,idx1);

        Float_t dPhi1 = (atan2(xSTI1,ySTI1)-atan2(xSTIII1,ySTIII1));
        Float_t dTh1 = atan((sqrt(MUTOO::SQUARE(xSTI1)+MUTOO::SQUARE(ySTI1)))/abs(zSTI1-BbcZVertex1))-atan((sqrt(MUTOO::SQUARE(xSTIII1)+MUTOO::SQUARE(ySTIII1)))/abs(zSTIII1-BbcZVertex1));

        // this does not work with too recent version of PHMuoTracks
        Float_t lastGap1( -99 );
        for(int igap=4; igap>0; igap--)
        {
          if (muo->is_muIDOOhit(idx1, igap, 0) || muo->is_muIDOOhit(idx1, igap, 1))
          {
            lastGap1 = igap;
            break;
          }
        }

        // dimuon's variables
        Float_t diP[3] = {
          P0[0] + P1[0],
          P0[1] + P1[1],
          P0[2] + P1[2]
        };

        Float_t dimass = vertex.get_mass();
        Float_t dicharge = ( muo->get_charge(idx0) != muo->get_charge(idx1 ) ) ? 0:muo->get_charge( idx0 );

        // Fill ntuple
        boost::array< Float_t, 66 > varNT;

        //== run
        varNT[0]  = RunNumber; // run number

        //== event
        varNT[1] = EventNumber; // event number
        varNT[2] = npart; // number of muons
        varNT[3] = bbcCentrality; // bbcCentrality
        varNT[4] = BbcZVertex; // BBC Zvertex
        varNT[5] = vertex.get_chisquare()/vertex.get_ndf(); // Vertex Chi2
        varNT[6] = sqrt(
          MUTOO::SQUARE( vertex.get_vtx_x() ) +
          MUTOO::SQUARE( vertex.get_vtx_y() ) );

        // mutoo vertex distance to the beam axis
        varNT[7] = vertex.get_vtx_z(); // Vertex Z
        varNT[8] = Tools::x1(dimass,diP,E_CMS); // dimuon x1
        varNT[9] = Tools::x2(dimass,diP,E_CMS); // dimuon x2

        //== first muon
        varNT[10] = eta0;  // road to track angular distance
        varNT[11] = phi0;   // road to track distance at gap0
        varNT[12] = dG00;  // road to track angular distance
        varNT[13] = ddG00;   // road to track distance at gap0
        varNT[14] = dS00; // DS3 of 1st muon
        varNT[15] = dS3ctp0; // DS3 of 1st muon (w const theta & phi)
        varNT[16] = chi0; // chi2 of 1st muon
        varNT[17] = muIDchis0; // Muid Chi2
        varNT[18] = muIDhits0; // muid hit pattern of 1st muon
        varNT[19] = muTRhits0; // mutr hit pattern of 1st muon

        varNT[20] = dA0; // deflection angle of the 1st muon
        varNT[21] = P0[0];  // px of 1st muon
        varNT[22] = P0[1];  // py of 1st muon
        varNT[23] = P0[2];  // pz of 1st muon
        varNT[24] = dPhi0;  //St1-St3 deflection angle
        varNT[25] = dTh0;   //St1-St3 deflection angle
        varNT[26] = lastGap0;
        varNT[27] = rdca0;
        varNT[28] = vr0;
        varNT[29] = vz0;

        //== second muon
        varNT[30] = eta1;  // road to track angular distance
        varNT[31] = phi1;   // road to track distance at gap0
        varNT[32] = dG01;  // road to track angular distance
        varNT[33] = ddG01;   // road to track distance at gap0
        varNT[34] = dS01; // DS3 of 1st muon
        varNT[35] = dS3ctp1; // DS3 of 1st muon (w const theta & phi)
        varNT[36] = chi1; // chi2 of 1st muon
        varNT[37] = muIDchis1; // Muid Chi2
        varNT[38] = muIDhits1; // muid hit pattern of 1st muon
        varNT[39] = muTRhits1; // mutr hit pattern of 1st muon

        varNT[40] = dA1; // deflection angle of the 1st muon
        varNT[41] = P1[0];  // px of 1st muon
        varNT[42] = P1[1];  // py of 1st muon
        varNT[43] = P1[2];  // pz of 1st muon
        varNT[44] = dPhi1;  //St1-St3 deflection angle
        varNT[45] = dTh1;   //St1-St3 deflection angle
        varNT[46] = lastGap1;
        varNT[47] = rdca1;
        varNT[48] = vr1;
        varNT[49] = vz1;

        //== dimuon
        varNT[50] = dicharge; // dimuon charge
        varNT[51] = Tools::costhetaCS(Const::MUMASS,P0,P1); // costhetaCS
        varNT[52] = dca;
        varNT[53] = dimass; // dimuon mass
        varNT[54] = Tools::p(diP);   // dimuon p
        varNT[55] = Tools::pT(diP);   // dimuon pT
        varNT[56] = Tools::rapidity(dimass,diP); // dimuon rapidity
        varNT[57] = Tools::xF(dimass,diP,E_CMS); // dimuon xF
        varNT[58] = SpinX_ID;       // corrected beam Xing ID
        varNT[59] = Pol_Y;          // corrected beam Xing ID

        varNT[60] = Pol_B;          // corrected beam Xing ID
        varNT[61] = GL1X_ID;        // corrected beam Xing ID
	varNT[62] = ll1_2D_SN;
	
	varNT[63] = ll1_SN1D;
	varNT[64] = ll1_SN1H;
	varNT[65] = ll1_trigbits;

        accDIMU++;
        dimuonsb2b->Fill( &varNT[0] );

      }
    }

  }

  if( verbosity >= default_verbosity ) cout << "MWGpico::FillDimuonsRun12ppBackToBack - done." << endl;
  return 0;
}
