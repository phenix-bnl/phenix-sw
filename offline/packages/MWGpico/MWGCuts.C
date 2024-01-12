// $Id: MWGCuts.C,v 1.26 2014/12/09 06:29:05 rseidl Exp $

/*!
\file		MWGCuts.C
\brief	 handle cuts on nanoDSTs to fill picoDSTs
\author	Hugo Pereira
\version $Revision: 1.26 $
\date		$Date: 2014/12/09 06:29:05 $
*/

#include "Tools.h"
#include "PHGlobal.h"
#include "PHMuoTracksOut.h"
#include "MUTOO.h"
#include <MWGpico.h>
#include "utiCentrality.h"
#include "getClass.h"

#include <string>
#include <iostream>

using namespace std;

ClassImp( MWGCuts )

//______________________________________________________________
MWGCuts::MWGCuts( void )
{

  /*
  in principle, all cuts which are set to false do not need to be mentioned
  they are kept here for local customization
  */

  // cut on bbc z
  set_do_cut( BBC_Z, false );
  set_bbc_z_cut( 40.0 );

  // check bbc and zdc
  set_do_cut(BBCZDC, false);

  // cut on centrality : true if CentralityMin < centrality <= CentralityMax
  set_do_cut( CENTRALITY, false );
  set_centrality_cut( -1, 100 );

  // cut on BBC vs Muid multiplicity (so called strange events)
  set_do_cut( MUID_BBC, false );

  // accept if roads from dimu correspond to different quadrants
  set_do_cut( IDQUAD, false );

  // cut on vertex mass
  set_do_cut( DIMU_MASS, false );
  set_dimu_mass_cut( 1.5 );

  // cut on vertex matching distance (old bend plane - bbc)
  set_do_cut( DIMU_VTX, false );
  set_dimu_vtx_cut( 25 );

  // cut on vertex chi2
  set_do_cut( VTX_CHISQUARE, false );
  set_vtx_chisquare_cut( 10 );

  // require that dimuon rapidity matches south arm
  set_do_cut( Y_DIMU_SOUTH, false );
  set_y_dimu_south_cut( -2.2, -1.2 );

  // require that dimuon rapidity matches north arm
  set_do_cut( Y_DIMU_NORTH, false );
  set_y_dimu_north_cut( 1.2, 2.4 );

  // requires that dimuon arm fired the level2 trigger
  set_do_cut( LEVEL2, false );

  // requires that offline reconstructed dimuon fullfill the level2 requirement
  set_do_cut( LEVEL2_DIMU, false );

  // ghost tracks rejection
  set_do_cut( GHOST_FLAG, true );

  // track chisquare/ndf cut
  set_do_cut( TRK_CHISQUARE, false );
  set_trk_chisquare_cut( 50 );

  // See if dimuon goes to north or south
  set_do_cut( TRK_PZ_NORTH, false );
  set_do_cut( TRK_PZ_SOUTH, false );

  // Minimum number of hits required in MuTr >= nmutrhitsCut
  set_do_cut( MUTR_HITS, false );
  set_mutr_hits_cut( 10 );

  // Distance of approach between track and beam
  set_do_cut( MUTR_RV, false );
  set_mutr_rv_cut( 5 );

  // one of the two gap fully hit
  set_do_cut( ST3_HITS, false );

  // Minimum number of hits required in MuId >= nmuidhitsCut
  set_do_cut( MUID_HITS, false );
  set_muid_hits_cut( 6 );

  // Cut on muid gapbit. MuidDepth >= 64 means at least Gap3 (from 0) is reached
  set_do_cut( MUID_DEPTH, false );
  set_muid_depth_cut( 64 );

  // check if missing tubes have a different orientation.
  set_do_cut( MUID_TUBES, false );

  // check that the missing tubes are not in the same plane (checking plane 0, 1 or 2 only).
  set_do_cut( MUID_CONT, false );

  // Distance of approach between road and beam
  set_do_cut( MUID_RV, false );
  set_muid_rv_cut( 150 );

  // pass if fabs(zdcaRoad-evt->getBbcZVertex())<zdcaRoadCut
  set_do_cut( MUID_DCA, false );
  set_muid_dca_cut( 350 );

  // distance from road to track at gap 0
  set_do_cut( DG0, false );
  set_dg0_cut( 60 );

  // distance from road to track at gap 0
  set_do_cut( DDG0, false );
  set_ddg0_cut( 20 );

  // distance from road to track at station3
  set_do_cut( DS3, false );
  set_ds3_cut( 60 );

  // distance froim road to track assuming they come from 0,0,0
  set_do_cut( DS3_CTP, false );
  set_ds3_ctp_cut( 25 );

}

//______________________________________________________________
void MWGCuts::init( const char* selection )
{

  MUTOO::PRINT( cout, "MWGCuts::init" );
  cout << "possible predefined set of cuts are:" << endl;
  cout << " nocuts" << endl;
  cout << " south_arm - to select south arm muons/dimuons only" << endl;
  cout << " north_arm - to select north arm muons/dimuons only" << endl;
  cout << " dAu03N2D, dAu03NDS, dAu03S2D, dAu03SDS - for run3 dAu analysis" << endl;
  cout << " CuAu12loose or CuAu12tight - for run12 CuAu analysis" << endl;

  if( !selection ) return;

  string selection_string( selection );

  //	 // reset all cuts
  //	 reset();

  if( selection_string == "nocuts" ) return;
  if( selection_string == "south_arm" )
  {

    // add cut on pz to select south arm only
    cout << "MWGCuts::init - selecting south arm dimuons only.\n";
    set_do_cut( TRK_PZ_SOUTH, true );
    set_do_cut( Y_DIMU_SOUTH, true );
    _y_dimu_south_cut[0]=-999;
    _y_dimu_south_cut[1]=0;

  }

  if( selection_string == "north_arm" )
  {

    // add cut on pz to select north arm only
    cout << "MWGCuts::init - selecting north arm dimuons only.\n";
    set_do_cut( TRK_PZ_NORTH, true );
    set_do_cut( Y_DIMU_NORTH, true );
    _y_dimu_north_cut[0]=0;
    _y_dimu_north_cut[1]=999;

  }

  // define standard cuts for dAu analysis
  if(
    selection_string == "dAu03N2D" ||
    selection_string == "dAu03NDS" ||
    selection_string == "dAu03S2D" ||
    selection_string == "dAu03SDS" )
  {
    set_do_cut( BBC_Z, true );
    _bbc_z_cut = 38;

    set_do_cut( DIMU_VTX, true );
    _dimu_vtx_cut = 25;

    set_do_cut( TRK_CHISQUARE, true );
    _trk_chisquare_cut = 20;

    if ( selection_string == "dAu03S2D" || selection_string == "dAu03SDS" )
    {

      // add cut on pz and rapidity to select south arm only
      set_do_cut( TRK_PZ_SOUTH, true );
      set_do_cut( Y_DIMU_SOUTH, true );
      _y_dimu_south_cut[0]=-2.2;
      _y_dimu_south_cut[1]=-1.2;

    }

    if( selection_string == "dAu03N2D" || selection_string == "dAu03NDS" )
    {

      // add cut on pz and rapidity to select north arm only
      set_do_cut( TRK_PZ_NORTH, true );
      set_do_cut( Y_DIMU_NORTH, true );
      _y_dimu_north_cut[0]=1.2;
      _y_dimu_north_cut[1]=2.4;

    }

  }

  if(selection_string == "CuAu12loose")
    {
      set_do_cut( BBC_Z, true );
      _bbc_z_cut = 30;
      
      /*THIS IS A LOW MASS CUT, NOT A HIGH MASS CUT (i.e. reject <_dimu_mass_cut)
	set_do_cut( DIMU_MASS, true );
	_dimu_mass_cut = 20;
      */
	
      set_do_cut( TRK_CHISQUARE, true );
      _trk_chisquare_cut = 20;
      
      /*
	set_do_cut( Y_DIMU_SOUTH, true );
	_y_dimu_south_cut[0]=-2.2;
	_y_dimu_south_cut[1]=-1.2;
      */
      
      set_do_cut( DG0, true );
      set_dg0_cut( 40 );

      set_do_cut( DDG0, true );
      set_ddg0_cut( 20 );

      set_do_cut( MUID_DEPTH, true );
      //set_muid_depth_cut( 64 );//at least gap 3 
      set_muid_depth_cut( 280 );//at least gap 4 

      set_do_cut( MUID_HITS, true );
      set_muid_hits_cut( 7 );//at least 6 hits (>=6)

      set_do_cut( VTX_CHISQUARE, true );
      set_vtx_chisquare_cut( 10 );

    }

}

//___________________________________________________
void MWGCuts::print() const
{
  MUTOO::PRINT( cout, "MWGCuts::print" );

  cout << "Event cuts are : " << endl;
  if ( do_cut( BBC_Z ) ) cout << " BBCzCut selection : BBCzCut = " << _bbc_z_cut << endl;
  if ( do_cut( MUID_BBC ) ) cout << " Centrality vs Muid multiplicity selection" << endl;
  if ( do_cut( BBCZDC ) ) cout << " Check the BBC and the ZDC" << endl;
  if ( do_cut( CENTRALITY ) ) cout << " Centrality selection min = " << _centrality_cut[0] << " max = " << _centrality_cut[1] << endl;
  cout << endl;

  cout << "Dimuon cuts are : " << endl;
  if ( do_cut( IDQUAD ) ) cout << " IDquad selection" << endl;
  if ( do_cut( DIMU_VTX ) ) cout << " VTXdimu selection : abs(vtx-zvtxbp)<" << _dimu_vtx_cut << endl;
  if ( do_cut( DIMU_MASS ) ) cout << " VTXmass selection : mass>=" << _dimu_mass_cut << " GeV" << endl;
  if ( do_cut( VTX_CHISQUARE ) ) cout << " VTXchi selection : chi2 <" << _vtx_chisquare_cut << endl;
  if ( do_cut( Y_DIMU_SOUTH ) ) cout << " YdimuSouth selection : " << _y_dimu_south_cut[0] << " < YdimuSouth < " << _y_dimu_south_cut[1] << endl;
  if ( do_cut( Y_DIMU_NORTH ) ) cout << " YdimuNorth selection : " << _y_dimu_north_cut[0] << " < YdimuNorth < " << _y_dimu_north_cut[1] << endl;
  if ( do_cut( LEVEL2 ) ) cout << " Level2 selection (at least one fast reconstructed pair/arm)" << endl;
  if ( do_cut( LEVEL2_DIMU ) ) cout << " Level2 selection (for each offline reconstructed pair)." << endl;
  if ( do_cut( LEVEL2_PRIMITIVES ) ) cout << " Level2 selection (using association to level2 primitives)." << endl;
  cout << endl;

  //
  cout << "Muon cuts are : " << endl;
  if ( do_cut( GHOST_FLAG ) ) cout << " Ghostflag selection" << endl;
  if ( do_cut( TRK_CHISQUARE ) ) cout << " Chisquare selection : chisquare<" << _trk_chisquare_cut	<< endl;
  if ( do_cut( TRK_PZ_SOUTH ) )	 cout << " Pz south selection" << endl;
  if ( do_cut( TRK_PZ_NORTH ) )	 cout << " Pz north selection" << endl;
  if ( do_cut( MUTR_HITS ) ) cout << " Nmutrhits selection : nmutrhits>=" << _mutr_hits_cut << endl;
  if ( do_cut( ST3_HITS ) ) cout << " Sta3gap selection" << endl;
  if ( do_cut( MUTR_RV ) ) cout << " rvTrack selection	 : rvTrack<" << _mutr_rv_cut << endl;
  if ( do_cut( MUID_HITS ) ) cout << " Nmuidhits selection : nmuidhits>= " << _muid_hits_cut << endl;
  if ( do_cut( MUID_DEPTH ) ) cout << " MuidDepth selection : muiddepth>= " << _muid_depth_cut << endl;
  if ( do_cut( MUID_TUBES ) ) cout << " CheckTube selection" << endl;
  if ( do_cut( MUID_CONT ) ) cout << " CheckCont selection" << endl;
  if ( do_cut( MUID_RV ) ) cout << " rvRoad selection : rvRoad<" << _muid_rv_cut << endl;
  if ( do_cut( MUID_DCA ) ) cout << " zdcaRoad selection : zdcaRoad<" << _muid_dca_cut << endl;
  if ( do_cut( DG0 ) ) cout << " DG0 selection : DG0<" << _dg0_cut << endl;
  if ( do_cut( DDG0 ) ) cout << " DDG0 selection : DDG0<" << _ddg0_cut << endl;
  if ( do_cut( DS3 ) ) cout << " DS3 selection : DS3<" << _ds3_cut << endl;
  if ( do_cut( DS3_CTP ) ) cout << " DS3ctp selection : DS3ctp<" << _ds3_ctp_cut << endl;
  if ( do_cut( LEVEL2_PRIMITIVES ) ) cout << " Level2 selection (using association to level2 primitives)." << endl;
  cout << endl;

  MUTOO::PRINT( cout, "**" );
}

//______________________________________________________________
bool MWGCuts::pass_event_cuts( PHCompositeNode *topNode ) const
{
  PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  // check pointer
  if( !global ) return true;

  if( do_cut( BBC_Z ) && !bbc_z_ok( global ) ) return false;
  if( do_cut( CENTRALITY ) && !centrality_ok( topNode ) ) return false;
  if( do_cut( BBCZDC ) && !bbczdc_ok( global) ) return false;
  if( do_cut( MUID_BBC ) && !muid_bbc_ok( global ) ) return false;
  return true;
}

//______________________________________________________________
bool MWGCuts::pass_dimuon_cuts( PHGlobal* global, int idimu, PHMuoTracksOut* muo, const int& framework ) const
{

  if( do_cut( DIMU_VTX ) && !dimu_vtx_ok( global, idimu, muo ) ) return false;
  if( do_cut( DIMU_MASS ) && !dimu_mass_ok(idimu, muo)) return false;
  if( do_cut( VTX_CHISQUARE ) && !vtx_chisquare_ok( idimu, muo ) ) return false;
  if( do_cut( IDQUAD ) && !idquad_ok(idimu,muo)) return false;
  if( do_cut( Y_DIMU_NORTH ) && !y_dimu_north_ok(idimu,muo, framework)) return false;
  if( do_cut( Y_DIMU_SOUTH ) && !y_dimu_south_ok(idimu,muo, framework )) return false;
  if( do_cut( LEVEL2 ) && !level2_ok( idimu,muo, framework ) ) return false;
  if( do_cut( LEVEL2_DIMU ) && !Tools::L2MuID_dimuOK(idimu,muo, MWGpico::RUN4 ) ) return false;
  if( do_cut( LEVEL2_PRIMITIVES ) && !Tools::L2MuidPairOK( idimu, muo ) ) return false;
  return true;
}

//______________________________________________________________
bool MWGCuts::pass_single_muon_cuts( PHGlobal* global, int imu, PHMuoTracksOut* muo, const int& framework ) const
{

  // Sanity check
  if( imu<0 ) return false;

  // Cuts on tracker info
  if( do_cut( GHOST_FLAG ) && !ghost_flag_ok(imu,muo)) return false;
  if( do_cut( TRK_CHISQUARE ) && !trk_chisquare_ok(imu,muo)) return false;
  if( do_cut( TRK_PZ_SOUTH ) && !(muo->get_pz(0,imu)<0)) return false;
  if( do_cut( TRK_PZ_NORTH ) && !(muo->get_pz(0,imu)>0)) return false;
  if( do_cut( MUTR_HITS ) && !mutr_hits_ok(imu,muo)) return false;
  if( do_cut( ST3_HITS ) && !st3_hits_ok(imu,muo)) return false;
  if( do_cut( MUTR_RV ) && !mutr_rv_ok(global,imu,muo)) return false;

  // Cuts on muID info [old framework version]
  if( framework == MWGpico::MUT) {

    if( do_cut( MUID_HITS ) && !muid_hits_ok(imu,muo)) return false;
    if( do_cut( MUID_DEPTH ) && !muid_depth_ok(imu,muo)) return false;
    if( do_cut( MUID_TUBES ) && !muid_tubes_ok(imu,muo)) return false;
    if( do_cut( MUID_CONT ) && !muid_cont_ok(imu,muo)) return false;
    if( do_cut( MUID_RV ) && !muid_rv_ok(global,imu,muo)) return false;
    if( do_cut( MUID_DCA ) && !muid_dca_ok(global,imu,muo)) return false;
    if( do_cut( DG0 ) && !dg0_ok(imu,muo)) return false;
    if( do_cut( DDG0 ) && !ddg0_ok(imu,muo)) return false;
    if( do_cut( DS3 ) && !ds3_ok(imu,muo)) return false;
    if( do_cut( DS3_CTP ) && !ds3_ctp_ok(imu,muo)) return false;
    if( do_cut( LEVEL2_PRIMITIVES ) ) cout << "MWGCuts::pass_single_muon_cuts - LEVEL2_PRIMITIVES not implemented for old framework" << endl;
  }

  /*
  Cuts on muID info [new framework version]
  get_best_road_oo returns the index of the first encountered
  road candidate which passes all cuts; -1 if none is found
  */
  else if( get_best_road_oo( imu, muo ) < 0 )
  {
    //cout << "MWGCuts::pass_single_muon_cuts - no associated road found. Track rejected." << endl;
    return false;
  }

  return true;

}

//______________________________________________________________
void MWGCuts::set_do_cut( const MWGCuts::Cut& cut, bool value )
{

  if( value ) _cuts.insert( cut );
  else _cuts.erase( cut );

}

//__________________________________________________________
int MWGCuts::get_best_road_oo( int imu, PHMuoTracksOut* muo) const
{

  // keep track of the "best" road
  //  int best_road( -1 );
  double best_dg0( -1 );

  // keep track of the "best" accepted road
  int best_accepted_road( -1 );
  double best_accepted_dg0( -1 );

  for( int i_road=0; i_road<3; i_road++ )
  {

    // check if road is present
    if(	!muo->get_muIDOOhits( i_road, imu ) ) continue;

    // retrieve track DG0
    double dg0( Tools::DG0( muo, imu, i_road ) );

    /*
    check if road DG0 is smaller than others
    it is done for debugging only. The 'true' check
    must be done only for accepted roads
    */
    if( best_dg0 < 0 || dg0 < best_dg0 )
    {
      best_dg0 = dg0;
      //      best_road = i_road;
    }

    // apply cuts
    if( do_cut( MUID_TUBES ) ) cout << "MWGCuts::pass_event_cuts - MUID_RV not implemented for new framework" << endl;
    if( do_cut( MUID_CONT ) ) cout << "MWGCuts::pass_event_cuts - MUID_RV not implemented for new framework" << endl;
    if( do_cut( MUID_RV ) )	 cout << "MWGCuts::pass_event_cuts - MUID_RV not implemented for new framework" << endl;
    if( do_cut( MUID_DCA ) ) cout << "MWGCuts::pass_event_cuts - MUID_DCA not implemented for new framework" << endl;
    if( do_cut( MUID_DEPTH ) && !muid_depth_oo_ok(imu, i_road, muo)) continue;
    if( do_cut( MUID_HITS ) && !muid_hits_oo_ok(imu, i_road, muo)) continue;
    if( do_cut( DG0 ) && !dg0_oo_ok(imu, i_road, muo)) continue;
    if( do_cut( DDG0 ) && !ddg0_oo_ok(imu, i_road, muo)) continue;
    if( do_cut( DS3 ) && !ds3_oo_ok(imu, i_road, muo)) continue;
    if( do_cut( DS3_CTP ) && !ds3_ctp_oo_ok(imu, i_road, muo)) continue;
    if( do_cut( LEVEL2_PRIMITIVES ) && !Tools::L2MuidRoadOK( imu, i_road, muo ) ) continue;

    /*
    if candidate passes all cuts
    compare it's DG0 to existing canditate if any
    keeps it if better
    */
    if( best_accepted_dg0<0 || dg0 < best_accepted_dg0 )
    {
      best_accepted_road = i_road;
      best_accepted_dg0 = dg0;
    }
  }

  // return the accepted road which have the smaller DG0
  return best_accepted_road;

}

//______________________________________________________________
bool MWGCuts::bbc_z_ok( PHGlobal* global ) const
{

  double bbc_z( global->getBbcZVertex() );
  return( fabs(bbc_z)< _bbc_z_cut );

}

//________________________________________________________
bool MWGCuts::bbczdc_ok( PHGlobal* global ) const
{
  if( !global ) return true;

  if (global->getBbcChargeS()>0 && global->getBbcChargeN()>0 && global->getZdcEnergyN()>0 && global->getZdcEnergyS()>0) return true;

  cout << "bbcs " << global->getBbcChargeS() << " bbcn " << global->getBbcChargeN() << " zdcn " << global->getZdcEnergyN() << " zdcs " << global->getZdcEnergyS() << endl;

  return false;

}//________________________________________________________
bool MWGCuts::centrality_ok( PHCompositeNode *topNode ) const
{

  float bbcCentralityByClock = (Float_t) PhUtilities::getCentralityByClockRun4(topNode);

  return( bbcCentralityByClock > _centrality_cut[0] && bbcCentralityByClock <= _centrality_cut[1] );
  return false;

}
//________________________________________________________
bool MWGCuts::muid_bbc_ok( PHGlobal* global ) const
{

  // check global
  if( !global ) return true;

  // hardwired cuts
  static const double min_bbc_charge( 20 );
  static const double slope( 10 );

  // retrieve BBC charge
  double bbc_charge = global->getBbcChargeS() + global->getBbcChargeN();

  // check bbc_charge is large enough
  if( bbc_charge > min_bbc_charge ) return true;

  // number of muid hits
  int n_mui_hits = 0;
  for (int arm=0; arm<2; arm++)
    for (int plane=0; plane<5; plane++)
    n_mui_hits += global->get_nMuidHits( arm, plane );

  //! check n_mui_hits is small enough wrt bbc charge
  if( n_mui_hits < slope*bbc_charge ) return true;

  // returns false by default
  return false;

}

//________________________________________________________
bool MWGCuts::idquad_ok(int idimu, PHMuoTracksOut* muo) const
{
  cout << "MWGCuts::idquad_ok" << endl;
  int idx0 = muo->get_ditrkIndex(0,idimu);
  int idx1 = muo->get_ditrkIndex(1,idimu);
  Float_t muIDquad0 = (muo->get_muID_gap0(0,idx0)>0) + 2*(muo->get_muID_gap0(1,idx0)<0);
  Float_t muIDquad1 = (muo->get_muID_gap0(0,idx1)>0) + 2*(muo->get_muID_gap0(1,idx1)<0);
  if (muIDquad0==muIDquad1) return false;
  return true;
}

//________________________________________________________
bool MWGCuts::dimu_vtx_ok(PHGlobal* global, int idimu, PHMuoTracksOut* muo) const
{
  int idx0 = muo->get_ditrkIndex(0,idimu);
  int idx1 = muo->get_ditrkIndex(1,idimu);
  Float_t mydca, xvtxbp, yvtxbp, zvtxbp;
  Tools::vtxBP(muo, idx0, idx1, mydca, xvtxbp, yvtxbp, zvtxbp);
  return (fabs(global->getBbcZVertex()-zvtxbp) < _dimu_vtx_cut );
}

//________________________________________________________
bool MWGCuts::dimu_mass_ok( int idimu, PHMuoTracksOut* muo ) const
{ return muo->get_dimass(idimu) >= _dimu_mass_cut; }

//________________________________________________________
bool MWGCuts::vtx_chisquare_ok( int idimu, PHMuoTracksOut* muo ) const
{ return muo->get_vtx_chisquare(idimu) < _vtx_chisquare_cut; }

//________________________________________________________
bool MWGCuts::y_dimu_south_ok(int idimu, PHMuoTracksOut* muo, const int& framework) const
{
  Float_t diP[3]={0,0,0};

  if( framework == MWGpico::MUTOO) {
    diP[0] = muo->get_vtx_px_1(idimu) + muo->get_vtx_px_2(idimu);
    diP[1] = muo->get_vtx_py_1(idimu) + muo->get_vtx_py_2(idimu);
    diP[2] = muo->get_vtx_pz_1(idimu) + muo->get_vtx_pz_2(idimu);
  } else if( framework == MWGpico::MUT) {
    int idx0 = muo->get_ditrkIndex(0,idimu);
    int idx1 = muo->get_ditrkIndex(1,idimu);
    diP[0] = muo->get_px(0,idx0) + muo->get_px(0,idx1);
    diP[1] = muo->get_py(0,idx0) + muo->get_py(0,idx1);
    diP[2] = muo->get_pz(0,idx0) + muo->get_pz(0,idx1);
  }

  Float_t dimass=muo->get_dimass(idimu);
  return ( Tools::rapidity(dimass,diP)>_y_dimu_south_cut[0] && Tools::rapidity(dimass,diP)<_y_dimu_south_cut[1] );
}

//________________________________________________________
bool MWGCuts::y_dimu_north_ok(int idimu, PHMuoTracksOut* muo, const int& framework) const
{
  Float_t diP[3]={0,0,0};
  if( framework == MWGpico::MUTOO ) {
    diP[0] = muo->get_vtx_px_1(idimu) + muo->get_vtx_px_2(idimu);
    diP[1] = muo->get_vtx_py_1(idimu) + muo->get_vtx_py_2(idimu);
    diP[2] = muo->get_vtx_pz_1(idimu) + muo->get_vtx_pz_2(idimu);
  }
  else if( framework == MWGpico::MUT ) {
    int idx0 = muo->get_ditrkIndex(0,idimu);
    int idx1 = muo->get_ditrkIndex(1,idimu);
    diP[0] = muo->get_px(0,idx0) + muo->get_px(0,idx1);
    diP[1] = muo->get_py(0,idx0) + muo->get_py(0,idx1);
    diP[2] = muo->get_pz(0,idx0) + muo->get_pz(0,idx1);
  }

  Float_t dimass=muo->get_dimass(idimu);
  return ( Tools::rapidity(dimass,diP) > _y_dimu_north_cut[0] && Tools::rapidity(dimass,diP) < _y_dimu_north_cut[1] );

}

//________________________________________________________
bool MWGCuts::level2_ok( int idimu, PHMuoTracksOut* muo, const int& framework ) const
{

  // retrieve dimuon pz
  double pz( 0 );
  if( framework == MWGpico::MUTOO ) {
    pz = muo->get_vtx_pz_1(idimu) + muo->get_vtx_pz_2(idimu);
  } else if( framework == MWGpico::MUT ) {
    int idx0 = muo->get_ditrkIndex(0,idimu);
    int idx1 = muo->get_ditrkIndex(1,idimu);
    pz = muo->get_pz(0,idx0) + muo->get_pz(0,idx1);
  } else return true;

  // we get the mass from dimuon pz sign
  unsigned int arm = pz < 0 ? 0:1;
  return Tools::L2MuidDecision( arm ) || Tools::L2MutrDecision( arm );

}

//________________________________________________________
bool MWGCuts::ghost_flag_ok(int imu, PHMuoTracksOut* muo) const
{
  if( muo->get_ghostflag(imu) )
  { cout << "MWGCuts::ghost_flag_ok - cut failed." << endl; }

  return (!muo->get_ghostflag(imu));
}

//________________________________________________________
bool MWGCuts::trk_chisquare_ok(int imu, PHMuoTracksOut* muo) const
{ return (muo->get_chisquare(imu)< _trk_chisquare_cut ); }

//________________________________________________________
bool MWGCuts::mutr_hits_ok(int imu, PHMuoTracksOut* muo) const
{ return (muo->get_nhits(imu)>= _mutr_hits_cut ); }

//________________________________________________________
bool MWGCuts::st3_hits_ok(int imu, PHMuoTracksOut* muo) const
{
  int mutrhits = muo->get_muTRhits(imu);
  return ((mutrhits&12288)==12288 || (mutrhits&49152)==49152);
}

//________________________________________________________
bool MWGCuts::mutr_rv_ok(PHGlobal* global, int imu, PHMuoTracksOut* muo) const
{
  float refPos[3]={0,0,0};
  float refDir[2]={0,0};
  float zeroPos[3]={0,0,0};
  float zeroDir[2]={0,0};
  float xdca1[3]={0,0,0};
  float xdca2[3]={0,0,0};
  int ifail=0; // output

  refPos[0] = muo->get_xpos(0,imu);
  refPos[1] = muo->get_ypos(0,imu);
  refPos[2] = muo->get_zpos(0,imu);

  refDir[0] = muo->get_px(0,imu)/muo->get_pz(0,imu);
  refDir[1] = muo->get_py(0,imu)/muo->get_pz(0,imu);

  Tools::distcls(zeroPos,refPos,zeroDir,refDir,xdca1,xdca2,ifail);

  float rvTrack = 100000;
  if (!ifail)
    rvTrack=sqrt(
    pow((refPos[0]+(global->getBbcZVertex()-refPos[2])*refDir[0]),2)+
    pow((refPos[1]+(global->getBbcZVertex()-refPos[2])*refDir[1]),2));

  return (rvTrack< _mutr_rv_cut );
}

//________________________________________________________
bool MWGCuts::muid_hits_ok(int imu, PHMuoTracksOut* muo) const
{

  int muidhits(muo->get_muIDhits(imu));
  int nmuidhits(0);
  for( int gap=0; gap<5; gap++ )
  {
    // HView
    if( muidhits & (1<<(2*gap)) ) nmuidhits++;

    // VView
    if( muidhits & (1<<(2*gap+1)) ) nmuidhits++;
  }

  return (nmuidhits>= _muid_hits_cut );

}

//________________________________________________________
bool MWGCuts::muid_hits_oo_ok(int imu, int iroad, PHMuoTracksOut* muo) const
{

  int muidhits( Tools::get_muid_hit_pattern( muo->get_muIDOOhits(iroad,imu ) ) );
  int nmuidhits(0);
  for( int gap=0; gap<5; gap++ )
  {
    // HView
    if( muidhits & (1<<(2*gap)) ) nmuidhits++;

    // VView
    if( muidhits & (1<<(2*gap+1)) ) nmuidhits++;
  }
  return (nmuidhits>= _muid_hits_cut );

}

//________________________________________________________
bool MWGCuts::muid_depth_ok(int imu, PHMuoTracksOut* muo) const
{ return (muo->get_muIDhits(imu)>= _muid_depth_cut); }

//________________________________________________________
bool MWGCuts::muid_depth_oo_ok(int imu, int iroad, PHMuoTracksOut* muo) const
{

  // get old style hit pattern
  int muid_hit_pattern( Tools::get_muid_hit_pattern( muo->get_muIDOOhits(iroad,imu ) ) );

  // compare to cut
  return( muid_hit_pattern >= _muid_depth_cut);

}

//________________________________________________________
bool MWGCuts::muid_tubes_ok(int imu, PHMuoTracksOut* muo) const
{

  int muidhits = muo->get_muIDhits(imu);
  int nmuidodd = 0; int ibit = 1;
  for (int jbit = 0; jbit<4; jbit++)
  {if (muidhits&ibit) nmuidodd++; ibit*=4;}

  int nmuideven = 0; ibit = 2;
  for (int jbit = 0; jbit<4; jbit++)
  {if (muidhits&ibit) nmuideven++; ibit*=4;}

  if (nmuidodd>=3 && nmuideven>=3) return true;
  if (muidhits>63) return true;
  return false;

}

//________________________________________________________
bool MWGCuts::muid_cont_ok(int imu, PHMuoTracksOut* muo) const
{
  int muidhits = muo->get_muIDhits(imu);
  return ((muidhits&3)!=0 && (muidhits&12)!=0 && (muidhits&48)!=0);
}

//________________________________________________________
bool MWGCuts::muid_rv_ok(PHGlobal* global, int imu, PHMuoTracksOut* muo) const
{
  cout << "MWGCuts::muid_rv_ok" << endl;
  float refPos[3]={0,0,0};
  float refDir[2]={0,0};
  float zeroPos[3]={0,0,0};
  float zeroDir[2]={0,0}; // input

  float xdca1[3]={0,0,0};
  float xdca2[3]={0,0,0};
  int ifail=0; // output

  refPos[0] = muo->get_muID_gap0(0,imu);
  refPos[1] = muo->get_muID_gap0(1,imu);
  refPos[2] = muo->get_muID_gap0(2,imu);

  refDir[0] = muo->get_muID_gap0(3,imu);
  refDir[1]=muo->get_muID_gap0(4,imu);
  Tools::distcls(zeroPos,refPos,zeroDir,refDir,xdca1,xdca2,ifail);

  float rvRoad=100000;
  if (!ifail)
    rvRoad = sqrt(
    pow((refPos[0]+(global->getBbcZVertex()-refPos[2])*refDir[0]),2)+
    pow((refPos[1]+(global->getBbcZVertex()-refPos[2])*refDir[1]),2));

  return (rvRoad< _muid_rv_cut );
}

//________________________________________________________
bool MWGCuts::muid_dca_ok(PHGlobal* global, int imu, PHMuoTracksOut* muo) const
{
  cout << "MWGCuts::muid_dca_ok" << endl;
  float refPos[3]={0,0,0};
  float refDir[2]={0,0};
  float zeroPos[3]={0,0,0};
  float zeroDir[2]={0,0}; // input
  float xdca1[3]={0,0,0};
  float xdca2[3]={0,0,0};
  int ifail=0; // output

  refPos[0]=muo->get_muID_gap0(0,imu);
  refPos[1]=muo->get_muID_gap0(1,imu);
  refPos[2]=muo->get_muID_gap0(2,imu);
  refDir[0]=muo->get_muID_gap0(3,imu);
  refDir[1]=muo->get_muID_gap0(4,imu);

  Tools::distcls(zeroPos,refPos,zeroDir,refDir,xdca1,xdca2,ifail);
  float zdcaRoad=100000;
  if (!ifail) zdcaRoad=(xdca1[2]+xdca2[2])/2;
  return (fabs(zdcaRoad-global->getBbcZVertex())< _muid_dca_cut );
}

//________________________________________________________
bool MWGCuts::dg0_ok(int imu, PHMuoTracksOut* muo) const
{ return ( Tools::DG0(muo,imu) < _dg0_cut ); }

//________________________________________________________
bool MWGCuts::dg0_oo_ok(int imu, int iroad, PHMuoTracksOut* muo) const
{ return ( Tools::DG0(muo,imu, iroad ) < _dg0_cut ); }

//________________________________________________________
bool MWGCuts::ddg0_ok(int imu, PHMuoTracksOut* muo) const
{ return ( Tools::DDG0(muo,imu) < _ddg0_cut ); }

//________________________________________________________
bool MWGCuts::ddg0_oo_ok(int imu, int iroad, PHMuoTracksOut* muo) const
{ return ( Tools::DDG0(muo,imu, iroad ) < _ddg0_cut ); }

//________________________________________________________
bool MWGCuts::ds3_ok(int imu, PHMuoTracksOut* muo) const
{ return ( Tools::DS3(muo,imu)< _ds3_cut ); }

//________________________________________________________
bool MWGCuts::ds3_oo_ok(int imu, int iroad, PHMuoTracksOut* muo) const
{ return ( Tools::DS3(muo,imu, iroad ) < _ds3_cut ); }

//________________________________________________________
bool MWGCuts::ds3_ctp_ok(int imu, PHMuoTracksOut* muo) const
{ return ( Tools::DS3ctp(muo,imu)<_ds3_cut ); }

//________________________________________________________
bool MWGCuts::ds3_ctp_oo_ok(int imu, int iroad, PHMuoTracksOut* muo) const
{ return ( Tools::DS3ctp( muo, imu, iroad )<_ds3_cut ); }
