// $Id: MuonTrigFilter.cxx,v 1.85 2017/12/13 23:57:55 shlim Exp $

/*!
  \file		MuonTrigFilter.cxx
  \ingroup supermodules
  \brief	 muid based offline trigger used to filter data on Deep Deep road pairs
  \author	Sean Kelly/Hugo Pereira
  \version $Revision: 1.85 $
  \date		$Date: 2017/12/13 23:57:55 $
*/

#include <Bbc.hh>
#include <Fun4AllReturnCodes.h>
#include <MutWire.h>
#include <PHGlobal.h>
#include <PHTimer.h>
#include <recoConsts.h>
#include <getClass.h>
#include <TMuiHitMapO.h>
#include <TMuiMCHitMapO.h>
#include <TMuiRoadMapO.h>
#include <TMutExtVtx.h>
#include <TMutGeo.h>
#include <TMutHitMap.h>
#include <TMutMCHitMap.h>
#include <TMutMCTrkMap.h>
#include <TMutVtxMap.h>
#include <TriggerHelper.h>
#include <Zdc.hh>

#include <TVector3.h>

#include "MuonTrigFilter.h"
#include "MuonUtil.h"

using namespace std;


//__________________________________________________________________________
MuonTrigFilter::MuonTrigFilter( const char* name, const MODE& mode, const ACTION& action ) :
  SubsysReco( name ),
  _z_vertex_min( -40 ),
  _z_vertex_max(	40 ),
  _centrality_min(0),
  _centrality_max(100),
  _bbc_charge_min(0),
  _bbc_charge_max(2500),
  _mass_min( 0 ),
  _mass_max( 100 ),
  _max_dimu_dcaz( 1.0 ),
  _min_track_pz( 3.0 ),
  _max_asymmetry( 0.8 ),
  _max_dimu_dcar( 1.0 ),
  _max_dimu_chi2( 10 ),
  _timer( PHTimeServer::get()->insert_new( name ) ),
  _mode( mode ),
  _action(action),
  _event_accepted( true ),
  _nevent(0),
  _nrejected(0),
  _mutr_hit_cut(600),
  _bbcout(0),
  _zdcout(0),
  _minpt(0),
  _maxpt(0),
  _global(0),
  _max_mut_nhits(1000)
{
  cout << "MuonTrigFilter::MuonTrigFilter - name: " << name << " mode: " << mode << endl;
}

//______________________________________________________
int MuonTrigFilter::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "MuonTrigFilter::InitRun" );
  try { TMutNode<BbcOut>::find_io_node( top_node, "BbcOut" ); }		catch( exception &e ) { cout << e.what() << endl; }
  try { TMutNode<ZdcOut>::find_io_node( top_node, "ZdcOut" ); }		catch( exception &e ) { cout << e.what() << endl; }
  try { TMutNode<PHGlobal>::find_io_node( top_node, "PHGlobal" );} catch( exception &e ) { cout << e.what() << endl; }
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//__________________________________________________________________________
int MuonTrigFilter::process_event(PHCompositeNode *top_node)
{
  ++_nevent;
  _timer.get()->restart();

  // load BBC/ZDC nodes
  _bbcout = 0;
  _zdcout = 0;
  _global = 0;
  try { _bbcout = TMutNode<BbcOut>::find_io_node( top_node, "BbcOut" ); } catch( exception &e ) {}
  try { _zdcout = TMutNode<ZdcOut>::find_io_node( top_node, "ZdcOut" ); } catch( exception &e ) {}
  try { _global = TMutNode<PHGlobal>::find_io_node( top_node, "PHGlobal" ); } catch( exception &e ) {}

  // trigger decision
  _event_accepted = true;

  // reinitialize the SKIP_MUTOO_RECO flag
  if( _action == SKIP_MUTOO )
  { recoConsts::instance()->set_IntFlag( "SKIP_MUTOO_RECO", 0 ); }

  // Offline based event selection
  switch ( _mode ) {

  case HI_EVENT:
    {
      TriggerHelper myTH(top_node);
      if( (myTH.didLevel1TriggerFire("UltraPeriph") ||
	    myTH.didLevel1TriggerFire("UltraPeriphMuon2DSouth") ||
	    myTH.didLevel1TriggerFire("UltraPeriphMuon2DNorth") ||
	    myTH.didLevel1TriggerFire("UltraPeriphMPC")) )
			{
				if( !is_mutnhits_accepted( top_node ) ){
					_event_accepted = false;
				}
			}else{
				if (!is_vertex_accepted( top_node ) && !is_mutnhits_accepted( top_node )){ 
					_event_accepted = false;
				}
			}
    }
    break;

    case MINIBIAS:
    {
      TriggerHelper myTH(top_node);
      if( !myTH.IsEventMinBias()) _event_accepted = false;
    }
    break;

    case N2D:
    {
      TriggerHelper myTH(top_node);
      if( !myTH.didLevel1TriggerFire("MUIDN_2D&BBCLL1") )
      { _event_accepted = false; }
    }
    break;

    case N1D1S:
    {
      TriggerHelper myTH(top_node);
      if( !(
        myTH.didLevel1TriggerFire("MUIDN_1D1S&BBCLL1") ||
        myTH.didLevel1TriggerFire("MUIDN_1D1S*BBCLL1") ) )
      { _event_accepted = false; }
    }
    break;

    case N1D:
    {
      TriggerHelper myTH(top_node);
      if( !myTH.didLevel1TriggerFire("MUIDN_1D&BBCLL1") )
      { _event_accepted = false; }
    }
    break;

    case S2D:
    {
      TriggerHelper myTH(top_node);
      if( !myTH.didLevel1TriggerFire("MUIDS_2D&BBCLL1"))
      { _event_accepted = false; }
    }
    break;

    case FVTX:
    {
      if (!is_vertex_accepted( top_node ))
      {
       _event_accepted = false;
       break;
      }

        // now look for a track matching hit more than 10 hits matching at least one hit in MuID
       if (!has_good_track( top_node ))
        _event_accepted = false;

       // for Au+Au we need a even tighter requirement in order to make compact filtered files. Lets select only events with dimuons
       if (!is_reco_dimuon( top_node ))
	   _event_accepted = false;
    }
    break;

    case RECO_COSMIC:
    {
       // now look for a golden Mutr cosmic tracks
       if (!has_good_cosmic_track( top_node ))
       {
       _event_accepted = false;
       cout << "Event accepted: " << _event_accepted << endl;
       }
    }
    break;

    case RECO_TWOARM_COSMIC:
    {
       // now look for a golden Mutr two arms cosmic tracks
       if (!has_good_twoarm_cosmic_track( top_node ))
       {
       _event_accepted = false;
       cout << "Event accepted: " << _event_accepted << endl;
       }
    }
    break;

    case RECO_ONEARM_COSMIC:
    {
       // now look for a golden Mutr one arm plus MuID road in the other arm cosmic tracks
       if (!has_good_onearm_cosmic_track( top_node ))
       {
       _event_accepted = false;
       cout << "Event accepted: " << _event_accepted << endl;
       }
    }
    break;

    case SG3:
    {
      TriggerHelper myTH(top_node);
      if( !(
	    myTH.didLevel1TriggerFire("MUON_SG3&BBCLL1") ||
	    myTH.didLevel1TriggerFire("MUON_S_SG3&MUIDLL1_S1D&BBCLL1novtx") ||
	    myTH.didLevel1TriggerFire("MUON_N_SG3&MUIDLL1_N1D&BBCLL1novtx") ||
	    myTH.didLevel1TriggerFire("MUON_S_SG3&BBCLL1") ||
	    myTH.didLevel1TriggerFire("MUON_N_SG3&BBCLL1") ||
	    myTH.didLevel1TriggerFire("MUON_S_SG3 && (MUIDLL1_S1D || S1H) && BBCLL1(noVtx)") ||
	    myTH.didLevel1TriggerFire("MUON_N_SG3 && (MUIDLL1_N1D || N1H) && BBCLL1(noVtx)") ||
	    myTH.didLevel1TriggerFire("MUON_S_SG1&BBCLL1") ||
	    myTH.didLevel1TriggerFire("MUON_N_SG1&BBCLL1") ||
	    myTH.didLevel1TriggerFire("MUON_S_SG1_RPC3_1_A||B||C") ||
	    myTH.didLevel1TriggerFire("MUON_N_SG1_RPC3_1_A||B||C")))
	{ _event_accepted = false; }
    }
    break;

    case S1D1S:
    {
      TriggerHelper myTH(top_node);
      if( !(
        myTH.didLevel1TriggerFire("MUIDS_1D1S&BBCLL1") ||
        myTH.didLevel1TriggerFire("MUIDS_1D1S*BBCLL1") ) )
      { _event_accepted = false; }
    }
    break;

    case S1D:
    {
      TriggerHelper myTH(top_node);
      if( !myTH.didLevel1TriggerFire("MUIDS_1D&BBCLL1") )
      { _event_accepted = false; }
    }
    break;

    case LEVEL2:
    if( !is_level2_accepted( top_node ) )
    { _event_accepted = false; }
    break;

    // pseudo LL1 triggers
    case PSEUDOLL1_N2D:
    if( !is_pseudoLL1_N2D( top_node ) )
    { _event_accepted = false; }
    break;

    case PSEUDOLL1_S2D:
    if( !is_pseudoLL1_S2D( top_node ) )
    { _event_accepted = false; }
    break;

    // pseudo BLT triggers
    case PSEUDOBLT_2D:
    if( !( is_pseudoBLT_N2D( top_node ) || is_pseudoBLT_S2D( top_node )  ) )
    { _event_accepted = false; }
    break;

    case PSEUDOBLT_N2D:
    if( !is_pseudoBLT_N2D( top_node ) )
    { _event_accepted = false; }
    break;

    case PSEUDOBLT_S2D:
    if( !is_pseudoBLT_S2D( top_node ) )
    { _event_accepted = false; }
    break;

    case PSEUDOBLT_1D1S:
    if( !( is_pseudoBLT_N1D1S( top_node ) || is_pseudoBLT_S1D1S( top_node )  ) )
    { _event_accepted = false; }
    break;

    case PSEUDOBLT_N1D1S:
    if( !is_pseudoBLT_N1D1S( top_node ) )
    { _event_accepted = false; }
    break;

    case PSEUDOBLT_S1D1S:
    if( !is_pseudoBLT_S1D1S( top_node ) )
    { _event_accepted = false; }
    break;

    case MC_HV:
    if( !is_hv_mutr_accepted( top_node ) )
    { _event_accepted = false; }
    break;

    case MC_SHALLOW_SHALLOW:
    if( !is_mc_shallow_shallow_accepted( top_node ) )
    { _event_accepted = false; }
    break;

    case MC_SHALLOW_SHALLOW_SOUTH:
    if( !is_mc_shallow_shallow_accepted( top_node, MUTOO::South ) )
    { _event_accepted = false; }
    break;

    case MC_SHALLOW_SHALLOW_NORTH:
    if( !is_mc_shallow_shallow_accepted( top_node, MUTOO::North ) )
    { _event_accepted = false; }
    break;

    case MC_DEEP_SHALLOW:
    if( !is_mc_deep_shallow_accepted( top_node ) )
    { _event_accepted = false; }
    break;

    case MC_DEEP_DEEP:
    if( !is_mc_deep_deep_accepted( top_node ) )
    { _event_accepted = false; }
    break;

    case Z_BBC:
    cout << "MuonTrigFilter::process_event - Z_BBC mode is obsolete. Use Z_VERTEX instead" << endl;
    if( !is_vertex_accepted( top_node ) )
    { _event_accepted = false; }
    break;

    case Z_VERTEX:
    if( !is_vertex_accepted( top_node ) )
    { _event_accepted = false; }
    break;

  case MUTHITS_ZVERTEX:
    if (!is_vertex_accepted( top_node ) && !is_mutnhits_accepted( top_node ))
    { _event_accepted = false; }
    break;

    case BBC_ZDC:
    if( !is_bbc_zdc_accepted( top_node ) )
    { _event_accepted = false; }
    break;

    case CENTRALITY:
    if( !is_centrality_accepted( top_node ) )
    { _event_accepted = false; }
    break;

    case BBC_CHARGE:
    if( !is_bbc_charge_accepted( top_node ) )
    { _event_accepted = false; }
    break;

    case BAD_MUTR_VS_BBC_EVENT:
    if( !( is_mutr_vs_bbc_accepted(top_node) || is_mui_vs_bbc_accepted(top_node) ) ) _event_accepted = false;
    break;

    case MUIOO_2D:
    if( !is_muioo_2d(top_node) ) _event_accepted = false;
    break;

    case RECO_DIMUON:
    if( !is_reco_dimuon( top_node ) ) _event_accepted = false;
    break;

    case TRK_MUON:
    if( !has_track( top_node ) ) _event_accepted = false;
    break;

    case TRK_HIPTMUON:
    if( !has_hipttrack( top_node ) ) _event_accepted = false;
    break;

    case ROAD_MUON:
    if( !has_road( top_node ) ) _event_accepted = false;
    break;

    case NONE:
    static unsigned int count( 0 );
    if( verbosity == 1 ) {
      if( count < 5 ) cout << "MuonTrigFilter::process_event [" << ThisName
        << "] - mode is NONE. Doing nothing." << endl;
      if( count == 5 ) cout << "MuonTrigFilter::process_event ["
        << ThisName << "]	 - mode is NONE - message disabled.\n";
      count++;
    }
    if( verbosity > 1 ) MuonUtil::dump_evtN_bbcZ_run5centrality( top_node );
    break;

    default: break;
  }

  _timer.get()->stop();

  // return if event is accepted
  if( _event_accepted ) return EVENT_OK;

  // take proper action if event is rejected
  _nrejected ++;
  switch ( _action ) {

    case DISCARD_EVENT:
    return DISCARDEVENT;

    case ABORT_EVENT:
    return ABORTEVENT;

    case SKIP_MUTOO:
    recoConsts::instance()->set_IntFlag( "SKIP_MUTOO_RECO", 1 );
    return EVENT_OK;

    case DO_NOTHING:
    return EVENT_OK;

    default:
    return EVENT_OK;

  }

  // should not reach there
  return EVENT_OK;

}

//__________________________________________________________________________
int MuonTrigFilter::End(PHCompositeNode* top_node)
{
//   _timer.get()->print_stat();
  MUTOO::PRINT(cout, ThisName );

  cout
    << ThisName
    << " mode: " << _mode
    << " total: " << _nevent
    << " accepted: " << _nevent - _nrejected
    << " rejected: " << _nrejected
    << " rejection: " << _nrejected/static_cast<double>(_nevent)
    << endl;
  MUTOO::PRINT(cout,"**");
  return 0;
}

//__________________________________________________________________________
void MuonTrigFilter::add_level2_trigger( const char* trigger_name )
{ if( trigger_name ) _level2_triggers.insert( trigger_name ); }

//__________________________________________________________________________
bool MuonTrigFilter::is_mutnhits_accepted(PHCompositeNode* top_node) const
{
  bool accepted =  true;

  // fill arrays
  TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
  int hitsN = hit_map->get( MUTOO::North ).count();
  int hitsS = hit_map->get( MUTOO::South ).count();
  int hitsNS = hitsS + hitsN;

  if (hitsNS>_max_mut_nhits) accepted = false;
  if (hitsNS==0) accepted = false;

  return accepted;

}

//__________________________________________________________________________
bool MuonTrigFilter::is_mutr_vs_bbc_accepted(PHCompositeNode* top_node) const
{

  bool accepted = true;

  try {


    // fill arrays
    TMutHitMap*	hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");

    // allocate arrays for number of hits/total charge per arm/station
    int hitsN = hit_map->get( MUTOO::North ).count();

    if( _bbcout ){
      double bbc_charge( _bbcout->get_ChargeSum(0) + _bbcout->get_ChargeSum(1) );
      if( hitsN >= _mutr_hit_cut + 120.0*bbc_charge ) {

        if( verbosity >= 1 ) {
          cout << "MuonTrigFilter::is_mutr_vs_bbc_accepted - event is rejected.\n";
          cout << "MuonTrigFilter::is_mutr_vs_bbc_accepted - bbc_charge=" << bbc_charge << endl;
          cout << "MuonTrigFilter::is_mutr_vs_bbc_accepted - multiplicity=" << hitsN << endl;
        }

        accepted = false;

      }
    }

  } catch( exception &e ) {cout << e.what() << endl;}

  return accepted;
}

//__________________________________________________________________________
bool MuonTrigFilter::is_mui_vs_bbc_accepted(PHCompositeNode* top_node) const
{
  bool accepted = true;

  // check BBC node
  if( !_bbcout ) accepted = true;
  else {

    // hard wired cuts
    static const double min_bbc_charge( 20 );
    static const double slope( 10 );

    // retrieves total charge
    double bbc_charge = _bbcout->get_ChargeSum(0) + _bbcout->get_ChargeSum(1);

    // accept if charge is large enough
    if( bbc_charge >= min_bbc_charge ) accepted = true;
    else try {

      // retrieve number of muid hits
      TMuiHitMapO* map = TMutNode<TMuiHitMapO>::find_node( top_node, "TMuiHitMapO" );
      unsigned int n_mui_hits = map->size();

      // check against BBC
      if( n_mui_hits >= slope*bbc_charge ) {

        if( verbosity >= 1 ) {
          cout << "MuonTrigFilter::is_mui_vs_bbc_accepted - event is rejected.\n";
          cout << "MuonTrigFilter::is_mui_vs_bbc_accepted - bbc_charge=" << bbc_charge << endl;
          cout << "MuonTrigFilter::is_mui_vs_bbc_accepted - multiplicity=" << n_mui_hits << endl;
        }

        accepted = false;

      }

    } catch( exception &e ) {cout << e.what() << endl;}
  }

  return accepted;

}

//__________________________________________________________________________
bool MuonTrigFilter::is_mc_mutr_accepted(PHCompositeNode* top_node, int arm ) const
{
  bool accepted = true;
  try {

    // stores number of good mc tracks in given arm
    int good_mc_tracks = 0;

    // retrieves map of MC tracks
    TMutMCTrkMap* mc_trk_map = MuonUtil::find_node<TMutMCTrkMap>( "TMutMCTrkMap" );
    TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->get( arm );

    // loop over mc tracks
    while( TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next() ) {

      // bit mask you get when all Mutr MC hits are found
      if( !is_mc_mutr_accepted( *mc_trk_ptr, mutr_mc_mask ) ) continue;

      // track is accepted
      good_mc_tracks++;
    }

    // check number of accepted MC tracks/arm
    accepted = (good_mc_tracks >= 2);

    if( verbosity >= 1 )
    cout
        << "MuonTrigFilter::is_mc_mutr_accepted -"
        << " arm=" << arm
        << " n_trks=" << mc_trk_iter.count()
        << " accepted=" << accepted << endl;

  } catch( exception &e ) {cout << e.what() << endl;}

  return accepted;

}

//__________________________________________________________________________
bool MuonTrigFilter::is_hv_mutr_accepted(PHCompositeNode* top_node, int arm ) const
{
  bool accepted = true;
  try {

    // stores number of good mc tracks in given arm
    int good_mc_tracks = 0;

    // retrieves map of MC tracks
    TMutMCTrkMap* mc_trk_map = MuonUtil::find_node<TMutMCTrkMap>( "TMutMCTrkMap" );
    TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->get( arm );

    // loop over mc tracks
    while( TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next() ) {

      // check that all mc_hits match the HV mask
      if( !is_hv_mutr_accepted( *mc_trk_ptr, mutr_mc_mask ) ) continue;

      // track is accepted
      good_mc_tracks++;
    }

    // check number of accepted MC tracks/arm
    accepted = (good_mc_tracks >= 2);

    if( verbosity >= 1 )
    cout
        << "MuonTrigFilter::is_hv_mutr_accepted -"
        << " arm=" << arm
        << " n_trks=" << mc_trk_iter.count()
        << " accepted=" << accepted << endl;

  } catch( exception &e ) {cout << e.what() << endl;}

  return accepted;

}

//__________________________________________________________________________
bool MuonTrigFilter::is_mc_shallow_shallow_accepted(PHCompositeNode* top_node, int arm ) const
{
  bool accepted = true;
  try {

    // stores number of good mc tracks in given arm
    int good_mc_tracks = 0;

    // retrieves map of MC tracks
    TMutMCTrkMap* mc_trk_map = MuonUtil::find_node<TMutMCTrkMap>( "TMutMCTrkMap" );
    TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->get( arm );

    // loop over mc tracks
    while( TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next() ) {

      // bit mask you get when all Mutr MC hits are found
      if( !is_mc_mutr_accepted( *mc_trk_ptr, mutr_mc_mask ) ) continue;

      // bit mask you get for a shallow muid road
      if( !is_mc_muid_accepted( *mc_trk_ptr, muid_mc_shallow_mask ) ) continue;

      // track is accepted
      good_mc_tracks++;
    }

    // check number of accepted MC tracks/arm
    accepted = (good_mc_tracks >= 2);

    if( verbosity >= 1 )
    cout
        << "MuonTrigFilter::is_mc_shallow_shallow_accepted -"
        << " arm=" << arm
        << " n_trks=" << mc_trk_iter.count()
        << " accepted=" << accepted << endl;

  } catch( exception &e ) {cout << e.what() << endl;}

  return accepted;

}

//__________________________________________________________________________
bool MuonTrigFilter::is_mc_deep_deep_accepted(PHCompositeNode* top_node, int arm ) const
{
  bool accepted = true;
  try {

    // stores number of good mc tracks/arm
    int good_mc_tracks = 0;

    // retrieves map of MC tracks
    TMutMCTrkMap* mc_trk_map = MuonUtil::find_node<TMutMCTrkMap>( "TMutMCTrkMap" );
    TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->get( arm );

    // loop over mc tracks
    while( TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next() ) {

      // bit mask you get when all Mutr MC hits are found
      if( !is_mc_mutr_accepted( *mc_trk_ptr, mutr_mc_mask ) ) continue;

      // bit mask you get for a deep muid road
      if( !is_mc_muid_accepted( *mc_trk_ptr, muid_mc_deep_mask ) ) continue;

      // track is accepted
      good_mc_tracks++;
    }

    // check number of accepted MC tracks/arm
    accepted = good_mc_tracks>=2;

  } catch( exception &e ) {cout << e.what() << endl;}

  return accepted;

}

//__________________________________________________________________________
bool MuonTrigFilter::is_mc_deep_shallow_accepted(PHCompositeNode* top_node, int arm ) const
{
  bool accepted = true;
  try {

    // stores number of good mc tracks/arm
    int deep_mc_tracks = 0;
    int shallow_mc_tracks = 0;

    // retrieves map of MC tracks
    TMutMCTrkMap* mc_trk_map = MuonUtil::find_node<TMutMCTrkMap>( "TMutMCTrkMap" );
    TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->get( arm );

    // loop over mc tracks
    while( TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next() ) {

      // bit mask you get when all Mutr MC hits are found
      if( !is_mc_mutr_accepted( *mc_trk_ptr, mutr_mc_mask ) ) continue;

      // bit mask you get for a deep/shallow muid road
      if( is_mc_muid_accepted( *mc_trk_ptr, muid_mc_deep_mask ) ) deep_mc_tracks++;
      else if( is_mc_muid_accepted( *mc_trk_ptr, muid_mc_shallow_mask ) ) shallow_mc_tracks++;

    }

    // check number of accepted MC tracks/arm
    accepted = ( deep_mc_tracks>=2 || ( deep_mc_tracks>=1 && shallow_mc_tracks>=1 ) );

  } catch( exception &e ) {cout << e.what() << endl;}

  return accepted;

}

//__________________________________________________________________________
bool MuonTrigFilter::is_vertex_accepted(PHCompositeNode* top_node) const
{

  static bool first = true;
  if( first ) {
    cout << "MuonTrigFilter::is_vertex_accepted - z_vertex_range=[ " << _z_vertex_min << " , " << _z_vertex_max << " ]"	<< endl;
    first = false;
  }

  // vertex z position
  double z_vertex(-9999.9);

  // Use MuonUtil for bbc_z
  
  z_vertex = MuonUtil::get_bbc_z( top_node );

  // try get vertex from TMutExtVtx
  // retrieve vertex point
  //bool error( false );
  //PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  //if( !error ) z_vertex = vtx.getZ();

  // if failed, try use PHGlobal
  //if( error && _global ) {

    // retrieve z_vertex from PHGlobal
 //   z_vertex = _global->getBbcZVertex();
 //   error = false;

 // }

  // if no vertex loaded, return false
  //if( error ) return false;

  // check vertex against cuts
  return ( ( z_vertex >= _z_vertex_min ) && ( z_vertex <= _z_vertex_max ) );

}

//__________________________________________________________________________
bool MuonTrigFilter::is_bbc_zdc_accepted(PHCompositeNode* top_node) const
{

  // bbc multiplicity/charge
  if( _bbcout )
  {
    // using BBC node
    if( !( _bbcout->get_nPmt( Bbc::South ) > 0 && _bbcout->get_nPmt( Bbc::North ) > 0 ) ) return false;
    if( !( _bbcout->get_ChargeSum( Bbc::South ) > 0 && _bbcout->get_ChargeSum( Bbc::North ) > 0 ) ) return false;

  } else if( _global ) {

    // using PHGlobal node
    if( !( _global->getBbcMultS() > 0 && _global->getBbcMultN() > 0 ) ) return false;
   	if( !( _global->getBbcChargeS() > 0 && _global->getBbcChargeN() > 0 ) ) return false;

  }

  // ZDC energy
  if( _zdcout && !( _zdcout->get_Energy( Zdc::South ) > 0 && _zdcout->get_Energy( Zdc::North ) > 0 ) ) return false;
  else if( _global && !( _global->getZdcEnergyS() > 0.0 && _global->getZdcEnergyN() > 0.0 ) ) return false;

  return true;

}

//__________________________________________________________________________
bool MuonTrigFilter::is_centrality_accepted(PHCompositeNode* top_node) const
{

  static bool first = true;

  if( first ) {
    cout << "MuonTrigFilter::is_centrality_accepted - centrality range=[ " << _centrality_min << " , " << _centrality_max << " ]"	<< endl;
    first = false;
  }

  double centrality( MuonUtil::get_centrality( top_node ) );
  //cout << "MuonTrigFilter::is_centrality_accepted - centrality: " << centrality << endl;

  // check centrality agains window
  return ( ( centrality > _centrality_min ) && ( centrality <= _centrality_max ) );

}

//__________________________________________________________________________
bool MuonTrigFilter::is_bbc_charge_accepted(PHCompositeNode* top_node) const
{

  static bool first( true );
  if( first )
  {
    first = false;
    cout << "MuonTrigFilter::is_bbc_charge_accepted - _bbc_charge_min: " << _bbc_charge_min << endl;
    cout << "MuonTrigFilter::is_bbc_charge_accepted - _bbc_charge_max: " << _bbc_charge_max << endl;
  }

  // bbc multiplicity/charge
  double charge = 0;
  if( _bbcout ) charge = _bbcout->get_ChargeSum( Bbc::South ) + _bbcout->get_ChargeSum( Bbc::North );
  else if( _global ) charge = _global->getBbcChargeS() + _global->getBbcChargeN();
  else return false;

  return (charge > _bbc_charge_min) && (charge < _bbc_charge_max);

}

//__________________________________________________________________________
bool MuonTrigFilter::is_level2_accepted(PHCompositeNode* top_node) const
{

  if( !_level2_triggers.size() ) {
    cout << "MuonTrigFilter::is_level2_accepted - no level2 trigger selected. Event accepted" << endl;
    return true;
  }

  for( std::set<std::string>::const_iterator trigger = _level2_triggers.begin();
       trigger !=	_level2_triggers.end();
       trigger++ )
    {
      if( MuonUtil::get_l2_trigger_decision( top_node, *trigger ) ) return true;
    }
  return false;

}

//__________________________________________________________________________
bool MuonTrigFilter::is_pseudoLL1_N2D( PHCompositeNode* top_node ) const
{
  if ( MuonUtil::get_2D_LL1trigger_decision( top_node, MUTOO::North ) ) return true;
  return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::is_pseudoLL1_S2D( PHCompositeNode* top_node ) const
{
  if ( MuonUtil::get_2D_LL1trigger_decision( top_node, MUTOO::South ) ) return true;
  return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::is_pseudoBLT_N2D( PHCompositeNode* top_node ) const
{
  if ( MuonUtil::get_2D_BLTtrigger_decision( top_node, MUTOO::North ) ) return true;
  return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::is_pseudoBLT_S2D( PHCompositeNode* top_node ) const
{
  if ( MuonUtil::get_2D_BLTtrigger_decision( top_node, MUTOO::South ) ) return true;
  return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::is_pseudoBLT_N1D1S( PHCompositeNode* top_node ) const
{
  if ( MuonUtil::get_1D1S_BLTtrigger_decision( top_node, MUTOO::North ) ) return true;
  return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::is_pseudoBLT_S1D1S( PHCompositeNode* top_node ) const
{
  if ( MuonUtil::get_1D1S_BLTtrigger_decision( top_node, MUTOO::South ) ) return true;
  return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::is_muioo_2d(PHCompositeNode* top_node) const
{
  bool north_2d=false;
  bool south_2d=false;
  for(int arm=0; arm<2; ++arm){
    // Check 2 deep roads in different groups
    //
    set<UShort_t> deep_group;
    TMuiRoadMapO* roadmap = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
    TMuiRoadMapO::const_iterator road_iter = roadmap->get(arm);
    while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next()){
      if(road_ptr->get()->get_depth()>=4) deep_group.insert(road_ptr->get()->get_group());
    }
    if(deep_group.size() >= 2 && arm==0) {south_2d=true;}
    if(deep_group.size() >= 2 && arm==1) {north_2d=true;}
  }
  return (north_2d || south_2d);
}

//__________________________________________________________________________
bool MuonTrigFilter::is_reco_dimuon(PHCompositeNode* top_node) const
{
  if ( !is_vertex_accepted( top_node ) ) return false;

  double z_vertex = MuonUtil::get_bbc_z( top_node );
  TMutVtxMap*	vtx_map = TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");
  TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
  while( TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next() )
  {
    double dimuon_asymmetry = fabs(vtx_ptr->get()->get_ptot1() - vtx_ptr->get()->get_ptot2())/(vtx_ptr->get()->get_ptot1() + vtx_ptr->get()->get_ptot2());
    cout << dimuon_asymmetry << endl;
    if (dimuon_asymmetry > _max_asymmetry ) continue;

    double dca_r = sqrt(MUTOO::SQUARE(vtx_ptr->get()->get_x()) + MUTOO::SQUARE(vtx_ptr->get()->get_y()));
    if (fabs(dca_r) > _max_dimu_dcar ) continue;

    float rap = fabs(vtx_ptr->get()->get_rapidity());
    if ( rap<1.2 || rap>2.2 ) continue;

    if (vtx_ptr->get()->get_chi_square() > _max_dimu_chi2 ) continue;

    if(
      // vtx_ptr->get()->get_sign() == TMutVtx::POSNEG &&
      vtx_ptr->get()->get_mass() >= _mass_min &&
      vtx_ptr->get()->get_mass() <= _mass_max &&
      fabs(vtx_ptr->get()->get_z()-z_vertex)< _max_dimu_dcaz &&
      fabs(vtx_ptr->get()->get_pz1())> _min_track_pz &&
      fabs(vtx_ptr->get()->get_pz2())> _min_track_pz)
    { return true; }

  }
  return false;
}

//________________________________________________________________________________________________
int MuonTrigFilter::get_mc_mutr_pattern( const TMutMCTrkMap::value_type& trk )
{

  // get mutr hit pattern
  int out( 0 );
  TMutMCHitMap::const_key_iterator mc_hit_iter = trk.get()->get_associated<TMutMCHit>();
  while( TMutMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next() ) {
    int index =
      mc_hit_ptr->get()->get_station() +
      MUTOO::NumberOfStations*mc_hit_ptr->get()->get_gap();

    out |= ( 1 << index );
  }

  return out;

}

//________________________________________________________________________________________________
int MuonTrigFilter::get_mc_muid_pattern( const TMutMCTrkMap::value_type& trk )
{

  // get muid hit pattern
  int out( 0 );
  TMuiMCHitMapO::key_iterator mui_hit_iter = trk.get()->get_associated<TMuiMCHitO>();
  while( TMuiMCHitMapO::pointer mui_hit_ptr = mui_hit_iter.next() ) {
    int index = mui_hit_ptr->get()->get_plane();
    out |= ( 1 << index );
  }

  return out;

}

//________________________________________________________________________________________________
int MuonTrigFilter::get_hv_mutr_pattern( const TMutMCTrkMap::value_type& trk )
{

  // get mutr hit pattern
  int out( 0 );
  TMutMCHitMap::const_key_iterator mc_hit_iter = trk.get()->get_associated<TMutMCHit>();
  while( TMutMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next() ) {
    int index =
      mc_hit_ptr->get()->get_station() +
      MUTOO::NumberOfStations*mc_hit_ptr->get()->get_gap();

    out |= ( is_hv_mutr_accepted( *mc_hit_ptr ) << index );
  }

  return out;

}

//________________________________________________________________________________________________
bool MuonTrigFilter::is_hv_mutr_accepted( const TMutMCHitMap::value_type& hit )
{
  MutWire *wire_ptr = TMutGeo::get_wire_geom(
      hit.get()->get_arm(),
      hit.get()->get_station(),
      hit.get()->get_octant(),
      hit.get()->get_half_octant(),
      hit.get()->get_gap(),
      hit.get()->get_wire());

  return (wire_ptr)	? (!wire_ptr->ChannelIsDead()):false;

}

//__________________________________________________________________________
bool MuonTrigFilter::has_track(PHCompositeNode* top_node, int arm ) const
{

  try {

    // retrieves map of tracks
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node( top_node, "TMutTrkMap" );
    return (trk_map->get( arm ).count()>0);

  } catch( exception &e ) {cout << e.what() << endl;}
  return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::has_hipttrack(PHCompositeNode* top_node, int arm ) const
{

  try {

    // retrieves map of tracks
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node( top_node, "TMutTrkMap" );

    TMutTrkMap::const_iterator trk_iter = trk_map->range();
	
    
    while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
	{
	  
	float p_xx = trk_ptr->get()->get_trk_par_vtx()->get_px();
	float p_yy = trk_ptr->get()->get_trk_par_vtx()->get_py();
	float p_t = sqrt(p_xx * p_xx + p_yy * p_yy);
	if (p_t < _minpt || p_t > _maxpt) continue;
	return true;
	}
  } catch( exception &e ) {cout << e.what() << endl;}
  return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::has_good_track(PHCompositeNode* top_node) const
{

  try {
    
    double z_vertex = MuonUtil::get_bbc_z( top_node );
    // retrieves MWG if exists
    PHMuoTracksOut* tracks = findNode::getClass<PHMuoTracksOut>(top_node,"PHMuoTracksOO");
    for (size_t itrk=0; itrk<tracks->get_npart(); itrk++)
      {
	// cut oout ery low momentum tracks
	if (fabs(tracks->get_pz(0,itrk))<3.0) continue;

	// check number of hits in the track
	size_t nhits = tracks->get_nhits(itrk);
	if (nhits<10) continue;

	if (tracks->get_chisquare(itrk) > 10) continue;

	// check if track points to an acceptable region in Z vertex
	if (fabs(tracks->get_zpos(0, itrk) - z_vertex)>0.5) continue;

	// check if tehre is any MuiD hit matching track
	bool has_muidhit = false;
	for (int i_road=0; i_road<4; i_road++)
	  if ( tracks->get_muIDOOhits( i_road, itrk))
	    {
	      has_muidhit = true;
	      break;
	    }
	if (!has_muidhit) continue;

	bool good_road = false;
	for (int iroad=0; iroad<4; iroad++)
	  {
	    if ( DG0( tracks, itrk, iroad ) > 20 ) continue;
	    if ( DDG0( tracks, itrk, iroad ) > 10) continue;
	    int nmuidhits = 0;
	    int lastgap = 0;
	    for(int igap=0; igap<5; igap++)
	      if (tracks->is_muIDOOhit( iroad, itrk, igap, 0) || tracks->is_muIDOOhit( iroad, itrk, igap, 1 ))
		{
		  lastgap = igap;
		  nmuidhits ++;
		}
	    // very tight cuts
	    if ((lastgap < 4) || (nmuidhits < 3)) continue;
	    good_road = true;
	    break;
	  }
	if (!good_road) continue;
	
	return true;
      }
  } catch( exception &e ) {cout << e.what() << endl;}
  return false;
}


//__________________________________________________________________________
bool MuonTrigFilter::has_good_cosmic_track(PHCompositeNode* top_node) const
{
	try {

		// retrieves MWG if exists
		PHMuoTracksOut* tracks = findNode::getClass<PHMuoTracksOut>(top_node,"PHMuoTracksOO");
                
		for (size_t itrk=0; itrk<tracks->get_npart(); itrk++)
		{

			if (tracks->get_chisquare(itrk) > 20) continue;

			// check if tehre is any MuiD hit matching track
			bool has_muidhit = false;
			for (int i_road=0; i_road<4; i_road++)
				if ( tracks->get_muIDOOhits( i_road, itrk))
				{
					has_muidhit = true;              break;
				}
			if (!has_muidhit) continue;

			bool good_road = false;
			for (int iroad=0; iroad<4; iroad++)
			{
				if ( DG0( tracks, itrk, iroad ) > 20 ) continue;
				if ( DDG0( tracks, itrk, iroad ) > 9) continue;
				int nmuidhits = 0;
				int lastgap = 0;
				for(int igap=0; igap<5; igap++)
					if (tracks->is_muIDOOhit( iroad, itrk, igap, 0) || tracks->is_muIDOOhit( iroad, itrk, igap, 1 ))
					{
						lastgap = igap;
						nmuidhits ++;
					}
				// very tight cuts
				if ((lastgap < 4) || (nmuidhits < 3)) continue;
				good_road = true;
				break;
			}
			if (!good_road) continue;
			
			// calculate the pseudorapidity for muons
			double px = tracks->get_px(0,itrk);
			double py = tracks->get_py(0,itrk);
			double pz = tracks->get_pz(0,itrk);

			TVector3 mom(px,py,pz);
			double eta = mom.PseudoRapidity();

			// 1.2 < eta < 2.5 for north muon arm, and -2.3 < eta < -1.2 for south muon arm
			if( !(eta > 1.2 && eta < 2.5) && !(eta > -2.3 && eta < -1.2) ) continue;

			return true;

		}

	} catch( exception &e ) {cout << e.what() << endl;}
	return false;
}


//__________________________________________________________________________
bool MuonTrigFilter::has_good_twoarm_cosmic_track(PHCompositeNode* top_node) const
{
	try {

		// retrieves MWG if exists
		PHMuoTracksOut* tracks = findNode::getClass<PHMuoTracksOut>(top_node,"PHMuoTracksOO");
                
		if(tracks->get_npart() < 2) return false;   // at least 2 tracks in the same events

		vector<unsigned long> SouthArm;
		vector<unsigned long> NorthArm;
		SouthArm.clear();
		NorthArm.clear();

		for (size_t itrk=0; itrk<tracks->get_npart(); itrk++)
		{

			if (tracks->get_chisquare(itrk) > 20) continue;

			// check if tehre is any MuiD hit matching track
			bool has_muidhit = false;
			for (int i_road=0; i_road<4; i_road++)
				if ( tracks->get_muIDOOhits( i_road, itrk))
				{
					has_muidhit = true;              break;
				}
			if (!has_muidhit) continue;

			bool good_road = false;
			for (int iroad=0; iroad<4; iroad++)
			{
				if ( DG0( tracks, itrk, iroad ) > 20 ) continue;
				if ( DDG0( tracks, itrk, iroad ) > 9) continue;
				int nmuidhits = 0;
				int lastgap = 0;
				for(int igap=0; igap<5; igap++)
					if (tracks->is_muIDOOhit( iroad, itrk, igap, 0) || tracks->is_muIDOOhit( iroad, itrk, igap, 1 ))
					{
						lastgap = igap;
						nmuidhits ++;
					}
				// very tight cuts
				if ((lastgap < 4) || (nmuidhits < 3)) continue;
				good_road = true;
				break;
			}
			if (!good_road) continue;
			
			// calculate the pseudorapidity for muons
			double px = tracks->get_px(0,itrk);
			double py = tracks->get_py(0,itrk);
			double pz = tracks->get_pz(0,itrk);

			TVector3 mom(px,py,pz);
			double eta = mom.PseudoRapidity();

			// 1.2 < eta < 2.5 for north muon arm, and -2.3 < eta < -1.2 for south muon arm
			if( !(eta > 1.2 && eta < 2.5) && !(eta > -2.3 && eta < -1.2) ) continue;
			
			if (eta > 1.2  && eta < 2.5) NorthArm.push_back(itrk);
			if (eta > -2.3 && eta < -1.2) SouthArm.push_back(itrk);

		}


		for(unsigned long istrk=0; istrk<SouthArm.size(); istrk++)
		{
			// calculate the pseudorapidity for muons
			double px1 = tracks->get_px( 0, SouthArm.at(istrk) );
			double py1 = tracks->get_py( 0, SouthArm.at(istrk) );
			double pz1 = tracks->get_pz( 0, SouthArm.at(istrk) );

			TVector3 mom1(px1,py1,pz1);
			double eta1 = mom1.PseudoRapidity();
			
			for(unsigned long intrk=0; intrk<NorthArm.size(); intrk++)
			{
			double px2 = tracks->get_px( 0, NorthArm.at(intrk) );
			double py2 = tracks->get_py( 0, NorthArm.at(intrk) );
			double pz2 = tracks->get_pz( 0, NorthArm.at(intrk) );

			TVector3 mom2(px2,py2,pz2);
			double eta2 = mom2.PseudoRapidity();

			double dipAngle = PI - acos(( mom1.Px()*mom2.Px() + mom1.Py()*mom2.Py() + mom1.Pz()*mom2.Pz() )/( mom1.Mag()*mom2.Mag()) );


			if( fabs(fabs(eta1) - fabs(eta2) ) > 0.15 ) continue; // loose delta rapidity cuts
			if( fabs(dipAngle) > 0.15 )  continue; // loose dip angle (angle between two tracks) cuts 

			return true;	
			}

		}

	} catch( exception &e ) {cout << e.what() << endl;}
	return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::has_good_onearm_cosmic_track(PHCompositeNode* top_node) const
{

	try {

		// retrieves MWG if exists
		PHMuoTracksOut* tracks = findNode::getClass<PHMuoTracksOut>(top_node,"PHMuoTracksOO");
                
		for (size_t itrk=0; itrk<tracks->get_npart(); itrk++)
		{

			if (tracks->get_chisquare(itrk) > 20) continue;

			// check if tehre is any MuiD hit matching track
			bool has_muidhit = false;
			for (int i_road=0; i_road<4; i_road++)
				if ( tracks->get_muIDOOhits( i_road, itrk))
				{
					has_muidhit = true;              break;
				}
			if (!has_muidhit) continue;

			bool good_road = false;
			for (int iroad=0; iroad<4; iroad++)
			{
				if ( DG0( tracks, itrk, iroad ) > 20 ) continue;
				if ( DDG0( tracks, itrk, iroad ) > 9) continue;
				int nmuidhits = 0;
				int lastgap = 0;
				for(int igap=0; igap<5; igap++)
					if (tracks->is_muIDOOhit( iroad, itrk, igap, 0) || tracks->is_muIDOOhit( iroad, itrk, igap, 1 ))
					{
						lastgap = igap;
						nmuidhits ++;
					}
				// very tight cuts
				if ((lastgap < 4) || (nmuidhits < 3)) continue;
				good_road = true;
				break;
			}
			if (!good_road) continue;

			bool good_road_in_opposite_arm = false;

			// calculate the pseudorapidity for muons
			double px = tracks->get_px(0,itrk);
			double py = tracks->get_py(0,itrk);
			double pz = tracks->get_pz(0,itrk);

			TVector3 mom(px,py,pz);
			double eta = mom.PseudoRapidity();

			// 1.2 < eta < 2.5 for north muon arm, and -2.3 < eta < -1.2 for south muon arm
			if( !(eta > 1.2 && eta < 2.5) && !(eta > -2.3 && eta < -1.2) ) continue;

			int arm = (eta > -2.3 && eta < -1.2) ? 0 : 1;

			TMuiRoadMapO* road_map = TMutNode<TMuiRoadMapO>::find_node( top_node, "TMuiRoadMapO" );
			TMuiRoadMapO::const_iterator road_iter = road_map->get(!arm);
			while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next()){

				if(road_ptr->get()->get_ghost_flag()) continue;
				if(!road_ptr->get()->get_golden()) continue;
				if(road_ptr->get()->get_depth() < 4) continue;
				if(road_ptr->get()->get_nhit() < 6) continue;
				if(road_ptr->get()->get_road_quality() > 15.) continue;

				good_road_in_opposite_arm = true;
			}

			if (!good_road_in_opposite_arm) continue;

			return true;

		}

	} catch( exception &e ) {cout << e.what() << endl;}

	return false;
}

//__________________________________________________________________________
bool MuonTrigFilter::has_road(PHCompositeNode* top_node, int arm ) const
{

  try {

    // retrieves map of tracks
    TMuiRoadMapO* road_map = TMutNode<TMuiRoadMapO>::find_node( top_node, "TMuiRoadMapO" );
    return (road_map->get( arm ).count()>0);

  } catch( exception &e ) {cout << e.what() << endl;}
  return false;
}

//__________________________________________________________________________
float MuonTrigFilter::DG0( PHMuoTracksOut* &muo, int idx, int iroad) const
{
  //Determine if kalman projection to gap0 is available.
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
  lastIndex = 3;

  Float_t x_mut = muo->get_xpos(lastIndex,idx);
  Float_t y_mut = muo->get_ypos(lastIndex,idx);    
  Float_t z_mut = muo->get_zpos(lastIndex,idx);
  
  Float_t dxdz_mut = muo->get_px(lastIndex,idx)/muo->get_pz(lastIndex,idx);
  Float_t dydz_mut = muo->get_py(lastIndex,idx)/muo->get_pz(lastIndex,idx);
    
  // muid point at gap 0
  Float_t x_mui=muo->get_muIDOO_gap0(0, iroad, idx);
  Float_t y_mui=muo->get_muIDOO_gap0(1, iroad, idx);
  Float_t z_mui=muo->get_muIDOO_gap0(2, iroad, idx);

  return sqrt( 
    MUTOO::SQUARE( x_mui - x_mut - dxdz_mut*(z_mui - z_mut) ) +
    MUTOO::SQUARE( y_mui - y_mut - dydz_mut*(z_mui - z_mut) )
  );
}

//__________________________________________________________________________
float MuonTrigFilter::DDG0( PHMuoTracksOut* &muo, int idx, int iroad) const
{
  int lastIndex = 4;
  if (muo->get_xpos(4,idx) == 0 && muo->get_ypos(4,idx) == 0 && muo->get_zpos(4,idx) == 0)
    {
//       std::cout<<"MWGpico::DDG0.C: This data does not seem to have kalman projection to Gap0"<<std::endl
// 	       <<"Using straight line projection from ST3."<<std::endl;
      lastIndex = 3;
    }

  // retrieve mutr momentum
  Float_t px_mut = muo->get_px(lastIndex,idx);
  Float_t py_mut = muo->get_py(lastIndex,idx);
  Float_t pz_mut = muo->get_pz(lastIndex,idx);  
  Float_t p_mut = sqrt( px_mut*px_mut + py_mut*py_mut + pz_mut*pz_mut );
  
  // retrieve muid slope
  Float_t slopex_mui = muo->get_muIDOO_gap0(3,iroad, idx);
  Float_t slopey_mui = muo->get_muIDOO_gap0(4,iroad, idx);
  Float_t slopez_mui = 1;

  // negate the muid slope to match pz sign
  if( pz_mut < 0 ) {
  
    slopex_mui *= -1;
    slopey_mui *= -1;
    slopez_mui *= -1;
   
  }
   
  Float_t slope_mui = sqrt( 
    MUTOO::SQUARE( slopex_mui )+
    MUTOO::SQUARE( slopey_mui )+
    MUTOO::SQUARE( slopez_mui ) );
  
  Float_t scalar = (px_mut*slopex_mui + py_mut*slopey_mui + pz_mut*slopez_mui )/( p_mut*slope_mui );
  if(scalar>=1) scalar = 1;

  return MUTOO::RAD_TO_DEG*acos(scalar);

}
