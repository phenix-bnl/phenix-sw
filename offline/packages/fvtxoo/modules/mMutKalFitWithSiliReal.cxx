// $Id: mMutKalFitWithSiliReal.cxx,v 1.38 2017/07/13 19:12:16 phnxbld Exp $

/*!
\file mMutKalFitWithSiliReal.cxx
\brief kalman filter for mutoo tracks, and adding silicon hits.
\author Melynda Brooks
\version $Revision: 1.38 $
\date $Date: 2017/07/13 19:12:16 $
*/

// IOC
#include<TMuiRoadMapO.h>
#include<MuiCommon.hh>

// MUTOO
#include<MUTOO.h>
#include<mMutKalFitWithSiliReal.h>
#include<mMutKalFitWithSiliRealPar.h>
#include<PHException.h>
#include<PHGeometry.h>
#include <PHGslMatrix.h>
#include<PHTFileServer.h>
#include<PHTimer.h>
#include<PHTrackIntegratorKF.h>
#include<TMutClusMap.h>
#include<TMutCoordMap.h>
#include<TFvtxCoordMap.h>
#include<TFvtxPisaHitMap.h>
#include<TMutMCTrkMap.h>
#include<TMutDatabaseInit.h>
#include<TMutGeo.h>
#include<TMutKalmanUtil.h>
#include<TMutMeasureModel.h>
#include<TMutNode.h>
#include<TMutRecoPar.hh>
#include<TMutTrackUtil.h>
#include<TMutTrkPar.hh>
#include<TMutExtVtx.h>

#include <mMutFitVtx.h>
#include <mFvtxKalFit.h>

// GLOBAL
#include<PHLine.h>
#include<PHPoint.h>
#include<PHVector.h>

// MUIGEOM
#include<MuiGeomClasses.hh>

// MUTGEOM
#include <MutWire.h>

// STL/BOOST
#include<iostream>
#include<string>
#include<list>

using namespace std;

//______________________________________________________________________
mMutKalFitWithSiliReal::mMutKalFitWithSiliReal() :
  _timer(PHTimeServer::get()->insert_new("mMutKalFitWithSiliReal") ),
  _top_node( 0 ),
  _mod_par( 0 ),
  _trk_map( 0 ),
  _filename( "mMutKalFitWithSiliReal.root" ),
  _tree( 0 ),
  _tree_match( 0 ),
  _init_done( false ),
  _arm( 0 ),
  _station( 0 ),
  _gap( 0 ),
  _cathode( 0 ),
  _chi_square( 0 ),
  _distance( 0 ),
  _dangle( 0 ),
  _match_dx( -9999 ),
  _match_dy( -9999 ),
  _mc_px( 0 ),
  _mc_py( 0 ),
  _mc_pz( 0 )
{
  _res.assign( 0 );
  _res_cov.assign( 0 );
  MUTOO::TRACE("initializing module mMutKalFitWithSiliReal");
  integrator = new PHTrackIntegratorKF();
}

//______________________________________________________________________
// Destructor
mMutKalFitWithSiliReal::~mMutKalFitWithSiliReal()
{
  // No need for this: in a normal fun4all cycle, finish_evaluation was called at FvtxReco::End
//  if ( _mod_par->get_do_evaluation() ) finish_evaluation();
  _nodes.clear();
  if(integrator) delete integrator;
}

//______________________________________________________________________
void mMutKalFitWithSiliReal::init_tree()
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  // check if file exist
  if( _tree ) return;
  // Make sure we are actually supposed to be doing this
  if ( !_mod_par->get_do_evaluation() ) return;

  _filename = _mod_par->get_evaluation_file();
  PHTFileServer::get().open( _filename, "RECREATE" );
  _tree = new TTree( "residuals", "single hit residuals" );
  _tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
  _tree->Branch( "gap", &_gap, "gap/I", BUFFER_SIZE );
  _tree->Branch( "cathode", &_cathode, "cathode/I", BUFFER_SIZE );
  _tree->Branch( "res", &_res[0], "res[3]/D", BUFFER_SIZE );
  _tree->Branch( "res_cov", &_res_cov[0], "res_cov[3]/D", BUFFER_SIZE );
  _tree->Branch( "chi_square", &_chi_square, "chi_square/D", BUFFER_SIZE );
  _tree->SetAutoSave( AUTO_SAVE );

  _tree_match = new TTree( "match", "fvtx mutr match" );
  _tree_match->Branch( "arm", &_match_arm, "arm/I", BUFFER_SIZE );
  _tree_match->Branch( "distance", &_distance, "distance/D", BUFFER_SIZE );
  _tree_match->Branch( "dangle", &_dangle, "dangle/D", BUFFER_SIZE );
  _tree_match->Branch( "match_dx", &_match_dx, "match_dx/D", BUFFER_SIZE );
  _tree_match->Branch( "match_dy", &_match_dy, "match_dy/D", BUFFER_SIZE );
  _tree_match->Branch( "mc_px", &_mc_px, "mc_px/D", BUFFER_SIZE );
  _tree_match->Branch( "mc_py", &_mc_py, "mc_py/D", BUFFER_SIZE );
  _tree_match->Branch( "mc_pz", &_mc_pz, "mc_pz/D", BUFFER_SIZE );
  _tree_match->Branch( "extrapx", &_extrapx, "extrapx/D", BUFFER_SIZE );
  _tree_match->Branch( "extrapy", &_extrapy, "extrapy/D", BUFFER_SIZE );
  _tree_match->Branch( "extrapz", &_extrapz, "extrapz/D", BUFFER_SIZE );
  _tree_match->Branch( "fvtxx", &_fvtxx, "fvtxx/D", BUFFER_SIZE );
  _tree_match->Branch( "fvtxy", &_fvtxy, "fvtxy/D", BUFFER_SIZE );
  _tree_match->Branch( "fvtxz", &_fvtxz, "fvtxz/D", BUFFER_SIZE );
  _tree_match->Branch( "mut_px", &_mut_px, "mut_px/D", BUFFER_SIZE );
  _tree_match->Branch( "mut_py", &_mut_py, "mut_py/D", BUFFER_SIZE );
  _tree_match->Branch( "mut_pz", &_mut_pz, "mut_pz/D", BUFFER_SIZE );
  _tree_match->Branch( "z_vertex", &_z_vertex, "z_vertex/D", BUFFER_SIZE );
  _tree_match->SetAutoSave( AUTO_SAVE );
}

//______________________________________________________________________
void mMutKalFitWithSiliReal::finish_evaluation( void )
{

  if( !_tree ) return;

  if ( _mod_par->get_do_evaluation() )
    {
      MUTOO::TRACE("mMutKalFitWithSiliReal::finish_evaluation");
      PHTFileServer::get().write( _filename );
    }
  return;
}

//______________________________________________________________________
// Event method.
PHBoolean mMutKalFitWithSiliReal::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();

  try {

    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // initialize tree if requested
    if( _mod_par->get_do_evaluation() && !_init_done ) init_tree();

    _init_done = true;

    // loop over tracks
    track_loop();

  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _trk_map->print();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();

  return True;
}

//______________________________________________________________________
void mMutKalFitWithSiliReal::set_interface_ptrs(PHCompositeNode* top_node)
{
  _top_node = top_node;
  _mod_par = TMutNode<mMutKalFitWithSiliRealPar>::find_node(top_node,"mMutKalFitWithSiliRealPar");
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  _fvtx_trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
}

//______________________________________________________________________
void mMutKalFitWithSiliReal::track_loop()
{
  // Loop over all track in the track map
  TMutTrkMap::iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::pointer trk_ptr = trk_iter.next())
  try {

    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
      cout << "mMutKalFitWithSiliReal::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << endl;

    // perform basic checks on the track
    if( !accept_track( trk_ptr ) ) {
      continue;
    }

    TMutKalmanFilter kalman_filter;
    kalman_filter.set_verbosity( _mod_par->get_verbosity() );

    // See if any fvtx tracks to associate with this muon track:
    if ( !associate_fvtx_trk( trk_ptr) ) continue;

    // Loop over each associated fvtx track, fit and...
    unsigned long best_trk_ptr = 0;

    bool first = true;
    TMutTrkPar trk_par_best;
    TMutTrkPar trk_par_fit;

    TFvtxTrkMap::key_iterator fvtx_trk_iter = trk_ptr->get()->get_associated<TFvtxTrk>();
    while(TFvtxTrkMap::pointer fvtx_trk_ptr = fvtx_trk_iter.next()){

        //ghost and bad track rejections
        if (fvtx_trk_ptr->get()->get_ghost()) continue;

      // initialize starting parameters and nodes
      init( trk_ptr, fvtx_trk_ptr, kalman_filter );

      // perform full fit
      if( !fit_nodes( kalman_filter ) ) {
        if( _mod_par->get_verbosity() >= MUTOO::SOME )
          cout << "mMutKalFitWithSiliReal::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << " fit failed." << endl;
        // trk_ptr->get()->set_kalman_fail();
        continue;
      }

      // redo full fit taking anode corrections into account
      if( _mod_par->get_use_anodes() ) {

        calculate_anode_corrections();
        if( !fit_nodes( kalman_filter ) ) {
          if( _mod_par->get_verbosity() >= MUTOO::SOME )
            cout << "mMutKalFitWithSiliReal::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << " refit failed." << endl;
          //trk_ptr->get()->set_kalman_fail();
          continue;
        }

      }

      // fill the track with node information if this fit is better than others.
      if (check_track( trk_ptr, fvtx_trk_ptr, first, trk_par_fit ) ){
        trk_par_best = trk_par_fit;
        best_trk_ptr = fvtx_trk_ptr->get()->get_key().get_obj_key();
      }

      first = false;

    }

    // disassociate the fvtx tracks which were not selected as the best track:
    fvtx_trk_iter.reset();
    while(TFvtxTrkMap::pointer fvtx_trk_ptr = fvtx_trk_iter.next()){
      if (fvtx_trk_ptr->get()->get_key().get_obj_key() != best_trk_ptr )
        PHKey::disassociate(fvtx_trk_ptr, trk_ptr);
    }

    // extrapolate track parameters toward vertex
    TMutTrkPar extrap_trk_par;
    extrapolate_to_vertex( trk_par_best, extrap_trk_par );

    fvtx_trk_iter.reset();
    while(TFvtxTrkMap::pointer fvtx_trk_ptr = fvtx_trk_iter.next())
      {
	if (fvtx_trk_ptr->get()->get_key().get_obj_key() == best_trk_ptr ){
	  extrap_trk_par.set_chi_square(trk_par_best.get_chi_square());
          fvtx_trk_ptr->get()->set_trk_par_mutr( extrap_trk_par );
        }

	if( _mod_par->get_verbosity() >= MUTOO::MAX )
	  {
	    fvtx_trk_ptr->get()->get_trk_par_mutr()->print();
	    MUTOO::PRINT( cout, "mMutKalFitWithSiliReal::track_loop - trk_par_mutr");
	  }
        // fvtx_trk_ptr->get()->set_trk_par_mutr( *trk_ptr->get()->get_trk_par_vtx() );
        // cout << "mMutKalfitWithSiliReal chi2 " << trk_ptr->get()->get_trk_par_vtx()->get_chi_square() << " w_chi_pdf " << trk_ptr->get()->get_w_chi_square_pdf() << endl;
    }

    // extrapolate track parameters toward muid first gap
    if( _mod_par->get_extrapolate_to_muid() )
      extrapolate_to_muid( trk_ptr );

    // fill evaluation tree if requested
    // For some reason this bombs out on the node access during certain events.  Need
    // to track down:
    //if( _mod_par->get_do_evaluation() ) fill_tree();

    // set track status
    // if( !trk_ptr->get()->get_kalman_fail() ) trk_ptr->get()->set_kalman_fit();
  } catch( exception &e ) { cout << "mMutKalFitWithSiliReal::track_loop - " << e.what() << endl; }

}

//______________________________________________________________________
bool mMutKalFitWithSiliReal::accept_track( TMutTrkMap::const_pointer trk_ptr )
{
  if(trk_ptr->get()->get_ghost()) return false;
  
  if(!trk_ptr->get()->get_reco_success()) return false;

  // check estimate bit (need for starting point)
  if( trk_ptr->get()->get_no_estimate() ) {
    trk_ptr->get()->set_no_fit();
    if( _mod_par->get_verbosity()>= MUTOO::SOME )
      cout << "mMutKalFitWithSiliReal::accept_track - trk="
      << trk_ptr->get()->get_key().get_obj_key()
      << " NO_ESTIMATE - setting NO_FIT.\n";
    return false;
  }
  // check low momentum bit
  if( trk_ptr->get()->get_low_mom() ) {
    trk_ptr->get()->set_no_fit();

    if( _mod_par->get_verbosity()>= MUTOO::SOME )
    {
      cout << "mMutKalFitWithSiliReal::accept_track - trk="
        << trk_ptr->get()->get_key().get_obj_key()
        << " LOW_MOM - setting NO_FIT.\n";
    }
    return false;
  }

  // check number of hits in cathode
  TMutCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  if ( coord_iter.count() < _mod_par->get_min_n_coord() )  {
    trk_ptr->get()->set_reco_min_hits();
    trk_ptr->get()->set_no_fit();

    //    if( _mod_par->get_verbosity()>= MUTOO::SOME )
    cout << "mMutKalFitWithSiliReal::accept_track - trk="
	 << trk_ptr->get()->get_key().get_obj_key()
	 << " not enough hits (" << coord_iter.count() << "<" << _mod_par->get_min_n_coord() << ").\n";

    return false;
  }

  return true;

}

//______________________________________________________________________
bool mMutKalFitWithSiliReal::associate_fvtx_trk( TMutTrkMap::pointer trk_ptr )
{

  int ntracks = 0;
  // check fvtx track map is present:
  if( !_fvtx_trk_map ) return false;

  // Calculate theta/phi of muon track so that we can pre-select FVTX tracks which
  // are in the same theta/phi region:
  double mu_theta, mu_phi;
  double mu_x, mu_y, mu_z;

  TMutTrkPar trk_par(*trk_ptr->get()->get_trk_par_station(MUTOO::Station1));
  mu_x = trk_par.get_x();
  mu_y = trk_par.get_y();
  mu_z = trk_par.get_z();
  mu_phi = atan2(mu_y, mu_x);
  mu_theta = atan2(sqrt(mu_x*mu_x + mu_y*mu_y), mu_z);

  // Loop over all track in the track map
  TFvtxTrkMap::iterator fvtx_trk_iter = _fvtx_trk_map->range();
  while(TFvtxTrkMap::pointer fvtx_trk_ptr = fvtx_trk_iter.next()) {

    //cout << "fvtx arm " << fvtx_trk_ptr->get()->get_arm() << ", muon arm = " << trk_ptr->get()->get_arm() << endl;
    if (fvtx_trk_ptr->get()->get_arm() != trk_ptr->get()->get_arm()) continue;

    // Place theta, phi cut before trying to exrapolate tracks:
    double fvtx_theta, fvtx_phi;
    double fvtx_x, fvtx_y, fvtx_z;
    if (fvtx_trk_ptr->get()->get_reco_success()){
      fvtx_x = fvtx_trk_ptr->get()->get_trk_par_list()->back().get_x();
      fvtx_y = fvtx_trk_ptr->get()->get_trk_par_list()->back().get_y();
      fvtx_z = fvtx_trk_ptr->get()->get_trk_par_list()->back().get_z();
      fvtx_phi = atan2(fvtx_y, fvtx_x);
      fvtx_theta = atan2(sqrt(fvtx_x*fvtx_x + fvtx_y*fvtx_y), fvtx_z);
    }
    else{
      continue;
    }

    double dphi = mu_phi - fvtx_phi;
    dphi = 0.5*atan2(sin(2*dphi),cos(2*dphi));
    if ( (fabs(mu_theta - fvtx_theta) >  _mod_par->get_fvtx_mutr_theta_cut()) ||
         (fabs(dphi) >  _mod_par->get_fvtx_mutr_phi_cut()) ) continue;

    if ( fvtx_mutr_match(trk_ptr, fvtx_trk_ptr) ){
      PHKey::associate( trk_ptr, fvtx_trk_ptr );
      ntracks++;
    }

  }

  if (ntracks > 0) return true;
  else return false;

}

//______________________________________________________________________
bool mMutKalFitWithSiliReal::fvtx_mutr_match(
  TMutTrkMap::const_pointer trk_ptr,
  TFvtxTrkMap::const_pointer fvtx_trk_ptr )
{
  integrator->clear();
  double fvtx_x(0);
  double fvtx_y(0);
  double fvtx_z(0);
  //double fvtx_px(0);
  //double fvtx_py(0);
  //double fvtx_pz(0);

  bool found = false;

  //const vector<TMutTrkPar>& trk_par_list( *fvtx_trk_ptr->get()->get_trk_par_list() );

  /*
  // This block is used if we want to always match muon tracks at the last FVTX station, which also means
  // that a station 3 hit must be present in the track:
  for( vector<TMutTrkPar>::const_iterator iter = trk_par_list.begin(); iter != trk_par_list.end(); iter++ ){
    if (fabs(iter->get_z()) > 35.0 && fabs(iter->get_z()) < 41.0){
      found = true;
      fvtx_x = iter->get_x();
      fvtx_y = iter->get_y();
      fvtx_z = iter->get_z();
      fvtx_px = iter->get_px();
      fvtx_py = iter->get_py();
      fvtx_pz = iter->get_pz();
    }
  }
  */

  // Allow for option to do muon track matching at an intermediate plane, not requiring
  // the track to necessarily have a station 3 hit:
  // extrapolate to track matching plane:

  // zref should be at negative z for south arm, positive z for north arm:
  float zref = (2*trk_ptr->get()->get_arm() - 1 ) * _mod_par->get_fvtx_mutr_proximity_zref();


  if (fvtx_trk_ptr->get()->get_reco_success()){
    integrator->initialize( fvtx_trk_ptr->get()->get_trk_par_list()->back() );
    integrator->extrapolate ( zref );
  }
  else{
    return false;
  }

  TMutTrkPar fvtx_extrap_trk_par;
  if (integrator->get_error()){

    //cout << "in mFvtxKalFitWithSiliReal extraploation to FVTX failed" << endl;
    return false;

  } else {
    found = true;
    integrator->finish( fvtx_extrap_trk_par );

  }

  if (!found){
    return false;
  }

  // logic is:
  // 1/ get the track parameters at MuTR station 1
  // 2/ extrapolate them backwards to the vertex position
  // 3/ fit together with the vertex when available
  // 4/ extrapolate forward to Fvtx last plane

  // get track parameters at station 1
  TMutTrkPar trk_par(*trk_ptr->get()->get_trk_par_station(MUTOO::Station1));
  if( _mod_par->get_use_vtx() )
  {
    bool error( false );
	  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
	  if( !error )
	  {

	    // fit together with the vertex
	    mMutFitVtx::Fitter vertex_fitter;
	    vertex_fitter.add_track( trk_ptr );

	    // disable usage of X and Y
	    vtx.setX(0);
	    vtx.setY(0);

	    // set vertex errors manually
	    PHPoint vtx_error( _mod_par->get_vtx_sigma_xy(), _mod_par->get_vtx_sigma_xy(), 
                               _mod_par->get_vtx_sigma_z() );
	    //PHPoint vtx_error( 5.0, 5.0, 25.0);

	    // add vertex information to fitter and do the fit
	    vertex_fitter.add_vertex( vtx, vtx_error );
	    vertex_fitter.fit();

	    // store fit result in trk_par
	    trk_par = TMutTrkPar(
	      vertex_fitter.get_vtx_x(),
	      vertex_fitter.get_vtx_y(),
	      vertex_fitter.get_vtx_z(),
	      vertex_fitter.get_px(0),
	      vertex_fitter.get_py(0),
	      vertex_fitter.get_pz(0),
	      static_cast<int>(trk_ptr->get()->get_charge() ),
	      vertex_fitter.get_chisquare() );
	    
	  }

          _z_vertex = vtx.getZ();
 
  }
  // extrapolate to track matching plane:
  //PHTrackIntegratorKF integrator;
  integrator->clear();
  integrator->initialize( trk_par );
  //Use this if extrapolating to the last FVTX station:
  //integrator.extrapolate ( fvtx_z );
  //Use this if extrapolating to an intermediate scattering plane:
  integrator->extrapolate ( zref );

  TMutTrkPar extrap_trk_par;
  if (integrator->get_error()){

    cout << "in mFvtxKalFitWithSiliReal extraploation to FVTX failed" << endl;
    extrap_trk_par = trk_par;

  } else {

    integrator->finish( extrap_trk_par );

  }

  //Either use FVTX last station or use extrapolation to intermediate plane:
  //PHVector direction_fvtx(fvtx_px, fvtx_py, fvtx_pz);
  PHVector direction_fvtx(fvtx_extrap_trk_par.get_px(), fvtx_extrap_trk_par.get_py(), fvtx_extrap_trk_par.get_pz());

  PHVector direction_extrap(extrap_trk_par.get_px(), extrap_trk_par.get_py(), extrap_trk_par.get_pz());
  double dangle = direction_fvtx.angle( direction_extrap );

  //Either calculate distance at FVTX last station or at extrapolated position:
  //double distance = sqrt( FVTXOO::SQUARE(extrap_trk_par.get_x() - fvtx_x) +
  //  FVTXOO::SQUARE(extrap_trk_par.get_y() - fvtx_y));
  double distance = sqrt( FVTXOO::SQUARE(extrap_trk_par.get_x() - fvtx_extrap_trk_par.get_x()) +
    FVTXOO::SQUARE(extrap_trk_par.get_y() - fvtx_extrap_trk_par.get_y()));

  fvtx_x = fvtx_extrap_trk_par.get_x();
  fvtx_y = fvtx_extrap_trk_par.get_y();
  fvtx_z = fvtx_extrap_trk_par.get_z();

  _match_arm = (int)trk_ptr->get()->get_arm();
  _distance = distance;
  _match_dx = extrap_trk_par.get_x() - fvtx_x;
  _match_dy = extrap_trk_par.get_y() - fvtx_y;

  _extrapx = extrap_trk_par.get_x();
  _extrapy = extrap_trk_par.get_y();
  _extrapz = extrap_trk_par.get_z();
  _fvtxx = fvtx_x;
  _fvtxy = fvtx_y;
  _fvtxz = fvtx_z;
  _mut_px = trk_par.get_px();
  _mut_py = trk_par.get_py();
  _mut_pz = trk_par.get_pz();

  TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();
  while( TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next() ) {
    _mc_px = mc_trk_ptr->get()->get_px_orig();
    _mc_py = mc_trk_ptr->get()->get_py_orig();
    _mc_pz = mc_trk_ptr->get()->get_pz_orig();
  }
  _dangle = dangle;

  if ( _tree_match ) _tree_match->Fill();


//  // check with the generically allowed proximity_cut
//  if (distance < _mod_par->get_fvtx_mutr_proximity_cut()) return true;

  double mom = sqrt( FVTXOO::SQUARE(extrap_trk_par.get_px()) + FVTXOO::SQUARE(extrap_trk_par.get_py()) +
    FVTXOO::SQUARE(extrap_trk_par.get_pz()) );
  double distance_cut = get_fvtx_mutr_match_cut( mom );
  /*
  std::cout << "mMutKalFitWithSiliReal::fvtx_mutr_match - distance : " << _distance << " dangle : " << _dangle << " mom " << mom << " cut : " << distance_cut << endl;
  */
  if (distance < distance_cut ) return true;
  else return false;

}

//______________________________________________________________________
double mMutKalFitWithSiliReal::get_fvtx_mutr_match_cut( double mom )
{
  return 10.0;
  /*
  if ( mom < 3 ) return 8.0;
  else if ( mom < 4 ) return 7.0;
  else if ( mom < 5 ) return 6.0;
  else if ( mom < 6 ) return 5.0;
  else if ( mom < 8 ) return 4.0;
  else return 3.0;
  */
}

//______________________________________________________________________
void mMutKalFitWithSiliReal::init(
  TMutTrkMap::pointer trk_ptr,
  TFvtxTrkMap::pointer fvtx_trk_ptr,
  TMutKalmanFilter& kalman_filter
  )

{

  integrator->clear();
  // make sure list of nodes is empty
  _nodes.clear();

  // stores reference to starting parameters localy and initialize
  TMutKalmanFilter::TrkPar& start_kf( kalman_filter.get_starting_parameters() );
  start_kf._direction = ( trk_ptr->get()->get_arm() == MUTOO::South ) ? -1:1;

  /*
  adds coordinate associated to the track to the kalman filter object
  */

  // retrieve coordinates associated to the track
  list<TMutCoordMap::value_type> coord_list;
  typedef list< TMutCoordMap::value_type >::iterator coord_iterator;

  TMutCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()) coord_list.push_back( *coord_ptr );

  // sort the list of coordinates from vertex to muid
  coord_list.sort( coord_less_ftor() );

  // add measurements
  for( coord_iterator coord = coord_list.begin(); coord != coord_list.end();  coord++ )
  {

    // look if it is a desactivated detectors
    if(_mod_par->get_desactivated(
      coord->get()->get_arm(),
      coord->get()->get_station(),
      coord->get()->get_gap(),
      coord->get()->get_cathode()))
    {

      if( _mod_par->get_verbosity() >= MUTOO::ALOT )
        cout
        << "mMutKalFitWithSiliReal::init - coord ["
        << coord->get()->get_arm() << ","
        << coord->get()->get_station() << ","
        << coord->get()->get_gap() << ","
        << coord->get()->get_cathode() << "]"
        << " ignored" << endl;
      continue;

    }

    // create new node
    KalmanFilterNode node( &(*coord) );

    /*
    weight covariance matrix. All errors are devided by the weight
    the smaller the weight, the smaller the corresponding station contributes to the chisquare
    */
    node.get_measurement_cov() *= MUTOO::SQUARE( 1.0/_mod_par->get_mutr_detector_weight(
      coord->get()->get_arm(),
      coord->get()->get_station(),
      coord->get()->get_gap(),
      coord->get()->get_cathode() ) );

    // add node
    _nodes.push_back( node );

  }

  // Add silicon hits from the FvtxTrk associated with this track


  // retrieve coordinates associated to the track
  list<TFvtxCoordMap::value_type> fvtx_coord_list;
  typedef list< TFvtxCoordMap::value_type >::iterator fvtx_coord_iterator;

  TFvtxCoordMap::key_iterator fvtx_coord_iter = fvtx_trk_ptr->get()->get_associated<TFvtxCoord>();
  while(TFvtxCoordMap::pointer fvtx_coord_ptr = fvtx_coord_iter.next()) fvtx_coord_list.push_back( *fvtx_coord_ptr );

  // sort the list of coordinates from vertex to muid
  fvtx_coord_list.sort( fvtx_coord_less_ftor() );

  // add measurements
  for( fvtx_coord_iterator fvtx_coord = fvtx_coord_list.begin(); fvtx_coord != fvtx_coord_list.end();	fvtx_coord++ )
  {

    // create new node
    KalmanFilterNode noder( &(*fvtx_coord), 0 );

    // add node
    _nodes.push_back( noder );

    // create new node
    KalmanFilterNode nodephi( &(*fvtx_coord), 1 );

    // add node
    _nodes.push_back( nodephi );

  }

  if ( _mod_par->get_use_svx()){

    const TMutTrkPar trk_par( *fvtx_trk_ptr->get()->get_trk_par() );

    TFvtxSvxClusterMap::key_iterator key_iter( fvtx_trk_ptr->get()->get_associated<TFvtxSvxCluster>() );
    while( TFvtxSvxClusterMap::const_pointer ptr = key_iter.next() )
    {

      /*
      int xyflag = 0;
      KalmanFilterNode nodex( *ptr->get()->get_cluster(), xyflag, _mod_par->get_vtx_phi_error(),
        _mod_par->get_vtx_r_error(), _mod_par->get_vtx_z_error(), trk_par.get_dxdz(), trk_par.get_dydz());
      _nodes.push_back( nodex );

      xyflag = 1;
      KalmanFilterNode nodey( *ptr->get()->get_cluster(), xyflag, _mod_par->get_vtx_phi_error(),
        _mod_par->get_vtx_r_error(), _mod_par->get_vtx_z_error(), trk_par.get_dxdz(), trk_par.get_dydz());
      _nodes.push_back( nodey );
      */

      int phiflag = 0;
      double drdz = sqrt(trk_par.get_dxdz()*trk_par.get_dxdz() + trk_par.get_dydz()*trk_par.get_dydz());
      KalmanFilterNode nodex( *ptr->get()->get_cluster(), phiflag, _mod_par->get_vtx_phi_error(),
        _mod_par->get_vtx_r_error(), _mod_par->get_vtx_z_error(), drdz);
      _nodes.push_back( nodex );

      phiflag = 1;
      KalmanFilterNode nodey( *ptr->get()->get_cluster(), phiflag, _mod_par->get_vtx_phi_error(), _mod_par->get_vtx_r_error(), _mod_par->get_vtx_z_error(), drdz);
      _nodes.push_back( nodey );
    }
  }


  // get track associated road, if any
  TMuiRoadMapO::key_iterator road_iter( trk_ptr->get()->get_associated<TMuiRoadO>() );

  /*
  adds muid clusters associated to the track to the kalman filter object
  if requested and if only one associated road is found
  */
  if( _mod_par->get_use_muid() && road_iter.count() > 0 )
  {

    // set association to best muid road clusters
    associate_road( trk_ptr );

    list<TMuiClusterMapO::value_type> mui_clus_list;
    typedef list< TMuiClusterMapO::value_type >::iterator mui_clus_iterator;

    TMuiClusterMapO::key_iterator clus_iter = road_iter->get()->get_associated<TMuiClusterO>();
    while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
      mui_clus_list.push_back( *clus_ptr );

    mui_clus_list.sort( mui_clus_less_ftor() );

    // add measurement
    for( mui_clus_iterator clus = mui_clus_list.begin(); clus != mui_clus_list.end(); clus++ )
      _nodes.push_back( KalmanFilterNode( &(*clus) ) );

    /*
    if fit starts from muid, gets the muid road track parameters (x,y,dxdz,dydz)
    use the momentum from the track
    */
    start_kf._z = mui_clus_list.back().get()->get_mean_z();
    const TMutTrkPar& trk_par( *trk_ptr->get()->get_trk_par_station( MUTOO::Station3 ) );

    // make the extrapolation from trk_par to gap 0

    integrator->initialize( trk_par );
    integrator->extrapolate( start_kf._z );
    TMutTrkPar trk_par_extrap;
    if( integrator->get_error() ) {
      cout << "mMutKalFitWithSiliReal::init - error extrapolating MC from trk_par to gap0" <<  endl;
      trk_par_extrap = trk_par;
    } else integrator->finish( trk_par_extrap );

    start_kf._state_kf = TMutKalmanUtil::get_state_vector_kf( trk_par_extrap );

  } else {

    // set z from last coordinate
    start_kf._z = coord_list.back().get()->get_mean_z();

    // extrapolate track parameters to _z
    const TMutTrkPar trk_par( *trk_ptr->get()->get_trk_par() );
    TMutTrkPar trk_par_extrap(
      trk_par.get_x() + trk_par.get_dxdz()*(start_kf._z - trk_par.get_z()),
      trk_par.get_y() + trk_par.get_dydz()*(start_kf._z - trk_par.get_z()),
      start_kf._z,
      trk_par.get_px(),
      trk_par.get_py(),
      trk_par.get_pz(),
      trk_par.get_charge() );

    start_kf._state_kf = TMutKalmanUtil::get_state_vector_kf( trk_par_extrap );

  }

  // in both cases, use the default covariance matrix for error
  start_kf._covar_kf(0, 0) = MUTOO::SQUARE( _mod_par->get_momentum_resolution()*start_kf._state_kf(0,0) );  // error on c/p
  start_kf._covar_kf(1, 1) = MUTOO::SQUARE( _mod_par->get_angular_resolution() );  // error on dx/dz
  start_kf._covar_kf(2, 2) = MUTOO::SQUARE( _mod_par->get_angular_resolution() );  // error on dy/dz
  start_kf._covar_kf(3, 3) = MUTOO::SQUARE( _mod_par->get_position_resolution() ); // error on x
  start_kf._covar_kf(4, 4) = MUTOO::SQUARE( _mod_par->get_position_resolution() ); // error on y

  // sort nodes from vertex to muid
  _nodes.sort();

  // dump all nodes
  if( _mod_par->get_verbosity() >= MUTOO::MAX )
  {
    for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ ) {
      MUTOO::PRINT( cout, "mMutKalFitWithSiliReal::init_nodes - node");
      cout << "measurement" << node->get_measurement();
      cout << "covariance" << node->get_measurement_cov();
      cout << "h" << node->get_h();
    }
  }

  if( _mod_par->get_verbosity() >= MUTOO::SOME ) {
    cout << "mMutKalFitWithSiliReal::init - trk="
      << trk_ptr->get()->get_key().get_obj_key()
      << " start parameters"
      << endl;
    start_kf.print();
  }


}

//_________________________________________________________________
void mMutKalFitWithSiliReal::calculate_anode_corrections( void )
{

  // calculate anode corrections if required; redo the fit
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node ++ ) {

    // check node has associated coordinate
    if( !node->has_coord() ) continue;

    //! calculate anode correction from associated coord
    PHPoint trk_point(
      node->get_smoothed()._state_kf(3,0),
      node->get_smoothed()._state_kf(4,0),
      node->get_smoothed()._z );

    MutWire *wire = TMutGeo::find_nearest_anode( node->get_arm(), trk_point );
    if( wire ) {

      PHLine line(wire->getGlobalPositionBegin(), wire->getGlobalPositionEnd());
      PHPoint projected( PHGeometry::closestApproachLinePoint( line, trk_point ) );

      const PHGslMatrix &h( node->get_h() );
      node->get_measurement()(0,0) -= ( projected.getX() - trk_point.getX() )*h( 0, 3 ) + ( projected.getY() - trk_point.getY() )*h( 0, 4 );

    }
  }

}


//___________________________________________________________________
bool mMutKalFitWithSiliReal::fit_nodes( TMutKalmanFilter &kalman_filter )
{

  // loop over max number of iterations
  unsigned int iteration( 0 );
  double old_chi_square = 1e6;
  for( ; iteration < _mod_par->get_max_iterations(); iteration++ )
  {

    double chi_square( 0 );

    // reset node status
    reset_nodes();

    // perform prediction and filter using reverse iterator
    for( reverse_node_iterator node = _nodes.rbegin(); node != _nodes.rend(); node ++ )
    {
      if( kalman_filter.predict( *node ) && kalman_filter.filter( *node ) ) chi_square += node->get_chi_square();
      else {
        if( _mod_par->get_verbosity()>= MUTOO::SOME )
          cout << "mMutKalFitWithSiliReal::fit_nodes - prediction/filter failed.\n";
        return false;
      }
    }

    // initialize smoothing starting from first node (closest to vertex)
    bool first_node( true );
    for( node_iterator node = _nodes.begin(); node != _nodes.end(); node ++ )
    {
      if( first_node ) {
        first_node = false;
        kalman_filter.initialize_smoother( *node );
      } else if( !kalman_filter.smooth( *node ) ) {
        cout << "mMutKalFitWithSiliReal::fit_nodes - prediction/filter failed.\n";
        return false;
      }
    }

    /*
    update starting parameters
    update is done before stop coundition check since used at refit stage event
    after the last iteration.
    Only position/momentum and charge should be updated.
    */

    // retrieves last node from the list
    KalmanFilterNode& last_node( _nodes.back() );

    // stores reference to starting parameters localy
    TMutKalmanFilter::TrkPar& start_kf( kalman_filter.get_starting_parameters() );
    start_kf._state_kf = last_node.get_smoothed()._state_kf;
    start_kf._z = last_node.get_smoothed()._z;
    start_kf._direction = last_node.get_smoothed()._direction;

    if( _mod_par->get_update_cov_matrix() ) {

      /*
      this is mathematicaly wrong since starting parameter
      covariance matrix should be kept large.
      Unfortunately, this improves resolution a lot
      */
      start_kf._covar_kf = last_node.get_smoothed()._covar_kf;

    } else {

      /*
      update the first term of the cov matrix using new p_tot
      since the meaningfull error given as argument  is dp_tot/p_tot
      whereas the error used in the matrix is c/p_tot
      */
      start_kf._covar_kf(0, 0) = MUTOO::SQUARE( _mod_par->get_momentum_resolution()*start_kf._state_kf(0,0) );  // error on c/p

    }

    if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
      MUTOO::TRACE( "mMutKalFitWithSiliReal::fit_nodes - start parameters [updated]");
      start_kf.print();
    }

    // dump chisquare
    if( _mod_par->get_verbosity()>=MUTOO::SOME )
      cout << "mMutKalFitWithSiliReal::fit_nodes - iteration " << iteration << ", chi2=" << chi_square << endl;

    if( iteration && fabs( chi_square - old_chi_square ) < _mod_par->get_chi_cut()*old_chi_square ) break;
    old_chi_square = chi_square;

  }

  if( _mod_par->get_verbosity() >= MUTOO::SOME )
    cout << "mMutKalFitWithSiliReal::fit_nodes - " << iteration << " iterations.\n";

  return true;
}

//___________________________________________
bool mMutKalFitWithSiliReal::check_track( TMutTrkMap::pointer trk_ptr, TFvtxTrkMap::pointer fvtx_trk_ptr, bool first,
  TMutTrkPar& trk_par_best )
{

  if( _mod_par->get_verbosity() >= MUTOO::SOME )
    cout << "mMutKalFitWithSiliReal::check_track - trk=" << trk_ptr->get()->get_key().get_obj_key() << endl;

  double chi_square_w( 0 );
  double chi_square_w_pdf( 0 );
  static double chi_square_w_pdf_best( 1E6 ) ;
  static TMutTrkPar best_trk_par;

  TMutTrkPar trk_par_front;

  // loop over nodes
  bool first_node = true;

  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ ) {

    // add smoothed trk_par and associated reco_par into track
    PHVector mom_mutoo = TMutKalmanUtil::mom_kalman_to_mutoo(
      trk_ptr->get()->get_arm(),
      PHVector(
      node->get_smoothed()._state_kf( 0,0 ),
      node->get_smoothed()._state_kf( 1,0 ),
      node->get_smoothed()._state_kf( 2,0 )
      )
      );

    TMutTrkPar trk_par(
      node->get_smoothed()._state_kf( 3, 0 ),
      node->get_smoothed()._state_kf( 4, 0 ),
      node->get_z(),
      mom_mutoo.getX(),
      mom_mutoo.getY(),
      mom_mutoo.getZ(),
      ( node->get_smoothed()._state_kf( 0,0 ) < 0 ) ? -1:1,
      node->get_chi_square() );

    PHGslMatrix cov_mutoo( TMutKalmanUtil::cov_kalman_to_mutoo(
      trk_par.get_charge(),
      node->get_smoothed()._covar_kf,
      mom_mutoo ) );

    for( unsigned int i=0; i<TMutTrkPar::COVAR_ROW; i++ )
      for( unsigned int j=0; j<TMutTrkPar::COVAR_ROW; j++ )
      trk_par.set_covar( i, j, cov_mutoo(i,j) );

    //Don't push results into MuTr track: 07/26/12
    // only for filling trk_par
    //    trk_ptr->get()->push_trk_par( trk_par );
    
    if (first_node){
      trk_par_front = trk_par;
      first_node = false;
    }


    // add node chi_square to total
    chi_square_w += node->get_chi_square();

  }

  if (_nodes.size() - 5 > 0) chi_square_w_pdf = chi_square_w/(_nodes.size() - 5);
  else chi_square_w_pdf = 1000;

  // Only use these track fit params if they are better than last associated track:
  if (!first && chi_square_w_pdf_best < chi_square_w_pdf){
     //trk_par_best = best_trk_par;
     return false;
  }
  else{

    // If this fit is better than the last one, store the results:
    chi_square_w_pdf_best = chi_square_w_pdf;
    best_trk_par = trk_par_front;
    trk_par_best = best_trk_par;
    trk_par_best.set_chi_square(chi_square_w_pdf);

    //Want to store fit results in fvtx-->trk_par_mutr, not directly in the mutr track: 07/26/12

    // Only for filling trk_par:
    //trk_ptr->get()->set_w_chi_square(chi_square_w);
    //trk_ptr->get()->set_r_chi_square(0);
    //trk_ptr->get()->set_ndf( _nodes.size() - 5 );
    //trk_ptr->get()->set_hit_pattern ( trk_ptr->get()->TMutTrk::get_hit_pattern() );

    // set track parameters in first gap
    //trk_ptr->get()->set_trk_par(  trk_ptr->get()->get_trk_par_list()->front() );

    return true;
  }
}

//_____________________________________________________________________________
bool mMutKalFitWithSiliReal::extrapolate_to_vertex( TMutTrkPar trk_par, TMutTrkPar& extrap_trk_par )
{

  // extrapolate track parameters from first gap to z located upstream of absorber (z_ref)
  integrator->clear();
  double z_ref;

  bool error( false );
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );

  if (error){
    z_ref = _mod_par->get_z_reference();
  }
  else{
    z_ref = vtx.getZ();
  }

  if (trk_par.get_pz()==0) return false;

  if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
    MUTOO::PRINT( cout, "mMutKalFitWithSiliReal::extrapolate_to_vertex - before extrapolation");
    TMutKalmanUtil::print_trk_par_kf( trk_par );
  }



  integrator->initialize( trk_par );
  integrator->extrapolate( z_ref );
  if( integrator->get_error() ) {
    if( _mod_par->get_verbosity() >= MUTOO::SOME ) {
      cout
        << "in mMutKalFitWithSiliReal::extrapolate_to_vertex - extrapolation towards vertex failed ("
        << trk_par.get_z() << "->" << z_ref << ")\n";
    }
    //trk_ptr->get()->set_kalman_fail();
    return false;

  } else {
    //TMutTrkPar extrap_trk_par;
    integrator->finish( extrap_trk_par );
    if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
      MUTOO::PRINT( cout, "mMutKalFitWithSiliReal::extrapolate_to_vertex - trk_par_vtx");
      extrap_trk_par.print();
    }
  }

  return true;

}

//_____________________________________________________________________________
bool mMutKalFitWithSiliReal::extrapolate_to_muid( TMutTrkMap::pointer trk_ptr )
{
  integrator->clear();
  // extrapolate track parameters from last gap to z of first MuID gap
  double z_ref( 0 );
  TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
  if( !road_iter.count() ) {

    // if no road is found get z from plane0 panel0 center.

    // retrieve panel 0 from plane 0 and track arm.
    TMuiPanelGeo* panel( TMuiGeometry::Geom()->getPanel( trk_ptr->get()->get_arm(), 0, 0 ) );

    // retrieve center position assign z to z_ref
    float x_panel( 0 ), y_panel( 0 ), z_panel( 0 );
    panel->CenterPos( x_panel, y_panel, z_panel );
    z_ref = z_panel;

    // some dump
    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
      cout << "mMutKalFitWithSiliReal::extrapolate_to_muid - track has no associated road. Using z=" << z_panel << " (panel 0, plane 0)" << endl;

  } else {

    // get the z from first road gap0 point
    z_ref = road_iter->get()->get_gap0_point().getZ();

  }


  integrator->initialize( trk_ptr->get()->get_trk_par_list()->back() );
  integrator->extrapolate( z_ref );
  if( integrator->get_error() ) {

    if( _mod_par->get_verbosity() >= MUTOO::SOME ) {
      cout
        << "in mMutKalFitWithSiliReal::extrapolate_to_muid - extrapolation towards MuID failed ("
        << trk_ptr->get()->get_trk_par_list()->back().get_z() << "->" << z_ref << ")\n";
    }
    return false;

  } else {

    TMutTrkPar extrap_trk_par;
    integrator->finish( extrap_trk_par );
    trk_ptr->get()->push_trk_par( extrap_trk_par );
    if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
      MUTOO::PRINT( cout, "mMutKalFitWithSiliReal::extrapolate_to_muid - trk_par_muid");
      extrap_trk_par.print();
    }

  }

  return true;

}

//___________________________________________
void mMutKalFitWithSiliReal::fill_tree( void )
{

  if( !_tree ) return;
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ )
  {
    if( node->has_coord() ) {

      // stores coord location
      _arm = node->get_coord().get()->get_arm();
      _station = node->get_coord().get()->get_station();
      _gap = node->get_coord().get()->get_gap();
      _cathode = node->get_coord().get()->get_cathode();

      // initialise residuals and errors
      _res.assign(0);
      _res_cov.assign(0);
      _chi_square = 0;

      // fill filtered residuals
      if( node->prediction_done() ) {
        _res[0] = node->get_predicted_residual()(0,0);
        _res_cov[0] = node->get_predicted_residual_cov()(0,0);
      }

      // fill filtered residuals
      if( node->filter_done() ) {
        _res[1] = node->get_filtered_residual()(0,0);
        _res_cov[1] = node->get_filtered_residual_cov()(0,0);
        _chi_square = node->get_chi_square();
      }

      // fill filtered residuals
      if( node->smooth_done() ) {
        _res[2] = node->get_smoothed_residual()(0,0);
        _res_cov[2] = node->get_smoothed_residual_cov()(0,0);
      }

      // fill tree
      _tree->Fill();

    }

  }

}

//__________________________________________________________
void mMutKalFitWithSiliReal::push_r_residual( TMutTrkMap::pointer trk_ptr, const TMutTrkPar &trk_par, TMutCoordMap::value_type coord )
{

  // stores smoothed track point
  PHPoint trk_point( trk_par.get_x(), trk_par.get_y(), trk_par.get_z() );

  // loop over associated TMutGapCoord and fill r residuals.  The check on
  // cathode is to prevent writing 2 r residuals for each gap coord.  If
  // no gap coord then we don't write a r residual (SK 6/17/03)
  if(coord.get()->get_cathode() == 0) {
    TMutGapCoordMap::key_iterator gap_coord_iter = coord.get()->get_associated<TMutGapCoord>();
    while(TMutGapCoordMap::pointer gap_ptr = gap_coord_iter.next()){
      double delta_r = TMutMeasureModel::anode_measure(trk_point, gap_ptr->get());
      trk_ptr->get()->push_r_residual(delta_r);
    }
  }

  return;
}

//__________________________________________________________
double mMutKalFitWithSiliReal::push_w_residual( TMutTrkMap::pointer trk_ptr, const TMutTrkPar& trk_par, TMutCoordMap::value_type coord )
{

  // stores smoothed track point
  PHPoint trk_point( trk_par.get_x(), trk_par.get_y(), trk_par.get_z() );

  // stores smoothed track point errors
  PHPoint trk_errors( trk_par.get_covar( 0, 0 ), trk_par.get_covar( 1, 1 ), 0 );

  // stores smoothed track tangents
  PHVector trk_tangent(
    trk_par.get_px()/trk_par.get_pz(),
    trk_par.get_py()/trk_par.get_pz(),
    1 );

  // stores track fit parameters
  TMutFitPar fit_par(
    trk_par.get_x(), trk_par.get_y(),trk_par.get_z(), // position
    trk_par.get_px()/trk_par.get_pz(), // dx/dz
    trk_par.get_py()/trk_par.get_pz(), // dy/dz
    trk_par.get_chi_square() );

  // calculate and store w_residual
  MUTOO::cathode_locator location = coord.get()->get_location();
  double r_trk = TMutGeo::get_point_cathode_rdist(trk_point, location, coord.get()->get_peak_strip());
  double cos_th_wz = PHGeometry::dot(TMutGeo::get_w_axis(location, coord.get()->get_peak_strip()),trk_tangent);
  double cos_th_r = PHGeometry::dot(TMutGeo::get_strip_direction(location, coord.get()->get_peak_strip()),trk_tangent);
  double w_fit_error = TMutMeasureModel::get_w_error(trk_point, trk_errors, coord.get());
  double delta_w = TMutMeasureModel::cathode_measure( trk_point, coord.get(), _mod_par->get_use_anodes() );
  double w_meas = coord.get()->get_w();            // measurement from mutoo
  double w_trk = coord.get()->get_w() + delta_w;  // 'corrected' track position offset

  // calculate cluster width
  TMutClusMap::const_key_iterator clus_iter = coord.get()->get_associated<TMutClus>();
  unsigned short clus_wid = 0;
  if(!clus_iter.at_end()) clus_wid = clus_iter->get()->get_n_strip();

  trk_ptr->get()->push_w_residual(TMutTrkRes(location,
    fit_par,
    coord.get()->get_cos_theta_wire(),
    w_trk,
    w_meas,
    r_trk,
    cos_th_wz,
    cos_th_r,
    coord.get()->get_q_peak(),
    coord.get()->get_q_tot(),
    w_fit_error,
    clus_wid,
    coord.get()->get_peak_strip(),
    coord.get()->get_status())
    );

  return MUTOO::SQUARE((w_trk-w_meas)/w_fit_error);

}

//__________________________________________________________
double mMutKalFitWithSiliReal::get_r_error()
{
  static double err = 0.5/sqrt(12.0);
  return err;
}

//__________________________________________________________
void mMutKalFitWithSiliReal::associate_road( TMutTrkMap::pointer trk_ptr ) const
{
  // get associated roads
  TMuiRoadMapO::key_iterator road_iter( trk_ptr->get()->get_associated<TMuiRoadO>() );
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) cout << "mMutKalFitWithSiliReal::associate_road - n_roads=" << road_iter.count() << endl;
  if( !road_iter.count() ) return;

  // get track parameters at station 3
  const TMutTrkPar* trk_par( trk_ptr->get()->get_trk_par_station( MUTOO::Station3 ) );

  // loop over associated roads
  double dg0_min( -1 );
  TMuiRoadMapO::pointer road_ptr_min( 0 );
  while( TMuiRoadMapO::pointer road_ptr = road_iter.next() )
  {
    PHPoint mui_point = road_ptr->get()->get_gap0_point();

    // get track point from last track parameters extrapolated to the muid z
    PHPoint trk_point( TMutTrackUtil::linear_track_model(
      trk_par,
      mui_point.getZ() ) );

    // calculate dg00
    double dg0 = PHGeometry::distancePointToPoint(mui_point,trk_point);
    if( dg0_min < 0 || dg0 < dg0_min ) {

      if( road_ptr_min )
        PHKey::disassociate( trk_ptr, road_ptr_min );

      road_ptr_min = road_ptr;
      dg0_min = dg0;

    } else PHKey::disassociate( trk_ptr, road_ptr );

  }

  // check min_road (useless)
  if( !road_ptr_min ) return;

  // get associated TMuiClusterO
  TMuiClusterMapO::key_iterator clus_iter( road_ptr_min->get()->get_associated< TMuiClusterO >() );
  while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
    PHKey::associate( trk_ptr, clus_ptr );
  return;

}

//______________________________________________________________________
void
mMutKalFitWithSiliReal::end(PHCompositeNode* top_node)
{
  _timer.get()->print_stat();

  return;
}

//______________________________________________________________________
mMutKalFitWithSiliReal::KalmanFilterNode::KalmanFilterNode( const TMutCoordMap::pointer& coord_ptr )
{

  set_z( coord_ptr->get()->get_mean_z() );
  set_arm( coord_ptr->get()->get_arm() );
  _has_coord = true;
  _has_mui_cluster = false;
  _coord = *coord_ptr;

  // measurement
  PHGslMatrix measurement( 1,1 );
  measurement( 0,0 ) = coord_ptr->get()->get_w_absolute();

  // error on measurement
  PHGslMatrix measurement_cov( 1,1 );
  measurement_cov( 0,0 ) = MUTOO::SQUARE( coord_ptr->get()->get_error());

  // projection matrix
  PHGslMatrix h( 1, 5 );

  // store projection matrix from state vector to measurement
  double angle( TMutGeo::get_cathode_angle(
    coord_ptr->get()->get_arm(),
    coord_ptr->get()->get_station(),
    coord_ptr->get()->get_octant(),
    coord_ptr->get()->get_half_octant(),
    coord_ptr->get()->get_gap(),
    coord_ptr->get()->get_cathode(),
    coord_ptr->get()->get_peak_strip()) );

  h( 0, 3 ) = -sin( angle );
  h( 0, 4 ) = cos( angle );

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );
}

//__________________________________________________________
mMutKalFitWithSiliReal::KalmanFilterNode::KalmanFilterNode( const TMuiClusterMapO::pointer& cluster_ptr )
{
  set_arm( cluster_ptr->get()->get_arm() );

  // stores mean z
  set_z ( 0.5*(
    cluster_ptr->get()->get_coord_begin().getZ() +
    cluster_ptr->get()->get_coord_end().getZ() ) );

  _has_coord = false;
  _has_mui_cluster = true;
  _mui_cluster = *cluster_ptr;

  // origin to first point of strip
  PHVector begin = PHVector( cluster_ptr->get()->get_coord_begin());

  // cathodes strip direction vector
  PHVector v = cluster_ptr->get()->get_coord().getDirection();
  v.normalize();

  // Base vector normal to V
  PHVector w = PHVector(v.getY(), -v.getX(), v.getZ());

  // stores measurement
  PHGslMatrix measurement( 1,1 );
  measurement( 0,0 ) = begin.dot(w);

  // stores measurement covariance
  unsigned short orientation( cluster_ptr->get()->get_orientation() );
  PHGslMatrix measurement_cov( 1,1 );
  measurement_cov( 0,0 ) = MUTOO::SQUARE(
    ( orientation == kHORIZ ) ?
    cluster_ptr->get()->get_centroidsigma().getY():
    cluster_ptr->get()->get_centroidsigma().getX()
    );

  // stores projection matrix from state vector to measurement
  PHGslMatrix h( 1, 5 );
  h( 0, 3 ) = w.getX(); // -sin(angle)
  h( 0, 4 ) = w.getY(); // +cos(angle)

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}

//__________________________________________________________
mMutKalFitWithSiliReal::KalmanFilterNode::KalmanFilterNode( const TFvtxCoordMap::pointer& coord_ptr, const int phiflag )
{

  set_z( coord_ptr->get()->get_mean_z() );
  set_arm( coord_ptr->get()->get_arm() );
  _fvtx_coord = *coord_ptr;
  _has_coord = true;

  PHPoint begin = coord_ptr->get()->get_coord_begin();
  PHPoint end = coord_ptr->get()->get_coord_end();

  // store measurement and error
  PHGslMatrix measurement( 1,1 );
  PHGslMatrix measurement_cov( 1,1 );
  if ( !phiflag) {

    measurement( 0,0 ) = coord_ptr->get()->get_w_absolute();
    measurement_cov( 0,0 ) = FVTXOO::SQUARE( coord_ptr->get()->get_error());

  } else {

   PHVector v( end - begin );
    v.normalize();
    PHVector mid( (end + begin)*0.5 );
    measurement(0,0) = mid.dot( v );

    // The covariance gets the error *squared*
    double error2 =
      FVTXOO::SQUARE(end.getY() - begin.getY())
      + FVTXOO::SQUARE(end.getX() - begin.getX());

    measurement_cov( 0,0 ) = error2/12.0;

  }

  double angle = atan2((end.getY() - begin.getY()), (end.getX() - begin.getX()));

  // if phi flag is set, we are to add the phi portion of the measurement to the coord
  // list (as opposed to the r portion of the measurement).     Phi measurement orientation
  // is 90 degrees from r orientation.

  PHGslMatrix h( 1, 5 );
 
  if (phiflag){
   angle -= M_PI/2;
  }
  else{
    // Ensure that the range is -pi/2, pi/2
    //
    angle = (angle < -M_PI_2) ? angle + M_PI : angle;
    angle = (angle > M_PI_2) ? angle - M_PI : angle;
  }

  h( 0, 3 ) = -sin( angle );
  h( 0, 4 ) = cos( angle );

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}

//__________________________________________________________
mMutKalFitWithSiliReal::KalmanFilterNode::KalmanFilterNode( const SvxCluster& clus, const int phiflag, double phi_error, double r_error, double z_error,
  double drdz)
{

// fill the KF nodes with VTX points which are translated from x,y into r,phi space so
// measurements are closer to the true VTX measurement directions:

  set_z( clus.get_xyz_global(2) );

  set_arm( (get_z()>0) ? MUTOO::North : MUTOO::South );

  // store measurement and error
  PHGslMatrix measurement( 1, 1 );
  PHGslMatrix measurement_cov( 1, 1 );

  // For calculating x, y measurement errors:

  double angle = atan2(clus.get_xyz_global(1), clus.get_xyz_global(0));

  if (phiflag == 0 )
  {
    // extract the r measurement, which we assume has an error primarily associated with
    // the r alignment and extrapolating to a measured z value:

    angle -= M_PI_2;
    measurement( 0,0 ) = sqrt(clus.get_xyz_global(0)*clus.get_xyz_global(0) +
      clus.get_xyz_global(1)*clus.get_xyz_global(1));

    // include phi measurement error, possible r alignment error, try to
    // include an error from z, projected into x:
    measurement_cov( 0,0 ) = FVTXOO::SQUARE(r_error) +
                             FVTXOO::SQUARE(z_error*drdz);

  } else if (phiflag == 1 )
  {

    // In the nominal phi direction the KF angle has been rotated until it passes
    // through the VTX x,y point, giving a phi measurement of '0':
    // Assume for now that the propagation z measurement error into phi is minimal
    // and see if it works o.k. to ignore it:

    measurement( 0,0 ) = 0;
    measurement_cov( 0,0 ) = FVTXOO::SQUARE(phi_error);

  } else {
    cout << "Invalid flag used for set_measurement (svxhit): " << phiflag << endl;
    return;
  }

  // store projection matrix
  PHGslMatrix h(1,5);

  h( 0, 3 ) = -sin(angle);
  h( 0, 4 ) =  cos(angle);

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}

//__________________________________________________________
mMutKalFitWithSiliReal::KalmanFilterNode::KalmanFilterNode( const SvxCluster& clus, const int xyflag, double phi_error, double r_error, double z_error, double dxdz, double dydz)
{
  set_z( clus.get_xyz_global(2) );

  set_arm( (get_z()>0) ? MUTOO::North : MUTOO::South );

  // store measurement and error
  PHGslMatrix measurement( 1, 1 );
  PHGslMatrix measurement_cov( 1, 1 );

  // For calculating x, y measurement errors:
  double rad = sqrt(FVTXOO::SQUARE(clus.get_xyz_global(0)) +
                    FVTXOO::SQUARE(clus.get_xyz_global(1)));
  double sinphi = clus.get_xyz_global(1)/rad;
  double cosphi = clus.get_xyz_global(0)/rad;

  if (xyflag == 0 )
  {

    measurement( 0,0 ) = -clus.get_xyz_global(0);
    // include phi measurement error, possible r alignment error, try to
    // include an error from z, projected into x:
    measurement_cov( 0,0 ) = FVTXOO::SQUARE(phi_error*sinphi) +
                             FVTXOO::SQUARE(r_error*cosphi) +
                             FVTXOO::SQUARE(z_error*dxdz);

  } else if (xyflag == 1 )
  {

    measurement( 0,0 ) = clus.get_xyz_global(1);
    measurement_cov( 0,0 ) = FVTXOO::SQUARE(phi_error*cosphi) +
                             FVTXOO::SQUARE(r_error*sinphi) +
                             FVTXOO::SQUARE(z_error*dydz);

  } else {
    cout << "Invalid flag used for set_measurement (svxhit): " << xyflag << endl;
    return;
  }


  // store projection matrix
  PHGslMatrix h(1,5);
  if (xyflag == 0)
  {
    // x measurement, strip angle = 90 degrees
    h( 0, 3 ) = -1.0;  // -sin(angle)
    h( 0, 4 ) = 0.0;   //  cos(angle)
  }

  else if (xyflag == 1)
  {
    // y measurement, strip angle = 0 degrees
    h( 0, 3 ) = 0.0;
    h( 0, 4 ) = 1.0;
  }

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}

//__________________________________________________________
mMutKalFitWithSiliReal::KalmanFilterNode::KalmanFilterNode( const SvxSnglPisaHit& svxhit, const int xyflag,
  const float zsmear, const float phismear )
{

  set_z( svxhit.GetZGlobal() + zsmear  );

  cout << "Z position of barrel hit in mMutKalFitWithSiliReal = " <<  svxhit.GetZGlobal() << endl;
  cout << "z smear of barrel hit in mMutKalFitWithSiliReal = " <<  zsmear << endl;

  set_arm( (get_z()>0) ? MUTOO::North : MUTOO::South );
  _svx_mchit = svxhit;

  // store measurement and error
  PHGslMatrix measurement( 1, 1 );
  PHGslMatrix measurement_cov( 1, 1 );
  if (xyflag == 0 )
  {

    measurement( 0,0 ) =
      -svxhit.GetXGlobal() +
      phismear*(svxhit.GetXGlobal()/sqrt(svxhit.GetXGlobal()*svxhit.GetXGlobal() +
      svxhit.GetYGlobal()*svxhit.GetYGlobal()) ) ;

  } else if (xyflag == 1 )
  {

    measurement( 0,0 ) =
      svxhit.GetYGlobal() +
      phismear*(svxhit.GetXGlobal()/sqrt(svxhit.GetXGlobal()*svxhit.GetXGlobal() +
      svxhit.GetYGlobal()*svxhit.GetYGlobal()) );

  } else {
    cout << "Invalid flag used for set_measurement (svxhit): " << xyflag << endl;
    return;
  }

  //  NEED TO SET THIS WITH A PARAMETER
  measurement_cov( 0,0 ) = 0.01;

  // store projection matrix
  PHGslMatrix h(1,5);
  if (xyflag == 0)
  {
    // x measurement, strip angle = 90 degrees
    h( 0, 3 ) = -1.0;  // -sin(angle)
      h( 0, 4 ) = 0.0;   //  cos(angle)
  }

  else if (xyflag == 1)
  {
    // y measurement, strip angle = 0 degrees
    h( 0, 3 ) = 0.0;
    h( 0, 4 ) = 1.0;
  }

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}

