// $Id: mMutFitVtx.cxx,v 1.26 2011/07/14 04:26:09 richi Exp $
/*!
  \file    mMutFitVtx.cxx
  \brief   Vertex Reco and Fit via linearized chisquare analytic minimisation
  \author  Hugo Pereira
  \version $Revision: 1.26 $
  \date    $Date: 2011/07/14 04:26:09 $
*/

// MUTOO headers
#include<BbcOut.h>
#include <mMutFitVtxPar.h>
#include <PHIODataNode.h>
#include <PHTFileServer.h>
#include <PHTrackIntegratorKF.h>
#include <TMutNode.h>
#include <TMutKalmanUtil.h>
#include <TMutTrkPar.hh>
#include <TMutMCTrkMap.h>
#include <TMutTrackUtil.h>
#include <TMutExtVtx.h>

#include "mMutFitVtx.h"

using namespace std;

//______________________________________________________________________
mMutFitVtx::mMutFitVtx() :
  _mod_par( 0 ),
  _vtx_map( 0 ),
  _trk_map( 0 ),
  _timer(PHTimeServer::get()->insert_new("mMutFitVtx"))
{
  MUTOO::TRACE("initializing module mMutFitVtx",MUTOO::ALOT);
}

//______________________________________________________________________
PHBoolean mMutFitVtx::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();

  try {

    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // performs double track vertex fit
    vertex_loop();

    // performs single track vertex fit
    if( _mod_par->get_single_trk_fit() )
    { track_loop( ); }

  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _vtx_map->print();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();

  return True;
}

//______________________________________________________________________
void mMutFitVtx::set_interface_ptrs(PHCompositeNode* top_node)
{
  _mod_par = TMutNode<mMutFitVtxPar>::find_node(top_node,"mMutFitVtxPar");
  _vtx_map = TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
}

//______________________________________________________________________
void mMutFitVtx::vertex_loop()
{

  /*
    vertex loop must be in a separate try/catch block
    to avoid that a failed vertex affects the others
  */
  TMutVtxMap::iterator vtx_iter = _vtx_map->range();
  while(TMutVtxMap::pointer vtx_ptr  = vtx_iter.next())
  try {

    fit_vertex( vtx_ptr );

  } catch( exception &e ) { cout << "mMutFitVtx::vertex_loop - " << e.what() << endl; }

}

//______________________________________________________________________
void mMutFitVtx::track_loop()
{

  /*
    track loop must be in a separate try/catch block
    to avoid that a failed vertex affects the others
  */
  TMutTrkMap::iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::pointer trk_ptr  = trk_iter.next())
  {
    if( !accept_track( trk_ptr ) ) continue;
    try {
      fit_track( trk_ptr );
    } catch( exception &e ) { cout << "mMutFitVtx::track_loop - " << e.what() << endl; }
  }

}

//______________________________________________
void mMutFitVtx::fit_vertex( TMutVtxMap::pointer vtx_ptr )
{

  // cout << "mMutFitVtx::fit_vertex." << endl;
  // retrieve associated tracks
  TMutTrkMap::key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();

  // check number of tracks in vertex
  // up to now only two track vertices are supported.
  if( trk_iter.count() != 2 ) {
    if( _mod_par->get_verbosity() > MUTOO::ALOT )
    cerr << "in mMutFitVtx::fit_vertex - wrong size of track list (" << trk_iter.count() << "). Aborted.\n";
    return;
  }

  // create fitter
  Fitter vertex_fitter;

  // add tracks
  TMutTrkMap::pointer first_track_ptr(0);
  TMutTrkMap::pointer second_track_ptr(0);
  while(TMutTrkMap::pointer trk_ptr = trk_iter.next())
  {

    vertex_fitter.add_track( trk_ptr );

    // store tracks in local pointers
    if( !first_track_ptr ) first_track_ptr = trk_ptr;
    else second_track_ptr = trk_ptr;

  }

  // add external vertex if any
  bool error( false );
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  if( error )
  {

    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    { cerr << "mMutFitVtx::load_ext_vertex - wrong external vertex.\n"; }

  } else {
    vertex_fitter.add_vertex( vtx, TMutExtVtx::get().get_vtx_error() );
  }

  // do the fit
  vertex_fitter.fit();

  // retrieve parameters and fill vertex object
  vtx_ptr->get()->set_x( vertex_fitter.get_vtx_x() );
  vtx_ptr->get()->set_y( vertex_fitter.get_vtx_y() );
  vtx_ptr->get()->set_z( vertex_fitter.get_vtx_z() );

  // vertex chisquare
  vtx_ptr->get()->set_chi_square( vertex_fitter.get_chisquare() );
  vtx_ptr->get()->set_ndf( vertex_fitter.get_ndf() );

  // tracks
  vtx_ptr->get()->set_px1( vertex_fitter.get_px( 0 ) );
  vtx_ptr->get()->set_py1( vertex_fitter.get_py( 0 ) );
  vtx_ptr->get()->set_pz1( vertex_fitter.get_pz( 0 ) );
  vtx_ptr->get()->set_charge1( first_track_ptr->get()->get_charge() );

  vtx_ptr->get()->set_px2( vertex_fitter.get_px( 1 ) );
  vtx_ptr->get()->set_py2( vertex_fitter.get_py( 1 ) );
  vtx_ptr->get()->set_pz2( vertex_fitter.get_pz( 1 ) );
  vtx_ptr->get()->set_charge2( second_track_ptr->get()->get_charge() );

  // vertex covariance matrix
  for( unsigned int i=0; i<7; i++ )
  {
    for( unsigned int j=0; j<7; j++ )
    { vtx_ptr->get()->set_covar( i, j, vertex_fitter.get_vtx_cov(i, j) ); }
  }

  return;

}

//______________________________________________
void mMutFitVtx::fit_track( TMutTrkMap::pointer trk_ptr )
{

  // dump the starting vertex parameters
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) {
    MUTOO::PRINT( cerr, "mMutFitVtx::fit_track - start");
    trk_ptr->get()->get_trk_par_vtx()->print();
  }

  // create fitter and add track
  Fitter vertex_fitter;
  vertex_fitter.add_track( trk_ptr );

  // add external vertex if any
  bool error( false );
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  if( error )
  {

    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    { cerr << "mMutFitVtx::load_ext_vertex - wrong external vertex.\n"; }

  } else {

    vertex_fitter.add_vertex( vtx, TMutExtVtx::get().get_vtx_error() );

    // do the fit
    vertex_fitter.fit();

    // create track par at vertex
    TMutTrkPar local_trk_par(
      vertex_fitter.get_vtx_x(),
      vertex_fitter.get_vtx_y(),
      vertex_fitter.get_vtx_z(),
      vertex_fitter.get_px(0),
      vertex_fitter.get_py(0),
      vertex_fitter.get_pz(0),
      static_cast<int>(trk_ptr->get()->get_charge() ),
      vertex_fitter.get_chisquare() );

    trk_ptr->get()->set_trk_par_vtx( local_trk_par );

  }

  // dump the starting vertex parameters
  if( _mod_par->get_verbosity() >= MUTOO::ALOT )
  {
    MUTOO::PRINT( cerr, "mMutFitVtx::fit_track - end");
    trk_ptr->get()->get_trk_par_vtx()->print();
  }

  return;

}

//______________________________________________
void mMutFitVtx::Fitter::add_track( TMutTrkMap::const_pointer trk_ptr )
{

  // get track parameters
  TMutTrkPar local_trk_par( *trk_ptr->get()->get_trk_par() );

  // extrapolate to the starting parameter z_vtx
  PHTrackIntegratorKF integrator;
  integrator.initialize( local_trk_par );

  // extrapolate to vertex z
  bool error( false );
  double vertex_z( TMutExtVtx::get().get_vtx( error ).getZ() );
  integrator.extrapolate( vertex_z );

  // if extrapolation is OK, update the local_trk_par
  if( !integrator.get_error() ) integrator.finish( local_trk_par );
  else
  {

    cout << "mMutFitVtx::initialize_node - extrapolation failed." << endl;
    cout << "  z:" << vertex_z << endl;
    cout << "  momentum:" << endl;
    local_trk_par.print();

  }

  // create a new node and initialize
  Node node;

  // store track parameters suited for vertex fit
  node._p(0,0) = local_trk_par.get_dxdz();
  node._p(1,0) = local_trk_par.get_dydz();
  node._p(2,0) = local_trk_par.get_x();
  node._p(3,0) = local_trk_par.get_y();

  // store track total momentum (it is signed with pz)
  node._ptot = local_trk_par.get_ptot();
  node._pz_sign = MUTOO::SIGN( local_trk_par.get_pz() );

  // store z position
  node._z = local_trk_par.get_z();

  // store track momentum (needed for cov matrix conversion)
  PHGslMatrix trk_p( 3, 1 );
  trk_p(0,0) = local_trk_par.get_px();
  trk_p(1,0) = local_trk_par.get_py();
  trk_p(2,0) = local_trk_par.get_pz();

  // store track covariance matrix
  // 5x5 track covariance matrix. Parameters are x,y,px,py,pz
  PHGslMatrix trk_cov( 5, 5 );
  for( unsigned int row=0; row<5; row++ )
  {
    for( unsigned int col=0; col<5; col++ )
    { trk_cov( row, col ) = local_trk_par.get_covar( row, col );}
  }

  // covariance matrix
  if( verbosity() ) trk_cov.print();

  // track gain matrix (5x5)
  node._trk_g = convert_trk_cov( trk_cov, trk_p ).invert();

  // stores subset of the gain matrix into node
  // the first row and column, corresponding to the c/p is skipped
  // because it is not accounted for in the fit
  for( unsigned int i=0; i<4; i++ )
  {
    for( unsigned int j=0; j<4; j++ )
    { node._g(i,j) = node._trk_g(i+1,j+1); }
  }

  // charge
  node._charge =  local_trk_par.get_charge();
  add_node( node );

}

//__________________________________________________________________
bool mMutFitVtx::accept_track( TMutTrkMap::const_pointer trk_ptr ) const
{
  // check if track is a ghost
  if( trk_ptr->get()->get_ghost() ) return false;

  // check if track was fitted
  if( !trk_ptr->get()->get_reco_success() ) return false;

  // all checks passed
  return true;

}
