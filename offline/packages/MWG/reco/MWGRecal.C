// $Id: MWGRecal.C,v 1.1 2009/07/04 18:33:51 hpereira Exp $

/*!
\file    MWGRecal.C
\brief   muon nanoDST creation, using new framework input nodes
\author  Hugo Pereira
\version $Revision: 1.1 $
\date    $Date: 2009/07/04 18:33:51 $
*/

#include <headerWrapper.h>
#include <MuonUtil.h>
#include <MUTOO.h>
#include <PHCompositeNode.h>
#include <PHGlobal.h>
#include <PHTrackIntegratorKF.h>
#include <TMutNode.h>
#include <TMutTrkPar.hh>
#include <Tools.h>

#include "MWGRecal.h"
#include "MWGVertex.h"

using namespace std;

//_______________________________________________________________
MWGRecal::MWGRecal( const char* name ):
  SubsysReco( name ),
  _z_vertex( 0 ),
  _z_vertex_error( 2 ),
  _flags( EXTRAPOLATE_TRACKS | FIT_DIMUON_VERTEX ),
  _timer(PHTimeServer::get()->insert_new("MWGRecal") )
{ return; }

//_______________________________________________________________
int MWGRecal::Init( PHCompositeNode* )
{
  MUTOO::PRINT( cout, "MWGRecal::Init" );
  cout << "flags: " << endl;
  cout << "MC                 : " << (get_flag( MC ) ? "true":"false" ) << endl;
  cout << "EXTRAPOLATE_TRACKS : " << (get_flag( EXTRAPOLATE_TRACKS ) ? "true":"false" ) << endl;  
  cout << "FIT_DIMUON_VERTEX  : " << (get_flag( FIT_DIMUON_VERTEX ) ? "true":"false" ) << endl;    
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//_______________________________________________________________
int MWGRecal::InitRun( PHCompositeNode* top_node )
{
  MUTOO::PRINT( cout, "MWGRecal::InitRun" );
  
  /* 
  initialize database
  only the mutr gets initialized, since MuID is not used
  */
  MuonUtil::initialize_database( top_node, true, false );
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//_______________________________________________________________
int MWGRecal::process_event(PHCompositeNode *top_node)
{
  
  // check event variables
  _timer.get()->restart();
  
  // loop over input muos; fill output; make cuts..
  try {
    
    // load vertex
    if( !load_vertex( top_node ) )
    { 
      cout << "MWGRecal::process_event - unable to load vertex" << endl;
      return 0;
    }
    
    // get pointer to PHMuoTracksOut
    PHMuoTracksOut* muo_tracks = TMutNode<PHMuoTracksOut>::find_io_node( top_node, "PHMuoTracksOO" );
    
    // loop over tracks
    if( get_flag( EXTRAPOLATE_TRACKS ) )
    {
      for( unsigned int i=0; i < muo_tracks->get_npart(); i++ )
      { extrapolate_track( muo_tracks, i ); }
    }
    
    // loop over dimuons
    if( get_flag( FIT_DIMUON_VERTEX ) )
    {
      for( int i=0; i < muo_tracks->get_ndimu(); i++ )
      { fit_dimuon_vertex( muo_tracks, i ); }
    }
    
  } catch( exception &e ) { cout << e.what() << endl; }
  
  _timer.get()->stop();
  
  return 0;
}

//______________________________________________________
int MWGRecal::End(PHCompositeNode* top_node)
{
  _timer.get()->print_stat();
  return 0;
}

//______________________________________________________
bool MWGRecal::load_vertex( PHCompositeNode* top_node )
{
  if( get_flag( MC ) )
  {
    
    // try load from PISA header
    bool error(true);
    headerWrapper* header = TMutNode<headerWrapper>::find_io_node(top_node, "header");
    _z_vertex = Tools::zVertexMC( header, error);
    return !error;
    
  } else {
   
    // try load from PHGlobal
    PHGlobal* phg = TMutNode<PHGlobal>::find_io_node( top_node, "PHGlobal" );
    _z_vertex = phg->getBbcZVertex();
    return true;
      
  }
  
}

//___________________________________________
bool MWGRecal::extrapolate_track( PHMuoTracksOut* muo, const int& i_track )
{
  
  // build TMutTrkPar at station 1
  TMutTrkPar trk_par( 
    muo->get_xpos( 1, i_track ),
    muo->get_ypos( 1, i_track ),
    muo->get_zpos( 1, i_track ),
    muo->get_px( 1, i_track ),
    muo->get_py( 1, i_track ),
    muo->get_pz( 1, i_track ),
    muo->get_charge( i_track ),
    0 );
  
  /*
  unfortunately the track covariance matrix is not stored at station 1
  will use covariance matrix at vertex. 
  it was checked that it does not affect the extrapolated position/momentum at the vertex
  after the extrapolation, the momentum is updated, but the covariance matrix from the old (incorrect) extrapolation 
  is kept. It was checked that it does not differ significantly from the covariance matrix obtained
  with the correct extrapolation, and does dot affect the di-muon vertex fit
  */
  for( int i=0; i<5; i++)
  {
    for( int j=0; j<5; j++ )
    { trk_par.set_covar(i,j, muo->get_cov(i, j, i_track ) ); }
  }
  
  // trk_par.print();
  
  PHTrackIntegratorKF integrator;
  integrator.initialize( trk_par );
  integrator.extrapolate( _z_vertex );
  if( integrator.get_error() )
  {
    cout << "MWGRecal - extrapolation failed." << endl;
    return false;
  }
  
  // extrapolation finished
  // update track parameters at vertex
  integrator.finish( trk_par );
  
  // some dump (for debugging)
  if( verbosity )
  {
    cout << "MWGRecal::extrapolate_track - before -"
      << " position= ("   
      << muo->get_xpos( 0, i_track ) << ","
      << muo->get_ypos( 0, i_track ) << ","
      << muo->get_zpos( 0, i_track ) << ")"
      << " momentum = (" 
      << muo->get_px( 0, i_track ) << ","
      << muo->get_py( 0, i_track ) << ","
      << muo->get_pz( 0, i_track ) << ")"
      << endl;
  }
  
  // copy into PHDimuoTracksOut
  muo->set_xpos(0, i_track, trk_par.get_x() );
  muo->set_ypos(0, i_track, trk_par.get_y() );
  muo->set_zpos(0, i_track, trk_par.get_z() );

  muo->set_px(0, i_track, trk_par.get_px() );
  muo->set_py(0, i_track, trk_par.get_py() );
  muo->set_pz(0, i_track, trk_par.get_pz() );
  
  // some dump (for debugging)
  if( verbosity )
  {
    cout << "MWGRecal::extrapolate_track - after  -"
      << " position= ("     
      << muo->get_xpos( 0, i_track ) << ","
      << muo->get_ypos( 0, i_track ) << ","
      << muo->get_zpos( 0, i_track ) << ")"
      << " momentum = ("  
      << muo->get_px( 0, i_track ) << ","
      << muo->get_py( 0, i_track ) << ","
      << muo->get_pz( 0, i_track ) << ")"
      << endl;
  }
  
  return true;
  
}

//___________________________________________
bool MWGRecal::fit_dimuon_vertex( PHMuoTracksOut* muo, const int& i_dimu )
{

  // retrieve the two tracks associated to the vertex
  int first_track( muo->get_ditrkIndex(0,i_dimu) );
  int second_track( muo->get_ditrkIndex(1,i_dimu) );
  
  // Remove dimuons with unassociated tracks (negative index)
  if( first_track < 0 || second_track < 0 ) 
  { return false; }

  // create vertex
  MWGVertex vertex;
  vertex.add_track( first_track, muo );
  vertex.add_track( second_track, muo );
  vertex.add_vertex( _z_vertex, _z_vertex_error );
  
  // do the fit
  vertex.fit();
  
  if( verbosity )
  {
    cout << "MWGRecal::fit_dimuon_vertex - before -"
      << " position= ("     
      << muo->get_vtx_xpos( i_dimu ) << ","
      << muo->get_vtx_ypos( i_dimu ) << ","
      << muo->get_vtx_zpos( i_dimu ) << ")"
      << " momentum_1 = ("  
      << muo->get_vtx_px_1( i_dimu ) << ","
      << muo->get_vtx_py_1( i_dimu ) << ","
      << muo->get_vtx_pz_1( i_dimu ) <<  ")"
      << " momentum_2 = ("  
      << muo->get_vtx_px_2( i_dimu ) << ","
      << muo->get_vtx_py_2( i_dimu ) << ","
      << muo->get_vtx_pz_2( i_dimu ) <<  ")"
      << " chisquare = " << muo->get_vtx_chisquare( i_dimu )
      << " mass = " << muo->get_dimass( i_dimu )
      << endl;    
      
    PHGslMatrix cov( 7, 7 );
    for( unsigned int i=0; i<7; i++ )
    { 
      for( unsigned int j=0; j<7; j++ )
      { cov( i, j ) = muo->get_vtx_cov( i, j, i_dimu ); }
    }
    
    cov.print();
      
  }
  
  // store result in PHMuoTracksOut
  muo->set_vtx_xpos( i_dimu, vertex.get_vtx_x() );
  muo->set_vtx_ypos( i_dimu, vertex.get_vtx_y() );
  muo->set_vtx_zpos( i_dimu, vertex.get_vtx_z() );
  
  muo->set_vtx_px_1( i_dimu, vertex.get_px(0) );
  muo->set_vtx_py_1( i_dimu, vertex.get_py(0) );
  muo->set_vtx_pz_1( i_dimu, vertex.get_pz(0) );
   
  muo->set_vtx_px_2( i_dimu, vertex.get_px(1) );
  muo->set_vtx_py_2( i_dimu, vertex.get_py(1) );
  muo->set_vtx_pz_2( i_dimu, vertex.get_pz(1) );
  
  muo->set_vtx_chisquare( i_dimu, vertex.get_chisquare_pdf() );
  muo->set_vtx_ndf( i_dimu, vertex.get_ndf() );
  
  // covariance matrix
  for( unsigned int i=0; i<7; i++ )
  { 
    for( unsigned int j=0; j<7; j++ ) 
    { muo->set_vtx_cov( i, j, i_dimu, vertex.get_vtx_cov( i, j ) ); }
  }
 
  // mass
  muo->set_dimass( i_dimu, vertex.get_mass() );
  
  if( verbosity )
  {
    cout << "MWGRecal::fit_dimuon_vertex - after  -"
      << " position= ("     
      << muo->get_vtx_xpos( i_dimu ) << ","
      << muo->get_vtx_ypos( i_dimu ) << ","
      << muo->get_vtx_zpos( i_dimu ) << ")"
      << " momentum_1 = ("  
      << muo->get_vtx_px_1( i_dimu ) << ","
      << muo->get_vtx_py_1( i_dimu ) << ","
      << muo->get_vtx_pz_1( i_dimu ) <<  ")"
      << " momentum_2 = ("  
      << muo->get_vtx_px_2( i_dimu ) << ","
      << muo->get_vtx_py_2( i_dimu ) << ","
      << muo->get_vtx_pz_2( i_dimu ) <<  ")"
      << " chisquare = " << muo->get_vtx_chisquare( i_dimu )
      << " mass = " << muo->get_dimass( i_dimu )
      << endl;    
      
    PHGslMatrix cov( 7, 7 );
    for( unsigned int i=0; i<7; i++ )
    { 
      for( unsigned int j=0; j<7; j++ )
      { cov( i, j ) = muo->get_vtx_cov( i, j, i_dimu ); }
    }
    
    cov.print(); 
  } 
  
  return true;
  
}
  
