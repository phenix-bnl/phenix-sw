// $Id: MWGVertex.C,v 1.2 2013/11/10 16:16:08 slash Exp $

/*!
  \file    MWGVertex.C
  \brief   vertex fit from NanoDST tracks and event vertex information
  \author  Hugo Pereira
  \version $Revision: 1.2 $
  \date    $Date: 2013/11/10 16:16:08 $
*/

#include <PHMuoTracksOut.h>
#include <TMutTrkPar.hh>
#include "MWGVertex.h"

using namespace std;

//________________________________________________________
void MWGVertex::add_track( int imu, PHMuoTracksOut* muo )
{

  // check pointer
  if( !muo ) return;
  
  // dump input momentum
  if( verbosity() ) 
  {
    cout 
      << "MWGVertex::add_track -"
      << " position: (" << muo->get_xpos(0,imu ) << "," << muo->get_ypos(0, imu ) << "," << muo->get_zpos( 0, imu ) << ")" << endl;
  
    cout 
      << "MWGVertex::add_track -"
      << " momentum: (" << muo->get_px( 0, imu ) << "," << muo->get_py( 0, imu ) << "," << muo->get_pz( 0, imu ) << ")" << endl;
  
    cout << "MWGVertex::add_track - charge: " << muo->get_charge( imu ) << endl;
  }
  
  // create a new node and initialize
  Node node;
  
  // store track parameters suited for vertex fit
  node._p(0,0) = muo->get_px(0,imu)/muo->get_pz(0,imu); // dxdz
  node._p(1,0) = muo->get_py(0,imu)/muo->get_pz(0,imu); // dxdz
  node._p(2,0) = muo->get_xpos(0,imu);
  node._p(3,0) = muo->get_ypos(0,imu);
  
  // store track total momentum (it is signed with pz)
  node._ptot = sqrt( 
    MUTOO::SQUARE( muo->get_px(0,imu) ) +
    MUTOO::SQUARE( muo->get_py(0,imu) ) +
    MUTOO::SQUARE( muo->get_pz(0,imu) ) );
  
  node._pz_sign = MUTOO::SIGN( muo->get_pz( 0, imu ) );
  
  // store z position
  node._z = muo->get_zpos(0,imu);
  
  // store track momentum (needed for cov matrix conversion)
  PHGslMatrix trk_p( 3, 1 );
  trk_p(0,0) = muo->get_px(0,imu);
  trk_p(1,0) = muo->get_py(0,imu);
  trk_p(2,0) = muo->get_pz(0,imu);
  
  // store track covariance matrix
  // 5x5 track covariance matrix. Parameters are x,y,px,py,pz
  PHGslMatrix trk_cov( 5, 5 );
  for( unsigned int row=0; row<5; row++ )
  {
    for( unsigned int col=0; col<5; col++ )
    { trk_cov( row, col ) = muo->get_cov( row, col, imu );}
  }
  
  // covariance matrix
  if( verbosity() ) trk_cov.print();
  
  // track gain matrix (5x5)
  node._trk_g = convert_trk_cov( trk_cov, trk_p ).invert();

  // charge
  node._charge =  muo->get_charge( imu );
  
  // stores subset of the gain matrix into node
  // the first row and column, corresponding to the c/p is skipped
  // because it is not accounted for in the fit
  for( unsigned int i=0; i<4; i++ )
  {
    for( unsigned int j=0; j<4; j++ ) 
    { node._g(i,j) = node._trk_g(i+1,j+1); }
  }
    
  // print node
  if( verbosity() ) 
  {
    MUTOO::PRINT(cout, "MWGVertex::add_track - node");
    node.print();
  }
  
  // adds to the list
  add_node( node );
    
  return;
  
}

//________________________________________________________
void MWGVertex::add_track( TMutTrkPar* trk_par )
{

  // dump input momentum
  if( verbosity() ) 
  {
    cout 
      << "MWGVertex::add_track -"
      << " position: (" << trk_par->get_x() << "," << trk_par->get_y() << "," << trk_par->get_z() << ")" << endl;
  
    cout 
      << "MWGVertex::add_track -"
      << " momentum: (" << trk_par->get_px() << "," << trk_par->get_py() << "," << trk_par->get_pz() << ")" << endl;
  
    cout << "MWGVertex::add_track - charge: " << trk_par->get_charge() << endl;
  }
  
  // create a new node and initialize
  Node node;
  
  // store track parameters suited for vertex fit
  node._p(0,0) = trk_par->get_dxdz(); // dxdz
  node._p(1,0) = trk_par->get_dydz(); // dxdz
  node._p(2,0) = trk_par->get_x();
  node._p(3,0) = trk_par->get_y();
  
  // store track total momentum (it is signed with pz)
  node._ptot = trk_par->get_ptot();
  
  node._pz_sign = MUTOO::SIGN( trk_par->get_pz() );
  
  // store z position
  node._z = trk_par->get_z();
  
  // store track momentum (needed for cov matrix conversion)
  PHGslMatrix trk_p( 3, 1 );
  trk_p(0,0) = trk_par->get_px();
  trk_p(1,0) = trk_par->get_py();
  trk_p(2,0) = trk_par->get_pz();
  
  // store track covariance matrix
  // 5x5 track covariance matrix. Parameters are x,y,px,py,pz
  PHGslMatrix trk_cov( 5, 5 );
  for( unsigned int row=0; row<5; row++ )
  {
    for( unsigned int col=0; col<5; col++ )
    { trk_cov( row, col ) = trk_par->get_covar( row, col );}
  }
  
  // covariance matrix
  if( verbosity() ) trk_cov.print();
  
  // track gain matrix (5x5)
  node._trk_g = convert_trk_cov( trk_cov, trk_p ).invert();

  // charge
  node._charge =  trk_par->get_charge();
  
  // stores subset of the gain matrix into node
  // the first row and column, corresponding to the c/p is skipped
  // because it is not accounted for in the fit
  for( unsigned int i=0; i<4; i++ )
  {
    for( unsigned int j=0; j<4; j++ ) 
    { node._g(i,j) = node._trk_g(i+1,j+1); }
  }
    
  // print node
  if( verbosity() ) 
  {
    MUTOO::PRINT(cout, "MWGVertex::add_track - node");
    node.print();
  }
  
  // adds to the list
  add_node( node );
    
  return;
  
}
