// $Id: TMutMCTrk.cxx,v 1.18 2015/04/01 21:56:16 snowball Exp $

/*!
  \file    TMutMCTrk.cxx
  \brief   The muon tracker Monte Carlo track object
  \author  Sean Kelly
  \version $Revision: 1.18 $
  \date    $Date: 2015/04/01 21:56:16 $
*/

#include <MUTOO.h>
#include <bitset>
#include <PHKeyIterator.h>
#include <PHConstKeyIterator.h>

#include <iostream>
#include <root_ptrk.h>

#include "TMutMCHitMap.h"
#include "TMutMCTrk.hh"

ClassImp(TMutMCTrk)

using namespace std;

//______________________________________________________
double TMutMCTrk::get_rapidity( void ) const
{

  double px = get_px_orig();
  double py = get_py_orig();
  double pz = get_pz_orig();
  float E = sqrt(
    MUTOO::SQUARE( px )+
    MUTOO::SQUARE( py )+
    MUTOO::SQUARE( pz )+
    MUTOO::MASS_MUON_SQUARE);
  return 0.5*std::log( (E+pz)/(E-pz) );

}

//_______________________________________
bool TMutMCTrk::same_octant( void ) const
{

  int octant( -1 );
  
  // retrieve associated MC hits
  TMutMCHitMap::const_key_iterator mc_hit_iter = get_associated<TMutMCHit>();
  while( TMutMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next() )
  if( octant < 0 ) octant = mc_hit_ptr->get()->get_octant();
  else if ( octant !=  mc_hit_ptr->get()->get_octant() ) return false;

  return true;

}  
  
//______________________________________________________
bool TMutMCTrk::has_hits(UShort_t station) const 
{ 

  // Sanity check
  //
  if(station>=MUTOO::NumberOfStations) return false;

  // Return true if has associated mc hit in given station
  //
  TMutMCHitMap::const_key_iterator mc_hit_iter = get_associated<TMutMCHit>();
  while(TMutMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next()){
    if(mc_hit_ptr->get()->get_station() == station) return true;
  }
  return false;
}

//______________________________________________________
bool TMutMCTrk::is_ingeo() const 
{ 
  // Bitset to keep track of stations that are hit
  //
  std::bitset<MUTOO::NumberOfStations> stations;
  TMutMCHitMap::const_key_iterator mc_hit_iter = get_associated<TMutMCHit>();
  while(TMutMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next()){
    stations.set(mc_hit_ptr->get()->get_station());
  }
  // Return true if all bits are set, ie there is at least one hit per 
  // station
  //
  return( static_cast<int>( stations.count() ) == MUTOO::NumberOfStations );
}

//______________________________________________________
void TMutMCTrk::from_pisa( int track_id, MUTOO::Verbosity verbosity ) 
{ 

  // Get the parent MC track information via a pisa call according to
  // the track id.
  // Fill in the fields.

  float ptot=0;
  float ptheta=0;
  float pphi=0;
  float r_vertex=0;
  float z_vertex=0;
  float theta_vertex=0;
  float phi_vertex=0;
  
  int nfile=0;
  int error=0;
  int itparent=0;
  int itgrandparent=0;
  int idparent=0;
  int idpart=0;

  // Make the PISA call to get track data
  dio_ptrkstack(
    &track_id, &nfile, &error, &ptot, &ptheta, &pphi,
    &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
    &itparent, &idparent, &idpart); 
  
  set_pid(idpart);
  set_track_id(track_id);
  set_parent_id(idparent);
  set_parent_track_id(itparent);
  set_grandparent_track_id(itgrandparent);
  
  // assign vertex
  set_z_orig(z_vertex);
  set_x_orig(r_vertex*cos(phi_vertex*MUTOO::DEG_TO_RAD));
  set_y_orig(r_vertex*sin(phi_vertex*MUTOO::DEG_TO_RAD));
  
  // assign momentum
  set_px_orig(ptot*sin(ptheta*MUTOO::DEG_TO_RAD)*cos(pphi*MUTOO::DEG_TO_RAD));
  set_py_orig(ptot*sin(ptheta*MUTOO::DEG_TO_RAD)*sin(pphi*MUTOO::DEG_TO_RAD));
  set_pz_orig(ptot*cos(ptheta*MUTOO::DEG_TO_RAD));

   // print vertex
  if( verbosity >= MUTOO::SOME )
  {
    cout << "TMutMCTrk::from_pisa - vertex:"
      << " r=" << r_vertex 
      << " z=" << z_vertex 
      << " theta=" << theta_vertex
      << " phi=" << phi_vertex
      << " -"
      << " x=" << get_x_orig()
      << " y=" << get_y_orig()
      << endl;
  }

  
  // set charge.
  // there is no way to retrieve the charge directly. It is infered from the particle ID.
  // for muon/pion/kaon/proton at least.
  if(idpart==5 || idpart==8 || idpart == 11 || idpart == 14) set_charge(1);
  else if(idpart == 6 || idpart == 9 || idpart == 12 || idpart == 15) set_charge(-1);
  else {
    
    if( verbosity >= MUTOO::SOME )
      { cout << "TMutMCTrk::from_pisa - unrecognized PID:" << idpart << ". setting charge to 0" << endl; }
    
    set_charge(0);
    
  }
  return;
  
}
