// $Id: mMutFindGapCoord.cxx,v 1.17 2011/12/24 04:48:29 slash Exp $

/*!
\file mMutFindGapCoord.cxx
\brief Matches TMutCoords across a gap and derives 
an associated TMutGapCoord
\author S. Kelly
\version $Revision: 1.17 $
\date $Date: 2011/12/24 04:48:29 $
*/

#include <fstream>

// MUTOO
#include <mMutFindGapCoord.h>
#include <mMutFindGapCoordPar.h>
#include <MUTOO.h>
#include <PHException.h>
#include <TMutClusMap.h>
#include <TMutHitMap.h>
#include <TMutChargeCorrection.h>
#include <TMutGapCoordMap.h>
#include <TMutGeo.h>
#include <TMutNode.h>
#include <TMutKeyGen.h>

// PHENIX headers
#include <PHGeometry.h>

// STL/BOOST
#include <cmath>
#include <iostream>
#include <string>

using namespace std;

//_________________________________________________________
mMutFindGapCoord::mMutFindGapCoord() : 
  _timer( PHTimeServer::get()->insert_new("mMutFindGapCoord") )
{
  MUTOO::TRACE("initializing module mMutFindGapCoord",MUTOO::ALOT);
}
  
//_________________________________________________________
PHBoolean mMutFindGapCoord::event(PHCompositeNode* top_node)
{
  
  _timer.get()->restart(); 
  
  try { 
    
    // Reset IOC pointers
    set_interface_ptrs(top_node);
    
    // clear gap_coord map
    _gap_coord_map->clear();
    
    // Associate groups of contiguous hits with TMutGapCoord objects
    find_gap_coords();
    
  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }	
    
  // If verbose dump the contents of the cluster map
  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _gap_coord_map->print();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();			 
  return True;
}

//_________________________________________________________
void mMutFindGapCoord::set_interface_ptrs(PHCompositeNode* top_node)
{	
  
  // module runtime parameters
  _mod_par = TMutNode<mMutFindGapCoordPar>::find_node(top_node,"mMutFindGapCoordPar");
  
  // TMutCoord IOC
  _coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");
  
  // TMutGapCoord IOC
  _gap_coord_map = TMutNode<TMutGapCoordMap>::find_node(top_node,"TMutGapCoordMap");
} 

//_________________________________________________________
void mMutFindGapCoord::find_gap_coords()
{
  
  // I'm not sure these makes things more of less readable ?
  // (optomizer has an easy time with this kind of temporary)
  unsigned short nar = MUTOO::NumberOfArms;
  unsigned short nst = MUTOO::NumberOfStations;
  unsigned short noc = MUTOO::NumberOfOctants;
  unsigned short nho = MUTOO::NumberOfHalfOctants;
  
  // Loop over all gaps (gap iterator would help here)
  for(int arm=0;arm<nar;++arm)
  {
    for(int station=0;station<nst;++station)
    {
      for(int octant=0; octant<noc;++octant)
      {
        for(int half_octant=0; half_octant<nho;++half_octant){
      
          // Number of gaps varies station to station
          unsigned short nga = TMutGeo::get_n_gaps(arm,station);
          
          for(int gap=0; gap<nga;++gap)		 
          { find_gap_coords( arm, station, octant, half_octant, gap ); }
        }
      }
      
    } 
    
  }
}

//_________________________________________________________
void mMutFindGapCoord::find_gap_coords( int arm, int station, int octant, int half_octant, int gap )
{
  
  // iterator to all coords in upstream cathode
  TMutCoordMap::iterator front_iter = _coord_map->get(arm,station,octant, half_octant,gap,0);
  
  // Attempt to match all pairs 
  while(TMutCoordMap::pointer front_ptr = front_iter.next()){
    
    // iterator to all coords in downstream cathode
    TMutCoordMap::iterator back_iter = _coord_map->get(arm,station,octant, half_octant,gap,1);
    
    // if coordinates match make a new TMutGapCoord
    while(TMutCoordMap::pointer back_ptr = back_iter.next())
      match_and_make(front_ptr,back_ptr);
    
  }
  
}

//_________________________________________________________
void mMutFindGapCoord::match_and_make(
  TMutCoordMap::pointer& first_coord, 
  TMutCoordMap::pointer& second_coord)
{
  
  // Check charge difference is below max
  const float &q0( first_coord->get()->get_q_tot() );
  const float &q1( second_coord->get()->get_q_tot() );
  if( fabs(q1-q0) > _mod_par->get_q_match_threshold())
  {
    if(_mod_par->get_verbosity() == MUTOO::ALOT) {
      MUTOO::PRINT(cout,"failed charge matching cut");
      first_coord->get()->print();
      second_coord->get()->print();
      MUTOO::PRINT(cout,"**");
    }
    return;
  }
  
  // check charge asymetry is below max
  if( _mod_par->get_q_rel_match_threshold() >= 0 && fabs(q1-q0)/fabs(q1+q0) > _mod_par->get_q_rel_match_threshold() ) 
  {
    if(_mod_par->get_verbosity() == MUTOO::ALOT) {
      MUTOO::PRINT(cout,"failed charge relative matching cut");
      first_coord->get()->print();
      second_coord->get()->print();
      MUTOO::PRINT(cout,"**");
    }
    return;
  }
  
  // fiducial cuts
  // Point on second_coord closest to first_coord 
  PHPoint point = PHGeometry::closestApproachLineLine(first_coord->get()->get_coord(), second_coord->get()->get_coord());
  
  // Check point is in fiducial volume
  if(!TMutGeo::in_fiducial(first_coord->get()->get_arm(),
    first_coord->get()->get_station(),
    first_coord->get()->get_octant(),
    first_coord->get()->get_half_octant(), point)) 
  {
      
    if(_mod_par->get_verbosity() == MUTOO::ALOT) {
      MUTOO::PRINT(cout,"failed fiducial cut");
      first_coord->get()->print();
      second_coord->get()->print();
      MUTOO::PRINT(cout,"**");
    }
    return;
  }	
  
  // Get wire number and distance to closest anode
  pair<int,double> anode_data = make_pair(0,0);
  try {
    anode_data = TMutGeo::find_anode_dca(first_coord->get()->get_arm(),point);
  } catch(exception& e) {
    if(_mod_par->get_verbosity() == MUTOO::ALOT) {
      MUTOO::PRINT(cout,"failed to find anode");
      first_coord->get()->print();
      second_coord->get()->print();
      cout << "point = {" << point.getX() << ","	<< point.getY() << "," << point.getZ() << "}" << endl;
    }
    return;
  }
  
  // Check that point is sufficiently close to anode wire
  if(anode_data.second > _mod_par->get_max_anode_dca()) {
    if(_mod_par->get_verbosity() == MUTOO::ALOT) {
      MUTOO::PRINT(cout,"failed anode proximity cut");
      first_coord->get()->print();
      second_coord->get()->print();
      MUTOO::PRINT(cout,"**");
    }
    return;
  } 
  
  // Replace this with a geometry call; 
  double z_wire = 0.5*(
    first_coord->get()->get_coord().getBasepoint().getZ() + 
    second_coord->get()->get_coord().getBasepoint().getZ()); 
  
  // GapCoord defined at z of wire plane 
  point.setZ(z_wire);
  
  // Make a new gap coordinate
  TMutGapCoordMap::iterator gap_iter = 
    _gap_coord_map->insert_new(first_coord->get()->get_arm(),
    first_coord->get()->get_station(),
    first_coord->get()->get_octant(),
    first_coord->get()->get_half_octant(),
    first_coord->get()->get_gap());
  
  // Set the coordinate
  gap_iter->get()->set_coord(point);
  gap_iter->get()->set_anode(anode_data.first);
  gap_iter->get()->set_anode_dca(anode_data.second);
  
  // set the charge correction
  gap_iter->get()->set_charge_corr(
    TMutChargeCorrection::get_correction( 
    gap_iter->get()->get_location(), 
    point.getX(), point.getY() ) );
  
  // Associate both TMutCoord with the TMutGapCoord
  PHKey::associate(gap_iter.current(),first_coord);
  PHKey::associate(gap_iter.current(),second_coord);
  
  // Associate the two coordinates together
  PHKey::associate( first_coord, second_coord );
  
}
