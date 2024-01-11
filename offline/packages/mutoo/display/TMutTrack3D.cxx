// $Id: TMutTrack3D.cxx,v 1.4 2011/12/24 04:48:22 slash Exp $

/*!
  \file    TMutTrack3D.cxx
  \brief   3D object for mutr tracks
  \author  Hugo Pereira
  \version $Revision: 1.4 $
  \date    $Date: 2011/12/24 04:48:22 $
*/

#include "TMutTrack3D.h"
#include <MUTOO.h>
#include <list>
#include <TMuiRoadMapO.h>
#include <TMutTrackUtil.h>
#include <PHTrackIntegratorKF.h>

using namespace std;

//_________________________________________________
void TMutTrack3D::_make_nodes( void )
{
  if( !_is_valid ) return;

  // put track points into a list
  list< PHPoint > points;
  
  // adds vertex
  //if( !_trk.get()->get_status( TMutTrk::BOTH_ARMS ) )
  {
    points.push_back( PHPoint(
      _trk.get()->get_trk_par_vtx()->get_x(),
      _trk.get()->get_trk_par_vtx()->get_y(),
      _trk.get()->get_trk_par_vtx()->get_z()
      ) );
  }
  
  // store locations of points to be retrieved.
  // it depends whether track belongs to both arms or not
  typedef std::vector< std::pair<unsigned short, unsigned short> > LocationList;
  LocationList locations;
  
  if( _trk.get()->get_status( TMutTrk::BOTH_ARMS ) )
  {
    locations.push_back( make_pair( MUTOO::South, MUTOO::Station3 ) ); 
    locations.push_back( make_pair( MUTOO::South, MUTOO::Station2 ) ); 
    locations.push_back( make_pair( MUTOO::South, MUTOO::Station1 ) ); 
    
    locations.push_back( make_pair( MUTOO::North, MUTOO::Station3 ) ); 
    locations.push_back( make_pair( MUTOO::North, MUTOO::Station2 ) ); 
    locations.push_back( make_pair( MUTOO::North, MUTOO::Station1 ) );  
  } else {
    locations.push_back( make_pair( _trk.get()->get_arm(), MUTOO::Station3 ) ); 
    locations.push_back( make_pair( _trk.get()->get_arm(), MUTOO::Station2 ) ); 
    locations.push_back( make_pair( _trk.get()->get_arm(), MUTOO::Station1 ) );  
  }    

  // loop over locations, and store corresponding points
  for( LocationList::const_iterator iter = locations.begin(); iter != locations.end(); iter++ )
  {
    
    // adds point for station 1
    points.push_back( PHPoint(
      _trk.get()->get_trk_par_station( iter->first, iter->second )->get_x(),
      _trk.get()->get_trk_par_station( iter->first, iter->second )->get_y(),
      _trk.get()->get_trk_par_station( iter->first, iter->second )->get_z()
      ) );
  }
    
  // check for associated muid roads
  std::set< double > gap0_z_set;
  TMuiRoadMapO::key_iterator road_iter = _trk.get()->get_associated<TMuiRoadO>();  
  while( TMuiRoadMapO::const_pointer road_ptr = road_iter.next() )
  {
  
    // make sure same extrapolation is not done twice
    double z = road_ptr->get()->get_gap0_point().getZ();
    if( gap0_z_set.find( z ) != gap0_z_set.end() ) continue;
    gap0_z_set.insert( z );
    
    //! extrapolate up to first gap
    PHTrackIntegratorKF integrator;
    integrator.initialize( *_trk.get()->get_trk_par_station( road_ptr->get()->get_arm(), MUTOO::Station3 ) );
    integrator.extrapolate( z );
    
    // check extrapolation success
    if( integrator.get_error() ) continue;
    
    // retrieve extrapolated parameters and store corresponding point
    TMutTrkPar extrap_trk_par;
    integrator.finish( extrap_trk_par );
    points.push_back( PHPoint(
      extrap_trk_par.get_x(),
      extrap_trk_par.get_y(),
      extrap_trk_par.get_z() ) ); 
   
  } 
    
  //! check size
  if( points.size() < 2 ) return;
  
  //! sort list
  points.sort( point_less_ftor() );
  
  if( _parent ) _parent->cd();
  
  // process list, make segments for each pair
  list<PHPoint>::iterator p0 = points.begin();
  list<PHPoint>::iterator p1 = points.begin();
  p1++;
    
  while( p1 != points.end() ) {
    _make_segment( *p0, *p1, 1 );
    p0 = p1;
    p1++;
  }
  
  return;
}
