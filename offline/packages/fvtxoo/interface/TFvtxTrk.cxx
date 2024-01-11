// $Id: TFvtxTrk.cxx,v 1.11 2013/12/20 06:48:48 slash Exp $

/*!
	\file TFvtxTrk.cxx
	\brief The Muon tracker Track object 
	\author S. Kelly
  \version $Revision: 1.11 $
  \date    $Date: 2013/12/20 06:48:48 $
*/

#include <TMutTrackUtil.h>

#include "TFvtxCoordMap.h"
#include "TFvtxSvxClusterMap.h"
#include "TFvtxTrk.h"

#include <FVTXOO.h>
#include <float.h>

ClassImp(TFvtxTrk)

using namespace std;

//_________________________________________________________________________
const TMutTrkPar* TFvtxTrk::get_trk_par_station(unsigned short station) const
{
  
  // check there are some track parameters in the list
  const trk_par_list* local_list( get_trk_par_list() );
  if( !local_list ) return get_trk_par();
  
  // Loop over assoicate TFvtxCoord in the station and use the minimal mean_z of TFvtxCoord.
  double target_z = DBL_MAX;
  TFvtxCoordMap::const_key_iterator coord_iter = get_associated<TFvtxCoord>();
  while(TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next())
  if( coord_ptr->get()->get_station()==station && fabs( coord_ptr->get()->get_mean_z()) < fabs( target_z ) ) 
  target_z = coord_ptr->get()->get_mean_z();

  // loop ovet the track_par_list and search closest track parameters
  double min_delta_z = DBL_MAX;
  const TMutTrkPar* trk_par_ptr=0;

  // Loop over trk par list and TMutTrkPar with z reference closest to 
  // gap 0 of request station
  for( trk_par_list::const_iterator trk_iter = local_list->begin();trk_iter != local_list->end();++trk_iter){
    double delta_z = fabs(target_z-trk_iter->get_z());    
    if(delta_z < min_delta_z) {
      min_delta_z = delta_z;
      trk_par_ptr = &(*trk_iter);
    }
  }
  
  // If TMutTrkPar* is still null then return the reference track pars
  return (trk_par_ptr) ? trk_par_ptr : get_trk_par();
}

//___________________________________________________
size_t TFvtxTrk::get_n_coord() const
{ return get_associated<TFvtxCoord>().count(); }

//___________________________________________________
size_t TFvtxTrk::get_ndf() const
{ return get_associated<TFvtxCoord>().count() - 5; }

//___________________________________________________
double TFvtxTrk::get_rapidity( void ) const
{

  if( !get_trk_par_vtx() ) {
    cerr << "TFvtxTrk::get_rapidity - parameters at vertex not set.\n";
    return 0;
  }
  
  double px = get_trk_par_vtx()->get_px();
  double py = get_trk_par_vtx()->get_py();
  double pz = get_trk_par_vtx()->get_pz();
  float E = sqrt(
    FVTXOO::SQUARE( px )+
    FVTXOO::SQUARE( py )+
    FVTXOO::SQUARE( pz )+
    FVTXOO::MASS_MUON_SQUARE);
  return 0.5*log( (E+pz)/(E-pz) );

}

//_________________________________________
unsigned short TFvtxTrk::get_hit_pattern() const
{

  unsigned short out( 0 );
  TFvtxCoordMap::const_key_iterator coord_iter = get_associated<TFvtxCoord>();
  while( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
  {
    
    // calculate plane index from the coordinate
    unsigned short index = 
        (1-coord_ptr->get()->get_arm()+coord_ptr->get()->get_sector())%2 + 2 * (
          coord_ptr->get()->get_station() );
    
    // update the pattern
    out |= ( 1 << index );
  }
  
  return out;

}

//_________________________________________
unsigned short TFvtxTrk::get_svxhit_pattern() const
{

  unsigned short out( 0 );
  TFvtxSvxClusterMap::const_key_iterator coord_iter = get_associated<TFvtxSvxCluster>();
  while ( TFvtxSvxClusterMap::const_pointer coord_ptr = coord_iter.next() )
    {
      unsigned short index = coord_ptr->get()->get_cluster()->get_layer();
      // update the pattern
      out |= ( 1 << index );
    }
  return out;
  
}

//_________________________________________
// See if there is a coordinate in a a given station, sector, column on this track
bool TFvtxTrk::has_coord(unsigned short cage, unsigned short station, unsigned short sector, unsigned short column) const
{
  TFvtxCoordMap::const_key_iterator coord_iter = get_associated<TFvtxCoord>();
  while (TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() ){
    if (coord_ptr->get()->get_cage() == cage &&
        coord_ptr->get()->get_station() == station &&
        coord_ptr->get()->get_sector() == sector &&
        coord_ptr->get()->get_column() == column )  return true;
  }
  return false;

}

//_________________________________________
// See if there is a coordinate in a a given station, sector on this track
bool TFvtxTrk::has_coord(unsigned short cage, unsigned short station, unsigned short sector) const
{
  TFvtxCoordMap::const_key_iterator coord_iter = get_associated<TFvtxCoord>();
  while (TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() ){
    if (coord_ptr->get()->get_cage() == cage &&
        coord_ptr->get()->get_station() == station &&
        coord_ptr->get()->get_sector() == sector )  return true;
  }
  return false;

}

//_________________________________________
// See if there is a coordinate in a a given station on this track
bool TFvtxTrk::has_coord(unsigned short station) const
{
  TFvtxCoordMap::const_key_iterator coord_iter = get_associated<TFvtxCoord>();
  while (TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() ){
    if (coord_ptr->get()->get_station() == station )  return true;
  }
  return false;

}

//_________________________________________
// See if there is a coordinate in a specific SVX layer
bool TFvtxTrk::has_svx_coord(unsigned short layer) const
{
  TFvtxSvxClusterMap::const_key_iterator coord_iter = get_associated<TFvtxSvxCluster>();
  while ( TFvtxSvxClusterMap::const_pointer coord_ptr = coord_iter.next() ) {
    if (coord_ptr->get()->get_cluster()->get_layer() == layer) return true;
  }
  return false;
}
