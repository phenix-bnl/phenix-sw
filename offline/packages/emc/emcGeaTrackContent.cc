//////////////////////////////////////////////////////////////////////////////////
//
// reincarnation of the dEmcGeaTrack table.
//
// demcGeaTrackContent.idl 
// GEANT track info, relevant to EMCal, extracted from "fkin". 
// All generations (ancestry levels) are stored in this table.
// Track is TRUE_TRACK, unique throughout the event, even if there are
// multiple subevents and/or input files (merge)
// 
// track numbers <= 0 are reserved for special purposes, see emctypes.h for
// their description.
//
////////////////////////////////////////////////////////////////////////////////// 


#include <typeinfo>

#include <algorithm>

#include <emcContainerT.h>
#include <emcGeaTrackContent.h>
#include <emcGeaTrackContentv3.h>
#include <emcGeaClusterContent.h>
#include <emcGeaClusterContainer.h>
#include <emcGeaTrackContainer.h>


ClassImp(emcGeaTrackContent);
template class emcContentT< emcGeaTrackContent >;
const bool emcGeaTrackContent::__buildhash;



emcGeaTrackContent * emcGeaTrackContent::createdef(emc_trkno_t trkno){ 
    return new emcGeaTrackContentv3(trkno); 
}



emcGeaTrackContainer * emcGeaTrackContent::get_trackcontainer() const { 
  emcContainerTNotObject<emcGeaTrackContent> * cont = get_container();
  emcGeaTrackContainer * ret = dynamic_cast<emcGeaTrackContainer *>( cont );
  // make sure it's not the crosscast that has failed
  assert( (cont == NULL) == (ret == NULL) );
  return ret;
}



emcGeaTowerContainer * emcGeaTrackContent::get_towercontainer() const {
  assert( get_trackcontainer() != NULL );
  emcGeaTowerContainer * towers = get_trackcontainer()->GetTowers(); 
  EMCLINKASSERT( towers );
  return towers;
}



emcGeaClusterContainer * emcGeaTrackContent::get_clustercontainer() const { 
  assert( get_trackcontainer() != NULL );
  emcGeaClusterContainer * clusters = get_trackcontainer()->GetClusters();
  EMCLINKASSERT( clusters );
  return clusters;
}




emc_tracklist_t const emcGeaTrackContent::get_track_list() const { 
  emc_tracklist_t ret; 
  ret.insert( get_trkno() ); 
  return ret; 
}



float emcGeaTrackContent::get_edep_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type) const { 
  if( trkno != get_trkno() ) return 0.0;
  else return get_edep(type); 
}


/*
emc_towerlist_t const emcGeaTrackContent::get_tower_list() const {
  
}



float emcGeaTrackContent::get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type) const {

}
*/


emc_clusterlist_t const emcGeaTrackContent::get_cluster_list() const {
  emc_clusterlist_t ret;
  emc_towerlist_t towerlist = get_tower_list();

  emcGeaClusterContainer * clusters = get_clustercontainer();
  for(size_t i = 0; i < clusters->size(); i++){
    emcGeaClusterContent * cluster = clusters->get(i);
    emc_towerlist_t tmp = cluster->get_tower_list();
    emc_towerlist_t isect;

    set_intersection( 
		     towerlist.begin(), towerlist.end(),
		     tmp.begin(), tmp.end(),
		     std::inserter(isect, isect.begin())
		     );

    if( !isect.empty() )
      ret.insert( cluster->get_clusterid() );
  }

  return ret;
}



float emcGeaTrackContent::get_edep_bycluster(emc_clusterid_t clusterid, emcGeaDeposit::datatype_t type) const {
  emcGeaClusterContainer * clusters = get_clustercontainer();
  emcGeaClusterContent * cluster = clusters->find( clusterid );
  if( cluster == NULL ) return 0;
  else return cluster->get_edep_bytrack( get_trkno(), type );
}








float emcGeaTrackContent::get_edep(emcGeaDeposit::datatype_t type) const {
  float ret = 0;

  emc_towerlist_t towerlist = get_tower_list();
  for(emc_towerlist_t::iterator i = towerlist.begin(); i != towerlist.end(); i++)
    ret += get_edep_bytower( *i, type );

  return ret;
}













void emcGeaTrackContent::copy(emcGeaTrackContent const * from){
  set_trkno( from->get_trkno() );
  set_input( from->get_input() );
  
  set_anclvl( from->get_anclvl() );
  set_parent_trkno( from->get_parent_trkno() );
  
  clear_daughter_list();
  emc_tracklist_t trklist = from->get_daughter_list();
  for(emc_tracklist_t::iterator i = trklist.begin(); i != trklist.end(); i++) 
    append_to_daughter_list(*i);

  set_pid( from->get_pid() );
  set_ekin( from->get_ekin() );
  set_x( from->get_x() );
  set_y( from->get_y() );
  set_z( from->get_z() );
  set_px( from->get_px() );
  set_py( from->get_py() );
  set_pz( from->get_pz() );
  set_impx( from->get_impx() );
  set_impy( from->get_impy() );
  set_impz( from->get_impz() );


  clear_deposits();
  

  //  set_towercontainer( from->get_towercontainer() );
  emc_towerlist_t twrlist = from->get_tower_list();
  for(emc_towerlist_t::iterator i = twrlist.begin(); i != twrlist.end(); i++){
    emcGeaDeposit::datatype_t type = emcGeaDeposit::GEA;
    set_edep_bytower(*i, from->get_edep_bytower(*i, type), type);
    set_tof_bytower(*i, from->get_tof_bytower(*i, type), type);
    
    type = emcGeaDeposit::ORIG;
    set_edep_bytower(*i, from->get_edep_bytower(*i, type), type);
    set_tof_bytower(*i, from->get_tof_bytower(*i, type), type);

    type = emcGeaDeposit::CALIB;
    set_edep_bytower(*i, from->get_edep_bytower(*i, type), type);
    set_tof_bytower(*i, from->get_tof_bytower(*i, type), type);
  }
}


emc_tracklist_t const & emcGeaTrackContent::get_daughter_list() const {
  PHOOL_VIRTUAL_WARNING; 
  static emc_tracklist_t dummy;
  return dummy;
}
