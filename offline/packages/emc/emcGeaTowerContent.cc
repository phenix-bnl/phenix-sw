//////////////////////////////////////////////////////////////////////////////////
//
// class that describes a tower. this class implements emcTowerContent
// but in a new way: it does not store the energy and tof values, but retrives
// them on the fly from an emcGeaTrackContainer.
//
// emcGeaTowerContent is derived from emcTowerContainer, and schema evolved.
//
//////////////////////////////////////////////////////////////////////////////////

#include <emcContainerT.h>

#include <emcGeaTrackContent.h>
#include <emcGeaTrackContainer.h>

#include <emcGeaTowerContent.h>
#include <emcGeaTowerContentv5.h>
#include <emcGeaTowerContainer.h>

#include <emcGeaClusterContent.h>
#include <emcGeaClusterContainer.h>




ClassImp(emcGeaTowerContent);
template class emcContentT< emcGeaTowerContent >;
const bool emcGeaTowerContent::__buildhash;





emcGeaTowerContent * emcGeaTowerContent::createdef(emc_towerid_t towerid){
  // factory for creating emcGeaTowerContents

  return new emcGeaTowerContentv5(towerid);
}



emcGeaTrackContainer * emcGeaTowerContent::get_trackcontainer() const { 
  assert( get_towercontainer() != NULL );
  emcGeaTrackContainer * tracks = get_towercontainer()->GetTracks();
  EMCLINKASSERT( tracks );
  return tracks;
}



emcGeaTowerContainer * emcGeaTowerContent::get_towercontainer() const {
  emcContainerTNotObject<emcGeaTowerContent> * cont = get_container();
  emcGeaTowerContainer * ret = dynamic_cast<emcGeaTowerContainer *>( cont );
  // make sure it's not the crosscast that has failed
  assert( (cont == NULL) == (ret == NULL) );
  return ret;
}



emcGeaClusterContainer * emcGeaTowerContent::get_clustercontainer() const { 
  assert( get_towercontainer() != NULL );
  emcGeaClusterContainer * clusters = get_towercontainer()->GetClusters();
  EMCLINKASSERT( clusters );
  return clusters;
}





emc_tracklist_t const emcGeaTowerContent::get_track_list() const {
  emc_tracklist_t ret;

  emcGeaTrackContainer * tracks = get_trackcontainer();
  for(size_t i = 0; i < tracks->size(); i++){
    emcGeaTrackContent * track = tracks->get(i);
    emc_towerlist_t twrlist = track->get_tower_list();
    if( twrlist.find(get_towerid()) != twrlist.end() ) ret.insert(track->get_trkno());
  }

  return ret;
}



float emcGeaTowerContent::get_edep_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type) const {
  emcGeaTrackContainer * tracks = get_trackcontainer();
  emcGeaTrackContent * track = tracks->find( trkno );
  if( track == NULL ) return 0;
  else return track->get_edep_bytower( get_towerid(), type );
}



emc_towerlist_t const emcGeaTowerContent::get_tower_list() const {
  emc_towerlist_t ret; 
  ret.insert( get_towerid() ); 
  return ret; 
}



float emcGeaTowerContent::get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type) const {
  if( towerid != get_towerid() ) return 0.0;
  else return get_edep(type); 
}



emc_clusterlist_t const emcGeaTowerContent::get_cluster_list() const {
  emc_clusterlist_t ret;

  emcGeaClusterContainer * clusters = get_clustercontainer();
  for(size_t i = 0; i < clusters->size(); i++){
    emcGeaClusterContent * cluster = clusters->get(i);
    emc_towerlist_t twrlist = cluster->get_tower_list();
    if( twrlist.find(get_towerid()) != twrlist.end() ) ret.insert(cluster->get_clusterid());
  }

  return ret;
}



float emcGeaTowerContent::get_edep_bycluster(emc_clusterid_t clusterid, emcGeaDeposit::datatype_t type) const {
  emcGeaClusterContainer * clusters = get_clustercontainer();
  emcGeaClusterContent * cluster = clusters->find( clusterid );
  if( cluster == NULL ) return 0;
  else return cluster->get_edep_bytower( get_towerid(), type );
}




































float emcGeaTowerContent::get_edep(emcGeaDeposit::datatype_t type) const {
  // returns the total energy deposited in this tower
  //
  // @param type is the deposit type, see \ref emcGeaDeposit for more info

  float ret = 0;

  emc_tracklist_t trklist = get_track_list();
  for(emc_tracklist_t::iterator i = trklist.begin(); i != trklist.end(); i++)
    ret += get_edep_bytrack( *i, type );

  return ret;
}



void emcGeaTowerContent::set_edep(float edep, emcGeaDeposit::datatype_t type) {
  // sets the total energy deposited in this tower. since the deposited energy is the
  // the summ of the energy deposited by all tracks, this function adjusts (scales) deposits
  // of all tracks to match the new energy. thus if the total deposited energy is 0, this 
  // call will probably fail, and you will got a debug message on the screen (you can 
  // not scale 0 up to be 1GeV).
  //
  // @param edep is the new energy for the tower
  // @param type is the deposit type, see \ref emcGeaDeposit for more info

  float origedep = get_edep(type);
  if( origedep == 0 ){ std::cerr << __PRETTY_FUNCTION__ << ": can't set edep: edep is 0" << std::endl; return; }
  scale_edep( edep/origedep, type );
}



void emcGeaTowerContent::shift_edep(float shift, emcGeaDeposit::datatype_t type){
  // adds to the energy deposited in this tower. since the deposited energy is the
  // the summ of the energy deposited by all tracks, this function adjusts (scales) deposits
  // of all tracks to match the new energy. thus if the total deposited energy is 0, this 
  // call will probably fail, and you will got a debug message on the screen.
  //
  // @param shift is the amount of energy to add to the tower's energy
  // @param type is the deposit type, see \ref emcGeaDeposit for more info

  float origedep = get_edep(type);
  if( origedep == 0 ){ std::cerr << __PRETTY_FUNCTION__ << ": can't shift shift: edep is 0" << std::endl; return; }
  scale_edep( 1.0 + shift/origedep, type );
}



void emcGeaTowerContent::scale_edep(float scale, emcGeaDeposit::datatype_t type) {
  // scales energy deposited in this tower.
  //
  // @param scale is the factor to apply to all deposits
  // @param type is the deposit type, see \ref emcGeaDeposit for more info

  emcGeaTrackContainer * tracks = get_trackcontainer();
  emc_towerid_t towerid = get_towerid();

  emc_tracklist_t trklist = get_track_list();
  for(emc_tracklist_t::iterator i = trklist.begin(); i != trklist.end(); i++){
    emcGeaTrackContent * track = tracks->find(*i); assert(track != NULL);
    float e = track->get_edep_bytower(towerid, type);
    track->set_edep_bytower(towerid, e * scale, type);
  }
}






float emcGeaTowerContent::get_tof(emcGeaDeposit::datatype_t type) const {
  float ret = NAN;

  emcGeaTrackContainer * tracks = get_trackcontainer();
  emc_tracklist_t trklist = get_track_list();
  for(emc_tracklist_t::iterator i = trklist.begin(); i != trklist.end(); i++){
    float trktof = tracks->find(*i)->get_tof_bytower( this->get_towerid(), type );
    if( !isnan(trktof) &&  (isnan(ret)  ||  trktof < ret) ) ret = trktof;
  }
  
  return ret;
}


void emcGeaTowerContent::set_tof(float tof, emcGeaDeposit::datatype_t type) {
  emcGeaTrackContainer * tracks = get_trackcontainer();
  emc_tracklist_t trklist = get_track_list();
  for(emc_tracklist_t::iterator i = trklist.begin(); i != trklist.end(); i++)
    tracks->find(*i)->set_tof_bytower( this->get_towerid(), tof, type );
}





/*
float emcGeaTowerContent::get_tof_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type) const {
  emcGeaTrackContainer * tracks = get_trackcontainer(); assert(tracks != NULL); /// \todo better error handling
  emcGeaTrackContent * track = tracks->find(trkno);
  if( track == NULL ) return NAN;
  else return track->get_tof_bytower( this->get_towerid(), type );
}
*/


