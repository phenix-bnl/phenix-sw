//////////////////////////////////////////////////////////////////////////////////
//
// class that describes a cluster. this class inheritsh from emcClusterContent 
// and extends it with mewthods to access information that used to reside in the 
// geant tables. you can query the towers and tracks that contributed
// to this culster.
//
//////////////////////////////////////////////////////////////////////////////////


#include <emctypes.h>
#include <emcContentT.h>
#include <emcContainerT.h>

#include <emcGeaTowerContent.h>
#include <emcGeaTowerContainer.h>

#include <emcGeaClusterContent.h>
#include <emcGeaClusterContentv6.h>
#include <emcGeaClusterContainer.h>


ClassImp(emcGeaClusterContent);
template class emcContentT< emcGeaClusterContent >;
const bool emcGeaClusterContent::__buildhash;



emcGeaClusterContent * emcGeaClusterContent::createdef(emc_clusterid_t clusterid){
  return new emcGeaClusterContentv6(clusterid);
}



emcGeaClusterContent * emcGeaClusterContent::clone(void) const {
  emcGeaClusterContent * ret = this->create();
  ret->copy(this);
  return ret;
}








emcGeaTrackContainer * emcGeaClusterContent::get_trackcontainer() const { 
  assert( get_clustercontainer() != NULL );
  emcGeaTrackContainer * tracks = get_clustercontainer()->GetTracks();
  EMCLINKASSERT( tracks );
  return tracks;
}



emcGeaTowerContainer * emcGeaClusterContent::get_towercontainer() const {
  assert( get_clustercontainer() != NULL );
  emcGeaTowerContainer * towers = get_clustercontainer()->GetTowers();
  EMCLINKASSERT( towers );
  return towers;
}



emcGeaClusterContainer * emcGeaClusterContent::get_clustercontainer() const { 
  emcContainerTNotObject<emcGeaClusterContent> * cont = get_container();
  emcGeaClusterContainer * ret = dynamic_cast<emcGeaClusterContainer *>( cont );
  // make sure it's not the crosscast that has failed
  assert( (cont == NULL) == (ret == NULL) );
  return ret;
}












const emc_tracklist_t emcGeaClusterContent::get_track_list() const {
  emc_tracklist_t  ret;

  emcGeaTowerContainer * towers = get_towercontainer();
  for(size_t i = 0; i < (size_t)multiplicity(); i++){
    emc_tracklist_t trklist = towers->find( towerid(i) )->get_track_list();
    ret.insert(trklist.begin(), trklist.end());
  }
      
  return ret;
}


  
float emcGeaClusterContent::get_edep_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type) const {
  float ret = 0;

  emcGeaTowerContainer * towers = get_towercontainer();
  for(size_t i = 0; i < (size_t)multiplicity(); i++){
    emcGeaTowerContent * tower = towers->find( towerid(i) ); assert( tower != NULL );
    ret += tower->get_edep_bytrack(trkno, type) * towerefrac(i);
  }

  return ret;
}




float emcGeaClusterContent::towerefrac(size_t i) const {
  if( i >= (size_t)multiplicity() || multiplicity() == 0 ) return 0;

  emcGeaTowerContent * tower = get_towercontainer()->find( towerid(i) ); assert( tower != NULL );
  float edepi = partesum(i) - ((i == 0) ? 0 : partesum(i-1));

  return edepi / tower->Energy(); // was edepi/tower->get_edep(), but let's use the same as the clusterizer
}



const emc_towerlist_t emcGeaClusterContent::get_tower_list() const {
  emc_towerlist_t ret;

  for(size_t i = 0; i < (size_t)multiplicity(); i++)
    ret.insert( towerid(i) );

  return ret;
}



float emcGeaClusterContent::get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type) const {
  size_t i;

  for(i = 0; i < (size_t)multiplicity(); i++)
    if( this->towerid( i ) == towerid ) break;

  if( i == (size_t)multiplicity() ) return 0.0; // not found

  emcGeaTowerContainer * towers = get_towercontainer();
  emcGeaTowerContent * tower = towers->find( towerid ); assert( tower != NULL );

  return tower->get_edep(type) * towerefrac(i);
  // could also return partesum(i) - ((i == 0) ? 0 : partesum(i-1)) for type == calibrated
}
  


const emc_clusterlist_t emcGeaClusterContent::get_cluster_list() const {
  emc_clusterlist_t ret; 
  ret.insert( get_clusterid() ); 
  return ret; 
}



float emcGeaClusterContent::get_edep_bycluster(emc_clusterid_t clusterid, emcGeaDeposit::datatype_t type) const {
  if( clusterid != get_clusterid() ) return 0.0;
  else return get_edep(type); 
}









float emcGeaClusterContent::get_edep(emcGeaDeposit::datatype_t type) const {
  float ret = 0.0;

  for(size_t i = 0; i < (size_t)multiplicity(); i++)
    ret += get_edep_bytower( towerid(i) );
  
  return ret;
}
  


















/*

namespace {
  typedef std::pair<emc_trkno_t, float> qqq_t;
  

  class qqqsort: public std::binary_function<qqq_t, qqq_t, bool> {
  public:
    bool operator () (const qqq_t & a, const qqq_t & b){
      return a.second > b.second;
    }
  };
}



std::vector<emc_trkno_t> emcGeaClusterContent::get_track_list_sorted() const {
  // get edeps
  emc_tracklist_t tracks = get_track_list(); // get track list
  std::vector<qqq_t> deps( tracks.size() ); // make storage
  for(emc_tracklist_t::iterator i = tracks.begin(); i != tracks.end(); i++){
    float e = get_edep_bytrack(*i);
    deps[ distance(tracks.begin(), i) ] = std::make_pair(*i, e);
  }
  
  sort(deps.begin(), deps.end(), qqqsort());
  
  // return sorted list
  std::vector<emc_trkno_t> ret( tracks.size() ); // make temporary storage
  for(size_t i = 0; i < tracks.size(); i++) ret[i] = deps[i].first; // copy trkno
  return ret;
}



*/
