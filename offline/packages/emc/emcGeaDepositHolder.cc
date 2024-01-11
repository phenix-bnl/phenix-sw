////////////////////////////////////////////////////////////////////////////////// 
//
// the emcGeaDepositHolder class defines the interface that can be used to query
// the main objects in the emcal simulation code about energy deposits.  the 
// classes implementing this interface are emcGeaTrackContent, emcGeaTowerContent 
// and emcGeaClusterContent.
//
// the list of the unique keys of the objects that share energy with this object
// can be queried using get_track_list(), get_tower_list() and get_cluster_list()
// methods.  then using these lists the amount of energy shared by this object and
// a particular instance of emcGeaTrackContent, emcGeaTowerContent or 
// emcGeaClusterContent can be queried using the get_edep_bytrack(), 
// get_edep_bytower() orget_edep_bycluser() methods, respectively.
//
// for example to see the amount of energy deposited by different tracks in a 
// tower you can use this code:
//
//
//     emcGeaTowerContent * tower = ...;
//     emc_tracklist_t xlist = tower->get_track_list();
//     for(emc_tracklist_t::iterator i = xlist.begin(); i != xlist.end(); i++){
//         printf("%2d: edep=%8.6f\n", *i, tower->get_edep_bytrack(*i));
//         ...
//     }
//
// for more examples see EmcEvaluatorSanity.cc
//
//
// to gather these information the instances of this class need to access the 
// containers storing instances of the emcGeaTrackContent, emcGeaTowerContent 
// and emcClusterContent classes belonging to the same event.  therefore this
// class is linked (through a cascade of pointers) to cointainers of these objects. 
// the links might not be set up properly up untill this object itself is stored 
// in a container.
//
////////////////////////////////////////////////////////////////////////////////// 


#include <iostream>
#include <algorithm>
#include <limits>

const static float NaN = std::numeric_limits<float>::quiet_NaN();

#include <emcGeaDeposit.h>
#include <emcGeaTrackContent.h>
#include <emcGeaTrackContainer.h>
#include <emcGeaTowerContent.h>
#include <emcGeaTowerContainer.h>
#include <emcGeaClusterContent.h>
#include <emcGeaClusterContainer.h>

#include <emcGeaDepositHolder.h>




ClassImp(emcGeaDepositHolder);



float emcGeaDepositHolder::get_simfrac(emcGeaDeposit::datatype_t type) const {
  // returns what fraction of the energy stored in this object comes from simulated
  // tracks.
  float simedep = 0, edep = get_edep(type);
  if( edep == 0 ) return NaN;

  emc_tracklist_t tracks = get_track_list();
  for(emc_tracklist_t::const_iterator i = tracks.begin(); i != tracks.end(); i++)
    if( isSimulatedTrkno(*i) ) simedep += get_edep_bytrack( *i, type );
  
  return simedep / get_edep(type);
}


/*

emc_tracklist_t const emcGeaDepositHolder::get_track_list() const {
  PHOOL_VIRTUAL_WARNING;
  return emc_tracklist_t();
}



float emcGeaDepositHolder::get_edep_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type) const {
  PHOOL_VIRTUAL_WARNING;
  return NaN;
}



emc_towerlist_t const emcGeaDepositHolder::get_tower_list() const {
  PHOOL_VIRTUAL_WARNING;
  return emc_towerlist_t();
}



float emcGeaDepositHolder::get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type) const {
  PHOOL_VIRTUAL_WARNING;
  return NaN;
}



emc_clusterlist_t const emcGeaDepositHolder::get_cluster_list() const {
  PHOOL_VIRTUAL_WARNING;
  return emc_clusterlist_t();
}



float emcGeaDepositHolder::get_edep_bycluster(emc_clusterid_t clusterid, emcGeaDeposit::datatype_t type) const {
  PHOOL_VIRTUAL_WARNING;
  return NaN;
}


*/
