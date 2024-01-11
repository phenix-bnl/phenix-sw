

#ifndef __EMC_GEA_DEPOSITHOLDER_H__
#define __EMC_GEA_DEPOSITHOLDER_H__


#include <cassert>

#include <Rtypes.h>

#include <phool.h>

#include <emctypes.h>
#include <emcGeaDeposit.h>


class emcGeaTrackContainer;
class emcGeaTowerContainer;
class emcGeaClusterContainer;



class emcGeaDepositHolder {
public:
  typedef enum { CACHED_OBJLIST, CACHED_ETOF, CACHED_ALL } cachetype_t;


protected:
  emcGeaDepositHolder(){}

public:
  virtual ~emcGeaDepositHolder(){ }


public:

  // returns the emcGeaTrackContainer this object is linked with.
  virtual emcGeaTrackContainer * get_trackcontainer() const = 0; //{ PHOOL_VIRTUAL_WARNING; return 0; }

  // returns the emcGeaTowerContainer this object is linked with.
  virtual emcGeaTowerContainer * get_towercontainer() const = 0; // { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns the emcGeaClusterContainer this object is linked with.
  virtual emcGeaClusterContainer * get_clustercontainer() const = 0; //{ PHOOL_VIRTUAL_WARNING; return 0; }



  // returns the total energy contained by this object.
  virtual float get_edep(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const = 0;

  //  virtual float get_tof(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;
  
  virtual float get_simfrac(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;




  // instructs object to discard cached data of a given type, because
  // the datasoruces have changed.
  virtual void invcache(cachetype_t type = CACHED_ALL) = 0;




  // returns a list containing the track numbers of the geant tracks
  // that have contributed energy to this object.
  virtual emc_tracklist_t const get_track_list() const = 0;

  // returns the energy contributed to this object by the geant track
  // specified by trkno.  see emcGeaDeposit::datatype_t about the variuos type 
  // of deposits (you most probably want the default emcGeaDeposit::CALIB). 
  virtual float get_edep_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const = 0;

  // returns a list containing the towerids of the towers
  // that have contributed energy to this object.
  virtual emc_towerlist_t const get_tower_list() const = 0;

  // returns the energy contributed to this object by the tower 
  // specified by towerid.  see emcGeaDeposit::datatype_t about the variuos type 
  // of deposits (you most probably want the default emcGeaDeposit::CALIB). 
  virtual float get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const = 0;

  // returns a list containing the clusterids of the clusters
  // that have contributed energy to this object.
  virtual emc_clusterlist_t const get_cluster_list() const = 0;

  // returns the energy contributed to this object by the cluster 
  // specified by clusterid.  see emcGeaDeposit::datatype_t about the variuos type 
  // of deposits (you most probably want the default emcGeaDeposit::CALIB). 
  virtual float get_edep_bycluster(emc_clusterid_t clusterid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const = 0;

  /*
  /// returns the list of energy deposits by track sorted by the amount of deposited
  /// energy (descending order).
  virtual std::vector< std::pair<emc_trkno_t, float> > get_parte_bytrack() const;

  /// returns partesum array, deposits sorted by track. if you are interested in the
  /// trackid as well use get_parte_bytrack() and roll-your-own (tm) sum (see implementation
  /// of this function).
  virtual std::vector<float> get_partesum_bytrack() const; 
  */





  ClassDef(emcGeaDepositHolder, 1)
};




#define EMCLINKASSERT(x) \
  if( (x) == NULL ){ \
  std::cerr << __PRETTY_FUNCTION__ << ": emcGeaContainers are not linked properly. " \
            << "probably you forgot to add the EmcGeaContainerImporter SubsysReco. " \
	    << std::endl; \
  assert( x != NULL ); \
}





#endif /* !__EMC_GEA_DEPOSITHOLDER_H__ */
