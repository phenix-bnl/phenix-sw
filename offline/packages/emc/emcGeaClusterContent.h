#ifndef __EMC_GEACLUSTER_CONTENT_H__
#define __EMC_GEACLUSTER_CONTENT_H__





#include <phool.h>



#include <emctypes.h>
#include <emcContentT.h>
#include <emcClusterContent.h>
#include <emcGeaDeposit.h>
#include <emcGeaDepositHolder.h>



class emcGeaClusterContainer;

class emcGeaClusterContent: public emcClusterContent, public emcContentT<emcGeaClusterContent>, public emcGeaDepositHolder {
public:
  // default constructor
  emcGeaClusterContent(emc_clusterid_t clusterid = EMC_INVALID_CLUSTERID){};

  // destructor
  virtual ~emcGeaClusterContent(){}

public:
  // returns id for this cluster (used to retrive key for this object)
  emc_clusterid_t get_clusterid() const { return id(); }

  emcGeaClusterContent * create(void) const { PHOOL_VIRTUAL_WARNING; return 0; }

  // factory for creating emcGeaClusterContents
  static emcGeaClusterContent * createdef(emc_clusterid_t clusterid = EMC_INVALID_CLUSTERID);


public:  /* emcContentT<emcGeaClusterContent> stuff */
  const static bool __buildhash = true;
  typedef emc_clusterid_t key_type;
  key_type get_key() const { return get_clusterid(); }
  virtual void copy(emcGeaClusterContent const * from){ PHOOL_VIRTUAL_WARNING; }


public:
  virtual emcGeaTrackContainer * get_trackcontainer() const;
  virtual emcGeaTowerContainer * get_towercontainer() const;
  virtual emcGeaClusterContainer * get_clustercontainer() const;

  // noop: no cached data at all.
  virtual void invcache(cachetype_t type = CACHED_ALL){ PHOOL_VIRTUAL_WARNING; }

  virtual emc_tracklist_t const get_track_list() const ;
  virtual float get_edep_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;
  
  virtual emc_towerlist_t const get_tower_list() const;
  virtual float get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;

  virtual emc_clusterlist_t const get_cluster_list() const;
  virtual float get_edep_bycluster(emc_clusterid_t clusterid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;




public:
  virtual emcGeaClusterContent * clone(void) const;
  virtual void Copy(const emcClusterContent & from){ PHOOL_VIRTUAL_WARNING; }


public:
  float get_edep(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;
  float towerefrac(size_t i) const;
  //  emc_tracklist_t get_track_list() const;
  //  std::vector<emc_trkno_t> emcGeaClusterContent::get_track_list_sorted() const;
  //  float get_edep_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;
  //  emc_towerlist_t get_tower_list() const;
  //  float get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;



  ClassDef(emcGeaClusterContent, 1)
};





#endif /* ! __EMC_GEACLUSTER_CONTENT_H__ */

