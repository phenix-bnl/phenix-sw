#ifndef __EMC_GEATOWER_CONTENT_H__
#define __EMC_GEATOWER_CONTENT_H__





#include <phool.h>

#include <emctypes.h>
#include <emcContentT.h>
#include <emcTowerContent.h>
#include <emcGeaDeposit.h>
#include <emcGeaDepositHolder.h>



class emcGeaTrackContainer;


class emcGeaTowerContent: public emcTowerContent, public emcContentT<emcGeaTowerContent>, public emcGeaDepositHolder {
protected:
public:
  emcGeaTowerContent(){}
  emcGeaTowerContent(emc_towerid_t towerid){}


public:

  
  emcGeaTowerContent * create(void) const { PHOOL_VIRTUAL_WARNING; return 0; }

  static emcGeaTowerContent * createdef(emc_towerid_t towerid = EMC_INVALID_TOWERID);


public:  /* phool/emcClusterContent stuff */
  virtual emcGeaTowerContent * clone() const { return new emcGeaTowerContent(*this); }
  virtual void Copy(const emcTowerContent& from) { PHOOL_VIRTUAL_WARNING; }
  virtual void Copy(TObject&) const { PHOOL_VIRTUAL_WARNING; } // unhide TObject::Copy

public: /* emcContentT<emcGeaTowerContent> stuff */
  const static bool __buildhash = true;
  typedef emc_towerid_t key_type;
  key_type get_key() const { return get_towerid(); }
  virtual void copy(emcGeaTowerContent const * from){ PHOOL_VIRTUAL_WARNING; }


public: /* emcGeaDepositHolder stuff */
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
  // sets towerid of this tower
  // 
  // @param towerid is the new towerid in the range [0..24767]
  virtual void set_towerid(emc_towerid_t towerid){ PHOOL_VIRTUAL_WARNING; };

  // returns towerid of this tower.
  virtual emc_towerid_t get_towerid() const { PHOOL_VIRTUAL_WARNING; return EMC_INVALID_TOWERID; }

  virtual float get_edep(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;

  virtual void set_edep(float edep, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB);


  virtual void shift_edep(float shift, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB);


  virtual void scale_edep(float scale, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB);



  virtual float get_tof(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;
  virtual void set_tof(float tof, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB);


  ClassDef(emcGeaTowerContent, 1)
};





#endif /* ! __EMC_GEATOWER_CONTENT_H__ */

