#ifndef __EMC_GEADEPOSIT_H__
#define __EMC_GEADEPOSIT_H__





#include <phool.h>
#include <PHObject.h>

#include <emctypes.h>
#include <emcContentT.h>


class emcGeaDeposit: public PHObject, public emcContentT<emcGeaDeposit> {

public:
  typedef enum { DONTCARE, DONTMERGE, MERGEALL, MERGESPECIALS } mergemode_t;
  typedef enum { GEA, ORIG, CALIB } datatype_t;


public:
  class key_type {
  public:
    key_type(): trkno(EMC_INVALID_TRKNO), towerid(EMC_INVALID_TOWERID), clusterid(EMC_INVALID_CLUSTERID){}
    key_type(const key_type & k): trkno(k.trkno), towerid(k.towerid), clusterid(k.clusterid){}
    key_type(emc_trkno_t trk, emc_towerid_t twr, emc_clusterid_t clus):
      trkno(trk), towerid(twr), clusterid(clus){}
      virtual ~key_type() {}
    emc_trkno_t trkno;
    emc_towerid_t towerid;
    emc_clusterid_t clusterid;

    ClassDef(key_type, 1)
  };


protected:
  emcGeaDeposit(){}


public:
  static emcGeaDeposit * createdef();
  static emcGeaDeposit * createdef(const emcGeaDeposit::key_type & k);


  virtual void copy(emcGeaDeposit const * from);

  emcGeaDeposit * clone() const { PHOOL_VIRTUAL_WARNING; return 0; }
  void identify(std::ostream& os = std::cout) const { PHOOL_VIRTUAL_WARNING; }
  void Reset(){ PHOOL_VIRTUAL_WARNING; }
  virtual int isValid() const { PHOOL_VIRTUAL_WARNING; return 0; }  
  virtual bool isSpecial() const;


  virtual void set_trkno(emc_trkno_t _trkno){ PHOOL_VIRTUAL_WARNING; }
  virtual emc_trkno_t get_trkno() const { PHOOL_VIRTUAL_WARNING; return EMC_INVALID_TRKNO; }

  virtual void set_towerid(emc_towerid_t _towerid){ PHOOL_VIRTUAL_WARNING; }
  virtual emc_towerid_t get_towerid() const { PHOOL_VIRTUAL_WARNING; return EMC_INVALID_TOWERID; }

  virtual void set_clusterid(emc_clusterid_t _clusterno){ PHOOL_VIRTUAL_WARNING; }
  virtual emc_clusterid_t get_clusterid() const { PHOOL_VIRTUAL_WARNING; return EMC_INVALID_CLUSTERID; }



  virtual void set_edep(float _edep, datatype_t = CALIB){ PHOOL_VIRTUAL_WARNING; }
  virtual float get_edep(datatype_t = CALIB) const { PHOOL_VIRTUAL_WARNING; return 0.0;}
  virtual void set_tof(float _tof, datatype_t = CALIB){ PHOOL_VIRTUAL_WARNING; }
  virtual float get_tof(datatype_t = CALIB) const { PHOOL_VIRTUAL_WARNING; return 0.0; }

  virtual float get_gea_edep() const { PHOOL_VIRTUAL_WARNING; return 0.0; }
  virtual void set_gea_edep(float _edep){ PHOOL_VIRTUAL_WARNING; }
  virtual void set_gea_tof(float _tof){ PHOOL_VIRTUAL_WARNING; }
  virtual float get_gea_tof() const { PHOOL_VIRTUAL_WARNING; return 0.0; }

  virtual void set_orig_edep(float _edep){ PHOOL_VIRTUAL_WARNING; }
  virtual float get_orig_edep() const { PHOOL_VIRTUAL_WARNING; return 0.0;}
  virtual void set_orig_tof(float _tof){ PHOOL_VIRTUAL_WARNING; }
  virtual float get_orig_tof() const { PHOOL_VIRTUAL_WARNING; return 0.0; }

  virtual void set_calib_edep(float _edep){ PHOOL_VIRTUAL_WARNING; }
  virtual float get_calib_edep() const { PHOOL_VIRTUAL_WARNING; return 0.0;}
  virtual void set_calib_tof(float _tof){ PHOOL_VIRTUAL_WARNING; }
  virtual float get_calib_tof() const { PHOOL_VIRTUAL_WARNING; return 0.0; }




  virtual void set_simulated(bool val) { PHOOL_VIRTUAL_WARNING; }
  virtual bool is_simulated() const { PHOOL_VIRTUAL_WARNING; return false; }



  virtual emcGeaDeposit * split(float frac){ PHOOL_VIRTUAL_WARNING; return 0; }
  virtual emcGeaDeposit * split_by_track(float frac, emc_trkno_t trkno){ PHOOL_VIRTUAL_WARNING; return 0; }
  virtual emcGeaDeposit * split_by_tower(float frac, emc_towerid_t towerid){ PHOOL_VIRTUAL_WARNING; return 0; }
  virtual emcGeaDeposit * split_by_cluster(float frac, emc_clusterid_t clusterid){ PHOOL_VIRTUAL_WARNING; return 0; }



  virtual emcGeaDeposit & operator += (emcGeaDeposit const & dep);
  


  const static bool __buildhash = true;
  key_type get_key() const { return key_type(get_trkno(), get_towerid(), get_clusterid()); }
  void set_key(key_type const & k){ set_trkno(k.trkno); set_towerid(k.towerid); set_clusterid(k.clusterid); }

  

protected:
  ClassDef(emcGeaDeposit, 1)
};




inline bool operator == (const emcGeaDeposit::key_type & a, const emcGeaDeposit::key_type & b){
  return a.trkno == b.trkno  &&  a.towerid == b.towerid  &&  a.clusterid == b.clusterid;
}




inline bool operator < (const emcGeaDeposit::key_type & a, const emcGeaDeposit::key_type & b){
  if( a.trkno < b.trkno ) return true;
  else if( a.trkno > b.trkno ) return false;

  if( a.towerid < b.towerid ) return true;
  else if( a.towerid > b.towerid ) return false;

  if( a.clusterid < b.clusterid ) return true;
  else if( a.clusterid > b.clusterid ) return false;

  return false;
}



//static ostream& operator << (ostream& os, const emcGeaDeposit::key_type & k){
//  os << "(" << k.trkno << "," << k.towerid << "," << k.clusterid << ")";
//  return os;
//}


#endif /* ! __EMC_GEADEPOSIT_H__ */

