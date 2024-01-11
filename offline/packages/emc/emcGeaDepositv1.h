#ifndef __EMC_GEADEPOSIT_V1_H__
#define __EMC_GEADEPOSIT_V1_H__





#include <cmath>

#include <emcGeaDeposit.h>


class emcGeaDepositv1: public emcGeaDeposit {
public:
  emcGeaDepositv1(){ Reset(); }
  ~emcGeaDepositv1(){ Reset(); }


public:
  virtual emcGeaDepositv1 * clone() const { return new emcGeaDepositv1(*this); }
  virtual void identify(std::ostream& os = std::cout) const { os << "emcGeaDepositv1"; }
  virtual void Reset();
  virtual int isValid() const { return 1; }  
  

  virtual void set_trkno(emc_trkno_t _trkno){ trkno = _trkno; }
  virtual emc_trkno_t get_trkno() const { return trkno; }

  virtual void set_towerid(emc_towerid_t _towerid){ towerid = _towerid; }
  virtual emc_towerid_t get_towerid() const { return towerid; }

  virtual void set_clusterid(emc_clusterid_t _clusterid){ clusterid = _clusterid; }
  virtual emc_clusterid_t  get_clusterid() const { return clusterid; }



  virtual void set_edep(float _edep, datatype_t type = CALIB);
  virtual float get_edep(datatype_t type = CALIB) const;
  virtual void set_tof(float _tof, datatype_t type = CALIB);
  virtual float get_tof(datatype_t type = CALIB) const;

  virtual void set_gea_edep(float _geaedep){ geaedep = _geaedep; }
  virtual float get_gea_edep() const { return geaedep;}
  virtual void set_gea_tof(float _geatof){ geatof = _geatof; }
  virtual float get_gea_tof() const { return geatof; }

  virtual void set_orig_edep(float _origedep){ origedep = _origedep; }
  virtual float get_orig_edep() const { return origedep;}
  virtual void set_orig_tof(float _origtof){ origtof = _origtof; }
  virtual float get_orig_tof() const { return origtof; }

  virtual void set_calib_edep(float _edep){ edep = _edep; }
  virtual float get_calib_edep() const { return edep;}
  virtual void set_calib_tof(float _tof){ tof = _tof; }
  virtual float get_calib_tof() const { return tof; }
  


  virtual void set_simulated(bool value){ simulated = value; }
  virtual bool is_simulated() const { return simulated; }



  virtual emcGeaDepositv1 * split(float frac);
  virtual emcGeaDepositv1 * split_by_track(float frac, emc_trkno_t trkno);
  virtual emcGeaDepositv1 * split_by_tower(float frac, emc_towerid_t towerid);
  virtual emcGeaDepositv1 * split_by_cluster(float frac, emc_clusterid_t clusterid);



  typedef emcGeaDeposit::key_type key_type;



private:
  emc_trkno_t trkno; // true trackno
  emc_towerid_t towerid; // towerid for this deposit
  emc_clusterid_t clusterid; // the cluster number
  float edep;        // deposited energy
  float tof;         // time of flight
  float origedep;    // original energy, not calibrated, but attenuated/etc if simulated
  float origtof;     // original time of flight, not calibrated, but attenuated/etc if simulated
  float geaedep;    // original energy, given by geant
  float geatof;     // original time of flight, given by geant

  bool simulated;    // does this deposit come from simulation?


  ClassDef(emcGeaDepositv1, 1)
};





#endif /* ! __EMC_GEADEPOSIT_V1_H__ */

