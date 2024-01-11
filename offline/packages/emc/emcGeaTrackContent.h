#ifndef __EMC_GEATRACK_H__
#define __EMC_GEATRACK_H__





#include <cmath>

#include <phool.h>
#include <PHObject.h>

#include <emctypes.h>
#include <emcContentT.h>
#include <emcGeaDeposit.h>
#include <emcGeaDepositHolder.h>


class emcGeaTowerContainer;


class emcGeaTrackContent: public PHObject, public emcContentT<emcGeaTrackContent>, public emcGeaDepositHolder {
protected:
public:
  emcGeaTrackContent(){};
  emcGeaTrackContent(emc_trkno_t trkno){};

public:
  virtual ~emcGeaTrackContent(){}

  // static funtion to create an emcGeaTrackContent instance
  static emcGeaTrackContent * createdef(emc_trkno_t trkno = EMC_INVALID_TRKNO);



public: /* emcContentT<emcGeaTrackContent> stuff */

  const static bool __buildhash = true;
  typedef emc_trkno_t key_type; // key type for emcGeaTrackContent: the track number
  key_type get_key() const { return get_trkno(); } // returns key for this object



public: /* emcGeaDepositHolder stuff */

  virtual emcGeaTrackContainer * get_trackcontainer() const;
  virtual emcGeaTowerContainer * get_towercontainer() const;
  virtual emcGeaClusterContainer * get_clustercontainer() const;

  // noop: no cached data at all.
  virtual void invcache(cachetype_t type = CACHED_ALL){ PHOOL_VIRTUAL_WARNING; }

  virtual emc_tracklist_t const get_track_list() const ;
  virtual float get_edep_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;

  /* virtual emc_towerlist_t const get_tower_list() const; */
  /* virtual float get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const; */

  virtual emc_clusterlist_t const get_cluster_list() const;
  virtual float get_edep_bycluster(emc_clusterid_t clusterid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;



public:
  // implementation of content-copy
  virtual void copy(emcGeaTrackContent const * from);

  // implementation of PHObject interface
  emcGeaTrackContent * clone() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // implementation of PHObject interface
  void identify(std::ostream& os = std::cout) const { PHOOL_VIRTUAL_WARNING; }

  // implementation of PHObject interface
  void Reset(){ }

  // implementation of PHObject interface
  virtual int isValid() const { PHOOL_VIRTUAL_WARNING; return 0; }


public:
  // returns the total initial momentum (in GeV/c)
  virtual float get_ptot() const { PHOOL_VIRTUAL_WARNING; return 0; }
  
  // returns initial transverse momentum (in GeV/c)
  virtual float get_pt() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns distance from (0,0,0) of place of birth
  virtual float get_radius() const { PHOOL_VIRTUAL_WARNING; return 0; }


public:
  // returns real track no. a few special track numbers are definied, see \ref emctypes.h
  virtual emc_trkno_t get_trkno() const { PHOOL_VIRTUAL_WARNING; return EMC_INVALID_TRKNO; }

  // returns id of input file that contained this track (used in mergeing).
  virtual short	get_input() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns new kind of ancestry level: for primary particle it is 0, for children
  // of the primary particles it is 1, for grandchildren of the promary particle it
  // is 2, etc...
  virtual short	get_anclvl() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns Particle ID (as defined by geant).
  virtual short	get_pid() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns initial kinetic energy
  virtual float	get_ekin() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns x coordinate of place of birth (in cm)
  virtual float	get_x() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns y coordinate of place of birth (in cm)
  virtual float	get_y() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns z coordinate of place of birth (in cm)
  virtual float	get_z() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns x component of initial momentum (in GeV/c)
  virtual float	get_px() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns y component of initial momentum (in GeV/c)
  virtual float	get_py() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns z component of initial momentum (in GeV/c)
  virtual float	get_pz() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns x coordinate of impact point. (first point where this track
  // deposited energy in the emcal)
  virtual float	get_impx() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns y coordinate of impact point. (first point where this track
  // deposited energy in the emcal)
  virtual float	get_impy() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns z coordinate of impact point. (first point where thsi track
  // deposited deposited energy in the emcal)
  virtual float	get_impz() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // returns track number of the parent of this track. if this track
  // is a primary particle (= has no parent), it returns 0.
  virtual emc_trkno_t get_parent_trkno() const { PHOOL_VIRTUAL_WARNING; return 0; }

  // return list of daughter tracks. a daughter track is a track, whose parent
  // is this track.
  virtual emc_tracklist_t const & get_daughter_list() const;

  virtual void set_trkno(emc_trkno_t _trkno){ PHOOL_VIRTUAL_WARNING; } ///< sets track number
  virtual void set_input(short _input){ PHOOL_VIRTUAL_WARNING; } ///< sets input file number
  virtual void set_anclvl(short _anclvl){ PHOOL_VIRTUAL_WARNING; } ///< sets ancestry level
  virtual void set_pid(short _pid){ PHOOL_VIRTUAL_WARNING; } ///< sets particle id
  virtual void set_ekin(float _ekin){ PHOOL_VIRTUAL_WARNING; } ///< sets kinetic energy
  virtual void set_x(float _x){ PHOOL_VIRTUAL_WARNING; } ///< sets x coordinate of place of birth
  virtual void set_y(float _y){ PHOOL_VIRTUAL_WARNING; } ///< sets y coordinate of place of birth
  virtual void set_z(float _z){ PHOOL_VIRTUAL_WARNING; } ///< sets z coordinate of place of birth
  virtual void set_px(float _px){ PHOOL_VIRTUAL_WARNING; } ///< sets x component of impulse
  virtual void set_py(float _py){ PHOOL_VIRTUAL_WARNING; } ///< sets y component of impulse
  virtual void set_pz(float _pz){ PHOOL_VIRTUAL_WARNING; } ///< sets z component of impulse
  virtual void set_impx(float _impx){ PHOOL_VIRTUAL_WARNING; } ///< sets x coordinate of impact point
  virtual void set_impy(float _impy){ PHOOL_VIRTUAL_WARNING; } ///< sets y coordinate of impact point
  virtual void set_impz(float _impz){ PHOOL_VIRTUAL_WARNING; } ///< sets z coordinate of impact point
  virtual void set_parent_trkno(emc_trkno_t _parent_trkno){ PHOOL_VIRTUAL_WARNING; } ///< sets parent track number
  virtual void clear_daughter_list(){ PHOOL_VIRTUAL_WARNING; } ///< clears list of daughters
  // adds a track number to the list of daughters
  virtual void append_to_daughter_list(emc_trkno_t trkno){ PHOOL_VIRTUAL_WARNING; }





  virtual float get_edep(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;

  virtual float get_tof(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const { PHOOL_VIRTUAL_WARNING; return NAN; }


  // returns the list of tower this track deposited energy in.
  virtual const emc_towerlist_t get_tower_list() const { PHOOL_VIRTUAL_WARNING; return emc_towerlist_t(); }

  virtual float get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const
    { PHOOL_VIRTUAL_WARNING; return NAN; }
  
  // sets the energy deposited in a particular tower. the track must be associated
  // with a towercontainer, so that the towers affected can be updated.
  // 
  // @param towerid is the id of tower 
  // @param edep is the new value of the deposited energy
  // @param type is the deposit type, see \ref emcGeaDeposit for more info
  virtual void set_edep_bytower(emc_towerid_t towerid, float edep, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB)
    { PHOOL_VIRTUAL_WARNING; }
  
  // returns the tof for this track for a particular tower.
  // 
  // @param towerid is the id of tower 
  // @param type is the tof type, see \ref emcGeaDeposit for more info
  virtual float get_tof_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const
    { PHOOL_VIRTUAL_WARNING; return NAN; }
  
  // sets the tof for this track for a particular tower. the track must be associated
  // with a towercontainer, so that the towers affected can be updated.
  // 
  // @param towerid is the id of tower 
  // @param tof is the new value of tof 
  // @param type is the tof type, see \ref emcGeaDeposit for more info
  virtual void set_tof_bytower(emc_towerid_t towerid, float tof, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB)
    { PHOOL_VIRTUAL_WARNING; }

  // clears all deposit and tof info for this track. the track must be associated
  // with a towercontainer, so that the towers affected can be updated.
  virtual void clear_deposits(){ PHOOL_VIRTUAL_WARNING; }



  ClassDef(emcGeaTrackContent, 1)
};





#endif /* ! __EMC_GEATRACK_H__ */

