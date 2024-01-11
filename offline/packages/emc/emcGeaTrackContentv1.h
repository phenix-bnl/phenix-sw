#ifndef __EMC_GEATRACK_V1_H__
#define __EMC_GEATRACK_V1_H__





#include <map>

#include <phool.h>
#include <PHObject.h>

#include <emcGeaTrackContent.h>
#include <emcGeaDeposit.h>


class emcGeaTowerContainer;


class emcGeaTrackContentv1: public emcGeaTrackContent {
public:
  /// default constructor
  emcGeaTrackContentv1(emc_trkno_t thetrkno = EMC_INVALID_TRKNO);

  /// copy constructor
  emcGeaTrackContentv1(const emcGeaTrackContentv1 & t);

  /// destructor
  ~emcGeaTrackContentv1(){ Reset(); }

  //  virtual void copy(emcGeaTrackContent const * from);


public: /* standard PHObject stuff */
  emcGeaTrackContentv1 * clone() const { return new emcGeaTrackContentv1(*this); }
  void identify(std::ostream& os = std::cout) const;
  void Reset();
  virtual int isValid() const { return (trkno != 0); }


public: /* emcGeadDepositHolder stuff */
  //virtual emcGeaTrackContainer * get_trackcontainer() const;
  //virtual emcGeaTowerContainer * get_towercontainer() const;
  //virtual emcGeaClusterContainer * get_clustercontainer() const;

  // noop: no cached data at all.
  virtual void invcache(cachetype_t type = CACHED_ALL){ }

  // virtual emc_tracklist_t const get_track_list() const ;
  // virtual float get_edep_bytrack(emc_trkno_t trkno, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;

  virtual emc_towerlist_t const get_tower_list() const;
  virtual float get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;

  //virtual emc_clusterlist_t const get_cluster_list() const;
  //  virtual float get_edep_bycluster(emc_clusterid_t clusterid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;



public:
  float get_ptot() const;
  float get_pt() const;
  float get_radius() const;


public:
  emc_trkno_t get_trkno() const { return trkno; }
  short	get_input() const { return input; }
  short	get_anclvl() const { return anclvl; }
  short	get_pid() const { return pid; }
  float	get_ekin() const { return ekin; }
  float	get_x() const { return xyz[0]; }
  float	get_y() const { return xyz[1]; }
  float	get_z() const { return xyz[2]; }
  float	get_px() const { return pxyz[0]; }
  float	get_py() const { return pxyz[1]; }
  float	get_pz() const { return pxyz[2]; }
  float	get_impx() const { return impxyz[0]; }
  float	get_impy() const { return impxyz[1]; }
  float	get_impz() const { return impxyz[2]; }
  emc_trkno_t get_parent_trkno() const { return itparent; }
  emc_tracklist_t const & get_daughter_list() const { return daughterlist; }

  void set_trkno(emc_trkno_t _trkno){ trkno = _trkno; }
  void set_input(short _input){ input = _input; }
  void set_anclvl(short _anclvl){ anclvl = _anclvl; }
  void set_pid(short _pid){ pid = _pid; }
  void set_ekin(float _ekin){ ekin = _ekin; }
  void set_x(float _x){ xyz[0] = _x; }
  void set_y(float _y){ xyz[1] = _y; }
  void set_z(float _z){ xyz[2] = _z; }
  void set_px(float _px){ pxyz[0] = _px; }
  void set_py(float _py){ pxyz[1] = _py; }
  void set_pz(float _pz){ pxyz[2] = _pz; }
  void set_impx(float _impx){ impxyz[0] = _impx; }
  void set_impy(float _impy){ impxyz[1] = _impy; }
  void set_impz(float _impz){ impxyz[2] = _impz; }
  void set_parent_trkno(emc_trkno_t _parent_trkno){ itparent = _parent_trkno; }
  void append_to_daughter_list(emc_trkno_t trkno){ daughterlist.insert(trkno); }
  void clear_daughter_list(){ daughterlist.clear(); }


  //  virtual emcGeaTowerContainer * get_towercontainer() const { return towers; }
  //  virtual void set_towercontainer(emcGeaTowerContainer * towers){ this->towers = towers; }

  //  virtual float get_edep(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;
  virtual float get_tof(emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;

  //  virtual const emc_towerlist_t get_tower_list() const;
  //  virtual float get_edep_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;
  virtual void set_edep_bytower(emc_towerid_t towerid, float edep, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB);
  virtual float get_tof_bytower(emc_towerid_t towerid, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB) const;
  virtual void set_tof_bytower(emc_towerid_t towerid, float tof, emcGeaDeposit::datatype_t type = emcGeaDeposit::CALIB);

  virtual void clear_deposits();


public:
  emc_trkno_t trkno;	// GEANT track number ("true_track" in fkin)
  short	input;		// serial number if more than one input present
  short	anclvl;		// ancestry level
  short	pid;		// GEANT PID
  float	ekin;		// total kinetic energy
  float	xyz[3];		// vertex, where born
  float	pxyz[3];	// momentum
  float	impxyz[3];	// Impact point - pos. of first hit - on the EMCal
  emc_trkno_t	itparent; // true track number of the parent
  long	twrhit;		// Number of towers where it deposited energy
  emc_tracklist_t daughterlist; // list of daghter tracks (not complete if a daughter did not deposit energy in emcal!)
  
  std::map<emc_towerid_t, emcGeaDeposit *> deposits; // deposits from this track sorted by tower


public:
  //  typedef emcGeaTrackContent::key_type key_type;



  ClassDef(emcGeaTrackContentv1, 1)
};





#endif /* ! __EMC_GEATRACK_V1_H__ */

