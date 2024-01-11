#ifndef __EMC_GEATOWER_CONTENT_V4_H__
#define __EMC_GEATOWER_CONTENT_V4_H__





#include <vector>

#include <emcGeaTowerContent.h>
#include <emcGeaDeposit.h>



class emcGeaTowerContentv4: public emcGeaTowerContent {
public:
  emcGeaTowerContentv4(emc_towerid_t towerid = EMC_INVALID_TOWERID);
  emcGeaTowerContentv4(const emcGeaTowerContentv4 & t){ this->copy(&t); }
  virtual ~emcGeaTowerContentv4(){}
  
  
   // standard PHObject stuff
public:
  virtual emcGeaTowerContentv4 * clone() const { return new emcGeaTowerContentv4(*this); }
  virtual void Reset();


  // emcTowerContent stuff
public:
  virtual void Copy(const emcTowerContent & from);

  virtual void set_towerid(emc_towerid_t towerid);
  virtual emc_towerid_t get_towerid() const { return _towerid; }
  virtual int TowerID(void) const { return get_towerid(); }
  virtual int TowerId(void) const { return get_towerid(); }
  virtual void SetID(int fem, int channel);
  virtual int towerid(void) const { return get_towerid(); }

  virtual bool canHaveCalib() const { return false; } // this is weird: can not have, but has calib...
  virtual bool hasCalib() const { return true; }
  virtual bool canHaveDC() const { return false; }
  virtual bool hasDC() const { return false; }
  virtual bool canHaveGain() const { return /*true*/ false; }
  virtual bool hasGain() const { return false; }
  virtual bool canHaveMerSimStatus() const { return false; } 
  virtual bool hasMerSimStatus() const { return false; }
  virtual bool canHaveRaw() const { return true; }
  virtual bool hasRaw() const { return hasraw; }
  virtual bool hasReference() const { return false; }

  virtual int Channel() const;
  virtual int FEM() const;

  virtual void SetDataError(int dataerror){ this->dataerror = dataerror; }
  virtual int DataError() const { return dataerror; }
  virtual void SetNeighbours(unsigned int error, unsigned int warning){ errormap = error; warnmap = warning; }
  virtual unsigned int ErrorNeighbours() const { return errormap; }
  virtual unsigned int WarnNeighbours(void) const { return warnmap; }
 
  virtual void SetRaw(int hgpost, int hgpre, int lgpost, int lgpre, int tac,
		      int amupre = 0, int amupost = 0, int amutac = 0, int beamclock = 0);
  virtual int HGPost() const { return hgpost; }
  virtual int HGPre() const { return hgpre; }  
  virtual int LGPost() const { return lgpost; }
  virtual int LGPre() const { return lgpre; }  
  virtual int TAC() const { return tac; }
  virtual int AMUPre() const { return amupre; }
  virtual int AMUPost() const { return amupost; }
  virtual int AMUTAC() const { return amutac; }
  virtual int BeamClock() const { return beamclock; }


  virtual void SetADCTDC(int adc, int tdc, int hg = 0, int lg = 0);
  virtual int HG() const { return higain; }
  virtual int LG() const { return logain; }
  virtual int TDC() const { return tdc; }


  virtual void SetCalibrated(float energy, float tof);
  virtual float Energy() const { return get_edep(emcGeaDeposit::CALIB); }
  virtual float ToF() const { return get_tof(emcGeaDeposit::CALIB); }
  virtual float UncorrectedToF() const { return get_tof(emcGeaDeposit::ORIG); }


  //  virtual emcGeaTrackContainer * get_trackcontainer() const { return tracks; }
  //  virtual void set_trackcontainer(emcGeaTrackContainer * tracks){ this->tracks = tracks; }
  //  virtual void add_track(emc_trkno_t trkno){ tracklist.insert(trkno); }
  //  virtual void clear_tracks(){ tracklist.clear(); }
  //  const emc_tracklist_t get_track_list() const { return tracklist; }

  // noop: no cached data at all.
  virtual void invcache(cachetype_t type = CACHED_ALL){ }


public:
  typedef emcGeaTowerContent::key_type key_type;
  virtual void copy(emcGeaTowerContent const * from);


protected:
  emc_towerid_t _towerid;

  int dataerror;
  unsigned int errormap;
  unsigned int warnmap;

  bool hasraw;
  int hgpost, hgpre;
  int lgpost, lgpre;
  int tac;
  int amupre, amupost, amutac, beamclock;

  bool hasgain;
  int higain;
  int logain;
  int tdc;

  //  emcGeaTrackContainer * tracks; //! table of tracks
  //  emc_tracklist_t tracklist;
 
  ClassDef(emcGeaTowerContentv4, 1)
};





#endif /* ! __EMC_GEATOWER_CONTENT_V4_H__ */

