#ifndef __CGLTRACK_H__
#define __CGLTRACK_H__

#include "PHObject.h"
#include <iostream>

class dCglTrackWrapper;
class CglSnglTrack;
class TClonesArray;

class CglTrack : public PHObject
{
 public:
  virtual ~CglTrack() {}

  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os=std::cout) const;

  virtual void FillFromWrapper(dCglTrackWrapper *wrap);
  virtual void FillFromClass(CglTrack *cgltrack);

  virtual unsigned int get_CglNTrack() const;
  virtual void set_CglNTrack(const unsigned int ntrk);

  virtual int set_TClonesArraySize(const unsigned int ntrk);

  virtual short get_arm(const unsigned int itrk) const;
  virtual void set_arm(const unsigned int itrk, const short ival);

  virtual short get_accrecid(const unsigned int itrk) const;
  virtual void set_accrecid(const unsigned int itrk, const short ival);

  virtual short get_mrpcrecid(const unsigned int itrk) const;
  virtual void set_mrpcrecid(const unsigned int itrk, const short ival);

  virtual short get_tofwrecid(const unsigned int itrk) const;
  virtual void set_tofwrecid(const unsigned int itrk, const short ival);

  virtual short get_dctracksid(const unsigned int itrk) const;
  virtual void set_dctracksid(const unsigned int itrk, const short ival);

  virtual short get_emcclusid(const unsigned int itrk) const;
  virtual void set_emcclusid(const unsigned int itrk, const short ival);

  virtual short get_hbdblobid(const unsigned int itrk) const;
  virtual void set_hbdblobid(const unsigned int itrk, const short ival);

  virtual short get_id(const unsigned int itrk) const;
  virtual void set_id(const unsigned int itrk, const short ival);

  virtual short get_pc1clusid(const unsigned int itrk) const;
  virtual void set_pc1clusid(const unsigned int itrk, const short ival);

  virtual short get_pc2clusid(const unsigned int itrk) const;
  virtual void set_pc2clusid(const unsigned int itrk, const short ival);

  virtual short get_pc3clusid(const unsigned int itrk) const;
  virtual void set_pc3clusid(const unsigned int itrk, const short ival);

  virtual short get_svxclusid(const unsigned int itrk, const short ilayer) const;
  virtual void set_svxclusid(const unsigned int itrk, const short ilayer, const short ival);

  virtual short get_pcrrecid(const unsigned int itrk) const;
  virtual void set_pcrrecid(const unsigned int itrk, const short ival);

  virtual short get_richringid(const unsigned int itrk) const;
  virtual void set_richringid(const unsigned int itrk, const short ival);

  virtual short get_tectrackid(const unsigned int itrk) const;
  virtual void set_tectrackid(const unsigned int itrk, const short ival);

  virtual short get_tecplaneid(const unsigned int itrk, const unsigned int iplane) const;
  virtual void set_tecplaneid(const unsigned int itrk, const unsigned int iplane, const short ival);

  virtual short get_tofrecid(const unsigned int itrk) const;
  virtual void set_tofrecid(const unsigned int itrk, const short ival);

  virtual short get_trackModel(const unsigned int itrk) const;
  virtual void set_trackModel(const unsigned int itrk, const short ival);

  virtual short get_tzrrecid(const unsigned int itrk) const;
  virtual void set_tzrrecid(const unsigned int itrk, const short ival);

  virtual float get_quality(const unsigned int itrk) const;
  virtual void set_quality(const unsigned int itrk, const float rval);

  virtual void ShutUp(const int i = 1);

  virtual CglSnglTrack* get_track(const unsigned int itrk) const;

 protected:

  virtual TClonesArray *GetCglTrk() const;

 private:
  void  virtual_warning(const char *funcname) const;

  ClassDef(CglTrack,1)
};

#endif /*__CGLTRACK_H__*/


