#ifndef __CGLTRACKV5_H__
#define __CGLTRACKV5_H__

#include <iostream>
#include "CglTrack.h"

class dCglTrackWrapper;
class TClonesArray;

class CglTrackv5 : public CglTrack
{
 public:
  CglTrackv5();
  virtual ~CglTrackv5();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os=std::cout) const;
  void  FillFromWrapper(dCglTrackWrapper *wrap);

  unsigned int get_CglNTrack() const {return CglNTrack;}
  void set_CglNTrack(const unsigned int ntrk) {CglNTrack = ntrk; return;}

  void AddCglTrack(const unsigned int itrk);

  short get_arm(const unsigned int itrk) const;
  void set_arm(const unsigned int itrk, const short ival);

  short get_id(const unsigned int itrk) const;
  void set_id(const unsigned int itrk, const short ival);

  short get_dctracksid(const unsigned int itrk) const;
  void set_dctracksid(const unsigned int itrk, const short ival);

  short get_tectrackid(const unsigned int itrk) const;
  void set_tectrackid(const unsigned int itrk, const short ival);

  short get_pc1clusid(const unsigned int itrk) const;
  void set_pc1clusid(const unsigned int itrk, const short ival);

  short get_pc2clusid(const unsigned int itrk) const;
  void set_pc2clusid(const unsigned int itrk, const short ival);

  short get_pc3clusid(const unsigned int itrk) const;
  void set_pc3clusid(const unsigned int itrk, const short ival);

  short get_tofrecid(const unsigned int itrk) const;
  void set_tofrecid(const unsigned int itrk, const short ival);

  short get_accrecid(const unsigned int itrk) const;
  void set_accrecid(const unsigned int itrk, const short ival);

  short get_mrpcrecid(const unsigned int itrk) const;
  void set_mrpcrecid(const unsigned int itrk, const short ival);

  short get_emcclusid(const unsigned int itrk) const;
  void set_emcclusid(const unsigned int itrk, const short ival);

  short get_richringid(const unsigned int itrk) const;
  void set_richringid(const unsigned int itrk, const short ival);

  short get_trackModel(const unsigned int itrk) const;
  void set_trackModel(const unsigned int itrk, const short ival);

  float get_quality(const unsigned int itrk) const;
  void set_quality(const unsigned int itrk, const float ival);

 protected:
  TClonesArray *GetCglTrk() const {return CglTrk;}
  unsigned int CglNTrack;
  TClonesArray *CglTrk;

  ClassDef(CglTrackv5,1)
};

#endif /*__CGLTRACKV5_H__*/
