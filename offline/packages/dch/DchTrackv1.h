#ifndef __DCHTRACKv1_H
#define __DCHTRACKv1_H

#include <iostream>
#include "TClonesArray.h"
#include "DchTrack.h"

class DchSnglTrackv1;
class PHPoint;
class PHVector;

class DchTrackv1 : public DchTrack
{
 public:
  DchTrackv1();
  virtual ~DchTrackv1();

  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;

  int set_TClonesArraySize(unsigned int fullsize);
  //----------------------------------
  int AddTrack(DchSnglTrackv1 *newtrack, const int i);  
  unsigned int get_DchNTrack() const {return DchNTrack;}
  void set_DchNTrack(const unsigned int ntrk) {DchNTrack = ntrk; return;}
  void set_trackid(const unsigned int i, const short val);
  void set_arm(const unsigned int i,const short val);
  void set_side(const unsigned int i,const short val);
  void set_quality(const unsigned int i,const short val);
  void set_phi(const unsigned int i,const float val);
  void set_alpha(const unsigned int i,const float val);
  void set_beta(const unsigned int i,const float val);
  void set_betaNoVertex(const unsigned int i,const float val);
  void set_zed(const unsigned int i,const float val);
  void set_phi0(const unsigned int i,const float val);
  void set_theta0(const unsigned int i,const float val);
  void set_momentum(const unsigned int i,const float val);
  void set_status(const unsigned int i,int val);
  void set_alpha1(const unsigned int i,const float val);
  void set_alpha2(const unsigned int i,const float val);
  void set_chi21(const unsigned int i,const float val);
  void set_chi22(const unsigned int i,const float val);
  void set_dist1(const unsigned int i,const float val);
  void set_dist2(const unsigned int i,const float val);
  void set_point(const unsigned int i,const PHPoint val); 
  void set_direction(const unsigned int i,const PHPoint val);
  void set_hits(const unsigned int i, const short plane, const short hit_id);

  short get_trackid(const unsigned int i) const;
  short get_arm(const unsigned int i) const;
  short get_side(const unsigned int i) const;
  short get_quality(const unsigned int i) const;
  float get_phi(const unsigned int i)  const;
  float get_alpha(const unsigned int i) const;
  float get_beta(const unsigned int i) const;
  float get_betaNoVertex(const unsigned int i) const;
  float get_zed(const unsigned int i)  const;
  float get_phi0(const unsigned int i)  const;
  float get_theta0(const unsigned int i) const;
  float get_momentum(const unsigned int i) const;
  int   get_status(const unsigned int i) const;
  float get_alpha1(const unsigned int i) const;
  float get_alpha2(const unsigned int i) const;
  float get_chi21(const unsigned int i) const;
  float get_chi22(const unsigned int i) const;
  float get_dist1(const unsigned int i) const;
  float get_dist2(const unsigned int i) const;
  PHPoint get_point(const unsigned int i) const;
  PHVector get_direction(const unsigned int i) const;  
  short get_hits(const unsigned int i,const short plane) const;
  short get_nx1hits(const unsigned int i) const;
  short get_nx2hits(const unsigned int i) const;
  short get_pc1hit(const unsigned int itrk) const;
  DchSnglTrackv1 * get_Track(const int i) const;
  void FillFromWrapper(dDchTracksWrapper *wrap, dDchTracksExtWrapper *wrapext);

  //--------------------------------
protected: 
  
  TClonesArray *GetDchTrk() const {return DchTrk;}
  void Clear(Option_t * = "");

  unsigned int DchNTrack;
  TClonesArray *DchTrk;

  ClassDef(DchTrackv1,1)

};

#endif
