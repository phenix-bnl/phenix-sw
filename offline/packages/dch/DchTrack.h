#ifndef __DCHTRACK_HH_
#define __DCHTRACK_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"
#include "PHPoint.h"
#include "PHVector.h"

class DchSnglTrackv1;
class dDchTracksWrapper;
class dDchTracksExtWrapper;

// The quality bits associated with each DchTrack are described in
// this 11 Mar 2002 note to phenix-off-l:

// The dch code still operates with the PC1 as the principle source of Z
// information.  When the PC choice is unique, the stereo wires try to verify.
// If they do, then UV hits exist and are unique by construction.  When they
// don't there are no UV hits.  If the PC has more than one available choice,
// the UV are consulted for the "best match".  In this case one either gets no
// UV, a best choice, or remaining ambiguity (tied).  The new bit pattern is"

// 0 (1)   X1 used
// 1 (2)   X2 used
// 2 (4)   UV found
// 3 (8)   UV unique
// 4 (16)  PC1 found
// 5 (48)  PC1 unique

// Valid patterns include:

// 49,50,51  1 1 0 0 x x PC1 found/unique, no UV
// 61,62,63  1 1 1 1 x x PC1 found/unique, UV found/unique

// 17,18,19  0 1 0 0 x x PC1 found/ambiguous, no UVs
// 21,22,23  0 1 0 1 x x PC1 found/ambiguous, UV found but tied
// 29,30,31  0 1 1 1 x x PC1 found/ambiguous, UV found w/ one best choice

// The very best case is 63.  Second best is 31, wherein the PC1 may be
// ambiguous, but the UV has a preference among choices.

// With the following we get the line number of the virtual function
// we called with PHWHERE
#define DCH_VIRTUAL_WARNING std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl

class DchTrack : public PHObject
{
 public:
  // We can't make this an anonymous enum because of limitations of
  // CINT - there doesn't seem to be any way to indicate linkage to an
  // anonymous enum.  Oh well.  There's also one little difference
  // between the enum here and the note above - I believe the value
  // for PC1unique is supposed to be 0x20 (decimal 32) (as it is here)
  // and not 0x30 (decimal 48) (as it is in the note).
  enum Q {
    X1used = 0x1,
    X2used = 0x2,
    UVfound = 0x4,
    UVunique = 0x8,
    PC1found = 0x10,
    PC1unique = 0x20
  };

  virtual ~DchTrack() {}

  virtual void Reset()
    {
      std::cout << PHWHERE 
		<< "ERROR: Reset() not implemented by daughter function" 
		<< std::endl;
      return;
    }

  virtual int isValid() const
    {
      std::cout << PHWHERE 
		<< "isValid() not implemented by daughter function" 
		<< std::endl;
      return 0;
    }

  virtual void identify(std::ostream &os=std::cout) const
    {
      os << "identify yourself: virtual DchTrack object" << std::endl;
      return;
    }

  virtual unsigned int get_DchNTrack() const {return 0;}
  virtual void set_DchNTrack(const unsigned int ntrk) {return;}

  virtual int set_TClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddDchTrack(const unsigned int itrk) {return;}
  virtual int AddTrack(DchSnglTrackv1 *newtrack, const int itrk){
    DCH_VIRTUAL_WARNING;
    return -1;
  }

  virtual void set_arm(const unsigned int itrk, const short ival) {DCH_VIRTUAL_WARNING;}
  virtual void set_nx1hits(const unsigned int itrk, const short ival) {DCH_VIRTUAL_WARNING;}
  virtual void set_nx2hits(const unsigned int itrk, const short ival) {DCH_VIRTUAL_WARNING;}
  virtual void set_pc1hit(const unsigned int itrk, const short ival) {DCH_VIRTUAL_WARNING;}
  virtual void set_quality(const unsigned int itrk, const short ival) {DCH_VIRTUAL_WARNING;}
  virtual void set_side(const unsigned int itrk, const short ival) {DCH_VIRTUAL_WARNING;}
  virtual void set_trackid(const unsigned int itrk, const short ival) {DCH_VIRTUAL_WARNING;}

  virtual void set_status(const unsigned int itrk,const int val){DCH_VIRTUAL_WARNING;}

  virtual void set_alpha(const unsigned int itrk, const float rval) {DCH_VIRTUAL_WARNING;}
  virtual void set_alpha1(const unsigned int itrk, const float val){DCH_VIRTUAL_WARNING;}
  virtual void set_alpha2(const unsigned int itrk, const float val){DCH_VIRTUAL_WARNING;}
  virtual void set_beta(const unsigned int itrk, const float rval) {DCH_VIRTUAL_WARNING;}
  virtual void set_betaNoVertex(const unsigned int itrk,const float val){DCH_VIRTUAL_WARNING;}
  virtual void set_chi21(const unsigned int itrk, const float val){DCH_VIRTUAL_WARNING;}
  virtual void set_chi22(const unsigned int itrk, const float val){DCH_VIRTUAL_WARNING;}
  virtual void set_dist1(const unsigned int itrk,const float val){DCH_VIRTUAL_WARNING;}
  virtual void set_dist2(const unsigned int itrk,const float val){DCH_VIRTUAL_WARNING;}
  virtual void set_momentum(const unsigned int itrk,const float val) {DCH_VIRTUAL_WARNING;}
  virtual void set_phi(const unsigned int itrk, const float rval) {DCH_VIRTUAL_WARNING;}
  virtual void set_phi0(const unsigned int itrk, const float rval) {DCH_VIRTUAL_WARNING;}
  virtual void set_theta0(const unsigned int itrk, const float rval) {DCH_VIRTUAL_WARNING;}
  virtual void set_zed(const unsigned int itrk, const float rval) {DCH_VIRTUAL_WARNING;}

  virtual void set_point(const unsigned int itrk,const PHPoint val){DCH_VIRTUAL_WARNING;}
  virtual void set_err_point(const unsigned int itrk,const PHPoint val) {DCH_VIRTUAL_WARNING;}
  virtual void set_direction(const unsigned int itrk,const PHPoint val){DCH_VIRTUAL_WARNING;}
  virtual void set_err_direction(const unsigned int itrk,const PHPoint val){DCH_VIRTUAL_WARNING;}
  virtual void set_hits(const unsigned int itrk, const short plane, const short hit_id){DCH_VIRTUAL_WARNING;}

  virtual short get_arm(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999;}
  virtual short get_hits(const unsigned int itrk,const short plane) const {DCH_VIRTUAL_WARNING;return -999;}
  virtual short get_nx1hits(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return 0;}
  virtual short get_nx2hits(const unsigned int itrk) const {DCH_VIRTUAL_WARNING; return 0;}
  virtual short get_pc1hit(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999;}
  virtual short get_quality(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return 0;}
  virtual short get_side(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999;}
  virtual short get_trackid(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -999;}

  virtual int   get_status(const unsigned int itrk) const {DCH_VIRTUAL_WARNING; return 0;}//use 0!

  virtual float get_alpha(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999.9;}
  virtual float get_alpha1(const unsigned int itrk) const {DCH_VIRTUAL_WARNING; return -100;}
  virtual float get_alpha2(const unsigned int itrk) const {DCH_VIRTUAL_WARNING; return -100;}
  virtual float get_beta(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999.9;}
  virtual float get_betaNoVertex(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999.9;}
  virtual float get_chi21(const unsigned int itrk) const {DCH_VIRTUAL_WARNING; return -1;}
  virtual float get_chi22(const unsigned int itrk) const {DCH_VIRTUAL_WARNING; return -1;}
  virtual float get_dist1(const unsigned int itrk) const {DCH_VIRTUAL_WARNING; return 100;}
  virtual float get_dist2(const unsigned int itrk) const {DCH_VIRTUAL_WARNING; return 100;}
  virtual float get_momentum(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999.9;}
  virtual float get_phi(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999.9;}
  virtual float get_phi0(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999.9;}
  virtual float get_theta0(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999.9;}
  virtual float get_zed(const unsigned int itrk) const {DCH_VIRTUAL_WARNING;return -9999.9;}

  virtual PHPoint get_point(const unsigned int itrk) const {
    DCH_VIRTUAL_WARNING;
    PHPoint aargh(-9999.9,-9999.9,-9999.9);
    return aargh;
  }
  virtual PHPoint get_err_point(const unsigned int itrk) const {
    DCH_VIRTUAL_WARNING;
    PHPoint aargh(-9999.9,-9999.9,-9999.9);
    return aargh;
  }
  virtual PHVector get_direction(const unsigned int itrk) const{
    DCH_VIRTUAL_WARNING;
    PHVector aargh(-9999.9,-9999.9,-9999.9);
    return aargh;
  }
  virtual PHPoint get_err_direction(const unsigned int itrk) const {
    DCH_VIRTUAL_WARNING;
    PHPoint aargh(-9999.9,-9999.9,-9999.9);
    return aargh;
  }
  virtual DchSnglTrackv1 * get_Track(const int i) const { DCH_VIRTUAL_WARNING; return 0;}

  virtual void FillFromWrapper(dDchTracksWrapper *wrap, dDchTracksExtWrapper *wrapext) {DCH_VIRTUAL_WARNING;}

  ClassDef(DchTrack,1)
};

#undef DCH_VIRTUAL_WARNING

#endif /* __DCHTRACK_HH_ */
