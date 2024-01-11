
#ifndef __ACCHITV1_H_
#define __ACCHITV1_H_

#include "TClonesArray.h"

#include "AccHit.h"
#include "AccSnglHitv1.h"

class AccHitv1 : public AccHit
{
 public:
  AccHitv1();
  AccHitv1(const AccHitv1&);
  AccHitv1& operator=(const AccHitv1&);
  virtual ~AccHitv1();

  AccHitv1* clone() const;

  // the standard PHObject response functions
  void Reset();
  int isValid() const;
  void identify(std::ostream& os=std::cout) const;

  // actual implementations of the set/get methods
  void set_nhit (const unsigned int NHIT) {nHit = NHIT; return;}
  int  get_nhit () const {return nHit;}

  // routines to manipulate the particle array
  int set_TClonesArraySize (const unsigned int nhit);
  void AddHit              (const unsigned int ihit);
  void RemoveHit           (const unsigned int ihit);
  AccSnglHitv1* AddHit     (const unsigned int ihit, const AccSnglHit& sngl);
  AccSnglHitv1* get_hit    (const unsigned int ihit) const;

  void set_boxid(const int ihit, const int val);
  void set_npe(const int ihit, const float val);
  void set_tof(const int ihit, const float val);
  void set_tdiff(const int ihit, const float val);
  void set_xyz(const int ihit, int ixyz, const float val);

  int get_boxid(const int ihit) const;
  float get_npe(const int ihit) const;
  float get_tof(const int ihit) const;
  float get_tdiff(const int ihit) const;
  float get_xyz(const int ihit, const int ixyz) const;

 protected:
  TClonesArray* GetHit() const {return AccHits;}
  unsigned int  nHit;
  TClonesArray* AccHits;

 private:
  // copy this to dest
  void copyto(AccHitv1& dest) const;

  ClassDef(AccHitv1,1)
};

#endif
