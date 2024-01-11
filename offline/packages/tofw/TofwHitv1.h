#ifndef __TOFWHITV1_H_
#define __TOFWHITV1_H_

#include "TClonesArray.h"

#include "TofwHit.h"
#include "TofwSnglHitv1.h"

class TofwHitv1 : public TofwHit
{
 public:
  TofwHitv1();
  TofwHitv1(const TofwHitv1&);
  TofwHitv1& operator=(const TofwHitv1&);
  virtual ~TofwHitv1();

  TofwHitv1* clone() const;

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
  TofwSnglHitv1* AddHit     (const unsigned int ihit, const TofwSnglHit& sngl);
  TofwSnglHitv1* get_hit    (const unsigned int ihit) const;

  void set_boxid(const int ihit, const int val);
  void set_chamberid(const int ihit, const int val);
  void set_nstrip(const int ihit, const int val);
  void set_max(const int ihit, const int istrip);
  void set_stripid(const int ihit, const int istrip, const int val);
  void set_time(const int ihit, const int istrip, const float val);
  void set_charge(const int ihit, const int istrip, const float val);
  void set_rawadc(const int ihit, const int istrip, int irawadc, const float val);
  void set_rawtdc(const int ihit, const int istrip, int irawtdc, const float val);
  void set_xyz(const int ihit, const int istrip, int ixyz, const float val);
 

  int   get_boxid(const int ihit) const;
  int   get_chamberid(const int ihit) const;
  int   get_nstrip(const int ihit) const;
  int   get_max(const int ihit) const;

  int   get_stripid(const int ihit, const int istrip) const;
  int   get_stripid(const int ihit) const;

  float get_time(const int ihit, const int istrip) const;
  float get_time(const int ihit) const;

  float get_charge(const int ihit, const int istrip) const;
  float get_charge(const int ihit) const;

  float get_rawadc(const int ihit, const int istrip, const int irawadc) const;
  float get_rawadc(const int ihit, const int irawadc) const;

  float get_rawtdc(const int ihit, const int istrip, const int irawtdc) const;
  float get_rawtdc(const int ihit, const int irawtdc) const;

  float get_xyz(const int ihit, const int istrip, const int ixyz) const;
  float get_xyz(const int ihit, const int ixyz) const;

 protected:
  TClonesArray* GetHit() const {return TofwHits;}
  unsigned int  nHit;
  TClonesArray* TofwHits;

 private:
  // copy this to dest
  void copyto(TofwHitv1& dest) const;

  ClassDef(TofwHitv1,1)
};

#endif
