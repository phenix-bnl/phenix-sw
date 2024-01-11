#ifndef __UIDLL1ROADV1_H__
#define __UIDLL1ROADV1_H__

#include "uIDLL1Road.h"
#include <iostream>

class uIDLL1SnglRoad;
class TClonesArray;

class uIDLL1Roadv1 : public uIDLL1Road
{
 public:
  uIDLL1Roadv1();
  uIDLL1Roadv1(const uIDLL1Roadv1&);
  uIDLL1Roadv1& operator=(const uIDLL1Roadv1&);
  virtual ~uIDLL1Roadv1();

  uIDLL1Roadv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os = std::cout) const;

  // Actual implementations of the set/get methods...
  void set_nRoad_deep      (const unsigned int NCLUSTER) {nDeepRoad = NCLUSTER; return;}
  int  get_nRoad_deep      () const {return nDeepRoad;}

  // Routines to manipulate the particle array...
  int set_TClonesArraySize(const unsigned int ntrk);
  void AddDeepRoad          (const unsigned int itrk);
  void RemoveDeepRoad       (const unsigned int itrk);
  uIDLL1SnglRoad* AddDeepRoad (const unsigned int itrk, const uIDLL1SnglRoad& sngl);
  uIDLL1SnglRoad* get_deep_road(const unsigned int itrk) const;

 protected:
  TClonesArray *GetDeepRoad() const {return DeepRoad;}
  unsigned int nDeepRoad;
  TClonesArray *DeepRoad;

private:
  // Copy this to dest.
  void copyto(uIDLL1Roadv1& dest) const;
  ClassDef(uIDLL1Roadv1,1)
};

#endif /* __UIDLL1ROADV1_H__ */






