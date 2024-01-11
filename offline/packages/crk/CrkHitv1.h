#ifndef __CRKHITV1_H__
#define __CRKHITV1_H__

#include "CrkHit.h"
#include <iostream>

class TClonesArray;
class dCrkHitWrapper;

class CrkHitv1 : public CrkHit
{
 public:
  CrkHitv1();
  virtual ~CrkHitv1();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os = std::cout) const;

  void FillFromWrapper (dCrkHitWrapper *wrp);

  unsigned int get_CrkNHit() const {return CrkNHit;}
  void set_CrkNHit(const unsigned int nhit) {CrkNHit = nhit; return;}

  int set_TClonesArraySize(const unsigned int ntrk);
  void AddCrkHit(const unsigned int ihit);

  short get_pmt(const unsigned int ihit) const;
  void set_pmt(const unsigned int ihit, const short ipmt);

  float get_npe(const unsigned int ihit) const;
  void set_npe(const unsigned int ihit, const float rval);

  float get_time(const unsigned int ihit) const;
  void set_time(const unsigned int ihit, const float rval);

 protected:

  TClonesArray *GetCrk() const {return Crk;}
  unsigned int CrkNHit;
  TClonesArray *Crk;

  ClassDef(CrkHitv1,1)

};

#endif /* __CRKHIT_H__ */
