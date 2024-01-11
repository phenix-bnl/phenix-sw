#ifndef __CRKHITEXTV1_H__
#define __CRKHITEXTV1_H__

#include "CrkHitExt.h"
#include <iostream>

class TClonesArray;

class CrkHitExtv1 : public CrkHitExt
{
 public:
  CrkHitExtv1();
  virtual ~CrkHitExtv1();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os = std::cout) const;

  unsigned int get_CrkNHit() const {return CrkNHit;}
  void set_CrkNHit(const unsigned int nhit) {CrkNHit = nhit; return;}

  int set_TClonesArraySize(const unsigned int ntrk);
  void AddCrkHitExt(const unsigned int ihit);

  short get_pmt(const unsigned int ihit) const;
  void set_pmt(const unsigned int ihit, const short ipmt);

  float get_npe(const unsigned int ihit) const;
  void set_npe(const unsigned int ihit, const float rval);

  float get_time(const unsigned int ihit) const;
  void set_time(const unsigned int ihit, const float rval);

  float get_posX(const unsigned int ihit) const;
  void set_posX(const unsigned int ihit, const float rval);

  float get_posY(const unsigned int ihit) const;
  void set_posY(const unsigned int ihit, const float rval);

  float get_posZ(const unsigned int ihit) const;
  void set_posZ(const unsigned int ihit, const float rval);

 protected:

  TClonesArray *GetCrk() const {return Crk;}
  unsigned int CrkNHit;
  TClonesArray *Crk;

  ClassDef(CrkHitExtv1,1)

};

#endif /* __CRKHITEXT_H__ */
