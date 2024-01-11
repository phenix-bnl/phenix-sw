#ifndef __CRKHITEXT_H
#define __CRKHITEXT_H

#include "PHObject.h"
#include <iostream>

class CrkHitExt : public PHObject
{
 public:
  virtual ~CrkHitExt() {}

  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;

  virtual unsigned int get_CrkNHit() const {return 0;}
  virtual void set_CrkNHit(const unsigned int nhit) {return;}

  virtual int set_TClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddCrkHitExt(const unsigned int ihit) {return;}

  virtual short get_pmt(const unsigned int ihit) const {return -999;}
  virtual void set_pmt(const unsigned int ihit, const short ipmt) {return;}

  virtual float get_npe(const unsigned int ihit) const {return -999;}
  virtual void set_npe(const unsigned int ihit, const float rval) {return;}

  virtual float get_time(const unsigned int ihit) const {return -999;}
  virtual void set_time(const unsigned int ihit, const float rval) {return;}

  virtual float get_posX(const unsigned int ihit) const {return -999;}
  virtual void set_posX(const unsigned int ihit, const float rval) {return;}

  virtual float get_posY(const unsigned int ihit) const {return -999;}
  virtual void set_posY(const unsigned int ihit, const float rval) {return;}

  virtual float get_posZ(const unsigned int ihit) const {return -999;}
  virtual void set_posZ(const unsigned int ihit, const float rval) {return;}

  ClassDef(CrkHitExt,1)

};

#endif /* __CRKHITEXT_H */
