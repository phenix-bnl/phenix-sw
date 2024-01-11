#ifndef __CRKHIT_H
#define __CRKHIT_H

#include "PHObject.h"
#include <iostream>

class dCrkHitWrapper;

class CrkHit : public PHObject
{
 public:
  virtual ~CrkHit() {}

  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;

  virtual void FillFromWrapper (dCrkHitWrapper *wrp) {return;}

  virtual unsigned int get_CrkNHit() const {return 0;}
  virtual void set_CrkNHit(const unsigned int nhit) {return;}

  virtual int set_TClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddCrkHit(const unsigned int ihit) {return;}

  virtual short get_pmt(const unsigned int ihit) const {return -999;}
  virtual void set_pmt(const unsigned int ihit, const short ipmt) {return;}

  virtual float get_npe(const unsigned int ihit) const {return -999;}
  virtual void set_npe(const unsigned int ihit, const float rval) {return;}

  virtual float get_time(const unsigned int ihit) const {return -999;}
  virtual void set_time(const unsigned int ihit, const float rval) {return;}


  ClassDef(CrkHit,1)

};

#endif /* __CRKHIT_H */
