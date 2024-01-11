#ifndef __PADRAW_H
#define __PADRAW_H

#include "PHObject.h"
#include <iostream>

class dPadRawWrapper;

class PadRaw : public PHObject
{
 public:
  virtual ~PadRaw() {}
  virtual void identify(std::ostream& os = std::cout) const;
  virtual void Reset();
  virtual int isValid() const;

  virtual void FillFromWrapper(dPadRawWrapper *wrap) {return;}
  virtual unsigned int get_PadNRaw() const {return 0;}
  virtual void set_PadNRaw(const unsigned int nclus) {return;}
  virtual int set_TClonesArraySize(const unsigned int nclus) {return 0;}
  virtual void AddPadRaw(const unsigned int iclus) {return;}

  virtual short get_arm(const unsigned int ihit) const {return -9999;}
  virtual short get_id(const unsigned int ihit) const {return -9999;}
  virtual short get_padtype(const unsigned int ihit) const {return -9999;}
  virtual short get_padx(const unsigned int ihit) const {return -9999;}
  virtual short get_padz(const unsigned int ihit) const {return -9999;}
  virtual short get_sector(const unsigned int ihit) const {return -9999;}
  virtual short get_side(const unsigned int ihit) const {return -9999;}

  virtual void set_arm(const unsigned int ihit, const short ival) {return;}
  virtual void set_id(const unsigned int ihit, const short ival) {return;}
  virtual void set_padtype(const unsigned int ihit, const short i) {return;}
  virtual void set_padx(const unsigned int ihit, const short ival) {return;}
  virtual void set_padz(const unsigned int ihit, const short ival) {return;}
  virtual void set_sector(const unsigned int ihit, const short ival) {return;}
  virtual void set_side(const unsigned int ihit, const short ival) {return;}

  ClassDef (PadRaw,1)

};

#endif /* __PADRAW_H */
