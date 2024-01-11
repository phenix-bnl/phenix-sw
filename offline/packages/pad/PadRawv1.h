#ifndef __PADRAWV1_H
#define __PADRAWV1_H

#include "PadRaw.h"
#include <iostream>

class dPadRawWrapper;
class TClonesArray;

class PadRawv1 : public PadRaw
{
 public:
  PadRawv1();
  virtual ~PadRawv1();
  void identify(std::ostream& os = std::cout) const;
  void Reset();
  int isValid() const;

  void FillFromWrapper(dPadRawWrapper *wrap);
  unsigned int get_PadNRaw() const {return PadNRaw;}
  void set_PadNRaw(const unsigned int nclus) {PadNRaw = nclus;return;}
  int set_TClonesArraySize(const unsigned int nclus);
  void AddPadRaw(const unsigned int iclus);

  short get_arm(const unsigned int ihit) const;
  short get_id(const unsigned int ihit) const;
  short get_padtype(const unsigned int ihit) const;
  short get_padx(const unsigned int ihit) const;
  short get_padz(const unsigned int ihit) const;
  short get_sector(const unsigned int ihit) const;
  short get_side(const unsigned int ihit) const;

  void set_arm(const unsigned int ihit, const short ival);
  void set_id(const unsigned int ihit, const short ival);
  void set_padtype(const unsigned int ihit, const short ival);
  void set_padx(const unsigned int ihit, const short ival);
  void set_padz(const unsigned int ihit, const short ival);
  void set_sector(const unsigned int ihit, const short ival);
  void set_side(const unsigned int ihit, const short ival);


 protected:
  TClonesArray *GetPcRaw() const {return PcRaw;}
  unsigned int PadNRaw;
  TClonesArray *PcRaw;

  ClassDef (PadRawv1,1)

};

#endif /* __PADRAWV1_H */

