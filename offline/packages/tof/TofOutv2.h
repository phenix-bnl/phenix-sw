#ifndef __TOFOUTV2_H
#define __TOFOUTV2_H

#include "phool.h"
#include "TofOut.h"

class dTofReconstructedWrapper;
class TClonesArray;

class TofOutv2: public TofOut
{

 public:

  TofOutv2();
  virtual ~TofOutv2();

  void identify(std::ostream& os = std::cout) const;

  int isValid() const;

  void Reset();

  unsigned int set_TClonesArraySize(const unsigned int nhit);

  unsigned int get_TofNHit() const {return TofNHit;}
  void set_TofNHit(const unsigned int nhit) {TofNHit=nhit;return;}
  

  void AddTofHit(const unsigned int thishit);

  short get_id(const unsigned int ihit) const;
  void set_id(const unsigned int ihit, const short ival);

  short get_panel(const unsigned int ihit) const;
  void set_panel(const unsigned int ihit, const short ival);

  short get_sector(const unsigned int ihit) const;
  void set_sector(const unsigned int ihit, const short ival);

  short get_side(const unsigned int ihit) const;
  void set_side(const unsigned int ihit, const short ival);

  short get_slat(const unsigned int ihit) const;
  void set_slat(const unsigned int ihit, const short ival);

  short get_slatid(const unsigned int ihit) const;
  void set_slatid(const unsigned int ihit, const short ival);

  short get_qvc(const unsigned int ihit, const short i) const;
  void set_qvc(const unsigned int ihit, const short i, const short ival);

  short get_tvc(const unsigned int ihit, const short i) const;
  void set_tvc(const unsigned int ihit, const short i, const short ival);

  float get_eloss(const unsigned int ihit) const;
  void set_eloss(const unsigned int ihit, const float rval);

  float get_eloss_err(const unsigned int ihit) const;
  void set_eloss_err(const unsigned int ihit, const float rval);

  float get_tof(const unsigned int ihit) const;
  void set_tof(const unsigned int ihit, const float rval);

  float get_tof_err(const unsigned int ihit) const;
  void set_tof_err(const unsigned int ihit, const float rval);

  float get_xtof(const unsigned int ihit, const short ival) const;
  void set_xtof(const unsigned int ihit, const short ival, const float rval);

  float get_xtof_err(const unsigned int ihit, const short ival) const;
  void set_xtof_err(const unsigned int ihit, const short ival, const float rval);

  float get_tdiff(const unsigned int ihit) const;
  void set_tdiff(const unsigned int ihit, const float rval);

 protected:
  
  TClonesArray *GetTofHits() const {return TofHits;}

  unsigned int TofNHit;
  TClonesArray *TofHits;

  ClassDef(TofOutv2,1)

};

#endif /* __TOFOUTV2_H */
