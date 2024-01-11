#ifndef __CRKRINGMICROV3_H
#define __CRKRINGMICROV3_H

#include "CrkRing.h"
#include <iostream>

class TClonesArray;


class CrkRingMicrov3 : public CrkRing
{
 public:
  CrkRingMicrov3();
  virtual ~CrkRingMicrov3();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os=std::cout) const;

  unsigned int get_CrkNRing() const {return CrkNRing;}  //FM previously was: return 0;
  void set_CrkNRing(const unsigned int nring) {CrkNRing = nring; return;}

  int set_TClonesArraySize(const unsigned int ntrk);
  void AddCrkRing(const unsigned int iring);

  short get_accepted(const unsigned int iring) const;
  void set_accepted(const unsigned int iring, const short ival);

  short get_panel(const unsigned int iring) const;
  void set_panel(const unsigned int iring, const short ival);

  short get_npmt0(const unsigned int iring) const;
  void set_npmt0(const unsigned int iring, const short ival);

  short get_npmt1(const unsigned int iring) const;
  void set_npmt1(const unsigned int iring, const short ival);

  short get_npmt2(const unsigned int iring) const; // TKH
  void set_npmt2(const unsigned int iring, const short ival); // TKH

  short get_npmt3(const unsigned int iring) const;
  void set_npmt3(const unsigned int iring, const short ival);

  float get_npe0(const unsigned int iring) const;
  void set_npe0(const unsigned int iring, const float rval);

  float get_npe1(const unsigned int iring) const;
  void set_npe1(const unsigned int iring, const float rval);

  float get_npe2(const unsigned int iring) const; // TKH
  void set_npe2(const unsigned int iring, const float rval); // TKH

  float get_npe3(const unsigned int iring) const;
  void set_npe3(const unsigned int iring, const float rval);

  float get_chi2(const unsigned int iring) const;
  void set_chi2(const unsigned int iring, const float rval);

  float get_disp(const unsigned int iring) const;
  void set_disp(const unsigned int iring, const float rval);

  float get_cross_phi(const unsigned int iring) const;
  void set_cross_phi(const unsigned int iring, const float rval);

  float get_cross_z(const unsigned int iring) const;
  void set_cross_z(const unsigned int iring, const float rval);

  float get_center_phi(const unsigned int iring) const;
  void set_center_phi(const unsigned int iring, const float rval);

  float get_center_z(const unsigned int iring) const;
  void set_center_z(const unsigned int iring, const float rval);

  float get_tcrk(const unsigned int iring) const;  //TKH
  void set_tcrk(const unsigned int iring, const float rval);  //TKH

  short get_cgltrackid(const unsigned int iring) const;  //FM 
  void  set_cgltrackid(const unsigned int iring, const unsigned int itrk);  //FM


 protected:
  TClonesArray *GetCrkRing() const {return CrkRing;}
  unsigned int CrkNRing;
  TClonesArray *CrkRing;
  ClassDef(CrkRingMicrov3,1)

};

#endif /* __CRKRINGMicrov3_H */

