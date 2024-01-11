#ifndef __CRKSNGLRINGMICROV1_H
#define __CRKSNGLRINGMICROV1_H

#include "TObject.h"

class CrkSnglRingMicrov1 : public TObject
{
 public:
  CrkSnglRingMicrov1();  
  virtual ~CrkSnglRingMicrov1() {}

  short get_accepted() const {return accepted;}
  void set_accepted(const short ival) {accepted = ival; return;}

  short get_panel() const {return panel;}
  void set_panel(const short ival) {panel = ival; return;}

  short get_npmt0() const {return npmt0;}
  void set_npmt0(const short ival) {npmt0 = ival; return;}

  short get_npmt1() const {return npmt1;}
  void set_npmt1(const short ival) {npmt1 = ival; return;}

  short get_npmt3() const {return npmt3;}
  void set_npmt3(const short ival) {npmt3 = ival; return;}

  float get_npe0() const {return npe0;}
  void set_npe0(const float rval) {npe0 = rval; return;}

  float get_npe1() const {return npe1;}
  void set_npe1(const float rval) {npe1 = rval; return;}

  float get_npe3() const {return npe3;}
  void set_npe3(const float rval) {npe3 = rval; return;}

  float get_chi2() const {return chi2;}
  void set_chi2(const float rval) {chi2 = rval; return;}

  float get_disp() const {return disp;}
  void set_disp(const float rval) {disp = rval; return;}

  float get_cross_phi() const {return cross_phi;}
  void set_cross_phi(const float rval) {cross_phi = rval; return;}

  float get_cross_z() const {return cross_z;}
  void set_cross_z(const float rval) {cross_z = rval; return;}

  float get_center_phi() const {return center_phi;}
  void set_center_phi(const float rval) {center_phi = rval; return;}

  float get_center_z() const {return center_z;}
  void set_center_z(const float rval) {center_z = rval; return;}



 protected:
  short accepted;
  short panel;
  short npmt0;
  short npmt1;
  short npmt3;
  float npe0;
  float npe1;
  float npe3;
  float chi2;
  float disp;
  float cross_phi;
  float cross_z;
  float center_phi;
  float center_z;

  ClassDef(CrkSnglRingMicrov1,1)
};

#endif /* __CRKSNGLRINGMICROV1_H */
