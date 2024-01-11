#ifndef __EMCSNGLCLUSTERLOCALEXTMICROV4_H__
#define __EMCSNGLCLUSTERLOCALEXTMICROV4_H__

#include "PHObject.h"

class EmcSnglClusterLocalExtMicrov4 : public TObject
{

 public:
  EmcSnglClusterLocalExtMicrov4();
  virtual ~EmcSnglClusterLocalExtMicrov4() {}

  int get_index() const {return index;}
  void set_index(int ival) {index = ival; return;}

  int get_warnmap() const {return warnmap;}
  void set_warnmap(int ival) {warnmap = ival; return;}

  int get_deadmap() const {return deadmap;}
  void set_deadmap(int ival) {deadmap = ival; return;}

  short get_twrhit() const {return twrhit;}
  void set_twrhit(short ival) {twrhit = ival; return;}

  float get_qual() const {return qual;}
  void set_qual(float rval) {qual = rval; return;}

  float get_chi2() const {return chi2;}
  void set_chi2(float rval) {chi2 = rval; return;}

  float get_chi2_sh() const {return chi2_sh;}
  void set_chi2_sh(float rval) {chi2_sh = rval; return;}

  float get_e() const {return e;}
  void set_e(float rval) {e = rval; return;}

  float get_ecore() const {return ecore;}
  void set_ecore(float rval) {ecore = rval; return;}

  float get_ecorr() const {return ecorr;}
  void set_ecorr(float rval) {ecorr = rval; return;}

  float get_ecent() const {return ecent;}
  void set_ecent(float rval) {ecent = rval; return;}

  float get_e9() const {return e9;}
  void set_e9(float rval) {e9 = rval; return;}

  float get_prob_photon() const {return prob_photon;}
  void set_prob_photon(float rval) {prob_photon = rval; return;}

  float get_prob_photon_sh() const {return prob_photon_sh;}
  void set_prob_photon_sh(float rval) {prob_photon_sh = rval; return;}

  float get_re9() const {return re9;}
  void set_re9(float rval) {re9 = rval; return;}

  float get_tofcorr() const {return tofcorr;}
  void set_tofcorr(float rval) {tofcorr = rval; return;}

  float get_tofmin() const {return tofmin;}
  void set_tofmin(float rval) {tofmin = rval; return;}

  float get_tofmax() const {return tofmax;}
  void set_tofmax(float rval) {tofmax = rval; return;}

  float get_disp(short i) const {return disp[i];}
  void set_disp(float rval, short i) {disp[i] = rval; return;}

  float get_padisp(short i) const {return padisp[i];}
  void set_padisp(float rval, short i) {padisp[i] = rval; return;}

  float get_yz_cg(short i) const {return yz_cg[i];}
  void set_yz_cg(float rval, short i) {yz_cg[i] = rval; return;}

  float get_xyz(short i) const {return xyz[i];}
  void set_xyz(float rval, short i) {xyz[i] = rval; return;}

 protected:
   short twrhit;
   int index;
   int warnmap;
   int deadmap;
   float chi2;
   float chi2_sh;
   float e;
   float ecore;
   float ecorr;
   float ecent;
   float e9;             // Weis Lvl2 Electrons
   float prob_photon;
   float prob_photon_sh;
   float re9;           // Weis Lvl2 Electrons
   float tofcorr;
   float tofmin;        // Gabor 21-Feb-2002
   float tofmax;        // Gabor 21-Feb-2002
   float disp[2];
   float padisp[2];     // Gabor 21-Feb-2002
   float xyz[3];
   float yz_cg[2];
   float qual;		// m.chiu 11-Mar-2002

   ClassDef(EmcSnglClusterLocalExtMicrov4,1)
};

#endif /*__EMCSNGLCLUSTERLOCALEXTMICROV4_H__*/
