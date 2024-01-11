#ifndef __EMCSNGLCLUSTERLOCALEXTV1_H__
#define __EMCSNGLCLUSTERLOCALEXTV1_H__

#include "PHObject.h"

/** (OLD) Kind of interim object between STAF times and Fun4All times.
Relevant object is now emcTowerContainer / emcTowerContent
@ingroup deprecated 
*/

class EmcSnglClusterLocalExtv1 : public TObject
{

 public:
  EmcSnglClusterLocalExtv1();
  virtual ~EmcSnglClusterLocalExtv1() {}

  short get_clusno() const {return clusno;}
  void set_clusno(const short ival) {clusno = ival; return;}

  short get_id() const {return id;}
  void set_id(const short ival) {id = ival; return;}

  short get_method() const {return method;}
  void set_method(const short ival) {method = ival; return;}

  short get_nsh() const {return nsh;}
  void set_nsh(const short ival) {nsh = ival; return;}

  short get_twrhit() const {return twrhit;}
  void set_twrhit(const short ival) {twrhit = ival; return;}

  short get_type() const {return type;}
  void set_type(const short ival) {type = ival; return;}


  int get_deadmap() const {return deadmap;}
  void set_deadmap(const int ival) {deadmap = ival; return;}

  int get_index() const {return index;}
  void set_index(const int ival) {index = ival; return;}

  int get_warnmap() const {return warnmap;}
  void set_warnmap(const int ival) {warnmap = ival; return;}

  int get_twrlist(const short i) const {return twrlist[i];}
  void set_twrlist(const int ival, const short i) {twrlist[i] = ival; return;}


  float get_chi2() const {return chi2;}
  void set_chi2(const float rval) {chi2 = rval; return;}

  float get_chi2_sh() const {return chi2_sh;}
  void set_chi2_sh(const float rval) {chi2_sh = rval; return;}

  float get_de() const {return de;}
  void set_de(const float rval) {de = rval; return;}

  float get_dtof() const {return dtof;}
  void set_dtof(const float rval) {dtof = rval; return;}

  float get_e() const {return e;}
  void set_e(const float rval) {e = rval; return;}

  float get_ecent() const {return ecent;}
  void set_ecent(const float rval) {ecent = rval; return;}

  float get_ecore() const {return ecore;}
  void set_ecore(const float rval) {ecore = rval; return;}

  float get_ecorr() const {return ecorr;}
  void set_ecorr(const float rval) {ecorr = rval; return;}

  float get_etofmax() const {return etofmax;}
  void set_etofmax(const float rval) {etofmax = rval; return;}

  float get_etofmin() const {return etofmin;}
  void set_etofmin(const float rval) {etofmin = rval; return;}

  float get_e9() const {return e9;}
  void set_e9(const float rval) {e9 = rval; return;}

  float get_phi() const {return phi;}
  void set_phi(const float rval) {phi = rval; return;}

  float get_pid() const {return pid;}
  void set_pid(const float rval) {pid = rval; return;}

  float get_prob_neuhad() const {return prob_neuhad;}
  void set_prob_neuhad(const float rval) {prob_neuhad = rval; return;}

  float get_prob_photon() const {return prob_photon;}
  void set_prob_photon(const float rval) {prob_photon = rval; return;}

  float get_prob_photon_sh() const {return prob_photon_sh;}
  void set_prob_photon_sh(const float rval) {prob_photon_sh = rval; return;}

  float get_qual() const {return qual;}
  void set_qual(const float rval) {qual = rval; return;}

  float get_re9() const {return re9;}
  void set_re9(const float rval) {re9 = rval; return;}

  float get_theta() const {return theta;}
  void set_theta(const float rval) {theta = rval; return;}

  float get_tof() const {return tof;}
  void set_tof(const float rval) {tof = rval; return;}

  float get_tofcorr() const {return tofcorr;}
  void set_tofcorr(const float rval) {tofcorr = rval; return;}

  float get_tofmax() const {return tofmax;}
  void set_tofmax(const float rval) {tofmax = rval; return;}

  float get_tofmaxcorr() const {return tofmaxcorr;}
  void set_tofmaxcorr(const float rval) {tofmaxcorr = rval; return;}

  float get_tofmean() const {return tofmean;}
  void set_tofmean(const float rval) {tofmean = rval; return;}

  float get_tofmin() const {return tofmin;}
  void set_tofmin(const float rval) {tofmin = rval; return;}

  float get_tofmincorr() const {return tofmincorr;}
  void set_tofmincorr(const float rval) {tofmincorr = rval; return;}

  float get_de_sh(const short i) const {return de_sh[i];}
  void set_de_sh(const float rval, const short i) {de_sh[i] = rval; return;}

  float get_disp(const short i) const {return disp[i];}
  void set_disp(const float rval, const short i) {disp[i] = rval; return;}

  float get_dxyz(const short i) const {return dxyz[i];}
  void set_dxyz(const float rval, const short i) {dxyz[i] = rval; return;}

  float get_ecorr_sh(const short i) const {return ecorr_sh[i];}
  void set_ecorr_sh(const float rval, const short i) {ecorr_sh[i] = rval; return;}

  float get_e_sh(const short i) const {return e_sh[i];}
  void set_e_sh(const float rval, const short i) {e_sh[i] = rval; return;}

  float get_padisp(const short i) const {return padisp[i];}
  void set_padisp(const float rval, const short i) {padisp[i] = rval; return;}

  float get_partesum(const short i) const {return partesum[i];}
  void set_partesum(const float rval, const short i) {partesum[i] = rval; return;}

  float get_unitv(const short i) const {return unitv[i];}
  void set_unitv(const float rval, const short i) {unitv[i] = rval; return;}

  float get_xyz(const short i) const {return xyz[i];}
  void set_xyz(const float rval, const short i) {xyz[i] = rval; return;}

  float get_yz_cg(const short i) const {return yz_cg[i];}
  void set_yz_cg(const float rval, const short i) {yz_cg[i] = rval; return;}

  float get_dxyz_sh(const short i, const short j) const {return dxyz_sh[i][j];}
  void set_dxyz_sh(const float rval, const short i, const short j) {dxyz_sh[i][j] = rval; return;}

  float get_xyz_sh(const short i, const short j) const {return xyz_sh[i][j];}
  void set_xyz_sh(const float rval, const short i, const short j) {xyz_sh[i][j] = rval; return;}

 protected:
   short clusno;
   short id;
   short method;
   short nsh;
   short twrhit;
   short type;

   int index;
   int deadmap;
   int warnmap;

   int twrlist[16];

   float chi2;
   float chi2_sh;
   float de;
   float dtof;
   float e;
   float ecent;
   float ecore;
   float ecorr;
   float etofmax;
   float etofmin;
   float e9;
   float phi;
   float pid;
   float prob_neuhad;
   float prob_photon;
   float prob_photon_sh;
   float qual;
   float re9;
   float theta;
   float tof;
   float tofcorr;
   float tofmax;
   float tofmaxcorr;
   float tofmean;
   float tofmin;
   float tofmincorr;

   float de_sh[2];
   float disp[2];
   float dxyz[3];
   float ecorr_sh[2];
   float e_sh[2];
   float padisp[2];
   float partesum[16];
   float unitv[3];
   float xyz[3];
   float yz_cg[2];

   float dxyz_sh[2][3];
   float xyz_sh[2][3];

   ClassDef(EmcSnglClusterLocalExtv1,1)
};

#endif /*__EMCSNGLCLUSTERLOCALEXTV1_H__*/
