#ifndef __EMCCLUSTERLOCALEXTV1_H__
#define __EMCCLUSTERLOCALEXTV1_H__

#include "EmcClusterLocalExt.h"
#include <iostream>

class dEmcClusterLocalExtWrapper;
class TClonesArray;

/** (OLD) Kind of interim object between STAF times and Fun4All times.
Relevant object is now emcClusterContainer / emcClusterContent
@ingroup deprecated 
*/

class EmcClusterLocalExtv1 : public EmcClusterLocalExt
{
 public:
  EmcClusterLocalExtv1();
  virtual ~EmcClusterLocalExtv1();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os = std::cout) const;

  void FillFromWrapper(dEmcClusterLocalExtWrapper *wrap);

  unsigned int get_EmcNCluster() const {return EmcNCluster;}
  void set_EmcNCluster(const unsigned int nclus) {EmcNCluster = nclus; return;}

  int set_TClonesArraySize(const unsigned int nclus);
  void AddEmcCluster(const unsigned int iclus);

  short get_arm(const unsigned int iclus) const;

  short get_clusno(const unsigned int iclus) const;
  void set_clusno(const unsigned int iclus, const short ival);

  short get_id(const unsigned int iclus) const;
  void set_id(const unsigned int iclus, const short ival);

  short get_ind(const unsigned int itower, const short i) const;

  short get_method(const unsigned int iclus) const;
  void set_method(const unsigned int iclus, const short ival);

  short get_nsh(const unsigned int iclus) const;
  void set_nsh(const unsigned int iclus, const short ival);

  short get_sector(const unsigned int iclus) const;

  short get_twrhit(const unsigned int iclus) const;
  void set_twrhit(const unsigned int iclus, const short ival);

  short get_type(const unsigned int iclus) const;
  void set_type(const unsigned int iclus, const short ival);


  int get_deadmap(const unsigned int iclus) const;
  void set_deadmap(const unsigned int iclus, const int ival);

  int get_index(const unsigned int iclus) const;
  void set_index(const unsigned int iclus, const int ival);

  int get_warnmap(const unsigned int iclus) const;
  void set_warnmap(const unsigned int iclus, const int ival);

  int get_twrlist(const unsigned int iclus, const short i) const;
  void set_twrlist(const unsigned int iclus, const short i, const int ival);



  float get_chi2(const unsigned int iclus) const;
  void set_chi2(const unsigned int iclus, const float rval);

  float get_chi2_sh(const unsigned int iclus) const;
  void set_chi2_sh(const unsigned int iclus, const float rval);

  float get_de(const unsigned int iclus) const;
  void set_de(const unsigned int iclus, const float rval);

  float get_dtof(const unsigned int iclus) const;
  void set_dtof(const unsigned int iclus, const float rval);

  float get_e(const unsigned int iclus) const;
  void set_e(const unsigned int iclus, const float rval);

  float get_ecore(const unsigned int iclus) const;
  void set_ecore(const unsigned int iclus, const float rval);

  float get_ecent(const unsigned int iclus) const;
  void set_ecent(const unsigned int iclus, const float rval);

  float get_ecorr(const unsigned int iclus) const;
  void set_ecorr(const unsigned int iclus, const float rval);

  float get_etofmax(const unsigned int iclus) const;
  void set_etofmax(const unsigned int iclus, const float rval);

  float get_etofmin(const unsigned int iclus) const;
  void set_etofmin(const unsigned int iclus, const float rval);

  float get_e9(const unsigned int iclus) const;
  void set_e9(const unsigned int iclus, const float rval);

  float get_phi(const unsigned int iclus) const;
  void set_phi(const unsigned int iclus, const float rval);

  float get_pid(const unsigned int iclus) const;
  void set_pid(const unsigned int iclus, const float rval);

  float get_prob_neuhad(const unsigned int iclus) const;
  void set_prob_neuhad(const unsigned int iclus, const float rval);

  float get_prob_photon(const unsigned int iclus) const;
  void set_prob_photon(const unsigned int iclus, const float rval);

  float get_prob_photon_sh(const unsigned int iclus) const;
  void set_prob_photon_sh(const unsigned int iclus, const float rval);

  float get_qual(const unsigned int iclus) const;
  void set_qual(const unsigned int iclus, const float rval);

  float get_re9(const unsigned int iclus) const;
  void set_re9(const unsigned int iclus, const float rval);

  float get_theta(const unsigned int iclus) const;
  void set_theta(const unsigned int iclus, const float rval);

  float get_tof(const unsigned int iclus) const;
  void set_tof(const unsigned int iclus, const float rval);

  float get_tofcorr(const unsigned int iclus) const;
  void set_tofcorr(const unsigned int iclus, const float rval);

  float get_tofmax(const unsigned int iclus) const;
  void set_tofmax(const unsigned int iclus, const float rval);

  float get_tofmaxcorr(const unsigned int iclus) const;
  void set_tofmaxcorr(const unsigned int iclus, const float rval);

  float get_tofmean(const unsigned int iclus) const;
  void set_tofmean(const unsigned int iclus, const float rval);

  float get_tofmin(const unsigned int iclus) const;
  void set_tofmin(const unsigned int iclus, const float rval);

  float get_tofmincorr(const unsigned int iclus) const;
  void set_tofmincorr(const unsigned int iclus, const float rval);



  float get_de_sh(const unsigned int iclus, const short i) const;
  void set_de_sh(const unsigned int iclus, const short i, const float rval);

  float get_disp(const unsigned int iclus, const short i) const;
  void set_disp(const unsigned int iclus, const short i, const float rval);

  float get_dxyz(const unsigned int iclus, const short i) const;
  void set_dxyz(const unsigned int iclus, const short i, const float rval);

  float get_ecorr_sh(const unsigned int iclus, const short i) const;
  void set_ecorr_sh(const unsigned int iclus, const short i, const float rval);

  float get_e_sh(const unsigned int iclus, const short i) const;
  void set_e_sh(const unsigned int iclus, const short i, const float rval);

  float get_padisp(const unsigned int iclus, const short i) const;
  void set_padisp(const unsigned int iclus, const short i, const float rval);

  float get_partesum(const unsigned int iclus, const short i) const;
  void set_partesum(const unsigned int iclus, const short i, const float rval);

  float get_unitv(const unsigned int iclus, const short i) const;
  void set_unitv(const unsigned int iclus, const short i, const float rval);

  float get_xyz(const unsigned int iclus, const short i) const;
  void set_xyz(const unsigned int iclus, const short i, const float rval);

  float get_yz_cg(const unsigned int iclus, const short i) const;
  void set_yz_cg(const unsigned int iclus, const short i, const float rval);

  float get_dxyz_sh(const unsigned int iclus, const short i, const short j) const;
  void set_dxyz_sh(const unsigned int iclus, const short i, const short j, const float rval);

  float get_xyz_sh(const unsigned int iclus, const short i, const short j) const;
  void set_xyz_sh(const unsigned int iclus, const short i, const short j, const float rval);

 protected:
  TClonesArray *GetEmcClus() const {return EmcClus;}

  unsigned int EmcNCluster;
  TClonesArray *EmcClus;

  ClassDef(EmcClusterLocalExtv1,1)

};

#endif /*__EMCCLUSTERLOCALEXTV1_H__*/
