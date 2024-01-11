#ifndef __EMCCLUSTERLOCALEXT_H__
#define __EMCCLUSTERLOCALEXT_H__

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class dEmcClusterLocalExtWrapper;

// with the following we get the line number of the virtual function we called with PHWHERE
#define EMC_VIRTUAL_WARNING std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl

/** (OLD) Kind of interim object between STAF times and Fun4All times.
Relevant object is now emcClusterContainer / emcClusterContent
@ingroup deprecated 
*/

class EmcClusterLocalExt : public PHObject
{
 public:
  virtual ~EmcClusterLocalExt() {}

  virtual void Reset()
    {
      std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
      return;
    }

  virtual int isValid() const
    {
      std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
      return 0;
    }

  virtual void identify(std::ostream &os=std::cout) const
    {
      os << "identify yourself: virtual EmcClusterLocalExt object" << std::endl;
      return;
    }

  virtual void FillFromWrapper(dEmcClusterLocalExtWrapper *wrap) {EMC_VIRTUAL_WARNING; return;}

  virtual unsigned int get_EmcNCluster() const {EMC_VIRTUAL_WARNING; return 0;}
  virtual void set_EmcNCluster(const unsigned int nclus) {EMC_VIRTUAL_WARNING; return;}

  virtual int set_TClonesArraySize(const unsigned int nclus) {EMC_VIRTUAL_WARNING; return 0;}
  virtual void AddEmcCluster(const unsigned int iclus) {EMC_VIRTUAL_WARNING; return;}

  virtual short get_arm(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}

  virtual short get_clusno(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_clusno(const unsigned int iclus, const short ival) {EMC_VIRTUAL_WARNING; return;}

  virtual short get_id(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_id(const unsigned int iclus, const short ival) {EMC_VIRTUAL_WARNING; return;}

  virtual short get_ind(const unsigned int itower, const short i) const {EMC_VIRTUAL_WARNING; return -9999;}

  virtual short get_method(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_method(const unsigned int iclus, const short ival) {EMC_VIRTUAL_WARNING; return;}

  virtual short get_nsh(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_nsh(const unsigned int iclus, const short ival) {EMC_VIRTUAL_WARNING; return;}

  virtual short get_sector(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}

  virtual short get_twrhit(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_twrhit(const unsigned int iclus, const short ival) {EMC_VIRTUAL_WARNING; return;}

  virtual short get_type(const unsigned int iclus) const;
  virtual void set_type(const unsigned int iclus, const short ival) {EMC_VIRTUAL_WARNING; return;}


  virtual int get_deadmap(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_deadmap(const unsigned int iclus, const int ival) {EMC_VIRTUAL_WARNING; return;}

  virtual int get_index(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_index(const unsigned int iclus, const int ival) {EMC_VIRTUAL_WARNING; return;}

  virtual int get_warnmap(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_warnmap(const unsigned int iclus, const int ival) {EMC_VIRTUAL_WARNING; return;}

  virtual int get_twrlist(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999;}
  virtual void set_twrlist(const unsigned int iclus, const short i, const int ival) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_chi2(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_chi2(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_chi2_sh(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_chi2_sh(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_de(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_de(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_dtof(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_dtof(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_e(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_e(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_ecore(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_ecore(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_ecent(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_ecent(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_ecorr(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_ecorr(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_etofmax(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_etofmax(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_etofmin(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_etofmin(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_e9(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_e9(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_phi(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_phi(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_pid(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_pid(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_prob_neuhad(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_prob_neuhad(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_prob_photon(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_prob_photon(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_prob_photon_sh(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_prob_photon_sh(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_qual(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_qual(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_re9(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_re9(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_theta(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_theta(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_tof(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tof(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_tofcorr(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tofcorr(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_tofmax(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tofmax(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_tofmaxcorr(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tofmaxcorr(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_tofmean(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tofmean(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_tofmin(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tofmin(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_tofmincorr(const unsigned int iclus) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_tofmincorr(const unsigned int iclus, const float rval) {EMC_VIRTUAL_WARNING; return;}



  virtual float get_de_sh(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_de_sh(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_disp(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_disp(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_dxyz(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_dxyz(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_ecorr_sh(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_ecorr_sh(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_e_sh(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_e_sh(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_padisp(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_padisp(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_partesum(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_partesum(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_unitv(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_unitv(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_xyz(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_xyz(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_yz_cg(const unsigned int iclus, const short i) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_yz_cg(const unsigned int iclus, const short i, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_dxyz_sh(const unsigned int iclus, const short i, const short j) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_dxyz_sh(const unsigned int iclus, const short i, const short j, const float rval) {EMC_VIRTUAL_WARNING; return;}

  virtual float get_xyz_sh(const unsigned int iclus, const short i, const short j) const {EMC_VIRTUAL_WARNING; return -9999.9;}
  virtual void set_xyz_sh(const unsigned int iclus, const short i, const short j, const float rval) {EMC_VIRTUAL_WARNING; return;}


  ClassDef(EmcClusterLocalExt,1)

};

#undef EMC_VIRTUAL_WARNING

#endif /*__EMCCLUSTERLOCALEXT_H__*/
