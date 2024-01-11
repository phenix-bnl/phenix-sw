#ifndef __EMCCLUSTERLOCALEXTMICROV4_H__
#define __EMCCLUSTERLOCALEXTMICROV4_H__

#include "EmcClusterLocalExt.h"
#include <iostream>

class TClonesArray;

class EmcSnglClusterLocalExtMicrov4;

class EmcClusterLocalExtMicrov4 : public EmcClusterLocalExt
{
 public:
  EmcClusterLocalExtMicrov4();
  virtual ~EmcClusterLocalExtMicrov4();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os=std::cout) const;

  unsigned int get_EmcNCluster() const {return EmcNCluster;}
  void set_EmcNCluster(const unsigned int nclus) {EmcNCluster = nclus; return;}

  int set_TClonesArraySize(const unsigned int nclus);
  void AddEmcCluster(const unsigned int iclus);

  int get_index(const unsigned int iclus) const;
  void set_index(const unsigned int iclus, const int ival);

  int get_warnmap(const unsigned int iclus) const;
  void set_warnmap(const unsigned int iclus, const int ival);

  int get_deadmap(const unsigned int iclus) const;
  void set_deadmap(const unsigned int iclus, const int ival);

  short get_arm(const unsigned int iclus) const;

  short get_sector(const unsigned int iclus) const;

  short get_ind(const unsigned int itower, const short i) const;

  short get_twrhit(const unsigned int iclus) const;
  void set_twrhit(const unsigned int iclus, const short ival);

  float get_qual(const unsigned int iclus) const;
  void set_qual(const unsigned int iclus, const float rval);

  float get_chi2(const unsigned int iclus) const;
  void set_chi2(const unsigned int iclus, const float rval);

  float get_chi2_sh(const unsigned int iclus) const;
  void set_chi2_sh(const unsigned int iclus, const float rval);

  float get_e(const unsigned int iclus) const;
  void set_e(const unsigned int iclus, const float rval);

  float get_ecore(const unsigned int iclus) const;
  void set_ecore(const unsigned int iclus, const float rval);

  float get_ecent(const unsigned int iclus) const;
  void set_ecent(const unsigned int iclus, const float rval);

  float get_ecorr(const unsigned int iclus) const;
  void set_ecorr(const unsigned int iclus, const float rval);

  float get_e9(const unsigned int iclus) const;
  void set_e9(const unsigned int iclus, const float rval);

  float get_prob_photon(const unsigned int iclus) const;
  void set_prob_photon(const unsigned int iclus, const float rval);

  float get_prob_photon_sh(const unsigned int iclus) const;
  void set_prob_photon_sh(const unsigned int iclus, const float rval);

  float get_re9(const unsigned int iclus) const;
  void set_re9(const unsigned int iclus, const float rval);

  float get_tofcorr(const unsigned int iclus) const;
  void set_tofcorr(const unsigned int iclus, const float rval);

  float get_tofmin(const unsigned int iclus) const;
  void set_tofmin(const unsigned int iclus, const float rval);

  float get_tofmax(const unsigned int iclus) const;
  void set_tofmax(const unsigned int iclus, const float rval);

  float get_disp(const unsigned int iclus, const short i) const;
  void set_disp(const unsigned int iclus, const short i, const float rval);

  float get_padisp(const unsigned int iclus, const short i) const;
  void set_padisp(const unsigned int iclus, const short i, const float rval);

  float get_xyz(const unsigned int iclus, const short i) const;
  void set_xyz(const unsigned int iclus, const short i, const float rval);

  float get_yz_cg(const unsigned int iclus, const short i) const;
  void set_yz_cg(const unsigned int iclus, const short i, const float rval);

 protected:
  TClonesArray *GetEmcClus() const {return EmcClus;}

  unsigned int EmcNCluster;
  TClonesArray *EmcClus;

  ClassDef(EmcClusterLocalExtMicrov4,1)

};

#endif /*__EMCCLUSTERLOCALEXTMICROV4_H__*/
