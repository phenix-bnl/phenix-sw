#ifndef __EMCGeaCLUSTERMICROV2_H__
#define __EMCGeaCLUSTERMICROV2_H__

#include <iostream.h>
#include "TClonesArray.h"
#include "phool.h"
#include "EmcGeaCluster.h"

class EmcGeaClusterv2 : public EmcGeaCluster
{

 public:
  EmcGeaClusterv2();
  virtual ~EmcGeaClusterv2();

  void Reset();

  int isValid() const;
  void identify(ostream &os=cout) const;

  unsigned int get_EmcNGeaCluster(){return EmcNGeaCluster;}
  void set_EmcNGeaCluster(unsigned int nclus){  EmcNGeaCluster = nclus; }

  unsigned int get_EmcNCluster() const { return EmcNGeaCluster; }
  void set_EmcNCluster(const unsigned int nclus)
    { int nnclus = nclus; set_EmcNGeaCluster(nnclus); }

  int  get_multi_embed_index(int iclus) const;
  void set_multi_embed_index(int iclus, int ival);  
  
  int set_TClonesArraySize(const unsigned int nclus);
  void AddEmcCluster(const unsigned int iclus) 
    { int iiclus = iclus; AddEmcGeaCluster(iiclus); }
  void AddEmcGeaCluster(unsigned int iclus);

  short get_arm(const unsigned int iclus) const;

  short get_sector(const unsigned int iclus) const;

  short get_type(const unsigned int iclus) const;
  
  int get_index(const unsigned int iclus) const;
  void set_index(const unsigned int iclus, const int ival);

  int get_warnmap(const unsigned int iclus) const;
  void set_warnmap(const unsigned int iclus, const int ival);

  int get_deadmap(const unsigned int iclus) const;
  void set_deadmap(const unsigned int iclus, const int ival);

  short get_twrhit(const unsigned int iclus) const;
  void set_twrhit(const unsigned int iclus, const short ival);

  float get_e(const unsigned int iclus) const;
  void set_e(const unsigned int iclus, const float rval);

  float get_ecore(const unsigned int iclus) const;
  void set_ecore(const unsigned int iclus, const float rval);

  float get_ecent(const unsigned int iclus) const;
  void set_ecent(const unsigned int iclus, const float rval);

  float get_ecorr(const unsigned int iclus) const;
  void set_ecorr(const unsigned int iclus, const float rval);

  float get_tofcorr(const unsigned int iclus) const;
  void set_tofcorr(const unsigned int iclus, const float rval);

  float get_chi2_sh(const unsigned int iclus) const;
  void set_chi2_sh(const unsigned int iclus, const float rval);

  float get_prob_photon_sh(const unsigned int iclus) const;
  void set_prob_photon_sh(const unsigned int iclus, const float rval);

  float get_chi2(const unsigned int iclus) const;
  void set_chi2(const unsigned int iclus, const float rval);

  float get_prob_photon(const unsigned int iclus) const;
  void set_prob_photon(const unsigned int iclus, const float rval);

  float get_disp(const unsigned int iclus, const short i) const;
  void set_disp(const unsigned int iclus, const short i, const float rval);

  // measured impact point (from associated emccluster)
  float get_xyz(const unsigned int iclus, const short ixyz) const;
  void set_xyz(const unsigned int iclus, const short ixyz, const float rval);

  // "true" impact point (from associated geatrack)
  float get_truexyz(const unsigned int iclus, const short ixyz);
  void set_truexyz(const unsigned int iclus, const short ixyz, const float rval);

  float get_vtx_xyz(const unsigned int iclus, const short ixyz);
  void set_vtx_xyz(const unsigned int iclus, const short ixyz, const float rval);

  float get_yz_cg(const unsigned int iclus, const short i) const;
  void set_yz_cg(const unsigned int iclus, const short i, const float rval);
  
  float get_real_event_zvertex() { return real_event_zvertex; }
  void set_real_event_zvertex(float val) { real_event_zvertex = val; }
  
  int get_real_event_centrality() { return real_event_centrality; }
  void set_real_event_centrality(int val) { real_event_centrality = val; }

  int get_real_event_emc_multiplicity() { return real_event_emc_multiplicity; }
  void set_real_event_emc_multiplicity(int val) { real_event_emc_multiplicity = val; }

  float get_real_event_bbcT0() { return real_event_bbcT0; }
  void set_real_event_bbcT0(float val) { real_event_bbcT0 = val; }

  float get_padisp(const unsigned int iclus, const short i) const;
  void set_padisp(const unsigned int iclus, const short i, const float rval);

  float get_qual(const unsigned int iclus) const;
  void set_qual(const unsigned int iclus, const float rval);
  
  float get_re9(const unsigned int iclus) const;
  void set_re9(const unsigned int iclus, const float rval);
  
  float get_e9(const unsigned int iclus) const;
  void set_e9(const unsigned int iclus, const float rval);
  
  float get_tofmin(const unsigned int iclus) const;
  void set_tofmin(const unsigned int iclus, const float rval);
  
  float get_tofmax(const unsigned int iclus) const;
  void set_tofmax(const unsigned int iclus, const float rval);
  
  short get_ind(const unsigned int itower, const short i) const;

  int get_geapart(int iclus, int which); 
  int get_pid(int iclus,int which); 
  float get_ptot(int iclus,int which); 
  float get_edep(int iclus,int which);  
  float get_dist_closest_real_pc3(int iclus); 
  float get_dist_closest_gea_pc3(int iclus); 
  float get_dist_closest_real_cgl(int iclus);
  float get_dist_closest_gea_cgl(int iclus); 
  
  void set_geapart(int iclus,int which, int inpart);
  void set_pid(int iclus,int which, int inpid);
  void set_ptot(int iclus,int which, float inptot);
  void set_edep(int iclus,int which, float inedep);  

  void set_dist_closest_real_pc3(int iclus,float inval);
  void set_dist_closest_gea_pc3(int iclus,float inval);
  void set_dist_closest_real_cgl(int iclus,float inval);
  void set_dist_closest_gea_cgl(int iclus,float inval);

  float get_partesum(const unsigned int iclus, const short i) const;
  void set_partesum(const unsigned int iclus, const short i, const float rval);

  float get_etofmax(const unsigned int iclus) const;
  void set_etofmax(const unsigned int iclus, const float rval);

  float get_etofmin(const unsigned int iclus) const;
  void set_etofmin(const unsigned int iclus, const float rval);

  float get_edep_nom(unsigned int iclus, unsigned int which);
  void set_edep_nom(unsigned int iclus,unsigned int which, float inedep);

  float get_etof(unsigned int iclus);
  void set_etof(unsigned int iclus, float inedep);
  
  short get_id(const unsigned int iclus) const;
  void set_id(const unsigned int iclus, const short ival);

  void * get_SnglItem(unsigned int nclus) 
    {if (nclus < EmcNGeaCluster) return EmcGeaClus->At(nclus); return NULL;}
  
 protected:

  TClonesArray *GetEmcGeaClus() const { return EmcGeaClus;}
  unsigned int EmcNGeaCluster;
  TClonesArray *EmcGeaClus;

  short real_event_centrality;  // if embedded--if not, absurd
  short real_event_emc_multiplicity;  
  float real_event_zvertex;
  float real_event_bbcT0;

  ClassDef(EmcGeaClusterv2,1)

};
#endif /* __EMCGeaCLUSTERMICROV2_H__ */

