#ifndef _HISTELECTRON_H
#define _HISTELECTRON_H
#include "SubsysReco.h"

class PHCompositeNode;
class PHCentralTrack;
class ErtOut;

class QAElectron: public SubsysReco
  {
  public:
    QAElectron(const char *name = "QAElectron") : SubsysReco(name)
    {}
    virtual ~QAElectron()
    {}

    int InitRun(PHCompositeNode *topNode);
    int process_event(PHCompositeNode *topNode);

    void set_zvertex_cut(float tmp)
    {
      zvertex_cut = tmp;
    };
    void set_emcmatch_cut(float tmp)
    {
      emcmatch_cut = tmp;
    };
    void set_min_mom_cut(float tmp)
    {
      min_mom_cut = tmp;
    };
    void set_max_mom_cut(float tmp)
    {
      max_mom_cut = tmp;
    };
    void set_n0_cut(int tmp)
    {
      n0_cut = tmp;
    };
    void set_chi2npe0_cut(float tmp)
    {
      chi2npe0_cut = tmp;
    };
    void set_disp_cut(float tmp)
    {
      disp_cut = tmp;
    };
    void set_min_dep_cut(float tmp)
    {
      min_dep_cut = tmp;
    };
    void set_max_dep_cut(float tmp)
    {
      max_dep_cut = tmp;
    };
    void set_min_ep_cut(float tmp)
    {
      min_ep_cut = tmp;
    };
    void set_max_ep_cut(float tmp)
    {
      max_ep_cut = tmp;
    };
    void set_pfoa_cut(float tmp)
    {
      pfoa_cut = tmp;
    };
    void set_deltaPhi_cut(float tmp)
    {
      deltaPhi_cut = tmp;
    };
    void set_deltaZ_cut(float tmp)
    {
      deltaZ_cut = tmp;
    };
    void set_apply_quality_cut(bool tmp)
    {
      quality_cut = tmp;
    };

  protected:
    float dep_emc(float E, float P, int dcarm, int sect);
    float pfoa(float th1, float PH1, float th2, float PH2);
    bool check_ert(PHCentralTrack *phcentraltrack, ErtOut *ertout, int itrck);
    bool check_ghost_sharing(PHCentralTrack *phcentraltrack, unsigned int i);

    float zvertex_cut;
    float emcmatch_cut;
    float min_mom_cut;
    float max_mom_cut;
    int n0_cut;
    float chi2npe0_cut;
    float disp_cut;
    float min_dep_cut;
    float max_dep_cut;
    float min_ep_cut;
    float max_ep_cut;
    float pfoa_cut;
    float deltaZ_cut;
    float deltaPhi_cut;
    bool quality_cut;
  };


#endif /* _HISTELECTRON_H */

//EOF
