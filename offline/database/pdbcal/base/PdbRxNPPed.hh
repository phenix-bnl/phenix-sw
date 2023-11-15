#ifndef __PDBRXNPPED_HH__
#define __PDBRXNPPED_HH__

#include "PdbCalChan.hh"

const int nchannel = 48;
const int namu = 64;

class PdbRxNPPed : public PdbCalChan 
{
  public:
    PdbRxNPPed();
    virtual ~PdbRxNPPed() {}

    float get_TDC_mean(int ch, int amu) const {return mean_tdc_ped[ch][amu];}
    void set_TDC_mean(int ch, int amu, float val) {mean_tdc_ped[ch][amu] = val;}

    float get_TDC_width(int ch, int amu) const {return width_tdc_ped[ch][amu];}
    void set_TDC_width(int ch, int amu, float val) {width_tdc_ped[ch][amu] = val;}

    float get_HG_mean(int ch, int amu) const {return mean_hg_ped[ch][amu];}
    void set_HG_mean(int ch, int amu, float val) {mean_hg_ped[ch][amu] = val;}

    float get_HG_width(int ch, int amu) const {return width_hg_ped[ch][amu];}
    void set_HG_width(int ch, int amu, float val) {width_hg_ped[ch][amu] = val;}

    float get_LG_mean(int ch, int amu) const {return mean_lg_ped[ch][amu];}
    void set_LG_mean(int ch, int amu, float val) {mean_lg_ped[ch][amu] = val;}

    float get_LG_width(int ch, int amu) const {return width_lg_ped[ch][amu];}
    void set_LG_width(int ch, int amu, float val) {width_lg_ped[ch][amu] = val;}


    virtual void print() const;

  private:
    float mean_tdc_ped[nchannel][namu];
    float width_tdc_ped[nchannel][namu]; 
    float mean_hg_ped[nchannel][namu];
    float width_hg_ped[nchannel][namu];
    float mean_lg_ped[nchannel][namu];
    float width_lg_ped[nchannel][namu];

  ClassDef(PdbRxNPPed,1);
};

#endif /* __PDBRXNPPED_HH__ */
