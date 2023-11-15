#ifndef __PDBRXNP_HH__
#define __PDBRXNP_HH__

#include "PdbCalChan.hh"

const int NBBC = 1; //.. currently we remove the bbc charge dependency.

class PdbRxNP : public PdbCalChan 
{
  public:
    PdbRxNP();
    virtual ~PdbRxNP() {}

    //.. tdc and gain calibration ...
    float get_TDC_coeff(int arm, int ring, int phi) const {return tdc_coeff[arm][ring][phi];}
    void set_TDC_coeff(int arm, int ring, int phi, float val) {tdc_coeff[arm][ring][phi] = val;}

    float get_HG_ADC_coeff(int arm, int ring, int phi, int ibbc) const {return hg_adc_coeff[arm][ring][phi][ibbc];}
    void set_HG_ADC_coeff(int arm, int ring, int phi, int ibbc, float val) {hg_adc_coeff[arm][ring][phi][ibbc] = val;}

    float get_LG_ADC_coeff(int arm, int ring, int phi, int ibbc) const {return lg_adc_coeff[arm][ring][phi][ibbc];}
    void set_LG_ADC_coeff(int arm, int ring, int phi, int ibbc, float val) {lg_adc_coeff[arm][ring][phi][ibbc] = val;}

    //.. pedestal calibration ...
    int get_channel(int arm, int ring, int phi) const {return channel[arm][ring][phi];}
    void set_channel(int arm, int ring, int phi, int ch) { channel[arm][ring][phi] = ch;} 

    float get_hg_ped_mean(int arm, int ring, int phi, int amu) const {return hg_ped_mean[arm][ring][phi][amu];}
    void set_hg_ped_mean(int arm, int ring, int phi, int amu, float val) {hg_ped_mean[arm][ring][phi][amu] = val;}
    float get_hg_ped_width(int arm, int ring, int phi, int amu) const {return hg_ped_width[arm][ring][phi][amu];}
    void set_hg_ped_width(int arm, int ring, int phi, int amu, float val) {hg_ped_width[arm][ring][phi][amu] = val;}

    float get_lg_ped_mean(int arm, int ring, int phi, int amu) const {return lg_ped_mean[arm][ring][phi][amu];}
    void set_lg_ped_mean(int arm, int ring, int phi, int amu, float val) {lg_ped_mean[arm][ring][phi][amu] = val;}
    float get_lg_ped_width(int arm, int ring, int phi, int amu) const {return lg_ped_width[arm][ring][phi][amu];}
    void set_lg_ped_width(int arm, int ring, int phi, int amu, float val) {lg_ped_width[arm][ring][phi][amu] = val;}

    float get_tdc_ped_mean(int arm, int ring, int phi, int amu) const {return tdc_ped_mean[arm][ring][phi][amu];}
    void set_tdc_ped_mean(int arm, int ring, int phi, int amu, float val) {tdc_ped_mean[arm][ring][phi][amu] = val;}
    float get_tdc_ped_width(int arm, int ring, int phi, int amu) const {return tdc_ped_width[arm][ring][phi][amu];}
    void set_tdc_ped_width(int arm, int ring, int phi, int amu, float val) {tdc_ped_width[arm][ring][phi][amu] = val;}

    int get_NBBC() const {return NBBC;}

    virtual void print() const;

  private:
    float tdc_coeff[2][2][12];

    float hg_adc_coeff[2][2][12][NBBC]; 
    float lg_adc_coeff[2][2][12][NBBC];

    int channel[2][2][12];
    
    float hg_ped_mean[2][2][12][64];
    float hg_ped_width[2][2][12][64];
    float lg_ped_mean[2][2][12][64];
    float lg_ped_width[2][2][12][64];
    float tdc_ped_mean[2][2][12][64];
    float tdc_ped_width[2][2][12][64];

  ClassDef(PdbRxNP,1);
};

#endif /* __PDBRXNP_HH__ */
