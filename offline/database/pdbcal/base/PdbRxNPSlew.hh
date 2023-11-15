#ifndef __PDBRXNPSLEW_HH__
#define __PDBRXNPSLEW_HH__

#include "PdbCalChan.hh"

const int N_ADC = 100; //.. number of ADC bins for TDC slewing correction

class PdbRxNPSlew : public PdbCalChan 
{
  public:
    PdbRxNPSlew();
    virtual ~PdbRxNPSlew() {}

    //.. phi, R and Z of each of the 6 survey point for each phi segment
    //-----------------------------------------------------------------------
    float get_TDC_LG_slewcoeff(int arm, int ring, int phi, int iadc) const {return tdc_lg_slew_coeff[arm][ring][phi][iadc];}
    void  set_TDC_LG_slewcoeff(int arm, int ring, int phi, int iadc, float val) {tdc_lg_slew_coeff[arm][ring][phi][iadc] = val;}

    float get_TDC_HG_slewcoeff(int arm, int ring, int phi, int iadc) const {return tdc_hg_slew_coeff[arm][ring][phi][iadc];}
    void  set_TDC_HG_slewcoeff(int arm, int ring, int phi, int iadc, float val) {tdc_hg_slew_coeff[arm][ring][phi][iadc] = val;}

    virtual void print() const;

  private:
    float tdc_lg_slew_coeff[2][2][12][N_ADC];
    float tdc_hg_slew_coeff[2][2][12][N_ADC];

  ClassDef(PdbRxNPSlew,1);
};

#endif /* __PDBRXNPSLEW_HH__ */
