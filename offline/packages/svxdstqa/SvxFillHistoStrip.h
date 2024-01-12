#ifndef __SVXFILLHISTOSTRIP__
#define __SVXFILLHISTOSTRIP__

#include <SvxFillHisto.h>

class TFile;
class TH1F;
class TH2F;

class SvxFillHistoStrip : public SvxFillHisto {
  public:
    SvxFillHistoStrip() : SvxFillHisto("HistoStrip") { m_checkStatus=false;}
    virtual ~SvxFillHistoStrip(){}


    virtual void initialize(TFile *outfile); // if NULL, not chagne gDirectory
    virtual void write(TFile *outfile);

    //virtual void fill(SvxRawhitList *rawlist, BbcOut* bbc); // called event by event
    virtual void fill(SvxEventContainer *cnt); // called event by event
    virtual void fillInitRun(int runnumber);

    virtual void setCheckStatus(bool flag=false){ m_checkStatus=flag; }

  private: 
    static const int NMODULE = 40; // Nladder
    static const int NRCC    = 6; // B2=5, B3=6
    static const int NCHIP   = 12;
    static const int NCHAN   = 128;

    int    m_processed_run; // 0-10; if exceed 10, then show error

    TH1F   *m_h_run;
    TH1F   *m_h_evt;
    TH1F   *m_h_zvtx;
    TH2F   *m_h_adcmap[NMODULE][NRCC][NCHIP];// module=0-39, nrcc=0-5, chip:0-11, ADC vs ch128
    TH2F   *m_h_hitmap[NMODULE][NRCC][2];       // module=0-39, nrcc=0-5, chip:0-11, LR:0-1, 6 vs ch128

    bool    m_checkStatus;

};
#endif
