#ifndef __SVXFILLHISTOPIXEL__
#define __SVXFILLHISTOPIXEL__

#include <SvxFillHisto.h>

class SvxRawhitList;
class BbcOut;

class TFile;
class TH1F;
class TH2F;
//class THmulf;

class SvxFillHistoPixel : public SvxFillHisto {
  public:
    SvxFillHistoPixel() : SvxFillHisto("HistoPixel") { m_checkStatus=false;}
    virtual ~SvxFillHistoPixel(){}


    virtual void initialize(TFile *outfile); // if NULL, not chagne gDirectory
    virtual void write(TFile *outfile);

    //virtual void fill(SvxRawhitList *rawlist, BbcOut* bbc); // called event by event
    virtual void fill(SvxEventContainer *cnt); // called event by event
    virtual void fillInitRun(int runnumber);

    virtual void setCheckStatus(bool flag=false){ m_checkStatus=flag; }

  private: 
    static const int NMODULE = 60;
    static const int NCHIP   = 8;
    static const int NROW    = 256;
    static const int NCOL    = 32;

    int    m_processed_run; // 0-10; if exceed 10, then show error

    TH1F   *m_h_run;
    TH1F   *m_h_evt;
    TH1F   *m_h_zvtx;
    TH2F   *m_h_hitmap[NMODULE][NCHIP];// module=0-59, chip:0-7
    //THmulf *m_hm_hitmap[NMODULE][NCHIP];// module=0-59, chip:0-7

    bool   m_checkStatus;

};
#endif
