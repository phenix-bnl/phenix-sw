#ifndef __SVXFILLHISTOBEAMPRO__
#define __SVXFILLHISTOBEAMPRO__

#include <SvxFillHisto.h>

class VtxOut;
class BbcOut;
class TFile;
class TH1F;
class TH2F;

class SvxFillHistoBeampro : public SvxFillHisto {
  public:
    SvxFillHistoBeampro() : SvxFillHisto("Beam profile") { m_checkStatus=false;}
    virtual ~SvxFillHistoBeampro(){}


    virtual void initialize(TFile *outfile); // if NULL, not chagne gDirectory
    virtual void write(TFile *outfile);

    virtual void fill(SvxEventContainer *cnt); // called event by event
    virtual void fillInitRun(int runnumber);

    virtual void setCheckStatus(bool flag=false){ m_checkStatus=flag; }

  private: 

    TH1F *m_h_primx;
    TH1F *m_h_primy;
//    TH1F *m_h_primz;
    
//    TH2F *m_h_primxy2d;
//    TH2F *m_h_primxz2d;
//    TH2F *m_h_primyz2d;

    bool   m_checkStatus;

};
#endif
