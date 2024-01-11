#ifndef PHENIX_CALIBRATORPEAK_HH
#define PHENIX_CALIBRATORPEAK_HH

#include "Bbc.hh"
#include "BbcCalibPar.hh"
#include "PdbPmtPeak.hh"

class BbcEvent;
class TH1;

class CalibratorPeak {
  private:
    BbcCalibPar<PdbPmtPeak>  Peak;

    int HistStatistics[BBC_N_PMT];
    TH1* PeakHist[BBC_N_PMT];
  public:
    CalibratorPeak(){};
    virtual ~CalibratorPeak(){};
    void Initualize( const BbcTime_t& time, char* CalibType );
    void FillHist( BbcEvent* bbc, char* CalibType );
    void Calculate();
    void Evaluate();
    void StoreToDB(const BbcTime_t& time, char* CalibType);
    void SaveHists(){};
};

#endif  /* PHENIX_CALIBRATORPEAK_HH */
