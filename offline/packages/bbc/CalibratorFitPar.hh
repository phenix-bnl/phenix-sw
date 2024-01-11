#ifndef PHENIX_CALIBRATORFITPAR_HH
#define PHENIX_CALIBRATORFITPAR_HH

#include "Bbc.hh"

#include "BbcEvent.hh"
#include "BbcCalib.hh"

#include "PdbPmtFitPar.hh"

#include "TProfile.h"
#include "TH2.h"
#include "TF1.h"

class CalibratorFitPar {
  private:
    BbcCalibPar<PdbPmtFitPar>  FitPar;

    TProfile*       Hist[BBC_N_PMT];
    TH2F*           Hist2D[BBC_N_PMT];
    int             HistStatistics[BBC_N_PMT];
    TH2F*           HistInit;
    TH2F*           HistLast;
  public:
    CalibratorFitPar(){};
    ~CalibratorFitPar(){};
    void Initialize( const BbcTime_t& time, char* CalibType );
    void ResetHist();
    void FillHist( BbcEvent* bbc, BbcEvent* ref, char* CalibType );
    void Calculate(BbcEvent* bbc, BbcEvent* ref);
    void Evaluate();
    void StoreToDB(const BbcTime_t& time, char* CalibType);
    void SaveHists(){};
    void FillInit( BbcEvent* bbc );
    void FillLast( BbcEvent* bbc );
};

#endif  /* PHENIX_CALIBRATORFITPAR_HH */
