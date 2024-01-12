#ifndef _SUMMARYERT_H
#define _SUMMARYERT_H

class TFile;
class TH2;

void qasummaryErt(TFile* qafile, char* textFile, char* statusFile, int runNumber);
void CommitToDatabase(TFile* qafile, int runnumber, float EndPoint);

namespace NameSpaceSummaryErt {
   const int N_ARMSECT   = 8;
   const int N_HALFSECT  = 16;
   const int N_TRIG_MODE = 5;

   extern const char* SECT_NAME[N_ARMSECT];
   extern const char* HALFSECT_NAME[N_HALFSECT];

   void CalcRocEff(TH2* gl1emu, Float_t eff[], Float_t eff_err[]);
   void CalcGl1NoiseRate(TH2* gl1emu, Float_t& rate, Float_t& rate_err);
   void CalcLiveSM(Int_t trig_mode, TH2* smhit, Int_t nlive[], Int_t nlive_err[]);
//   void CalcRFactor(Int_t trig_mode, TH2* smhit, UInt_t sd, Float_t bbc_live, Int_t rf[], Int_t rf_err[]);
};

#endif 
