#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>

#include <summaryQA.h>
#include "summaryErt.h"

using namespace std;

const char* NameSpaceSummaryErt::SECT_NAME[N_ARMSECT] = {
   "W0", "W1", "W2", "W3", "E3", "E2", "E1", "E0"
};

const char* NameSpaceSummaryErt::HALFSECT_NAME[N_HALFSECT] = {
   "W0N", "W0S", "W1N", "W1S", "W2N", "W2S", "W3N", "W3S",
   "E3S", "E3N", "E2S", "E2N", "E1S", "E1N", "E0S", "E0N"
};

void NameSpaceSummaryErt::CalcRocEff(TH2* gl1emu, Float_t eff[], Float_t eff_err[])
{
   for (Int_t i = 0; i < N_HALFSECT; i++) {
      Float_t n_emu  = gl1emu->GetBinContent(i + 1, 2); // emu
      Float_t n_both = gl1emu->GetBinContent(i + 1, 3); // emu && GL1
      if (n_emu > 0) {
         eff[i]     = n_both / n_emu;
         eff_err[i] = sqrt(eff[i] * (1 - eff[i]) / n_emu);
      } else {
         eff[i]     = -1;
         eff_err[i] =  0;
      }
   }
}

void NameSpaceSummaryErt::CalcGl1NoiseRate(TH2* gl1emu, Float_t& rate, Float_t& rate_err)
{
   Float_t n_gl1  = gl1emu->Integral(0, N_HALFSECT + 1, 1, 1); // GL1
   Float_t n_both = gl1emu->Integral(0, N_HALFSECT + 1, 3, 3); // emu && GL1
   if (n_gl1 > 0) {
      rate     = (n_gl1 - n_both) / n_gl1;
      rate_err = sqrt(rate * (1 - rate) / n_gl1);
   } else {
      rate     = -1;
      rate_err = 0;
   }
}

void NameSpaceSummaryErt::CalcLiveSM(
   Int_t trig_mode, TH2* smhit, Int_t nlive[], Int_t nlive_err[])
{
   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      nlive[ias] = 0;
      nlive_err[ias] = 0;

      Int_t n_sm;
      if (trig_mode != 4) n_sm = ias < 6  ?  18  :  32; // EMCal
      else                n_sm = 32; // RICH

      for (Int_t ism = 0; ism < n_sm; ism++) {
         Float_t nhit = smhit->GetBinContent(ism + 1, ias + 1);
         if (nhit > 0) nlive[ias]++;
         if (nhit > 2) nlive_err[ias]++;
      }
      // error is defined as the difference between different thresholds
      nlive_err[ias] = nlive[ias] - nlive_err[ias];
   }
}

//void NameSpaceSummaryErt::CalcRFactor(
//   Int_t trig_mode, TH2* smhit, UInt_t sd, Float_t bbc_live, Int_t rf[], Int_t rf_err[])
//{
//
//}

using namespace NameSpaceSummaryErt;

int QASummary::processErt()
{
   cout << "Ert..." << endl;

   fstream textFile(outputName1, ios::in);
   if (textFile) {
      textFile.close();
      textFile.open(outputName1, ios::app | ios::out);
   }
   fstream statusFile(outputName2, ios::in);
   if (statusFile) {
      statusFile.close();
      statusFile.open(outputName2, ios::app | ios::out);
   }
   textFile << " ----------------------------------------------------\n";
   textFile << " -- ERT QA Summary --\n";
   textFile << " ----------------------------------------------------\n\n";

   ostringstream oss;

   ////
   //// get histos and values
   ////
   TH1F* ert_counter = (TH1F*)qafile->Get("ert_counter");

   TH2F* ert_smhit_4x4a = (TH2F*)qafile->Get("ert_smhit_4x4a");
   TH2F* ert_smhit_4x4b = (TH2F*)qafile->Get("ert_smhit_4x4b");
   TH2F* ert_smhit_4x4c = (TH2F*)qafile->Get("ert_smhit_4x4c");
   TH2F* ert_smhit_2x2  = (TH2F*)qafile->Get("ert_smhit_2x2");
   TH2F* ert_smhit_RICH = (TH2F*)qafile->Get("ert_smhit_RICH");

//   TH2F* ert_smhit_mb_4x4a = (TH2F*)qafile->Get("ert_smhit_mb_4x4a");
//   TH2F* ert_smhit_mb_4x4b = (TH2F*)qafile->Get("ert_smhit_mb_4x4b");
//   TH2F* ert_smhit_mb_4x4c = (TH2F*)qafile->Get("ert_smhit_mb_4x4c");
//   TH2F* ert_smhit_mb_2x2  = (TH2F*)qafile->Get("ert_smhit_mb_2x2");
//   TH2F* ert_smhit_mb_RICH = (TH2F*)qafile->Get("ert_smhit_mb_RICH");

   TH2F* ert_gl1emu_4x4a = (TH2F*)qafile->Get("ert_gl1emu_4x4a");
   TH2F* ert_gl1emu_4x4b = (TH2F*)qafile->Get("ert_gl1emu_4x4b");
   TH2F* ert_gl1emu_4x4c = (TH2F*)qafile->Get("ert_gl1emu_4x4c");
   TH2F* ert_gl1emu_2x2  = (TH2F*)qafile->Get("ert_gl1emu_2x2");
   TH2F* ert_gl1emu_el   = (TH2F*)qafile->Get("ert_gl1emu_el");
   
   float sd_bbc  = ert_counter->GetBinContent(1);
   float sd_4x4a = ert_counter->GetBinContent(2);
   float sd_4x4b = ert_counter->GetBinContent(3);
   float sd_4x4c = ert_counter->GetBinContent(4);
   float sd_2x2  = ert_counter->GetBinContent(5);
   float sd_el   = ert_counter->GetBinContent(6);

   float nevt_live_bbc  = ert_counter->GetBinContent(11);
   float nevt_live_4x4a = ert_counter->GetBinContent(12);
   float nevt_live_4x4b = ert_counter->GetBinContent(13);
   float nevt_live_4x4c = ert_counter->GetBinContent(14);
   float nevt_live_2x2  = ert_counter->GetBinContent(15);
   float nevt_live_el   = ert_counter->GetBinContent(16);

   float nevt_scale_bbc  = ert_counter->GetBinContent(21);
   float nevt_scale_4x4a = ert_counter->GetBinContent(22);
   float nevt_scale_4x4b = ert_counter->GetBinContent(23);
   float nevt_scale_4x4c = ert_counter->GetBinContent(24);
   float nevt_scale_2x2  = ert_counter->GetBinContent(25);
   float nevt_scale_el   = ert_counter->GetBinContent(26);

   ////
   //// add here analysis and printout results to textFile
   ////

   textFile << "scaledown\t" << sd_bbc << "\t" << sd_4x4a << "\t" 
            << sd_4x4b << "\t" << sd_4x4c << "\t" 
            << sd_2x2 << "\t" << sd_el << endl;
   textFile << "nevt_live\t" << nevt_live_bbc << "\t" << nevt_live_4x4a << "\t"
            << nevt_live_4x4b << "\t" << nevt_live_4x4c << "\t"
            << nevt_live_2x2 << "\t" << nevt_live_el << endl;
   textFile << "nevt_scale\t" << nevt_scale_bbc << "\t" << nevt_scale_4x4a << "\t"
            << nevt_scale_4x4b << "\t" << nevt_scale_4x4c << "\t"
            << nevt_scale_2x2 << "\t" << nevt_scale_el << endl;

   ////
   //// ROC efficiency
   ////
   Float_t roc_eff[N_TRIG_MODE][N_HALFSECT], roc_eff_err[N_TRIG_MODE][N_HALFSECT];
   CalcRocEff(ert_gl1emu_4x4a, roc_eff[0], roc_eff_err[0]);
   CalcRocEff(ert_gl1emu_4x4b, roc_eff[1], roc_eff_err[1]);
   CalcRocEff(ert_gl1emu_4x4c, roc_eff[2], roc_eff_err[2]);
   CalcRocEff(ert_gl1emu_2x2,  roc_eff[3], roc_eff_err[3]);
   CalcRocEff(ert_gl1emu_el,   roc_eff[4], roc_eff_err[4]);

   textFile << endl 
            << "<< ROC efficiency (GL1&&emu/emu in MB evts) >>" << endl
            << "trigmode  half_sect  eff  eff_err" << endl;
   for (int itrig = 0; itrig < N_TRIG_MODE; itrig++) {
      for (int ias = 0; ias < N_HALFSECT; ias++) {
         const char* trigger_name[N_TRIG_MODE] = {"4x4a", "4x4b", "4x4c","2x2", "el"};
         oss.str("");
         oss << "ROC eff, " << trigger_name[itrig] << ", " << HALFSECT_NAME[ias];
         CommitToQADatabase("Ert", const_cast<char*>(oss.str().c_str()), 
                            roc_eff[itrig][ias], roc_eff_err[itrig][ias]);

         textFile << itrig << "\t" << ias << "\t" << roc_eff[itrig][ias]
                  << "\t" << roc_eff_err[itrig][ias] << endl;
      }
   }

   ////
   //// GL1 noise rate
   ////
   Float_t gl1_noise[N_TRIG_MODE], gl1_noise_err[N_TRIG_MODE];
   CalcGl1NoiseRate(ert_gl1emu_4x4a, gl1_noise[0], gl1_noise_err[0]);
   CalcGl1NoiseRate(ert_gl1emu_4x4b, gl1_noise[1], gl1_noise_err[1]);
   CalcGl1NoiseRate(ert_gl1emu_4x4c, gl1_noise[2], gl1_noise_err[2]);
   CalcGl1NoiseRate(ert_gl1emu_2x2,  gl1_noise[3], gl1_noise_err[3]);
   CalcGl1NoiseRate(ert_gl1emu_el,   gl1_noise[4], gl1_noise_err[4]);

   textFile << endl 
            << "<< GL1 noise rate (1 - GL1&&emu/GL1 in MB evts) >>" << endl
            << "trigmode  rate  rate_err" << endl;
   for (int itrig = 0; itrig < N_TRIG_MODE; itrig++) {
      const char* trigger_name[N_TRIG_MODE] = { "4x4a", "4x4b", "4x4c","2x2", "el" };
      oss.str("");
      oss << "GL1 noise rate, " << trigger_name[itrig];
      CommitToQADatabase("Ert", const_cast<char*>(oss.str().c_str()), 
                         gl1_noise[itrig], gl1_noise_err[itrig]);

      textFile << itrig << "\t" << gl1_noise[itrig]
               << "\t" << gl1_noise[itrig] << endl;
   }

   ////
   //// N of live SM's
   ////
   Int_t nlive[N_TRIG_MODE][N_ARMSECT], nlive_err[N_TRIG_MODE][N_ARMSECT];
   CalcLiveSM(0, ert_smhit_4x4a, nlive[0], nlive_err[0]);
   CalcLiveSM(1, ert_smhit_4x4b, nlive[1], nlive_err[1]);
   CalcLiveSM(2, ert_smhit_4x4c, nlive[2], nlive_err[2]);
   CalcLiveSM(3, ert_smhit_2x2,  nlive[3], nlive_err[3]);
   CalcLiveSM(4, ert_smhit_RICH, nlive[4], nlive_err[4]);

   textFile << endl 
            << "<< The number of live SM's >>" << endl
            << "trigmode  sect  #liveSM  #liveSM_err" << endl;
   for (int itrig = 0; itrig < N_TRIG_MODE; itrig++) {
      for (int ias = 0; ias < N_ARMSECT; ias++) {
         const char* trigger_name[N_TRIG_MODE] = {"4x4a", "4x4b", "4x4c","2x2", "RICH"};
         oss.str("");
         oss << "Live SM, " << trigger_name[itrig] << ", " << SECT_NAME[ias];
         CommitToQADatabase("Ert", const_cast<char*>(oss.str().c_str()), 
                            nlive[itrig][ias], nlive_err[itrig][ias]);

         textFile << itrig << "\t" << ias << "\t" << nlive[itrig][ias] 
                  << "\t" << nlive_err[itrig][ias] << endl;
      }
   }

   ////
   //// sector-by-sector rejection factor
   ////
//   Int_t rf_4x4a[N_ARMSECT], rf_4x4a_err[N_ARMSECT];
//   Int_t rf_4x4b[N_ARMSECT], rf_4x4b_err[N_ARMSECT];
//   Int_t rf_4x4c[N_ARMSECT], rf_4x4c_err[N_ARMSECT];
//   Int_t rf_2x2 [N_ARMSECT], rf_2x2_err [N_ARMSECT];
//   Int_t rf_RICH[N_ARMSECT], rf_RICH_err[N_ARMSECT];
//   CalcRFactor(0, ert_smhit_4x4a, sd_, bbc_live, rf_4x4a, rf_4x4a_err);
//   CalcRFactor(1, ert_smhit_4x4b, sd_, bbc_live, rf_4x4b, rf_4x4b_err);
//   CalcRFactor(2, ert_smhit_4x4c, sd_, bbc_live, rf_4x4c, rf_4x4c_err);
//   CalcRFactor(3, ert_smhit_2x2,  sd_, bbc_live, rf_2x2,  rf_2x2_err);
//   CalcRFactor(4, ert_smhit_RICH, sd_, bbc_live, rf_RICH, rf_RICH_err);
      
   cout << "    ...done." << endl;
   return 0;
}
