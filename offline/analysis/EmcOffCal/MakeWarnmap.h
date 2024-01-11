#ifndef MAKEWARNMAP_H__
#define MAKEWARNMAP_H__

#include "EmcAnaCommon.h"

#include <string>

class TH3D;

class MakeWarnmap
{
   static const int N_RUN_RANGE_MAX = 1000;
   static const int N_ECORE_RANGE_MAX = 100;
   
   std::string m_fname_run_range;
   int m_n_run_range;
   int m_run_range[N_RUN_RANGE_MAX + 1];

   int m_n_ecore_range;
   double m_ecore_range[N_ECORE_RANGE_MAX + 1];
   
   TH3D* m_h3_nhit[N_RUN_RANGE_MAX][EmcAnaCommon::N_ARMSECT];
   
   Int_t m_warnmap[EmcAnaCommon::N_ARMSECT][EmcAnaCommon::N_YPOS_PBGL][EmcAnaCommon::N_ZPOS_PBGL];
   Int_t m_current_map[EmcAnaCommon::N_ARMSECT][EmcAnaCommon::N_YPOS_PBGL][EmcAnaCommon::N_ZPOS_PBGL];
   
 public:
   MakeWarnmap(const Int_t n_ecore_range, const Double_t* ecore_range, 
               const Int_t n_run_range, const Int_t* run_range);
   virtual ~MakeWarnmap();
   //// for making warnmap
   void AddNhitInRun(Char_t* run_data_list);
   
   void CalcDeadTower();
   void CalcHotTower(double sigma_cut, double sigma_cut_pbgl=-1);
   
   void SetUncalibTower(Char_t* fname_uncalib_list);
   void SetAroundAndEdge();
   void DumpResult(char* fname_map);
   void DrawEcoreDist();
   
 protected:
   void SetRunRange();
   void SetRunRange(const Int_t n_run_range, const Int_t* run_range);
   void SetEcoreRange(const Int_t n_ecore_range, const Double_t* ecore_range);
   
   void DrawCurrentMap(Char_t* epsfname);
   
};

#endif // MAKEWARNMAP_H__
