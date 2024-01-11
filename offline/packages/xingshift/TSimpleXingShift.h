#ifndef __TSimpleXingShift_H__
#define __TSimpleXingShift_H__

#include "xingshift_chi2.h"

#include <vector>


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// class TSimpleXingShift
//
//    calculate absolute and relative (blue vs. yellow) xing shifts, in a simple way
//    
///////////////////////////////////////////
//
// 1. set intended crossing fill pattern (only distinction between filled vs. unfilled)
//        void SetIntendedPatterns(bool fillpat_blue[120], bool fillpat_yellow[120]);
// 2. set observed physics counts/crossing
//        void SetObservedCounts(int n_trigs_used, long long trig_counts[N_TRIG_MAX][120]);
// 3. calculate the mapping from physics pattern to intended pattern (absolute shift and relative shift)
//        void CalculateShifts(bool& success, int verb = 0);
// 4. get calculated shifts with one of following methods
//        void GetResults(int& absolute_shift, int& relative_shift);
//    ----OR-------
//    for compatability with TGeneralXingShift class behavior, get shifts with
//        const std::vector<xingshift_chi2>& get_result();
// 
//
///////////////////////////////////////////////////////////////////////////
class TSimpleXingShift
{
 public:
  static const int N_TRIG_MAX = 10; //maximum number of triggers allowed to be used for backup calibration  

 public:
  TSimpleXingShift();
  virtual ~TSimpleXingShift(){}


 public:
  void SetIntendedPatterns(bool fillpat_blue[120], bool fillpat_yellow[120]); //TRing::nBunch is just 120
  void SetObservedCounts(int n_trigs_used, long long trig_counts[N_TRIG_MAX][120]);
  void CalculateShifts(bool& success, int verb = 0);
  void GetResults(int& absolute_shift, int& relative_shift);
  const std::vector<xingshift_chi2>& get_result() const { return xingshift_result; }



 private: 
  void ShiftTrigCounts(int n_index, bool fillpat_blue[], bool fillpat_yellow[], long long counts[], int& abs_shift, int& rel_shift, int verb);


 private:
  int a_shift;
  int r_shift;
  bool fp_b[120];
  bool fp_y[120];
  int n_trig;
  long long counts[N_TRIG_MAX][120];
  
  
  std::vector<xingshift_chi2> xingshift_result;
  
};

#endif /* __TSimpleXingShift_H__ */

