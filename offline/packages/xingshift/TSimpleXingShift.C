#include "TSimpleXingShift.h"

#include <cstring>

using namespace std;

TSimpleXingShift::TSimpleXingShift():
  a_shift(0),
  r_shift(0),
  n_trig(0)
{
  memset(fp_b,0,sizeof(fp_b));
  memset(fp_y,0,sizeof(fp_y));
  memset(counts,0,sizeof(counts));
}

void TSimpleXingShift::SetIntendedPatterns(bool fillpat_blue[120], bool fillpat_yellow[120]){

  for(int icrossing = 0; icrossing<120; icrossing++){
    fp_b[icrossing] = fillpat_blue[icrossing];
    fp_y[icrossing] = fillpat_yellow[icrossing];
  }

  return;
}
 
void TSimpleXingShift::SetObservedCounts(int n_trigs_used, long long trig_counts[TSimpleXingShift::N_TRIG_MAX][120]){
  n_trig = n_trigs_used;

  for(int icrossing = 0; icrossing<120; icrossing++){
    for(int itrig = 0; itrig < TSimpleXingShift::N_TRIG_MAX; itrig++){
      counts[itrig][icrossing] = trig_counts[itrig][icrossing];
    }
  }

  return;
}

void TSimpleXingShift::GetResults(int& absolute_shift, int& relative_shift){
  absolute_shift = a_shift;
  relative_shift = r_shift;
  return;
}

void TSimpleXingShift::CalculateShifts(bool& success, int verb){
  success = false;
  
  int abs_shift_array[TSimpleXingShift::N_TRIG_MAX] = {0};
  int rel_shift_array[TSimpleXingShift::N_TRIG_MAX] = {0};
  
  
  for(int itrig = 0; itrig < n_trig; itrig++){
    
    long long _counts[120];
    for(int ii = 0; ii< 120; ii++){
      _counts[ii] = counts[itrig][ii];
    }
    
    int _temp_abs;
    int _temp_rel;
    TSimpleXingShift::ShiftTrigCounts(120,fp_b,fp_y,_counts,_temp_abs,_temp_rel,verb);
  
    abs_shift_array[itrig] = _temp_abs;
    rel_shift_array[itrig] = _temp_rel;
    if ( itrig > 0 ) { //if not matching for all trigger selections used, fails
      if ( abs_shift_array[itrig]!=abs_shift_array[itrig-1] || rel_shift_array[itrig]!=rel_shift_array[itrig-1] ) { 
	//one method for storing results
	a_shift = 0;
	r_shift = 0; 
	  
	//other method for storing results for compatibility with other code
	xingshift_chi2 result;
	result.ishift = 0;
	result.relativeshift = 0;
	result.chi2 = 0;//obviously!
	xingshift_result.clear();
	xingshift_result.push_back(result);

	success = false;
	return;
      }
    }

  }

  //one method for storing results
  a_shift = abs_shift_array[0];
  r_shift = rel_shift_array[0]; 

  //other method for storing results for compatibility with other code
  xingshift_chi2 result;
  result.ishift = abs_shift_array[0];
  result.relativeshift = rel_shift_array[0];
  result.chi2 = 0;//obviously!
  xingshift_result.clear();
  xingshift_result.push_back(result);

  success = true;
  return;
}

void TSimpleXingShift::ShiftTrigCounts(int n_index, bool fillpat_blue[], bool fillpat_yellow[], long long counts[], int& abs_shift, int& rel_shift, int verb){

  abs_shift = 0;
  rel_shift = 0;

  //now try aligning by minimization of N contained in unfilled bunches

  float frac_in_unfilled = 1;
  
  int _abs_shift = 0;
  int _rel_shift = 0;
  //int unfilled[13] = {38,39,78,79,111,112,11,114,115,116,117,118,119};
  

  long long sum_xings = 0;
  for(int ii = 0; ii<n_index; ii++){
    sum_xings+=counts[ii];
  }
  long long sum_unfilled = 0;
  long long sum_unfilled_prev = sum_xings;
  for(int aa = 0; aa<n_index; aa++){//over all possible absolute shifts
    for(int rr = 0; rr<n_index; rr++){//over all possible relative shifts
      for(int ii = 0; ii<n_index; ii++){
	if( (fillpat_blue[ii] && fillpat_yellow[(ii+rr)%n_index]) == false){sum_unfilled+=counts[(ii+aa)%n_index];}
      }
      
      if( sum_unfilled<sum_unfilled_prev){
	sum_unfilled_prev = sum_unfilled;
	frac_in_unfilled = (double)sum_unfilled/(double)sum_xings;
	if(verb>1){cout<<"next best guess is abs, rel: "<<aa<<", "<<rr<<" with frac: "<<frac_in_unfilled<<" and sum: "<<sum_unfilled<<endl;}
	_abs_shift = aa;
	_rel_shift = rr;
      }      
      sum_unfilled = 0;
    }
  }
  
  if(verb>0){cout<<"converged on frac_in_unfilled "<<frac_in_unfilled<<" at absolute shift "<<_abs_shift<<" and relative shift "<<_rel_shift<<"."<<endl;}

  abs_shift = _abs_shift;
  rel_shift = _rel_shift;


  return;

}


