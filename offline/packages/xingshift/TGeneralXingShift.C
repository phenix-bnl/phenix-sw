#include <TGeneralXingShift.h>

#include <algorithm>
#include <iostream>

using namespace std;

const TRingPattern& TGeneralXingShift::get_testxing(beam_color color) const
{
  return intended_ring_pattern_testxing[color];
}


void TGeneralXingShift::make_testxing_and(int relativeshift)
{
  for( int i = 0;i < TRing::nBunch ;i++ ){
    int iblue = i;
    int iyellow = ( i + relativeshift ) % TRing::nBunch;

    // fill intended test case blue
    if ( intended_ring_pattern[yellow].at(iyellow) == TBunch::unfill ) {
      intended_ring_pattern_testxing[blue].at(i) = TBunch::unfill;
    }else {
      intended_ring_pattern_testxing[blue].at(i) = intended_ring_pattern[blue].at(iblue);
    }    

    // fill intended test case yellow
    if ( intended_ring_pattern[blue].at(iblue) == TBunch::unfill ) {
      intended_ring_pattern_testxing[yellow].at(i) = TBunch::unfill;
    } else {
      intended_ring_pattern_testxing[yellow].at(i) = intended_ring_pattern[yellow].at(iyellow);
    }
  }
}

void TGeneralXingShift::make_testxing_or(int relativeshift)
{
  for( int i = 0;i < TRing::nBunch;i++ ){
    int iblue = i;
    int iyellow = ( i + relativeshift ) % TRing::nBunch;
    // fill intended test case blue
    intended_ring_pattern_testxing[blue].at(i)   = intended_ring_pattern[blue].at(iblue);
    // fill intended test case yellow
    intended_ring_pattern_testxing[yellow].at(i) = intended_ring_pattern[yellow].at(iyellow);
  }
}


void TGeneralXingShift::find_shift_using_match()
{
  xingshift_result.clear();

  // loop over all possibility
  for( int relative = 0; relative < TRing::nBunch; relative ++ ) {
    make_testxing_or(relative);

    for( int ishift = 0; ishift < TRing::nBunch; ishift ++ ) {
      if ( TRingPattern::physics_match_intended(physics_ring_pattern[blue],
						intended_ring_pattern_testxing[blue],ishift) &&
	   TRingPattern::physics_match_intended(physics_ring_pattern[yellow],
						intended_ring_pattern_testxing[yellow],ishift)){
	xingshift_chi2 result;
	result.ishift = ishift;
	result.relativeshift = relative;
	result.chi2 = 0;
	xingshift_result.push_back(result);
      }
    }
  }
}

void TGeneralXingShift::find_shift_using_chi2_neglect_pattern()
{
  normalize_physics_ring();
  vector<xingshift_chi2> ranking;
  for( int relative = 0; relative < TRing::nBunch; relative++ ){
    make_testxing_and(relative);
    for ( int ishift = 0 ; ishift < TRing::nBunch; ishift++ ){
      xingshift_chi2 test;
      test.chi2 = chi2_neglect_pattern(ishift);
      test.relativeshift = relative;
      test.ishift = ishift;
      ranking.push_back(test);
    }
  }
  sort(ranking.begin(),ranking.end());
  vector<xingshift_chi2>::iterator iter = ranking.begin();

  xingshift_result.clear();

  for(int i =0;i<10;i++){
    if ( verbosity > 5 ) iter->show();
    make_testxing_or(iter->relativeshift);
    if ( TRingPattern::physics_match_intended(physics_ring_pattern[blue],
					      intended_ring_pattern_testxing[blue],iter->ishift) &&
	 TRingPattern::physics_match_intended(physics_ring_pattern[yellow],
					      intended_ring_pattern_testxing[yellow],iter->ishift) ){
      if ( verbosity > 5 ) cout << "      this matches the intended pattern" << endl;
      xingshift_result.push_back(*iter);
    }else {
      if ( verbosity > 5 ) cout << "      this doesn't match the intended pattern" << endl;
    }
    iter++;
  }

  if ( xingshift_result.size() == 0 ){
    if ( verbosity > 5 ) cout << " NOTHING MATCHED " << endl;
  }else if ( xingshift_result.size() > 1 ) {
    if ( verbosity > 5 ) cout << " multiple possibility" << endl;
  }
}

void TGeneralXingShift::normalize_physics_ring(){
  int nbunch = 0;
  for( int i = 0;i < TRing::nBunch; i++ ){
    if ( intended_ring_pattern[blue].at(i) != TBunch::unfill ) nbunch++;
  }
  if ( nbunch == 0 ) return ;
  
  for( int ibeam = 0; ibeam < 2; ibeam ++ ){
    float total = 0;
    for( int i = 0; i < TRing::nBunch; i++ )
      total += physics_ring[ibeam].at(i).get_total();
    for( int i = 0; i < TRing::nBunch; i++ )
      physics_ring[ibeam].at(i).multiply((float)nbunch/total);
  }
}

float TGeneralXingShift::chi2_neglect_pattern(int ishift)
{
  float result = 0;
  for( int i = 0;i < TRing::nBunch; i++ ){
    //////// 
    int iphysics = (i + ishift) % TRing::nBunch;
    int iintended = i;

    for( int ibeam = 0; ibeam < 2; ibeam++ ){
      float point = 0;
      if ( intended_ring_pattern_testxing[ibeam].at(iintended) != TBunch::unfill ) point = 1;
      result += physics_ring[ibeam].at(iphysics).chi2_neglect_pattern(point);
    }
  }
  return result;
}

void TGeneralXingShift::find_shift_using_chi2()
{
  cout << " TGeneralXingShift::find_shift_using_chi2 -- not implemented. " << endl;
}

void TGeneralXingShift::clear()
{
  TGeneralXingShiftInput::clear();
  for( int i = 0;i<2;i++) intended_ring_pattern_testxing[i].clear();
  xingshift_result.clear();
}
