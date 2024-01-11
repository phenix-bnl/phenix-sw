#include "TBunch.h"
#include <iostream>

// definitions of the constants
const int TBunch::int_pattern[npat] = {  1,-1,0,-99 };
const int TBunch::npat; // defined in TBunch.h

float TBunch::dummy = 0;

//////////////// methods

TBunch::TBunch()
{
  for( int i = 0;i< npat; i++ ) nevts[i] = 0.;
}

TBunch::TBunch(int nup,int ndown,int nunpol,int nunfill)
{
  nevts[0] = nup;
  nevts[1] = ndown;
  nevts[2] = nunpol;
  nevts[3] = nunfill;
}

void TBunch::clear()
{
  for( int i = 0;i< npat; i++ ) nevts[i] = 0.;
}

float TBunch::get(pattern pat) const{
  if ( ! bound_check(pat) ) return 0;
  return nevts[pat];
}

void TBunch::set(pattern pat,float value) {
  if ( ! bound_check(pat) ) return;
  nevts[pat] = value;
}

float TBunch::get_total() const
{
  float total = 0;
  for( int i = 0;i < npat; i ++ ) total += nevts[i];
  return total;
}

TBunch::pattern TBunch::get_index_majority() const {
  if ( get_total() == 0. ) return unfill;
  float max = 0.;
  int imajority = 0;
  for( int i = 0;i<npat;i++) {
    if ( max < nevts[i] ) {
      max = nevts[i];
      imajority = i;
    }
  }
  return (TBunch::pattern)imajority;
}

void TBunch::multiply(float val)
{
  for( int i = 0; i< npat; i++ ) nevts[i] *= val;
}

int TBunch::pat2num(pattern pat)
{
  if ( !bound_check(pat) ) return -99;
  return int_pattern[pat];
}

TBunch::pattern TBunch::num2pat(int int_pattern)
{
  switch( int_pattern ){
  case -1:
    return down;
  case 0:
    return unpol;
  case 1:
    return up;
  }
  return unfill;
}

float TBunch::chi2(pattern pat) const
{
  if ( !bound_check(pat) ) return 0;
  float intended[npat];
  for( int i = 0; i < npat; i ++ ) intended[i] = 0;
  intended[pat] = 1.;

  float val_tmp[npat];
  for(int i = 0;i<npat;i++ ) val_tmp[i] = nevts[i];

  if ( get_total() == 0. || get_index_majority() == unfill ){
    val_tmp[unfill] = 1.;
  }

  float result = 0;
  for( int i = 0;i < npat; i++ ){
    result += (val_tmp[i] - intended[i])*(val_tmp[i] - intended[i]);
  }

  return result;
}

float TBunch::chi2_neglect_pattern(float point) const
{
  float total = get_total();
  return ( total - point )*(total - point);
}

bool TBunch::bound_check(pattern pat){
  if ( pat >=0 && pat < npat ) return true;
  return false;
}

char TBunch::get_char(pattern pat){
  switch(pat){
  case up:
    return '+';
  case down:
    return '-';
  case unpol:
    return '0';
  case unfill:
    return '*';
  }
  return '?';
}

void TBunch::show(std::ostream& out) const
{
  out << "\t" << get_char(up)    << ":" << get(up)
      << "\t" << get_char(down)  << ":" << get(down)
      << "\t" << get_char(unpol) << ":" << get(unpol)
      << "\t" << get_char(unfill)<< ":" << get(unfill)
      << std::endl;
}

float TBunch::instability() const
{
  float total = get_total();
  if ( total == 0 ) return 0;
  float max = get(get_index_majority());
  return (total - max) / total;
}

const float& TBunch::at(pattern pat) const
{
  if ( !bound_check(pat) ) return dummy;
  return nevts[pat];
}

float& TBunch::at(pattern pat)
{
  if ( !bound_check(pat) ) return dummy;
  return nevts[pat];
}
