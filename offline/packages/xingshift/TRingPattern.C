//INCLUDECHECKER: Removed this line: #include "TBunch.h"
#include "TRingPattern.h"
#include "TRing.h"

#include <iostream>

TRingPattern::TRingPattern():ring_pat(TRing::nBunch,TBunch::unfill)
{
}

void TRingPattern::clear()
{
  ring_pat.assign(TRing::nBunch,TBunch::unfill);
}

void TRingPattern::show(std::ostream& out,int ishift) const
{
  for( int i = 0; i < TRing::nBunch; i ++ ){
    int ibunch = ( i + ishift ) % TRing::nBunch;
   out << TBunch::get_char(ring_pat.at(ibunch));
  }
  out << std::endl;
}

bool TRingPattern::physics_match_intended(const TRingPattern& physics,const TRingPattern& intended,int ishift)
{
  if ( ishift < 0 ) return false;
  for( int i = 0; i< (signed) TRing::nBunch ; i++ ){
    int iphysics = ( i + ishift ) % TRing::nBunch;
    if ( physics.at(iphysics) == TBunch::unfill ) continue;
    if ( physics.at(iphysics) != intended.at(i) ) return false;
  }
  return true;
}

std::string TRingPattern::get_pattern(int bunchzero) const
{
  const int npat_width = 8;
  if ( get_n_filled_bunches() < npat_width ) return "unknown";
  // identify the pattern first.
  TBunch::pattern pat[npat_width];
  std::string pattern_name;
  int current = 0;
  for( int i = 0;i<TRing::nBunch ; i++){
    int ibunch = (i + bunchzero) % TRing::nBunch;
    if ( ring_pat.at(ibunch) != TBunch::unfill ) {
      pat[current] = ring_pat.at(ibunch);
      pattern_name += TBunch::get_char(ring_pat.at(ibunch));
      current++;
      if ( current >= npat_width ) break;
    }
  }

  //std::cout << "identified pattern " << pattern_name << std::endl;

  // validate the identified pattern.
  current = 0;
  int abort_gap = 0;
  for( int i = 0; i<TRing::nBunch; i++){
    int ibunch = (i + bunchzero) % TRing::nBunch;
    if ( ring_pat.at(ibunch) == TBunch::unfill ) {
      abort_gap++;
    }else{
      if ( pat[current] != ring_pat.at(ibunch) && abort_gap > 0 ){
	// try next.
	current++;
	if ( current >= npat_width ) current = 0;
	if ( pat[current] != ring_pat.at(ibunch) ) goto unknown_pattern;
      }
      //std::cout << "#" << ibunch << ":" << TBunch::get_char(pat[current]) << std::endl;
      current++;
      if ( current >= npat_width ) current = 0;
      abort_gap = 0;
    }
  }
  return pattern_name;

  unknown_pattern:
  pattern_name = "unknown";
  return pattern_name;
}

int TRingPattern::get_n_filled_bunches() const
{
  int n = 0;
  for( int i = 0; i<TRing::nBunch; i++){
    if ( ring_pat.at(i) != TBunch::unfill ) n++;
  }
  return n;
}
