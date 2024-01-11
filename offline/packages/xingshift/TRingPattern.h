#ifndef __TRingPattern_h_
#define __TRingPattern_h_

#include "TBunch.h"
#include <vector>

///////////////////////////////////////////////////////////////////////////
//
// class TRingPattern
//
//   represents one of the beam ring.(blue / yellow)
//   container of TBunch::pattern.
//
//     Author: Kazuya Aoki(Kyoto Univ.)
//             April 25,2005
///////////////////////////////////////////////////////////////////////////

class TRingPattern
{
public:
  TRingPattern();

  TBunch::pattern& at(int ibunch) { return ring_pat.at(ibunch); }
  const TBunch::pattern& at(int ibunch) const { return ring_pat.at(ibunch); }
  void clear();

  void show(std::ostream& out = std::cout,int ishift = 0) const;

  static bool physics_match_intended(const TRingPattern& physics,const TRingPattern& intended,int ishift);
  // if ishift < 0 , you get false;

  std::string get_pattern(int bunchzero = 0) const;
  int get_n_filled_bunches() const;

  typedef std::vector<TBunch::pattern> vec_pat;
private:
  vec_pat ring_pat;
};


#endif
