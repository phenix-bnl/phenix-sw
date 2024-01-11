#ifndef __TRing_h
#define __TRing_h

#include <vector>
#include <iostream>

class TRingPattern;
class TBunch;

//////////////////////////////////////////////////////////////
//
// class TRing
//
//    represents one of the beam.(blue / yellow)
//    container of TBunch( # of TBunch elements is nBunch=120)
//
//       Author: Kazuya Aoki (Kyoto Univ.)
//               April 25,2005
/////////////////////////////////////////////////////////////

class TRing
{
public: // constants
  static const int nBunch = 120;
  typedef std::vector<TBunch> vec_bunch;

public: // methods
  TRing();

  TBunch& at(int ibunch) { return bunches.at(ibunch); }
  const TBunch& at(int ibunch) const { return bunches.at(ibunch); }
  void clear();

  TRingPattern get_ring_pattern() const;
  void show(std::ostream& out = std::cout) const;
  
private:
  vec_bunch bunches;

};

#endif
