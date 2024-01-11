//INCLUDECHECKER: Removed this line: #include "TBunch.h"
#include "TRingPattern.h"
#include "TRing.h"

const int TRing::nBunch;

TRing::TRing():bunches(nBunch)
{
}

void TRing::clear()
{
  bunches.assign(nBunch,TBunch());
}

TRingPattern TRing::get_ring_pattern() const
{
  TRingPattern result;
  for( int i = 0; i < nBunch; i++ ){
    result.at(i) = at(i).get_index_majority();
  }
  return result;
}

void TRing::show(std::ostream& out) const{
  for( int i = 0; i < (signed)bunches.size(); i++ ){
    out << "bunch# "<< i;
    bunches.at(i).show(out);
  }
}
