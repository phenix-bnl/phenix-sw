#include <TFvtxCompactCoord.h>
#include <FVTXOO.h>
#include <PHGeometry.h>

ClassImp(TFvtxCompactCoord)

//_____________________________________________
PHPoint TFvtxCompactCoord::get_coord_midpoint() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return (get_coord_begin() + get_coord_end()) * 0.5;
}




