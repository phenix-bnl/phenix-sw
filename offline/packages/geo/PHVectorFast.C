//  Creator: Tim Miller

#include "PHVectorFast.h"
//INCLUDECHECKER: Removed this line: #include <cmath>

void 
PHVectorFast::calcLength()
{
  dlSqr = dx * dx + dy * dy + dz * dz;
  dl = sqrt(dlSqr);
}


