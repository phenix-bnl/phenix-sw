
#include "emcClusterAuxInfoV1.h"

emcClusterAuxInfoV1::emcClusterAuxInfoV1 ( )
    {
      _chi2 = 0;
      _ecore = 0;
      _x = 0;
      _y = 0;
    }

emcClusterAuxInfoV1::emcClusterAuxInfoV1 ( const float c, const float e, const float x, const float y)
    {
      _chi2 = c;
      _ecore = e;
      _x = x;
      _y = y;
    }

ClassImp(emcClusterAuxInfoV1);

