// $Id: emcWalkTofs.C,v 1.5 2002/01/08 16:48:11 aphecetc Exp $
#include "emcWalkTofs.h"
#include "emcWalkTofFEM.h"

float emcWalkTofs::GetValue1(int ichannel) const {
  emcWalkTofFEM* fem = dynamic_cast<emcWalkTofFEM*>(fFEMs[ichannel/144]) ;
  if (fem) {
    return fem->getValue(ichannel%144,0) ;
  }
  else {
    return -9999 ;
  }
}

float emcWalkTofs::GetValue2(int ichannel) const {
  emcWalkTofFEM* fem = dynamic_cast<emcWalkTofFEM*>(fFEMs[ichannel/144]) ;
  if (fem) {
    return fem->getValue(ichannel%144,1) ;
  }
  else {
    return -9999 ;
  }
}
