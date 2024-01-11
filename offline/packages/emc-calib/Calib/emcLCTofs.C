// $Id: emcLCTofs.C,v 1.5 2002/01/08 16:48:10 aphecetc Exp $

#include "emcLCTofs.h"
#include "emcLCTofFEM.h"

float emcLCTofs::GetValue1(int ichannel) const {
  emcLCTofFEM* fem = dynamic_cast<emcLCTofFEM*>(fFEMs[ichannel/144]) ;
  if (fem) {
    return fem->getValue(ichannel%144,0) ;
  }
  else {
    return -9999 ;
  }
}

float emcLCTofs::GetValue2(int ichannel) const {
  emcLCTofFEM* fem = dynamic_cast<emcLCTofFEM*>(fFEMs[ichannel/144]) ;
  if (fem) {
    return fem->getValue(ichannel%144,1) ;
  }
  else {
    return -9999 ;
  }
}
