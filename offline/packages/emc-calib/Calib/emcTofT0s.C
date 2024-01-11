// $Id: emcTofT0s.C,v 1.1 2010/01/08 17:52:54 phnxemc Exp $
#include "emcTofT0s.h"
#include "emcTofT0FEM.h"

float emcTofT0s::GetValue1(int ichannel) const {
  emcTofT0FEM* fem = dynamic_cast<emcTofT0FEM*>(fFEMs[ichannel/144]) ;
  if (fem) {
    return fem->getValue(ichannel%144,0) ;
  }
  else {
    return -9999 ;
  }
}

float emcTofT0s::GetValue2(int ichannel) const {
  emcTofT0FEM* fem = dynamic_cast<emcTofT0FEM*>(fFEMs[ichannel/144]) ;
  if (fem) {
    return fem->getValue(ichannel%144,1) ;
  }
  else {
    return -9999 ;
  }
}
