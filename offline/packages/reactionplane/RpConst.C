#include "RpConst.h"

namespace RP {
  int calcIdCode(const int det, const int kind, const int nhar){
    int ret = (((det&0xFF)<<16) | ((kind&0xFF)<<8) | (nhar&0xFF));
    return ret;
  }

  void reverseIdCode(const int idcode, int& det, int& kind, int& nhar){
    det  = ((idcode>>16)&0xFF);
    kind = ((idcode>>8) &0xFF);
    nhar = ((idcode)    &0xFF);
  }

};
