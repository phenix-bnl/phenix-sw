#ifndef MBBCUNPACKMDC2MODULE_H
#define MBBCUNPACKMDC2MODULE_H

#include "phool.h"

class PHCompositeNode;

class mBbcUnpackMDC2Module
{
public:
  mBbcUnpackMDC2Module() {}
  virtual ~mBbcUnpackMDC2Module() {}
  virtual PHBoolean event(PHCompositeNode *);
};

#endif
