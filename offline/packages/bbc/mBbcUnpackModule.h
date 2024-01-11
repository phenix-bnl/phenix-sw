#ifndef MBBCUNPACKMODULE_H
#define MBBCUNPACKMODULE_H

#include "phool.h"

class PHCompositeNode;

class mBbcUnpackModule
{
  public:
    mBbcUnpackModule() {}
    virtual ~mBbcUnpackModule() {}
    virtual PHBoolean event(PHCompositeNode *);
};

#endif
