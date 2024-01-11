#ifndef __MBBCRAWOUTMODULE_H__
#define __MBBCRAWOUTMODULE_H__

#include <phool.h>

class PHCompositeNode;

class mBbcRawOutModule
{
public:
  mBbcRawOutModule() {}
  virtual ~mBbcRawOutModule() {}
  virtual PHBoolean event(PHCompositeNode *);
};
#endif /*__MBBCRAWOUTMODULE_H__*/
