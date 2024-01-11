#ifndef __CGLVTX_H__
#define __CGLVTX_H__

#include "phool.h"
#include "PHObject.h"

class PHCompositeNode;

class cglVtx: public PHObject 
{
public:
  virtual ~cglVtx() {}
  virtual PHBoolean event(PHCompositeNode *r) = 0;
  virtual void reset() = 0;
  virtual int isValid() const = 0;
  virtual double Zvertex() = 0;
  virtual double ZvertexError() = 0;

  ClassDef(cglVtx, 1)
};

#endif /* __CGLVTX_H__ */
