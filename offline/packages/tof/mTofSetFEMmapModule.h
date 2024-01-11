#ifndef __MTOFSETFEMMAPMODULE_H__
#define __MTOFSETFEMMAPMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mTofSetFEMmapModule
{
public:
  mTofSetFEMmapModule(){}
  virtual ~mTofSetFEMmapModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MTOFSETFEMMAPMODULE_H__*/
