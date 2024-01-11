#ifndef __MEMCGEACLUSTEREVAL2MODULE_H__
#define __MEMCGEACLUSTEREVAL2MODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

class PHCompositeNode;

class mEmcGeaClusterEval2Module
{
public:
  mEmcGeaClusterEval2Module(){}
  virtual ~mEmcGeaClusterEval2Module(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCGEACLUSTEREVAL2MODULE_H__*/
