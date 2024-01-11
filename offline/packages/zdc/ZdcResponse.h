#ifndef __ZDCRESPONSE_H__
#define __ZDCRESPONSE_H__

#include <iostream>

#include "phool.h"
#include "PHCompositeNode.h"
#include "PHRawDataNode.h"

#include "ZdcGeaHits.h"
#include "ZdcRaw.h"

class ZdcResponse
{
public:
  ZdcResponse();
  virtual ~ZdcResponse () { }

  void Clear();

  int calculate();
  PHBoolean PutEndProduct( PHCompositeNode *topNode );

private:
  ZdcGeaHits *zdcgea;
  ZdcRaw *zdcraw;
  PHRawDataNode *phrawdatanode;
};

#endif	// __ZDCRESPONSE_H__

