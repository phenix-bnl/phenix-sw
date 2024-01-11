#include "emcOMFEMtupleT.h"
#include "emcLCTofs.h"
#include "emcPgStorageManager.h"

namespace 
{
  const emcOMFEMtupleT<emcPgStorageManager,emcLCTofs> gemcOMLCTofs("emcOMLCTofs","Read/Write emcLCTofs objects");
}
