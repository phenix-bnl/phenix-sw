#include "emcOMFEMtupleT.h"
#include "emcLCTofs.h"
#include "emcAsciiStorageManager.h"

namespace
{
  const emcOMFEMtupleT<emcAsciiStorageManager,emcLCTofs> gemcOMLCTofs("emcOMLCTofs","Read/Write emcLCTofs objects");
}
