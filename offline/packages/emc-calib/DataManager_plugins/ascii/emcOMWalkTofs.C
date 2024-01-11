#include "emcOMFEMtupleT.h"
#include "emcWalkTofs.h"
#include "emcAsciiStorageManager.h"

namespace
{
  const emcOMFEMtupleT<emcAsciiStorageManager,emcWalkTofs> gemcOMWalkTofs("emcOMWalkTofs","Read/Write emcWalkTofs objects");
}
