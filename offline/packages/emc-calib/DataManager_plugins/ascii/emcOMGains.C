#include "emcOMFEMtupleT.h"
#include "emcGains.h"
#include "emcAsciiStorageManager.h"

namespace
{
  const emcOMFEMtupleT<emcAsciiStorageManager,emcGains> gemcOMGains("emcOMGains","Read/Write emcGains objects");
}
