#include "emcOMFEMtupleT.h"
#include "emcGains.h"
#include "emcPgStorageManager.h"

typedef emcOMFEMtupleT<emcPgStorageManager,emcGains> emcOMGains;

namespace 
{
  const emcOMGains gemcOMGains("emcOMGains","Read/Write emcGains objects");
}
