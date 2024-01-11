#include "emcOMFEMtupleT.h"
#include "emcQAs.h"
#include "emcPgStorageManager.h"

namespace 
{
  const emcOMFEMtupleT<emcPgStorageManager,emcQAs> gemcOMQAs("emcOMQAs","Read/Write emcQAs objects");
}
