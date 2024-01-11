#include "emcOMFEMtupleT.h"
#include "emcPedestals5.h"
#include "emcPgStorageManager.h"

typedef emcOMFEMtupleT<emcPgStorageManager,emcPedestals5> emcOMPedestals5;

namespace 
{
  const emcOMPedestals5 gemcOMPedestals5("emcOMPedestals5","Read/Write emcPedestals5 objects");
}
