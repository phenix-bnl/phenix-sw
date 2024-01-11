#include "emcOMTracedFEMT.h"
#include "emcPgStorageManager.h"
#include "emcGainFEM.h"
#include "emcTacPedFEM.h"
#include "emcTofT0FEM.h"

namespace 
{
  const emcOMTracedFEMT<emcPgStorageManager,emcGainFEM> 
  gemcOMGainFEM("emcOMGainFEM","Read/Write emcGainFEM objects");

  const emcOMTracedFEMT<emcPgStorageManager,emcTacPedFEM> 
  gemcOMTacPedFEM("emcOMTacPedFEM","Read/Write emcTacPedFEM objects");
}
