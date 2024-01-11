#include "emcOMGeometryT.h"
#include "emcPgStorageManager.h"

namespace 
{
  const emcOMGeometryT<emcPgStorageManager> gemcOMGeometry("emcOMGeometry","Read/Write EMCAL Geometry");
}
