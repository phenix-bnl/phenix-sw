#include "emcOMRejectListT.h"
#include "emcPgStorageManager.h"

namespace 
{
  const emcOMRejectListT<emcPgStorageManager> gemcOMRejectList("emcOMRejectList","Read/Write EMCAL RejectList");
}
