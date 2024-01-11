#include "emcOMCalibrationDataT.h"
#include "emcPgStorageManager.h"

namespace
{
  /**@class emcOMCalibrationData(pg)
     DM plugin to R/W emcCalibrationData from/to Postgres
  */
  const emcOMCalibrationDataT<emcPgStorageManager> gemcOMCalibrationData("emcOMCalibrationData","Handle various calibration objects");
}
