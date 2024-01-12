#ifndef __TECCHARGERECAL_H__
#define __TECCHARGERECAL_H__

#include "Recalibrator.h"
#include "PHTimeStamp.h"

#include <string>
#include <vector>

class TecCalibrationObject;

class TecChargeRecal : public Recalibrator
{
 public:

  TecChargeRecal();

  virtual ~TecChargeRecal();

  int Init(PHCompositeNode*);
  int InitRun(PHCompositeNode*);
  int process_event(PHCompositeNode*);
  int isValidRun(const int runno) const;

 private:
  
  TecCalibrationObject *TCO;

};

#endif
