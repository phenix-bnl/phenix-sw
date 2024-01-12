#ifndef _HISTTEC_H
#define _HISTTEC_H

#include "SubsysReco.h"

class TecCalibrationObject;

class QATec: public SubsysReco
{
 public:
  QATec(const char *name = "QATec"): SubsysReco(name)  {}
  virtual ~QATec() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  float tecmatchalpha_cut;
  float tecmatchphi_cut;
  TecCalibrationObject *TCO;
};

#endif

// EOF
