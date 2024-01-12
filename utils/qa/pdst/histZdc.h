#ifndef _HISTZDC_H
#define _HISTZDC_H

#include "SubsysReco.h"

class PHCompositeNode;

class QAZdc: public SubsysReco
{
 public:
  QAZdc(const char *name = "QAZdc"): SubsysReco(name) {}
  virtual ~QAZdc();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
};

#endif /* _HISTZDC_H */

//EOF
