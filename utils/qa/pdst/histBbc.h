#ifndef _HISTBBC_H
#define _HISTBBC_H
#include "SubsysReco.h"

class PHCompositeNode;

class QABbc: public SubsysReco
{
 public:
  QABbc(const char *name = "QABbc"): SubsysReco(name) {}
  virtual ~QABbc() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
};

#endif /*_HISTBBC_H*/

//EOF
