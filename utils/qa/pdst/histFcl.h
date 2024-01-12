/*
 * histFcl.h
 *
 * Routines to book and fill FCL QA histograms.
 */

#ifndef _HISTFCL_H
#define _HISTFCL_H
#include "SubsysReco.h"

class PHCompositeNode;

class QAFcl: public SubsysReco
{
 public:
  QAFcl(const char *name = "QAFcl"): SubsysReco(name) {}
  virtual ~QAFcl() {}

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
};

#endif /* _HISTFCL_H */

//EOF
