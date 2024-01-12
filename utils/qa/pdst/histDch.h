/*
 * histDch.h
 * $Id: histDch.h,v 1.4 2005/11/15 23:20:18 slash Exp $
 */

#ifndef _HISTDCH_H
#define _HISTDCH_H

#include "SubsysReco.h"

class PHCompositeNode;

class QADch: public SubsysReco
{
 public:
  QADch(const char *name = "QADch"): SubsysReco(name) {}
  virtual ~QADch() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
};

#endif /* _HISTDCH_H */

// EOF
