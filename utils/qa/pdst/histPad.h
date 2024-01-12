/*
 * histPad.h
 * $Id: histPad.h,v 1.4 2005/11/12 03:19:36 slash Exp $
 *
 * Routines for booking and filling pad chamber QA histograms
 */

#ifndef _HISTPAD_H
#define _HISTPAD_H
#include "SubsysReco.h"

class PHCompositeNode;

class QAPad: public SubsysReco
{
 public:
  QAPad(const char *name = "QAPad"): SubsysReco(name) {}
  virtual ~QAPad() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
};

#endif /* _HISTPAD_H */

// EOF
