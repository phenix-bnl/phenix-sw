#ifndef  __HISTSPIN_H__
#define __HISTSPIN_H__

//===================================================================
//
//  QA module for SpinDataEventOut node
//      Original version was created by Y.Fukao 22/Dec/2004
//      Modified by H.Torii for QA module in CVS
//
//===================================================================
#include "SubsysReco.h"

class PHCompositeNode;

class QASpin : public SubsysReco{
 public:
  QASpin(const char *name="QASpin"): SubsysReco(name){};
  virtual ~QASpin(void) {};
  int InitRun(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  void DefineHistogram(void);
  int process_event(PHCompositeNode *topNode);
  int process_event_CDEV(PHCompositeNode *topNode);
  int process_event_spin(PHCompositeNode *topNode);
  int process_event_special(PHCompositeNode *topNode);

 private:

};
#endif /* __HISTSPIN_H__ */
