#ifndef _DSTQA_H
#define _DSTQA_H

#include "SubsysReco.h"

class QAReco: public SubsysReco
{
 public:
  QAReco();  
  QAReco(const char *name);
  QAReco(const char *file, const char *options);
  virtual ~QAReco() {}

  int Init(PHCompositeNode *topNode);
  void Print(char *out);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  int End(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode) {return 0;}
	
 protected:
};

#endif


//EOF


