#ifndef _HISTCRK_H
#define _HISTCRK_H
#include "SubsysReco.h"

class PHCompositeNode;

class QACrk: public SubsysReco
{
 public:
  QACrk(const char *name = "QACrk"): SubsysReco(name) {}
  virtual ~QACrk() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int get_pmt_coords(int pmtnum, int* sector, float* z, float* phi); 
  void ring_finder(float hitmat[5120]); 
};

#endif /*_HISTCRK_H*/

//EOF
