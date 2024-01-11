#ifndef __EMC_PISA2GEAEDEP_H__
#define __EMC_PISA2GEAEDEP_H__





#include <Rtypes.h>

#include <SubsysReco.h>



class PHCompositeNode;


class EmcGeaPisa2GeaEdep: public SubsysReco {

public:
  EmcGeaPisa2GeaEdep();
  ~EmcGeaPisa2GeaEdep();
  

public:
  int InitRun(PHCompositeNode * root);
  int process_event(PHCompositeNode * root);
  int Reset(PHCompositeNode * root);
  int ResetEvent(PHCompositeNode * root){ return Reset(root); }
  int End(PHCompositeNode * root){ return Reset(root); }


  ClassDef(EmcGeaPisa2GeaEdep, 0)
};





#endif /* ! __EMC_PISA2GEAEDEP_H__ */

