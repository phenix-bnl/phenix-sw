#ifndef __EMC_PARAMS_SIMMAKER_H__
#define __EMC_PARAMS_SIMMAKER_H__


#include <Rtypes.h>

#include <SubsysReco.h>


class PHCompositeNode;


class EmcGeaParamsSimMaker: public SubsysReco {
public:
  EmcGeaParamsSimMaker();
  ~EmcGeaParamsSimMaker();

  int InitRun(PHCompositeNode * root);


  ClassDef(EmcGeaParamsSimMaker, 0)
};





#endif /* ! __EMC_PARAMS_SIMMAKER_H__ */

