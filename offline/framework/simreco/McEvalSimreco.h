#ifndef __McEvalSIMRECO_H__
#define __McEvalSIMRECO_H__

#include <SubsysReco.h>

class  mCentralTrackEvaluator_v1;
class  mNewDchEvaluator;

class McEvalSimreco: public SubsysReco
{
 public:
  McEvalSimreco(const std::string &name = "McEval");
  virtual ~McEvalSimreco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  //global charged track evaluator(they should just be renamed)
  mCentralTrackEvaluator_v1* mEvaluate;
  mNewDchEvaluator* mNewDchEvaluate;
};
#endif
