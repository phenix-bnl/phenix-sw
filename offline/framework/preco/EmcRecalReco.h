#ifndef __EMCRECALRECO_H__
#define __EMCRECALRECO_H__





#include <SubsysRecoStack.h>
#include <string>



class EmcRecalReco : public SubsysRecoStack
{
 public:
  EmcRecalReco(const std::string &name = "EmcRecalReco");
  virtual ~EmcRecalReco();

  int InitRun(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode);
};





#endif /* ! __EMCRECALRECO_H__ */


