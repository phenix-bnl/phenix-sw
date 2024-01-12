#ifndef __EMCRECO3_H__
#define __EMCRECO3_H__





#include <SubsysRecoStack.h>
#include <string>



class EmcReco3 : public SubsysRecoStack
{
 public:
  EmcReco3(const std::string &name = "EmcReco3");
  virtual ~EmcReco3();

  int InitRun(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
};





#endif /* ! __EMCRECO3_H__ */

