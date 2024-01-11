#ifndef __MPCEXRECO_H__
#define __MPCEXRECO_H__

#include <SubsysRecoStack.h>
#include <vector>
#include <string>
class mMpcExEventCut;

class MpcExReco : public SubsysRecoStack {

 public:
  MpcExReco();
  virtual ~MpcExReco();

  void SetTriggerNames(const std::vector<std::string>& names);

 private:
  mMpcExEventCut *_eventCut;
};

#endif /* __MPCEXRECO_H__ */
