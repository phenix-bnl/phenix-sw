#ifndef __MMPCEXEVTCUT_H__
#define __MMPCEXEVTCUT_H__

#include <SubsysReco.h>
#include <string>
#include <vector>

class PHCompositeNode;

class mMpcExEventCut: public SubsysReco {
 public:
  mMpcExEventCut();
  virtual ~mMpcExEventCut() {}

  int process_event(PHCompositeNode *topNode);

  void SetTriggerNames(const std::vector<std::string>& names){
    _triggerNames.clear();
    _triggerNames = names;
    _updatedTriggers = true;
  }

 private:
  bool _updatedTriggers;
  std::vector<std::string> _triggerNames;

};

#endif /* __MMPCEXEVTCUT_H__ */
