#ifndef __SPINRECO_H__
#define __SPINRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class SpinEvent;
class SpinEventGL1p;

class SpinReco: public SubsysReco
{
 public:
  SpinReco(const std::string &name = "SPIN");
  virtual ~SpinReco();
  
  int  Init(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  process_event(PHCompositeNode *topNode);
      
 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  SpinEvent* pSpinEvent;
  SpinEventGL1p* pSpinEventGL1p;
  
};

#endif /* __SPINRECO_H__ */
