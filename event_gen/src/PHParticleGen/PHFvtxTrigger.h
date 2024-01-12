#ifndef __PHFTXTRIGGER_H__
#define __PHFTXTRIGGER_H__

#include <PHPyTrigger.h>

class PHFvtxTrigger : public PHPyTrigger 
{
public:
  PHFvtxTrigger(const std::string &name = "PHFvtxTrigger");
  virtual ~PHFvtxTrigger() {};

  // Methods Derived from SubsysReco
  int Init(PHCompositeNode *topNode);
  //int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

protected:
  int FvtxAcceptance(PHPythiaContainer *phpylist);

};

#endif // __PHFTXTRIGGER_H__
