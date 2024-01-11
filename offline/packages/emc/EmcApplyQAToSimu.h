#ifndef __EmcApplyQAToSimu_h__
#define __EmcApplyQAToSimu_h__

#include <vector>

#include <SubsysReco.h>


class EmcApplyQAToSimu : public SubsysReco {
public:
  EmcApplyQAToSimu();
  virtual ~EmcApplyQAToSimu();

  virtual int Init(PHCompositeNode * root);
  virtual int InitRun(PHCompositeNode * root);
  virtual int process_event(PHCompositeNode * root);


protected:
  // data for additional masks via EMCDEADRECALDATASOURCE
  bool emcdeadrecal;
  std::vector<unsigned int> errorraw;
  std::vector<unsigned int> warnraw;

};

#endif
