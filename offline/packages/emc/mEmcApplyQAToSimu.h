#ifndef __mEmcApplyQAToSimu_h__
#define __mEmcApplyQAToSimu_h__

#include <SubsysReco.h>

/** Module to update dead/warn status of simulated towers, from real Q&A. */

class mEmcApplyQAToSimu : public SubsysReco
{
public:
  mEmcApplyQAToSimu();
  virtual ~mEmcApplyQAToSimu();

  int process_event(PHCompositeNode*);
};
#endif
