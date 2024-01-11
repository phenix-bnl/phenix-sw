#ifndef __mEmcGetDCMModule_h__
#define __mEmcGetDCMModule_h__

#include <SubsysReco.h>

/** Legacy tiny "wrapper" to the EmcGetDCM() function, so it offers a plain SubsysReco interface. */

class mEmcGetDCMModule : public SubsysReco
{

public:
  mEmcGetDCMModule();
  virtual ~mEmcGetDCMModule(){}

  int process_event(PHCompositeNode*);
};
#endif
