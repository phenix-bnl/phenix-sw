#ifndef __mEmcPutDCMModule_h__
#define __mEmcPutDCMModule_h__

#include <SubsysReco.h>

/** Legacy tiny "wrapper" to the EmcPutDCM() function, so it offers a plain SubsysReco interface. */

class mEmcPutDCMModule : public SubsysReco
{
public:
  mEmcPutDCMModule();
  virtual ~mEmcPutDCMModule(){}

  int process_event(PHCompositeNode*);
};

#endif
