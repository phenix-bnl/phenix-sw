#ifndef __mEmcPutDCMLongModule_h__
#define __mEmcPutDCMLongModule_h__

#include <SubsysReco.h>

/** Legacy tiny "wrapper" to the EmcPutDCMLong() function, so it offers a plain SubsysReco interface. */

class mEmcPutDCMLongModule : public SubsysReco
{
public:
  mEmcPutDCMLongModule();
  virtual ~mEmcPutDCMLongModule(){}

  int process_event(PHCompositeNode*);
};

#endif
