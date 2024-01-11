#ifndef __mEmcGeaGetHits_h__
#define __mEmcGeaGetHits_h__

#include <SubsysReco.h>

/** Legacy tiny "wrapper" to the EmcGetGEA function, so it offers a plain SubsysReco
    interface. */

class mEmcGeaGetHits : public SubsysReco
{
public:
  mEmcGeaGetHits();
  virtual ~mEmcGeaGetHits();

  int process_event(PHCompositeNode*);

};

#endif
