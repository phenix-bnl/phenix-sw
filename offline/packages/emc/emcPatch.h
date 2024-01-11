#ifndef __emcPatch_h__
#define __emcPatch_h__

#include <SubsysReco.h>

/** Test module to build, from RDO, MDO and CDO, the new
    emcTowerContainer object. */

class emcPatch : public SubsysReco
{
public:
  emcPatch();
  virtual ~emcPatch();

  int process_event(PHCompositeNode *);
};
#endif
