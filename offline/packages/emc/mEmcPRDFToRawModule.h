#ifndef __mEmcPRDFToRawModule_h__
#define __mEmcPRDFToRawModule_h__

#include <SubsysReco.h>

/** (STAF) Converts packet to dEmcRaw table. 
@ingroup staf
*/

class PHCompositeNode;

class mEmcPRDFToRawModule : public SubsysReco
{

public:
  mEmcPRDFToRawModule();
  virtual ~mEmcPRDFToRawModule(){}

  int process_event(PHCompositeNode*);
};

#endif
