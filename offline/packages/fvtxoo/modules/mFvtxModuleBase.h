
#ifndef __MFVTXMODULEBASE_H__
#define __MFVTXMODULEBASE_H__

// Abstract interface for mutoo framework modules
#include <phool.h>

class PHCompositeNode;

class mFvtxModuleBase 
{

public:
  virtual ~mFvtxModuleBase() {}
  virtual void init(PHCompositeNode* top_node) = 0;
  virtual void init_run(PHCompositeNode* top_node) = 0;
  virtual PHBoolean event(PHCompositeNode* top_node) = 0;
  virtual void end(PHCompositeNode* top_node) = 0;

};

#endif // __MFVTXMODULEBASE_H__
