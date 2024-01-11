#ifndef __MUTRGEMULATEHIT__
#define __MUTRGEMULATEHIT__

#include <string>

class PHCompositeNode;
class MutrgHitArray;

class MutrgEmulateHit{
public:
  MutrgEmulateHit(void);
  virtual ~MutrgEmulateHit(void){;}

  int Init(PHCompositeNode *top_node);
  int InitRun(PHCompositeNode *top_node);
  int ProcessEvent(PHCompositeNode *top_node);

  const char *ClassName(void){return class_name.c_str();}

protected:
  std::string class_name;

  MutrgHitArray *mutrg_hits;
};

#endif /* __MUTRGEMULATEHIT__ */
