#ifndef __MUTRGSIMRECO__
#define __MUTRGSIMRECO__

#include "SubsysReco.h"

class MutrgEmulateHeader;
class MutrgEmulateHit;

class MutrgSimreco : public SubsysReco{
public:
  MutrgSimreco(const char *name="MutrgSimreco");
  virtual ~MutrgSimreco(void);

  virtual int Init(PHCompositeNode *top_node);
  virtual int InitRun(PHCompositeNode *top_node);
  virtual int process_event(PHCompositeNode *top_node);

  virtual void CreateModule(void);
  virtual void DoEmulateHeader(bool flag){flag_emu_header=flag;}
  virtual void DoEmulateHit(bool flag){flag_emu_hit=flag;}

protected:
  bool flag_emu_header;
  bool flag_emu_hit;

  MutrgEmulateHeader *mutrg_emu_header;
  MutrgEmulateHit *mutrg_emu_hit;
};

#endif /* __MUTRGSIMRECO__ */
