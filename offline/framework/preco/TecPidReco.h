#ifndef __TECPIDRECO_H__
#define __TECPIDRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class mTecCglModule;

class TecPidReco: public SubsysReco {

public:
  TecPidReco(const std::string &name = "TECPID");
  virtual ~TecPidReco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

  mTecCglModule* get_mTecCgl() {return mTecCgl;}

 protected:
  mTecCglModule* mTecCgl;

};

#endif /* __TECPIDRECO_H__ */


