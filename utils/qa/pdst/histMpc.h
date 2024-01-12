#ifndef _MYHISTMPC_H
#define _MYHISTMPC_H
#include "SubsysReco.h"

class PHCompositeNode;

class QAMpc: public SubsysReco
{
 protected:
  std::string OutFileName;

 public:
  QAMpc(const char* name = "QAMpc", const char* outfn="NONE"): SubsysReco(name)
    {OutFileName = outfn;}
  virtual ~QAMpc() {}

  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

 

};

#endif /* _QAMPC_H */



