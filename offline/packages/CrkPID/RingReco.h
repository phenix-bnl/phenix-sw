#ifndef __RINGRECO_H__
#define __RINGRECO_H__

#include <SubsysReco.h>
#include <PHLine.h>
//#include <fstream>

class PHCompositeNode;
class CrkPID;

class RingReco: public SubsysReco
{
 public:
  RingReco(const std::string &name = "RING");
  virtual ~RingReco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  PHLine ReflectInZ(const PHLine &trk);
  CrkPID *crkpid;
  // std::ofstream dumpfile;
};

#endif /* __RINGRECO_H__ */
