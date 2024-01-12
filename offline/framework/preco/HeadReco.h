#ifndef __HEADRECO_H__
#define __HEADRECO_H__

#include <SubsysReco.h>

#include <string>

class PHCompositeNode;
class RawDataCheck;

class HeadReco: public SubsysReco
{
 public:
  HeadReco(const std::string &name = "HEAD");
  virtual ~HeadReco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  void Print(const std::string&) const {}
  int EndRun(const int runno);
  void SetRawDataCheck(RawDataCheck *raw) {chk = raw;}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  RawDataCheck *chk;

};

#endif /* __HEADRECO_H__ */
