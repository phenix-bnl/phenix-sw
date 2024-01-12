#ifndef PdbCalFlagSaveReco_H__
#define PdbCalFlagSaveReco_H__

#include <SubsysReco.h>

#include <string>

class PHCompositeNode;
class RawDataCheck;

class PdbCalFlagSaveReco: public SubsysReco
{
 public:
  PdbCalFlagSaveReco(const std::string &name = "HEAD");
  virtual ~PdbCalFlagSaveReco() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  void Print(const std::string&) const {}
  int EndRun(const int runno);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

};

#endif /* PdbCalFlagSaveReco_H__ */
