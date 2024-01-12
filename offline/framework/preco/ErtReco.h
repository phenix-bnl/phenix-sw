#ifndef __ERTRECO_H__
#define __ERTRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class EMCalRichDecode;

class ErtReco: public SubsysReco
{
 public:
  ErtReco(const std::string &name = "ERT");
  virtual ~ErtReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  EMCalRichDecode* ERTdecode;

};

#endif /* __ERTRECO_H__ */
