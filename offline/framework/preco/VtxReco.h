#ifndef VTXRECO_H__
#define VTXRECO_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;

class VtxReco: public SubsysReco
{
 public:
  VtxReco(const std::string &name = "VTX");
  virtual ~VtxReco() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

};

#endif /* __VTXRECO_H__ */
