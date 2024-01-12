#ifndef __TOFRECO_H__
#define __TOFRECO_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;
class TofEvent;
class TofAddressObject;
class TofCalibObject;
class TofGeometryObject;

class TofReco: public SubsysReco
{
 public:
  TofReco(const std::string &name="TOF");
  virtual ~TofReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:

  int CreateNodeTree(PHCompositeNode *topNode);
  int copyWrapper(PHCompositeNode *topNode);

  TofEvent*         tofevent;
  TofAddressObject* TofAddress;
  TofCalibObject* TofCalib;
  TofGeometryObject* TofGeometry;


};

#endif /* __TOFRECO_H__ */
