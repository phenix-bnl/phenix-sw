#ifndef __HBDMINICELLRESET_H__
#define __HBDMINICELLRESET_H__

#include "SubsysReco.h"

class PHCompositeNode;

class HBDMiniCellReset: public SubsysReco
{
 public:
  HBDMiniCellReset();
  virtual ~HBDMiniCellReset();

  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset        (PHCompositeNode *topNode) {return 0;}
  int ResetEvent   (PHCompositeNode *topNode);

  void Print(const std::string&) const {}

 protected:

};

#endif /* __HBDMINICELLRESET_H__ */




