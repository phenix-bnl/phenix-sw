#ifndef __FCLRECAL__
#define __FCLRECAL__

#include "SubsysReco.h"


class PHCompositeNode;
class FclCalib;
class FclOut;

class FclRecal : public SubsysReco
{
public:
  FclRecal(const char *name ="FCLRECAL");
  virtual ~FclRecal();
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  void Print(const std::string& ="") const {}

  int CreateNodeTree(PHCompositeNode *topNode);
  void calibrateData(FclOut* fclO, float zdce);
  FclCalib* southCal;
  FclCalib* northCal;

};

#endif
