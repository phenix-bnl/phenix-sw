#ifndef __MPCSCALE_H__
#define __MPCSCALE_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;
class MpcEvent;
class MpcMap;
class mpcTowerContainer;

class MpcScale: public SubsysReco
{
public:
  MpcScale(const std::string &name = "MPCSCALE", float scaleS=1.0, float scaleN = 1.0);
  virtual ~MpcScale();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  void print(mpcTowerContainer *twr = 0);

  int SetScale(float scale, int arm);  
  
 private:
  
  MpcMap *mpcmap;
  float fScale[2];
  

};

#endif /* __MPCSCALE_H__ */

