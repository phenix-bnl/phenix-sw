#ifndef __SVXCGLRECO_H__
#define __SVXCGLRECO_H__

#include <SubsysReco.h>
#include <vector>


class PHCompositeNode;

class SvxCglReco : public SubsysReco
{
  public:
    SvxCglReco(){};
    ~SvxCglReco(){}
    
    int Init(PHCompositeNode *topNode){return 0;}
    int InitRun(PHCompositeNode *topNode){return 0;}
    int process_event(PHCompositeNode *topNode);
    int End(PHCompositeNode *topNode){return 0;}
    
  private:
    std::vector<int> org[64][64];
    std::vector<float> segpt;
    std::vector<float> segphi;
    std::vector<float> segtheta;
};


#endif
