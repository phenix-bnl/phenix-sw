#ifndef __SVXERECAL_H__
#define __SVXERECAL_H__

#include "SubsysReco.h"


class PHCompositeNode;
class SvxCentralTrack;
class SvxClusterList;
class TMCParticle;
class TRandom3;
class TH1F;
class TFile;

class SvxERecal: public SubsysReco
{
 public:
  SvxERecal(const std::string &name = "SvxERecal");
  virtual ~SvxERecal();
  
 int Init(PHCompositeNode *topNode);
 int InitRun(PHCompositeNode *topNode);
 int process_event(PHCompositeNode *topNode);
 int End(PHCompositeNode *topNode);



 void set_redotracking(bool flag) { d_redotracking = flag;}


 protected:


 private:

 bool d_redotracking;

};


#endif	/* __SMEARING_H__ */
