
#ifndef __SVXRPEVENTQA_H__
#define __SVXRPEVENTQA_H__

#include <SubsysReco.h>
#include <vector>

class PHCompositeNode;
//class TH1;
class TProfile;
class Fun4AllHistoManager;

class SvxRpEventQA: public SubsysReco
{

 public:
  SvxRpEventQA(const std::string& filename = "SvxRpEventQA.root");
  virtual ~SvxRpEventQA();
  
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  
 private:
  int CreateNodeTree(PHCompositeNode *topNode);
  
  const static int ncent=5; //10
  const static int nzv  =5; //20;
  const static int ndet=76;
  const static int nhar=6;
  
  Fun4AllHistoManager *HistoManager;
  
  TProfile* avexy[ncent][nzv][ndet][2];
  TProfile* wei[ncent][nzv];

  std::string m_outFileName;

};


#endif
