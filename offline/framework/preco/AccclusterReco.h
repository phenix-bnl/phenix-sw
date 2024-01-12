#ifndef __ACCCLUSTERRECO_H_
#define __ACCCLUSTERRECO_H_

#include <SubsysReco.h>

class AccProj;

class AccclusterReco : public SubsysReco
{
 public:
  AccclusterReco(const std::string &name = "AccclusterReco", const int ver=1);
  virtual ~AccclusterReco();

  int Init         (PHCompositeNode* topNode);
  int process_event(PHCompositeNode* topNode);
  int ResetEvent   (PHCompositeNode* topNode);

 protected:
  int version;

  AccProj* d_accproj;

};

#endif
