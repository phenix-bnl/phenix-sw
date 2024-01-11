#ifndef __LPOLRECO_H__
#define __LPOLRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class LPolEvent;

class LPolReco: public SubsysReco
{
 public:
  LPolReco(const std::string &name = "LPOL");
  virtual ~LPolReco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  LPolEvent *mlpolEvent;
};

#endif /* __LPOLRECO_H__ */
