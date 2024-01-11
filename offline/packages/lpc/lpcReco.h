#ifndef __LPCRECO_H__
#define __LPCRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class lpcEvent;

class lpcReco: public SubsysReco
{
 public:
  lpcReco(const std::string &name = "LPC");
  virtual ~lpcReco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  lpcEvent *mlpcEvent;
};

#endif /* __LPCRECO_H__ */
