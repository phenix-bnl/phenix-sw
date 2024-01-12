#ifndef TRIGSIMRECO_H__
#define TRIGSIMRECO_H__

#include <preco/TrigReco.h>

class PHCompositeNode;

class TrigSimreco: public TrigReco
{
 public:
  TrigSimreco(const char *name = "TRIG");
  virtual ~TrigSimreco() {}

  int process_event(PHCompositeNode *topNode);

 protected:

};

#endif /* __TRIGSIMRECO_H__ */
