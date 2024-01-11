#ifndef __V24MAKER_H__
#define __V24MAKER_H__

#include "SubsysReco.h"

class PHCompositeNode;

class V24maker: public SubsysReco
{
 public:
  V24maker(const char* aName, const char* bName);
  virtual ~V24maker();

  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset        (PHCompositeNode *topNode) {return 0;}
  int ResetEvent   (PHCompositeNode *topNode);

  void Print(const std::string& ="") const {}

 protected:

  std::string InputName;
  std::string OutputName;

};

#endif /* __V24MAKER_H__ */




