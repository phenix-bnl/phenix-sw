#ifndef __HEADSIMRECO_H__
#define __HEADSIMRECO_H__

#include <preco/HeadReco.h>

#include <ctime>
#include <string>

class PHCompositeNode;

class HeadSimreco: public HeadReco
{
 public:
  HeadSimreco(const std::string &name = "HEAD");
  virtual ~HeadSimreco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
 
  int eventsequence;
  time_t startrunticks;
};

#endif /* __HEADSIMRECO_H__ */
