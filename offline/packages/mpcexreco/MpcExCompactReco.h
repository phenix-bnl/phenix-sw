#ifndef MPCEXCOMPACTRECO_H__
#define MPCEXCOMPACTRECO_H__

#include <SubsysReco.h>
#include <string>
#include <vector>

class PHCompositeNode;


class MpcExCompactReco: public SubsysReco
{
 public:
  MpcExCompactReco(const std::string &name = "MpcExCompactReco");
  virtual ~MpcExCompactReco() {}

  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:
  std::vector<unsigned int> safechannels;
  std::vector<unsigned short> stacks;
  std::vector<unsigned short> statephases;
  std::vector<unsigned short> cellids;
  std::vector<unsigned short> parsttimes;
  unsigned short stack;
  unsigned short statephase;
  unsigned short cellid;
  unsigned short parsttime; 
  int nevent;  
};

#endif /* MPCEXCOMPACTRECO_H__ */
