#ifndef __MPCEMBEDRECO_H__
#define __MPCEMBEDRECO_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;
class MpcEvent;
class MpcMap;
class mpcTowerContainer;

class MpcEmbedReco: public SubsysReco
{
public:
  MpcEmbedReco(const std::string &name = "MPCRECAL", const char* simname = "SIM");
  virtual ~MpcEmbedReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int EndRun(const int runno);
  int End(PHCompositeNode* =0) { return 0; }
  void print(mpcTowerContainer *twr = 0);
  void Verbosity(int verbosity){fVerbose = verbosity;}
  int SetScale(float scale){  
    if(scale > 0){fScale=scale; return 1;}
    return 0;
  }
  int CreateNodeTree(PHCompositeNode*);
  
  void SetNodesToMerge(const char *dest_node, const char *src1_node);

  int merge_mode;
  enum { MERGE_TREES = 0, MERGE_NODES };

private:
  const char* simNodename;
  std::string destination_NodeName;
  std::string src1_NodeName;
  int fVerbose;
  MpcMap *mpcmap;
  float fScale;
  

};

#endif /* __MPCEMBED_H__ */

