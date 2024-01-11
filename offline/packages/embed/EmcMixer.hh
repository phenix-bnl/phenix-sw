#ifndef __EmcMixer_HH__
#define __EmcMixer_HH__

class PHCompositeNode;
class PHEmbedStat;
class EmcMixer{
 public:
  EmcMixer(){}
  virtual ~EmcMixer(){}
  int InitRun(PHCompositeNode* sngl,PHCompositeNode* real,PHCompositeNode* merged);
  int merge();

  int       getVerbose()       {return verbose;}
  void      setVerbose(int val){verbose = val;}
 protected:
  int verbose;
  //should have a class which tells the current MC particle index.
  PHCompositeNode *node1;  //single particle node
  PHCompositeNode *node2;  //nodes contains  DST
  PHCompositeNode *node3;  //nodes contains merged table information

  PHEmbedStat* embedStat;//status of the embeded hit
};
#endif
