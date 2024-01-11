#ifndef __TFWSIMRECO_H__
#define __TFWSIMRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class tfwghitWrapper;
class TofwSimEvent;
class TofwRaw;
class TofwHit;
class TofwGeometry;

class TfwSimreco: public SubsysReco
{
 public:
  TfwSimreco(const std::string &name = "TFWSIMRECO");
  virtual ~TfwSimreco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:

  tfwghitWrapper         *tfwghit;

  TofwSimEvent *d_tofw;
  TofwRaw      *d_raw;
  TofwHit      *d_hit;
  TofwGeometry *d_geom;
};

#endif /* __TFWSIMRECO_H__ */
