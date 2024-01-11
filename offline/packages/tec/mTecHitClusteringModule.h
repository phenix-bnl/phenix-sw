#ifndef __MTECHITCLUSTERINGMODULE_H__
#define __MTECHITCLUSTERINGMODULE_H__


class PHCompositeNode;
class TecClusterV1;

class mTecHitClusteringModule
{

public:
///
  mTecHitClusteringModule();
///
  virtual ~mTecHitClusteringModule();
///
  int event(PHCompositeNode*);
  void set_min_ntimebins(int a) {min_ntimebins = a;}
private:

TecClusterV1* teccluster;
 int  min_ntimebins;
};

#endif /*__MTECHITCLUSTERINGMODULE_H__*/
