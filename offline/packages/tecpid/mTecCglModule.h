#ifndef __MTECCGLMODULE_H__
#define __MTECCGLMODULE_H__

#include <phool.h>

class PHCompositeNode;

/**
This module makes the Tec Cluster - DCh tracks association
and fill up the CglTrack bank
*/

class mTecCglModule
{
public:
///
  mTecCglModule();
///
  virtual ~mTecCglModule();
///
  PHBoolean event(PHCompositeNode*);

///  set the maximum distance between the Tec Cluster and DCH track
  void set_min_distance_cut(float a) {min_distance_cut = a;}

private:
  float min_distance_cut;
};
#endif /*__MTECCGLMODULE_H__*/

