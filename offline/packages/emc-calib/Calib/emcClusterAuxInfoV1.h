#ifndef __EMCCLUSTERAUXINFOV1_H__
#define __EMCCLUSTERAUXINFOV1_H__

#include "emcClusterAuxInfo.h"

class emcClusterAuxInfoV1 : public emcClusterAuxInfo 
{
 public:
  emcClusterAuxInfoV1 ();
  emcClusterAuxInfoV1 ( const float c, const float e, const float pxc, const float pyc);
  float getLocalChi2() {return _chi2;};
  float getLocalEcore() {return _ecore;};
  float getLocalx( ) { return _x; }
  float getLocaly( ) { return _y; }

  void setLocalChi2( const float x ) { _chi2 = x;}
  void setLocalEcore( const float x ) { _ecore = x;}
  void setLocalpxc( const float x ) { _x = x;}
  void setLocalpyc( const float x ) { _y = x;}

 protected:

  float _chi2;
  float _ecore;
  float _x;
  float _y;


  ClassDef(emcClusterAuxInfoV1,1) // Array of emcTowerContentDST

};

#endif
