#ifndef __mixDst_hh
#define __mixDst_hh
//=============================================================================
//==================================================================

#include "Dst.hh"

class MixedDst :  public Dst{
public:
  MixedDst(){/* nothing !!!   */};
  MixedDst(char* name,char* title): Dst(name,title){ };
  void Show();
protected:
  virtual void Init(PHCompositeNode*,const PHAccessType);
  void InitRead(PHCompositeNode*);
  void InitWrite(PHCompositeNode*);
  virtual int getData();
  int mountData();
public:
  dEmcCalibTowerWrapper* dEmcCalibTowermix1;
  dEmcClusterLocalWrapper* dEmcClusterLocalmix1;
  dEmcClusterLocalExtWrapper* dEmcClusterLocalExtmix1;
  dEmcCalibTowerWrapper* dEmcCalibTowermix2;
  dEmcClusterLocalWrapper* dEmcClusterLocalmix2;
  dEmcClusterLocalExtWrapper* dEmcClusterLocalExtmix2;
protected:
  PHIODataNode<PHTable>* dEmcCalibTowerNodemix1;
  PHIODataNode<PHTable>* dEmcClusterLocalNodemix1;
  PHIODataNode<PHTable>* dEmcClusterLocalExtNodemix1;
  PHIODataNode<PHTable>* dEmcCalibTowerNodemix2;
  PHIODataNode<PHTable>* dEmcClusterLocalNodemix2;
  PHIODataNode<PHTable>* dEmcClusterLocalExtNodemix2;
};
//=============================================================================
#endif
//=============================================================================
