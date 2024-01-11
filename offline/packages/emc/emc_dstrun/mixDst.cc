#ifndef __mixDst_cc
#define __mixDst_cc
//==================================================================
#include "mixDst.hh"

//==================================================================
void MixedDst::InitWrite(PHCompositeNode* dstNode){
  
  Dst::InitWrite(dstNode);
  int mr;
  mr=6000;
  dEmcClusterLocalExtmix1 = new dEmcClusterLocalExtWrapper("dEmcClusterLocalExtmix1",mr);
  dEmcClusterLocalExtNodemix1 = new PHIODataNode<PHTable>(dEmcClusterLocalExtmix1,"dEmcClusterLocalExtmix1");
  dstNode->addNode(dEmcClusterLocalExtNodemix1);

  mr=6000;
  dEmcClusterLocalmix1 = new dEmcClusterLocalWrapper("dEmcClusterLocalmix1",mr);
  dEmcClusterLocalNodemix1 = new PHIODataNode<PHTable>(dEmcClusterLocalmix1,"dEmcClusterLocalmix1");
  dstNode->addNode(dEmcClusterLocalNodemix1);

  mr=10000;
  dEmcCalibTowermix1 = new dEmcCalibTowerWrapper("dEmcCalibTowermix1",mr);
  dEmcCalibTowerNodemix1 = new PHIODataNode<PHTable>(dEmcCalibTowermix1,"dEmcCalibTowermix1");
  dstNode->addNode(dEmcCalibTowerNodemix1);

  mr=6000;
  dEmcClusterLocalExtmix2 = new dEmcClusterLocalExtWrapper("dEmcClusterLocalExtmix2",mr);
  dEmcClusterLocalExtNodemix2 = new PHIODataNode<PHTable>(dEmcClusterLocalExtmix2,"dEmcClusterLocalExtmix2");
  dstNode->addNode(dEmcClusterLocalExtNodemix2);

  mr=6000;
  dEmcClusterLocalmix2 = new dEmcClusterLocalWrapper("dEmcClusterLocalmix2",mr);
  dEmcClusterLocalNodemix2 = new PHIODataNode<PHTable>(dEmcClusterLocalmix2,"dEmcClusterLocalmix2");
  dstNode->addNode(dEmcClusterLocalNodemix2);

  mr=10000;
  dEmcCalibTowermix2 = new dEmcCalibTowerWrapper("dEmcCalibTowermix2",mr);
  dEmcCalibTowerNodemix2 = new PHIODataNode<PHTable>(dEmcCalibTowermix2,"dEmcCalibTowermix2");
  dstNode->addNode(dEmcCalibTowerNodemix2);
}
//==================================================================
void MixedDst::Init(PHCompositeNode* dstNode,const PHAccessType type){
  if( type == PHReadOnly )
    InitRead(dstNode);
  if( type == PHWrite )
    InitWrite(dstNode);
};
//==================================================================
void MixedDst::InitRead(PHCompositeNode* dstnode){
  Dst::InitRead(dstnode);
  PHNodeIterator mainIter(dstnode);
  dEmcCalibTowerNodemix1 = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcCalibTowermix1");
  dEmcClusterLocalNodemix1 = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcClusterLocalmix1");
  dEmcClusterLocalExtNodemix1 = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcClusterLocalExtmix1");
  dEmcCalibTowerNodemix2 = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcCalibTowermix2");
  dEmcClusterLocalNodemix2 = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcClusterLocalmix2");
  dEmcClusterLocalExtNodemix2 = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcClusterLocalExtmix2");

};
//==================================================================
int MixedDst::getData(){
  return MixedDst::mountData();
};
//==================================================================
int MixedDst::mountData(){
  int _readerr = 0;
  _readerr = Dst::mountData();
  if (!dEmcCalibTowerNodemix1) {
    _readerr ++;
  } else {
    dEmcCalibTowermix1 = (dEmcCalibTowerWrapper*)dEmcCalibTowerNodemix1->getData();
  }
  if (!dEmcClusterLocalNodemix1) {
    _readerr ++;
  } else {
    dEmcClusterLocalmix1 = (dEmcClusterLocalWrapper*)dEmcClusterLocalNodemix1->getData();
  }
  if (!dEmcClusterLocalExtNodemix1) {
    _readerr ++;
  } else {
    dEmcClusterLocalExtmix1 = (dEmcClusterLocalExtWrapper*)dEmcClusterLocalExtNodemix1->getData();
  }
  if (!dEmcCalibTowerNodemix2) {
    _readerr ++;
  } else {
    dEmcCalibTowermix2 = (dEmcCalibTowerWrapper*)dEmcCalibTowerNodemix2->getData();
  }
  if (!dEmcClusterLocalNodemix2) {
    _readerr ++;
  } else {
    dEmcClusterLocalmix2 = (dEmcClusterLocalWrapper*)dEmcClusterLocalNodemix2->getData();
  }
  if (!dEmcClusterLocalExtNodemix2) {
    _readerr ++;
  } else {
    dEmcClusterLocalExtmix2 = (dEmcClusterLocalExtWrapper*)dEmcClusterLocalExtNodemix2->getData();
  }

  return _readerr;
};
//==================================================================
void MixedDst::Show(){
  cout<<" MixedDst::Show"<<endl;
  cout<<"\t|dEmcCalibTower \t";
  {
    if( dEmcCalibTower )
      cout<<dEmcCalibTower->RowCount();
    cout<<"\t";
    if( dEmcCalibTowermix1 )
      cout<<dEmcCalibTowermix1->RowCount();
    cout<<"\t";
    if( dEmcCalibTowermix2 )
      cout<<dEmcCalibTowermix2->RowCount();
    cout<<endl;
  }
  cout<<"\t|dEmcClusterLocal \t";
  {
    if( dEmcClusterLocal )
      cout<<dEmcClusterLocal->RowCount();
    cout<<"\t";
    if( dEmcClusterLocalmix1 )
      cout<<dEmcClusterLocalmix1->RowCount();
    cout<<"\t";
    if( dEmcClusterLocalmix2 )
      cout<<dEmcClusterLocalmix2->RowCount();
    cout<<endl;
  }
  cout<<"\t|dEmcClusterLocalExt \t";
  {
    if( dEmcClusterLocalExt )
      cout<<dEmcClusterLocalExt->RowCount();
    cout<<"\t";
    if( dEmcClusterLocalExtmix1 )
      cout<<dEmcClusterLocalExtmix1->RowCount();
    cout<<"\t";
    if( dEmcClusterLocalExtmix2 )
      cout<<dEmcClusterLocalExtmix2->RowCount();
    cout<<endl;
  }


}
//==================================================================
//==================================================================
#endif
//==================================================================
//==================================================================
