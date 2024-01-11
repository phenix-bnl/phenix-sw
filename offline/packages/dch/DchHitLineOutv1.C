#include <iostream>
#include "DchHitLineOutv1.hh"

ClassImp(DchHitLineOutv1);

using namespace std;

void DchHitLineOutv1::Init(){
    xyz_x =  xyz_y =  xyz_z = 0;
  exyz_x = exyz_y = exyz_z = 0;
  vxyz_x = vxyz_y = vxyz_z = 0;

  id = -1;
  idmirror = -1;
  plane = -999;
  cell = -999;
  side = -999;
  arm = -999;
  associatedCandidate = -1;
  used = -1;
  distance = -999;
  width = -999;
  idraw1 = -1;
  idraw2 = -1;
  time1 = -999;
  time2 = -999;
}
DchHitLineOutv1::DchHitLineOutv1()
{
  Init();
}

DchHitLineOutv1::DchHitLineOutv1(DchHitLineOut*hit){
  if(!dynamic_cast < DchHitLineOutv1*>(hit)){
    cout<<" hit is not of DchHitLineOutv1 type"<<endl;
    Init();
    return;
  }
  used         = hit->getUsed();
  id           = hit->getId();
  idmirror     = hit->getIdmirror();
  arm          = hit->getArm();
  side         = hit->getSide();
  plane        = hit->getPlane();
  cell         = hit->getCell();
  associatedCandidate = hit->getAssociatedCandidate();
  distance     = hit->getDistance();
  width        = hit->getWidth();
  idraw1       = hit->getIdraw1();
  idraw2       = hit->getIdraw2();
  time1        = hit->getTime1();
  time2        = hit->getTime2();
  PHPoint p;
  p            = hit->getXYZ();
  xyz_x        = p.getX();
  xyz_y        = p.getY();
  xyz_z        = p.getZ();
  p            = hit->getEXYZ();
  exyz_x       = p.getX();
  exyz_y       = p.getY();
  exyz_z       = p.getZ();
  PHVector v;
  v            = hit->getVXYZ();
  vxyz_x       = v.getX();
  vxyz_y       = v.getY();
  vxyz_z       = v.getZ();
}
void DchHitLineOutv1::print(){
  cout<<id<<" "<<" "<<distance<<" "<<time1<<endl;
}
