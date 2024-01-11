#include "DchRawOutv1.hh"
ClassImp(DchRawOutv1)
void DchRawOutv1::Init(){
  global = -1;
  id = -1;
  arm = -1;
  plane = -1;
  cell = -1;
  side = -1;
  edge = -1;  
  time = -999; 
}

DchRawOutv1::DchRawOutv1() 
{
  Init();
}
DchRawOutv1::DchRawOutv1(DchRawOutv1*raw) 
{
  Init();
  if(!raw)return;
  global = raw->getGlobal();
  id     = raw->getId();
  arm    = raw->getArm();
  plane  = raw->getPlane();
  cell   = raw->getCell();
  side   = raw->getSide();
  edge   = raw->getEdge();
  time   = raw->getTime();
}

DchRawOutv1::~DchRawOutv1() {}




