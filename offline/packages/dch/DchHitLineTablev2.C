#include "DchHitLineTablev2.hh"
#include "DchHitLineOutv2.hh"

#include <TClonesArray.h>

#include <iostream>

#define INITIALSIZE 100
#define DCHHITSTEPSIZE 100

using namespace std;

ClassImp(DchHitLineTablev2)
DchHitLineTablev2::DchHitLineTablev2()
{
  nDCHits = 0;
  hitstat = 0;
  DchHits = new TClonesArray("DchHitLineOutv2",INITIALSIZE);
  return ;
}

DchHitLineTablev2::DchHitLineTablev2(int fullsize)
{
  nDCHits = 0;
  DchHits = new TClonesArray("DchHitLineOutv2",fullsize);
  return ;
}

DchHitLineTablev2::~DchHitLineTablev2()
{
  if (DchHits)
    {
      DchHits->Clear();
      delete DchHits;
    }
  return ;
}

void DchHitLineTablev2::Clear(Option_t *option) 
{
  DchHits->Clear();
  DchHits->Expand(0);
  nDCHits = 0;
  return;
}

void DchHitLineTablev2::Reset()
{
  Clear();
  return;
}

int DchHitLineTablev2::isValid() const
{
  return((nDCHits>0) ? 1 : 0);
}

int DchHitLineTablev2::Expand(int fullsize)
{
  DchHits->Expand(fullsize);
  return 1;
}

void DchHitLineTablev2::identify(ostream& out) const
{
  out << "identify yourself: I am a DchHitLineTablev2 object" << endl;
  return;
}

int DchHitLineTablev2::AddHit(DchHitLineOut* hit)
{
  if(nDCHits<DchHits->GetSize()){
    TClonesArray &newhit = *DchHits;
    new (newhit[nDCHits]) DchHitLineOutv2(hit); 
    nDCHits++;
  }else{
    int size = DchHits->GetSize() + DCHHITSTEPSIZE;
    DchHits->Expand(size);
    TClonesArray &newhit = *DchHits;
    new (newhit[nDCHits]) DchHitLineOutv2(hit); 
    nDCHits++;
  }
  return nDCHits;
}

DchHitLineOut* DchHitLineTablev2::getHit(int i) 
{
  if (i <DchHits->GetSize()&& i < nDCHits) {
    return (DchHitLineOut*)DchHits->UncheckedAt(i);
  }else {
    cout << "Element " << i << " does not exist !! " << endl;
    return 0;
  }
}
int DchHitLineTablev2::getId(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getId();
  return -1;
}
int DchHitLineTablev2::getIdmirror(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getIdmirror();
  return -100;
}
short DchHitLineTablev2::getArm(int i)	
{
  DchHitLineOut* hit = getHit(i);
  if(hit)return hit->getArm();
  return -1;
}				
short DchHitLineTablev2::getPlane(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)return hit->getPlane();
  return -1;
}
short DchHitLineTablev2::getCell(int i)					
{
  DchHitLineOut* hit = getHit(i);
  if(hit)return hit->getCell(); 
  return -1;
}
short DchHitLineTablev2::getSide(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)return hit->getSide();
  return -1;
}
float DchHitLineTablev2::getDistance(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)return hit->getDistance();
  return -1;
}
float DchHitLineTablev2::getWidth(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)return hit->getWidth();
  return -1;
}
int DchHitLineTablev2::getTime1(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)return hit->getTime1();
  return -1;
}
PHPoint DchHitLineTablev2::getXYZ(int i)
{
  PHPoint null(-1000,-1000,-1000);
  DchHitLineOut* hit = getHit(i);
  if(hit)return hit->getXYZ();
  return null;
}

void DchHitLineTablev2::setId(int i,int val)
{
  DchHitLineOut* hit = getHit(i);
  hit->setId(val);
}
void DchHitLineTablev2::setIdmirror(int i,int val)
{
  DchHitLineOut* hit = getHit(i);
  hit->setIdmirror(val);
}
void DchHitLineTablev2::setArm(int i, short val)	
{
  DchHitLineOut* hit = getHit(i);
  hit->setArm(val);
}				
void DchHitLineTablev2::setPlane(int i,short val)
{
  DchHitLineOut* hit = getHit(i);
  hit->setPlane(val);
}
void DchHitLineTablev2::setCell(int i, short val)
{
  DchHitLineOut* hit = getHit(i);
  hit->setCell(val); 
}
void DchHitLineTablev2::setSide(int i,short val)
{
  DchHitLineOut* hit = getHit(i);
  hit->setSide(val);
}
void DchHitLineTablev2::setDistance(int i, float val)
{
  DchHitLineOut* hit = getHit(i);
  hit->setDistance(val);
}
void DchHitLineTablev2::setWidth(int i, float val)
{
  DchHitLineOut* hit = getHit(i);
  hit->setWidth(val);
}
void  DchHitLineTablev2::setTime1(int i, int val)
{
  DchHitLineOut* hit = getHit(i);
  hit->setTime1(val);
}
void DchHitLineTablev2::setXYZ(int i, PHPoint val)
{
  DchHitLineOut* hit = getHit(i);
  hit->setXYZ(val);
}

//-----------
short DchHitLineTablev2::getAssociatedCandidate(int i)
{
  cout<<"I am short hit, don't call this function"<<endl;
  return -9999;
}
short DchHitLineTablev2::getUsed(int i)
{
  cout<<"I am short hit, don't call this function"<<endl;
  return -9999;
}
short DchHitLineTablev2::getIdraw1(int i)
{ 
  cout<<"I am short hit, don't call this function"<<endl;
  return -9999;
}
short DchHitLineTablev2::getIdraw2(int i)
{
  cout<<"I am short hit, don't call this function"<<endl;
  return -9999;
}
int DchHitLineTablev2::getTime2(int i)
{
  cout<<"I am short hit, don't call this function"<<endl;
  return -9999;
}
PHPoint DchHitLineTablev2::getEXYZ(int i)
{
  cout<<"I am short hit, don't call this function"<<endl;
  PHPoint null(-9999,-9999,-9999);
  return null;
}
PHVector DchHitLineTablev2::getVXYZ(int i)
{
  cout<<"I am short hit, don't call this function"<<endl;
  PHVector null(-9999,-9999,-9999);
  return null;
}

void DchHitLineTablev2::setAssociatedCandidate(int i, short val)
{  
  cout<<"I am short hit, don't call this function"<<endl;
}
void  DchHitLineTablev2::setUsed(int i, short val)
{
  cout<<"I am short hit, don't call this function"<<endl;
}

void DchHitLineTablev2::setIdraw1(int i, short val)
{ 
  cout<<"I am short hit, don't call this function"<<endl;
}
void DchHitLineTablev2::setIdraw2(int i, short val)
{
  cout<<"I am short hit, don't call this function"<<endl;
}
void  DchHitLineTablev2::setTime2(int i, int val)
{
  cout<<"I am short hit, don't call this function"<<endl;
}
void DchHitLineTablev2::setEXYZ(int i,PHPoint val)
{
  cout<<"I am short hit, don't call this function"<<endl;
}
void DchHitLineTablev2::setVXYZ(int i,PHVector val)
{
  cout<<"I am short hit, don't call this function"<<endl;
}





