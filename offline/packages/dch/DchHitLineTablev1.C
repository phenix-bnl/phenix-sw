#include <iostream>
#include "DchHitLineTablev1.hh"
//INCLUDECHECKER: Removed this line: #include "DchHitLineOutv1.hh"

#define INITIALSIZE 1
#define DCHHITSTEPSIZE 100

using namespace std;

ClassImp(DchHitLineTablev1)

DchHitLineTablev1::DchHitLineTablev1()
{
  nDCHits = 0;
  hitstat  = 0;
  DchHits = new TClonesArray("DchHitLineOutv1",INITIALSIZE);
  return ;
}
DchHitLineTablev1::DchHitLineTablev1(int fullsize)
{
  nDCHits = 0;
  DchHits = new TClonesArray("DchHitLineOutv1",fullsize);
  return ;
}

DchHitLineTablev1::~DchHitLineTablev1()
{
  if (DchHits)
    {
      DchHits->Clear();
      delete DchHits;
    }
  return ;
}

void DchHitLineTablev1::Clear(Option_t *option) 
{
  DchHits->Clear();
  nDCHits = 0;
  return;

}

void DchHitLineTablev1::Reset()
{
  Clear();
  return;
}

int DchHitLineTablev1::isValid() const
{
  return((nDCHits>0) ? 1 : 0);
}

int DchHitLineTablev1::Expand(int fullsize)
{
  DchHits->ExpandCreate(fullsize);
  return 1;
}

void DchHitLineTablev1::identify(ostream& out) const
{
  out << "identify yourself: I am a DchHitLineTablev1 object" << endl;
  return;
}

int DchHitLineTablev1::AddHit(DchHitLineOut* hit)
{
  if(!dynamic_cast <DchHitLineOutv1*>(hit)){
    cout<<" hit is not of DchHitLineOutv1 type"<<endl;
    return 1;
  }

  if(nDCHits<DchHits->GetSize()){
    TClonesArray &newhit = *DchHits;
    new (newhit[nDCHits]) DchHitLineOutv1(hit); 
    nDCHits++;
  }else{
    int size = DchHits->GetSize() + DCHHITSTEPSIZE;
    DchHits->Expand(size);
    TClonesArray &newhit = *DchHits;
    new (newhit[nDCHits]) DchHitLineOutv1(hit); 
    nDCHits++;
  }
  return nDCHits;
}
   
DchHitLineOut* DchHitLineTablev1::getHit(int i) 
{
  if (i <DchHits->GetSize()&& i < nDCHits) {
    return (DchHitLineOut*)DchHits->UncheckedAt(i);
  }else {
    cout << "Element " << i << " does not exist !! " << endl;
    cout << "nDCHits " << nDCHits;
    cout << "size " << DchHits->GetSize() << endl;
    return 0;
  }
}

short DchHitLineTablev1::getAssociatedCandidate(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getAssociatedCandidate();
  return -1;
}

short DchHitLineTablev1::getUsed(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getUsed();
  return 0;
}
int DchHitLineTablev1::getId(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getId();
  return -1;
}
int DchHitLineTablev1::getIdmirror(int i) 
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getIdmirror();
  return -100;
}	   
short DchHitLineTablev1::getArm(int i)	
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getArm();
  return 0;
}				
short DchHitLineTablev1::getPlane(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getPlane();
  return -1;
}
short DchHitLineTablev1::getCell(int i)					
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getCell(); 
  return -1;
}
short DchHitLineTablev1::getSide(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getSide();
  return -1;
}
float DchHitLineTablev1::getDistance(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getDistance();
  return -1;
}
float DchHitLineTablev1::getWidth(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getWidth();
  return -1;
}
short DchHitLineTablev1::getIdraw1(int i)
{ 
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getIdraw1();
  return -1;
}
short DchHitLineTablev1::getIdraw2(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getIdraw2();
  return -1;
}
int DchHitLineTablev1::getTime1(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getTime1();
  return -1;
}
int DchHitLineTablev1::getTime2(int i)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getTime2();
  return -1;
}
PHPoint DchHitLineTablev1::getXYZ(int i)
{
  PHPoint null(-1000,-1000,-1000);
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getXYZ();
  return null;
}
PHPoint DchHitLineTablev1::getEXYZ(int i)
{
  PHPoint null(-1000,-1000,-1000);
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getEXYZ();
  return null;  
}
PHVector DchHitLineTablev1::getVXYZ(int i)
{
  PHVector null(-1000,-1000,-1000);
  DchHitLineOut* hit = getHit(i);
  if(hit) return hit->getVXYZ();
  return null;  
}

//-----------
void DchHitLineTablev1::setAssociatedCandidate(int i, short val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setAssociatedCandidate(val);
}
void  DchHitLineTablev1::setUsed(int i, short val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setUsed(val);
}
void DchHitLineTablev1::setId(int i,int val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setId(val);
}
void DchHitLineTablev1::setIdmirror(int i, int val) 
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setIdmirror(val);
}	   
void DchHitLineTablev1::setArm(int i, short val)	
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setArm(val);
}				
void DchHitLineTablev1::setPlane(int i,short val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)  hit->setPlane(val);
}
void DchHitLineTablev1::setCell(int i, short val)	
{
  DchHitLineOut* hit = getHit(i);
  if(hit)  hit->setCell(val); 
}
void DchHitLineTablev1::setSide(int i,short val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)  hit->setSide(val);
}
void DchHitLineTablev1::setDistance(int i, float val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setDistance(val);
}
void DchHitLineTablev1::setWidth(int i, float val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setWidth(val);
}
void DchHitLineTablev1::setIdraw1(int i, short val)
{ 
  DchHitLineOut* hit = getHit(i);
  if(hit)  hit->setIdraw1(val);
}
void DchHitLineTablev1::setIdraw2(int i, short val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setIdraw2(val);
}
void  DchHitLineTablev1::setTime1(int i, int val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setTime1(val);
}
void  DchHitLineTablev1::setTime2(int i, int val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)hit->setTime2(val);
}
void DchHitLineTablev1::setXYZ(int i, PHPoint val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)hit->setXYZ(val);
}
void DchHitLineTablev1::setEXYZ(int i,PHPoint val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit) hit->setEXYZ(val);
}
void DchHitLineTablev1::setVXYZ(int i,PHVector val)
{
  DchHitLineOut* hit = getHit(i);
  if(hit)hit->setVXYZ(val);
}
