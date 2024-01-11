#include "DchRawOutv1.hh"
#include "DchRawTablev1.hh"

#include "phool.h"

#include "TClonesArray.h"

#include <iostream>

static const int INITIALSIZE = 0;
static const int DCHRAWSTEPSIZE = 100;

ClassImp(DchRawTablev1)

using namespace std;

DchRawTablev1::DchRawTablev1()
{
  entries = 0;
  DchRaws = new TClonesArray("DchRawOutv1",INITIALSIZE);
}

DchRawTablev1::DchRawTablev1(int fullsize)
{
  entries = 0;
  DchRaws = new TClonesArray("DchRawOutv1",fullsize);
}

DchRawTablev1::~DchRawTablev1()
{
  if (DchRaws)
    {
      DchRaws->Clear();
      delete DchRaws;
    }
}

int
DchRawTablev1::Size()
{
  return DchRaws->GetSize();
}

void DchRawTablev1::identify(ostream& out) const
{
  out << "identify yourself: I am a DchRawTablev1 object" << endl;
}

int 
DchRawTablev1::AddRaw(DchRawOutv1* raw)
{
  if (entries >= DchRaws->GetSize())
    {
      int size = DchRaws->GetSize() + DCHRAWSTEPSIZE;
      DchRaws->Expand(size);
    }
  TClonesArray &newraw = *DchRaws;
  new (newraw[entries]) DchRawOutv1(raw);
  entries++;
  return entries;
}

DchRawOutv1*
DchRawTablev1::getRaw(int i)
{
  if (i < DchRaws->GetSize() && i < entries)
    {
      return (DchRawOutv1*)(DchRaws->UncheckedAt(i));
    }
  else
    {
      cout << PHWHERE << "Index " << i << " out of range!"
	   << " Maximum allowed value: " << entries;
      return 0;
    }
}

void 
DchRawTablev1::Clear(Option_t *option)
{
  DchRaws->Clear();
  DchRaws->Expand(0);
  entries = 0;
}

void 
DchRawTablev1::Reset()
{
  Clear();
}

int DchRawTablev1::Expand(int fullsize)
{
  DchRaws->Expand(fullsize);
  return 1;
}

int DchRawTablev1::getGlobal(int i)
{
  return getRaw(i)->getGlobal();
}

short DchRawTablev1::getId(int i)
{
  return getRaw(i)->getId();
}

short DchRawTablev1::getArm(int i)	
{
  return getRaw(i)->getArm();
}				

short DchRawTablev1::getPlane(int i)					
{
  return getRaw(i)->getPlane();
}

short DchRawTablev1::getCell(int i)					
{
  return getRaw(i)->getCell(); 
}

short DchRawTablev1::getSide(int i)
{
  return getRaw(i)->getSide();
}

int DchRawTablev1::getTime(int i)
{
 return getRaw(i)->getTime();
}

short DchRawTablev1::getEdge(int i)
{
 return getRaw(i)->getEdge();
}

void DchRawTablev1::setGlobal(int i, int val)
{
  getRaw(i)->setGlobal(val);
}

void DchRawTablev1::setId(int i, short val)
{
  getRaw(i)->setId(val);
}

void DchRawTablev1::setArm(int i, short val)	
{
   getRaw(i)->setArm(val);
}				

void DchRawTablev1::setPlane(int i,short val)					
{
   getRaw(i)->setPlane(val);
}

void DchRawTablev1::setCell(int i, short val)					
{
   getRaw(i)->setCell(val); 
}

void DchRawTablev1::setSide(int i,short val)
{
  getRaw(i)->setSide(val);
}

void  DchRawTablev1::setTime(int i, int val)
{
  getRaw(i)->setTime(val);
}

void DchRawTablev1::setEdge(int i, short val)
{
  getRaw(i)->setEdge(val);
}

