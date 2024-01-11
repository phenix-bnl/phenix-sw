#include "HbdHitMap.h"
#include <iostream>

ClassImp(HbdHitMap)

using namespace std;


HbdHitMap::HbdHitMap()
{
  Nentries = 0;
}

void
HbdHitMap::identify(ostream& os) const
{
  os << "identify yourself: HbdHitMap Object";
  os << " entries: " << Nentries << endl;
  for(int i=0;i<Nentries;i++)
    {
      os << "  Entry " << i << "  ";
      hbdhitmap[i].identify(os);
    }
  return;
}

void
HbdHitMap::AddHit(const short int index, const HbdHitMapEntry &hit)
{
  hbdhitmap[index] = hit;
  Nentries++;
  return;
}

void
HbdHitMap::Reset()
{
  //hbdhitmap.clear();
  Nentries = 0;
  return;
}

const HbdHitMapEntry *
HbdHitMap::get_cell(const short int index) const
{
  return &hbdhitmap[index];
}

int HbdHitMap::get_nCells() const
{
  return Nentries;
}
