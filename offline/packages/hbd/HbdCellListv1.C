#include "HbdCellListv1.h"
#include "HbdCellv1.h"
#include <iostream>

ClassImp(HbdCellListv1)

using namespace std;

#define HBDNCELL 3000

HbdCellListv1::HbdCellListv1()
{
  nCells = 0;
  Cell = new TClonesArray("HbdCellv1",HBDNCELL);
}

HbdCellListv1::HbdCellListv1(const HbdCellListv1& rhs)
{
  Cell = 0;
  rhs.copyto(*this);
}

HbdCellListv1& 
HbdCellListv1::operator=(const HbdCellListv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
HbdCellListv1::copyto(HbdCellListv1& dest) const
{
  delete dest.Cell;
  dest.Cell = new TClonesArray("HbdCellv1",nCells);
  dest.nCells = nCells;
  for ( unsigned int i = 0; i < nCells; ++i ) 
    {
      HbdCellv1* src = static_cast<HbdCellv1*>
	(get_cell(i));
      if ( src ) 
	{
	  dest.AddCell(i);
	  HbdCellv1* d = static_cast<HbdCellv1*>
	    (dest.get_cell(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

HbdCellListv1* 
HbdCellListv1::clone() const
{
  return new HbdCellListv1(*this);
}

HbdCellListv1::~HbdCellListv1()
{
  Cell->Clear();
  delete Cell;
  return;
}

HbdCellv1* HbdCellListv1::get_cell (const unsigned int icell) const {
  HbdCellv1 *Particle = (HbdCellv1 *) GetCell()->UncheckedAt(icell);
  return Particle;
}

void HbdCellListv1::identify(ostream& os) const
{
  os << "identify yourself: HbdCellListv1 Object\n"
     << "No of Cells: " << nCells << std::endl;
  return;
}

void HbdCellListv1::Reset()
{
  Cell->Clear();
  if (nCells>HBDNCELL)
    {
      Cell->Expand(HBDNCELL);
    }
  nCells = 0;
  return;
}

int HbdCellListv1::isValid() const
{
  return((nCells>0) ? 1 : 0);
}

int HbdCellListv1::set_TClonesArraySize(const unsigned int ncell)
{
  if (ncell > HBDNCELL)
    {
      Cell->Expand(ncell);
    }
  return ncell;
}

void  HbdCellListv1::AddCell(const unsigned int icell)
{
  TClonesArray &Particle = *Cell;
  new(Particle[icell]) HbdCellv1();
  return;
}

HbdCellv1* 
HbdCellListv1::AddCell(const unsigned int icell,
			     const HbdCell& cellt)
{
  const HbdCellv1* test = dynamic_cast<const HbdCellv1*>
    (&cellt);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type HbdCellv1"
	   << endl;
      return 0;
    }

  return new((*Cell)[icell]) HbdCellv1(*test);
}


void  HbdCellListv1::RemoveCell(const unsigned int icell)
{
  Cell->RemoveAt(icell);
  return;
}

