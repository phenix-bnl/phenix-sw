#include "HbdCellListv2.h"
#include "HbdCellv2.h"
#include <iostream>

ClassImp(HbdCellListv2)

using namespace std;

#define HBDNCELL 3000

HbdCellListv2::HbdCellListv2()
{
  nCells = 0;
  Cell = new TClonesArray("HbdCellv2",HBDNCELL);
}

HbdCellListv2::HbdCellListv2(const HbdCellListv2& rhs)
{
  Cell = 0;
  rhs.copyto(*this);
}

HbdCellListv2& 
HbdCellListv2::operator=(const HbdCellListv2& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
HbdCellListv2::copyto(HbdCellListv2& dest) const
{
  delete dest.Cell;
  dest.Cell = new TClonesArray("HbdCellv2",nCells);
  dest.nCells = nCells;
  for ( unsigned int i = 0; i < nCells; ++i ) 
    {
      HbdCellv2* src = static_cast<HbdCellv2*>
	(get_cell(i));
      if ( src ) 
	{
	  dest.AddCell(i);
	  HbdCellv2* d = static_cast<HbdCellv2*>
	    (dest.get_cell(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

HbdCellListv2* 
HbdCellListv2::clone() const
{
  return new HbdCellListv2(*this);
}

HbdCellListv2::~HbdCellListv2()
{
  Cell->Clear();
  delete Cell;
  return;
}

HbdCellv2* HbdCellListv2::get_cell (const unsigned int icell) const {
  HbdCellv2 *Particle = (HbdCellv2 *) GetCell()->UncheckedAt(icell);
  return Particle;
}

void HbdCellListv2::identify(ostream& os) const
{
  os << "identify yourself: HbdCellListv2 Object\n"
     << "No of Cells: " << nCells << std::endl;
  return;
}

void HbdCellListv2::Reset()
{
  Cell->Clear();
  if (nCells>HBDNCELL)
    {
      Cell->Expand(HBDNCELL);
    }
  nCells = 0;
  return;
}

int HbdCellListv2::isValid() const
{
  return((nCells>0) ? 1 : 0);
}

int HbdCellListv2::set_TClonesArraySize(const unsigned int ncell)
{
  if (ncell > HBDNCELL)
    {
      Cell->Expand(ncell);
    }
  return ncell;
}

void  HbdCellListv2::AddCell(const unsigned int icell)
{
  TClonesArray &Particle = *Cell;
  new(Particle[icell]) HbdCellv2();
  return;
}

HbdCellv2* 
HbdCellListv2::AddCell(const unsigned int icell,
			     const HbdCell& cellt)
{
  const HbdCellv2* test = dynamic_cast<const HbdCellv2*>
    (&cellt);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type HbdCellv2"
	   << endl;
      return 0;
    }

  return new((*Cell)[icell]) HbdCellv2(*test);
}


void  HbdCellListv2::RemoveCell(const unsigned int icell)
{
  Cell->RemoveAt(icell);
  return;
}

