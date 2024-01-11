#include "HbdMiniCellListv1.h"
#include "HbdMiniCellv1.h"
#include <iostream>

ClassImp(HbdMiniCellListv1)

using namespace std;

#define HBDNCELL 3000

HbdMiniCellListv1::HbdMiniCellListv1()
{
  nCells = 0;
  Cell = new TClonesArray("HbdMiniCellv1",HBDNCELL);
}

HbdMiniCellListv1::HbdMiniCellListv1(const HbdMiniCellListv1& rhs)
{
  Cell = 0;
  rhs.copyto(*this);
}

HbdMiniCellListv1& 
HbdMiniCellListv1::operator=(const HbdMiniCellListv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
HbdMiniCellListv1::copyto(HbdMiniCellListv1& dest) const
{
  delete dest.Cell;
  dest.Cell = new TClonesArray("HbdMiniCellv1",nCells);
  dest.nCells = nCells;
  for ( unsigned int i = 0; i < nCells; ++i ) 
    {
      HbdMiniCellv1* src = static_cast<HbdMiniCellv1*>
	(get_cell(i));
      if ( src ) 
	{
	  dest.AddCell(i);
	  HbdMiniCellv1* d = static_cast<HbdMiniCellv1*>
	    (dest.get_cell(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

HbdMiniCellListv1* 
HbdMiniCellListv1::clone() const
{
  return new HbdMiniCellListv1(*this);
}

HbdMiniCellListv1::~HbdMiniCellListv1()
{
  Cell->Clear();
  delete Cell;
  return;
}

HbdMiniCellv1* HbdMiniCellListv1::get_cell (const unsigned int icell) const {
  HbdMiniCellv1 *Particle = (HbdMiniCellv1 *) GetCell()->UncheckedAt(icell);
  return Particle;
}

void HbdMiniCellListv1::identify(ostream& os) const
{
  os << "identify yourself: HbdMiniCellListv1 Object\n"
     << "No of Cells: " << nCells << std::endl;
  return;
}

void HbdMiniCellListv1::Reset()
{
  Cell->Clear();
  if (nCells>HBDNCELL)
    {
      Cell->Expand(HBDNCELL);
    }
  nCells = 0;
  return;
}

int HbdMiniCellListv1::isValid() const
{
  return((nCells>0) ? 1 : 0);
}

int HbdMiniCellListv1::set_TClonesArraySize(const unsigned int ncell)
{
  if (ncell > HBDNCELL)
    {
      Cell->Expand(ncell);
    }
  return ncell;
}

void  HbdMiniCellListv1::AddCell(const unsigned int icell)
{
  TClonesArray &Particle = *Cell;
  new(Particle[icell]) HbdMiniCellv1();
  return;
}

HbdMiniCellv1* 
HbdMiniCellListv1::AddCell(const unsigned int icell,
			     const HbdMiniCell& cellt)
{
  const HbdMiniCellv1* test = dynamic_cast<const HbdMiniCellv1*>
    (&cellt);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type HbdMiniCellv1"
	   << endl;
      return 0;
    }

  return new((*Cell)[icell]) HbdMiniCellv1(*test);
}


void  HbdMiniCellListv1::RemoveCell(const unsigned int icell)
{
  Cell->RemoveAt(icell);
  return;
}

