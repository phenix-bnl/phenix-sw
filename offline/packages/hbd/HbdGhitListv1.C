#include "HbdGhitListv1.h"
#include "HbdGhitv1.h"
#include <iostream>

ClassImp(HbdGhitListv1)

using namespace std;

#define HBDNGHIT 3000

HbdGhitListv1::HbdGhitListv1()
{
  nGhits = 0;
  Ghit = new TClonesArray("HbdGhitv1",HBDNGHIT);
}

HbdGhitListv1::HbdGhitListv1(const HbdGhitListv1& rhs)
{
  Ghit = 0;
  rhs.copyto(*this);
}

HbdGhitListv1& 
HbdGhitListv1::operator=(const HbdGhitListv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
HbdGhitListv1::copyto(HbdGhitListv1& dest) const
{
  delete dest.Ghit;
  dest.Ghit = new TClonesArray("HbdGhitv1",nGhits);
  dest.nGhits = nGhits;
  for ( unsigned int i = 0; i < nGhits; ++i ) 
    {
      HbdGhitv1* src = static_cast<HbdGhitv1*>
	(get_ghit(i));
      if ( src ) 
	{
	  dest.AddGhit(i);
	  HbdGhitv1* d = static_cast<HbdGhitv1*>
	    (dest.get_ghit(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

HbdGhitListv1* 
HbdGhitListv1::clone() const
{
  return new HbdGhitListv1(*this);
}

HbdGhitListv1::~HbdGhitListv1()
{
  Ghit->Clear();
  delete Ghit;
  return;
}

HbdGhitv1* HbdGhitListv1::get_ghit (const unsigned int ihit) const {
  HbdGhitv1 *Particle = (HbdGhitv1 *) GetGhit()->UncheckedAt(ihit);
  return Particle;
}

void HbdGhitListv1::identify(ostream& os) const
{
  os << "identify yourself: HbdGhitListv1 Object\n"
     << "No of Ghits: " << nGhits << std::endl;
  return;
}

void HbdGhitListv1::Reset()
{
  Ghit->Clear();
  if (nGhits>HBDNGHIT)
    {
      Ghit->Expand(HBDNGHIT);
    }
  nGhits = 0;
  return;
}

int HbdGhitListv1::isValid() const
{
  return((nGhits>0) ? 1 : 0);
}

int HbdGhitListv1::set_TClonesArraySize(const unsigned int nhit)
{
  if (nhit > HBDNGHIT)
    {
      Ghit->Expand(nhit);
    }
  return nhit;
}

void  HbdGhitListv1::AddGhit(const unsigned int ihit)
{
  TClonesArray &Particle = *Ghit;
  new(Particle[ihit]) HbdGhitv1();
  return;
}

HbdGhitv1* 
HbdGhitListv1::AddGhit(const unsigned int ihit,
			     const HbdGhit& hit)
{
  const HbdGhitv1* test = dynamic_cast<const HbdGhitv1*>
    (&hit);

  if (!test)
    {
      cerr << PHWHERE << " ghit is not of type HbdGhitv1"
	   << endl;
      return 0;
    }

  return new((*Ghit)[ihit]) HbdGhitv1(*test);
}

void  HbdGhitListv1::RemoveGhit(const unsigned int ihit)
{
  Ghit->RemoveAt(ihit);
  return;
}

void HbdGhitListv1::print()
{

  unsigned int i;

  cout << "HbdGhitListv1: Number of hits = " << nGhits << endl;

  for (i=0; i<nGhits; i++) {
    cout << "HbdGhit number " << i << ":" << endl;
    get_ghit(i)->print();
  }

}



