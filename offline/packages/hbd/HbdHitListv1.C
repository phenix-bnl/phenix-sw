#include "HbdHitListv1.h"
#include "HbdHitv1.h"
#include <iostream>

ClassImp(HbdHitListv1)

using namespace std;

#define HBDNHIT 3000

//______________________________
HbdHitListv1::HbdHitListv1()
{
  nHits = 0;
  Hit = new TClonesArray("HbdHitv1",HBDNHIT);
}

//___________________________________________________
HbdHitListv1::HbdHitListv1(const HbdHitListv1& rhs)
{
  Hit = 0;
  rhs.copyto(*this);
}

//___________________________________________________
HbdHitListv1& HbdHitListv1::operator=(const HbdHitListv1& rhs)
{
  if ( this != &rhs )
  {
    rhs.copyto(*this);
  }
  return *this;
}

//___________________________________________________
void HbdHitListv1::copyto(HbdHitListv1& dest) const
{
  
  // clear destination list and delete
  dest.Hit->Delete();
  delete dest.Hit;
  
  dest.Hit = new TClonesArray("HbdHitv1",nHits);
  dest.nHits = nHits;
  for ( unsigned int i = 0; i < nHits; ++i ) 
  {
    HbdHitv1* src = static_cast<HbdHitv1*>
      (get_hit(i));
    if ( src ) 
    {
      dest.AddHit(i);
      HbdHitv1* d = static_cast<HbdHitv1*>
        (dest.get_hit(i));
      *d = *src;
    } else {
      cerr << PHWHERE << "src particle is null ?" << endl;
    }
  }
}

//________________________________________________
HbdHitListv1* HbdHitListv1::clone() const
{ return new HbdHitListv1(*this);}

//________________________________________________
HbdHitListv1::~HbdHitListv1()
{
  
  // clear and delete TClonesArray
  Hit->Delete();
  delete Hit;
  
  return;
}

//________________________________________________
HbdHitv1* HbdHitListv1::get_hit (const unsigned int ihit) const 
{
  HbdHitv1 *Particle = (HbdHitv1 *) GetHit()->UncheckedAt(ihit);
  return Particle;
}

//____________________________________________________________
void HbdHitListv1::identify(ostream& os) const
{
  os << "identify yourself: HbdHitListv1 Object\n"
     << "No of Hits: " << nHits << std::endl;
  return;
}

void HbdHitListv1::Reset()
{
  Hit->Clear();
  if (nHits>HBDNHIT)
    {
      Hit->Expand(HBDNHIT);
    }
  nHits = 0;
  return;
}

int HbdHitListv1::isValid() const
{
  return((nHits>0) ? 1 : 0);
}

int HbdHitListv1::set_TClonesArraySize(const unsigned int nhit)
{
  if (nhit > HBDNHIT)
    {
      Hit->Expand(nhit);
    }
  return nhit;
}

void  HbdHitListv1::AddHit(const unsigned int ihit)
{
  TClonesArray &Particle = *Hit;
  new(Particle[ihit]) HbdHitv1();
  return;
}

HbdHitv1* 
HbdHitListv1::AddHit(const unsigned int ihit,
			     const HbdHit& hit)
{
  const HbdHitv1* test = dynamic_cast<const HbdHitv1*>
    (&hit);

  if (!test)
    {
      cerr << PHWHERE << " hit is not of type HbdHitv1"
	   << endl;
      return 0;
    }

  return new((*Hit)[ihit]) HbdHitv1(*test);
}

void  HbdHitListv1::RemoveHit(const unsigned int ihit)
{
  Hit->RemoveAt(ihit);
  return;
}

void HbdHitListv1::print()
{

  unsigned int i;

  cout << "HbdHitListv1: Number of hits = " << nHits << endl;

  for (i=0; i<nHits; i++) {
    cout << "HbdHit number " << i << ":" << endl;
    get_hit(i)->print();
  }

}
