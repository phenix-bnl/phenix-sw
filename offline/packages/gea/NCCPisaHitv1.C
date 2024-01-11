#include "NCCPisaHitv1.h"
#include "NCCSnglPisaHitv1.h"
#include <iostream>

ClassImp(NCCPisaHitv1)

using std::cout;
using std::cerr;
using std::endl;

#define PADNHIT 400

  // First we implement the "standard functions"...
NCCPisaHitv1::NCCPisaHitv1()
{
  nNCCPisaHit = 0;
  myNCCPisaHit  = new TClonesArray("NCCSnglPisaHitv1",PADNHIT);
}

NCCPisaHitv1::NCCPisaHitv1(const NCCPisaHitv1& rhs)
{
  myNCCPisaHit=0;
  rhs.copyto(*this);
}

NCCPisaHitv1& 
NCCPisaHitv1::operator=(const NCCPisaHitv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
NCCPisaHitv1::copyto(NCCPisaHitv1& dest) const
{
  delete dest.myNCCPisaHit;
  dest.myNCCPisaHit = new TClonesArray("NCCSnglPisaHitv1",nNCCPisaHit);
  dest.nNCCPisaHit = nNCCPisaHit;
  for ( unsigned int i = 0; i < nNCCPisaHit; ++i ) 
    {
      NCCSnglPisaHitv1* src = static_cast<NCCSnglPisaHitv1*>
	(GetHit(i));
      if ( src ) 
	{
	  dest.AddPisaHit(i);
	  NCCSnglPisaHitv1* d = static_cast<NCCSnglPisaHitv1*>
	    (dest.GetHit(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

NCCPisaHitv1* 
NCCPisaHitv1::clone() const
{
  return new NCCPisaHitv1(*this);
}

NCCPisaHitv1::~NCCPisaHitv1()
{
  myNCCPisaHit->Clear();
  return;
}

NCCSnglPisaHitv1* NCCPisaHitv1::GetHit (const unsigned int itrk) const {
  NCCSnglPisaHitv1 *Particle = (NCCSnglPisaHitv1 *) GetPisaHit()->UncheckedAt(itrk);
  return Particle;
}


void NCCPisaHitv1::identify(std::ostream& os) const
{
  os << "identify yourself: NCCPisaHitv1 Object\n"
     << "No of myNCCPisaHits: " << nNCCPisaHit << std::endl;
  return;
}

void NCCPisaHitv1::Reset()
{
 myNCCPisaHit->Clear();
 if (nNCCPisaHit>PADNHIT)
   {
     myNCCPisaHit->Expand(PADNHIT);
   }
 nNCCPisaHit = 0;
 return;
}

int NCCPisaHitv1::isValid() const
{
  return((nNCCPisaHit>0) ? 1 : 0);
}

int NCCPisaHitv1::SetTClonesArraySize(const unsigned int nHits)
{
  if (nHits > PADNHIT)
    {
      myNCCPisaHit->Expand(nHits);
     }
  return nHits;
}

void  NCCPisaHitv1::AddPisaHit(const unsigned int itrk)
{
  TClonesArray &Particle = *myNCCPisaHit;
  new(Particle[itrk]) NCCSnglPisaHitv1();
  return;
}

NCCSnglPisaHitv1* 
NCCPisaHitv1::AddPisaHit(const unsigned int itrk,
				const NCCSnglPisaHit& track)
{
  const NCCSnglPisaHitv1* test = dynamic_cast<const NCCSnglPisaHitv1*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type NCCSnglPisaHitv1"
	   << endl;
      return 0;
    }

  return new((*myNCCPisaHit)[itrk]) NCCSnglPisaHitv1(*test);
}


void  NCCPisaHitv1::RemovePisaHit(const unsigned int itrk)
{
  myNCCPisaHit->RemoveAt(itrk);
  return;
}


