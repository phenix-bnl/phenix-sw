#include "NCCPisaHitv2.h"
#include "NCCSnglPisaHitv2.h"
#include <iostream>

ClassImp(NCCPisaHitv2)

using std::cout;
using std::cerr;
using std::endl;

#define PADNHIT 400

  // First we implement the "standard functions"...
NCCPisaHitv2::NCCPisaHitv2()
{
  nNCCPisaHit = 0;
  myNCCPisaHit  = new TClonesArray("NCCSnglPisaHitv2",PADNHIT);
}

NCCPisaHitv2::NCCPisaHitv2(const NCCPisaHitv2& rhs)
{
  myNCCPisaHit=0;
  rhs.copyto(*this);
}

NCCPisaHitv2& 
NCCPisaHitv2::operator=(const NCCPisaHitv2& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
NCCPisaHitv2::copyto(NCCPisaHitv2& dest) const
{
  delete dest.myNCCPisaHit;
  dest.myNCCPisaHit = new TClonesArray("NCCSnglPisaHitv2",nNCCPisaHit);
  dest.nNCCPisaHit = nNCCPisaHit;
  for ( unsigned int i = 0; i < nNCCPisaHit; ++i ) 
    {
      NCCSnglPisaHitv2* src = static_cast<NCCSnglPisaHitv2*>
	(GetHit(i));
      if ( src ) 
	{
	  dest.AddPisaHit(i);
	  NCCSnglPisaHitv2* d = static_cast<NCCSnglPisaHitv2*>
	    (dest.GetHit(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

NCCPisaHitv2* 
NCCPisaHitv2::clone() const
{
  return new NCCPisaHitv2(*this);
}

NCCPisaHitv2::~NCCPisaHitv2()
{
  myNCCPisaHit->Clear();

  delete myNCCPisaHit;
  return;
}

NCCSnglPisaHitv2* NCCPisaHitv2::GetHit (const unsigned int itrk) const {
  NCCSnglPisaHitv2 *Particle = (NCCSnglPisaHitv2 *) GetPisaHit()->UncheckedAt(itrk);
  return Particle;
}


void NCCPisaHitv2::identify(std::ostream& os) const
{
  os << "identify yourself: NCCPisaHitv2 Object\n"
     << "No of myNCCPisaHits: " << nNCCPisaHit << std::endl;
  return;
}

void NCCPisaHitv2::Reset()
{
 myNCCPisaHit->Clear();
 if (nNCCPisaHit>PADNHIT)
   {
     myNCCPisaHit->Expand(PADNHIT);
   }
 nNCCPisaHit = 0;
 return;
}

int NCCPisaHitv2::isValid() const
{
  return((nNCCPisaHit>0) ? 1 : 0);
}

int NCCPisaHitv2::SetTClonesArraySize(const unsigned int nHits)
{
  if (nHits > PADNHIT)
    {
      myNCCPisaHit->Expand(nHits);
     }
  return nHits;
}

void  NCCPisaHitv2::AddPisaHit(const unsigned int itrk)
{
  TClonesArray &Particle = *myNCCPisaHit;
  new(Particle[itrk]) NCCSnglPisaHitv2();
  return;
}

NCCSnglPisaHitv2* 
NCCPisaHitv2::AddPisaHit(const unsigned int itrk,
				const NCCSnglPisaHit& track)
{
  const NCCSnglPisaHitv2* test = dynamic_cast<const NCCSnglPisaHitv2*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type NCCSnglPisaHitv2"
	   << endl;
      return 0;
    }

  return new((*myNCCPisaHit)[itrk]) NCCSnglPisaHitv2(*test);
}


void  NCCPisaHitv2::RemovePisaHit(const unsigned int itrk)
{
  myNCCPisaHit->RemoveAt(itrk);
  return;
}


