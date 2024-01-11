#include "SvxPisaHitv1.h"
#include "SvxSnglPisaHitv1.h"
#include <iostream>

ClassImp(SvxPisaHitv1)

using std::cout;
using std::cerr;
using std::endl;

#define TECNHIT 400

  // First we implement the "standard functions"...
SvxPisaHitv1::SvxPisaHitv1()
{
  nSvxPisaHit = 0;
  mySvxPisaHit  = new TClonesArray("SvxSnglPisaHitv1",TECNHIT);
}

SvxPisaHitv1::SvxPisaHitv1(const SvxPisaHitv1& rhs)
{
  mySvxPisaHit=0;
  rhs.copyto(*this);
}

SvxPisaHitv1& 
SvxPisaHitv1::operator=(const SvxPisaHitv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxPisaHitv1::copyto(SvxPisaHitv1& dest) const
{
  delete dest.mySvxPisaHit;
  dest.mySvxPisaHit = new TClonesArray("SvxSnglPisaHitv1",nSvxPisaHit);
  dest.nSvxPisaHit = nSvxPisaHit;
  for ( unsigned int i = 0; i < nSvxPisaHit; ++i ) 
    {
      SvxSnglPisaHitv1* src = static_cast<SvxSnglPisaHitv1*>
	(GetHit(i));
      if ( src ) 
	{
	  dest.AddPisaHit(i);
	  SvxSnglPisaHitv1* d = static_cast<SvxSnglPisaHitv1*>
	    (dest.GetHit(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxPisaHit* 
SvxPisaHitv1::clone() const
{
  return new SvxPisaHitv1(*this);
}

SvxPisaHitv1::~SvxPisaHitv1()
{
  if ( mySvxPisaHit ) mySvxPisaHit->Clear();
  delete mySvxPisaHit;
  return;
}

SvxSnglPisaHit* 
SvxPisaHitv1::GetHit (const unsigned int itrk) const {
  SvxSnglPisaHitv1 *Particle = (SvxSnglPisaHitv1 *) GetPisaHit()->UncheckedAt(itrk);
  return Particle;
}


void SvxPisaHitv1::identify(std::ostream& os) const
{
  os << "SvxPisaHitv1 Object\n"
     << "No of mySvxPisaHits: " << nSvxPisaHit << std::endl;
  return;
}

void SvxPisaHitv1::Reset()
{
 mySvxPisaHit->Clear();
 if (nSvxPisaHit>TECNHIT)
   {
     mySvxPisaHit->Expand(TECNHIT);
   }
 nSvxPisaHit = 0;
 return;
}

int SvxPisaHitv1::isValid() const
{
  return((nSvxPisaHit>0) ? 1 : 0);
}

int SvxPisaHitv1::SetTClonesArraySize(const unsigned int nHits)
{
  if (nHits > TECNHIT)
    {
      mySvxPisaHit->Expand(nHits);
     }
  return nHits;
}

void  SvxPisaHitv1::AddPisaHit(const unsigned int itrk)
{
  TClonesArray &Particle = *mySvxPisaHit;
  new(Particle[itrk]) SvxSnglPisaHitv1();
  return;
}

SvxSnglPisaHit* 
SvxPisaHitv1::AddPisaHit(const unsigned int itrk,
				const SvxSnglPisaHit& track)
{
  const SvxSnglPisaHitv1* test = dynamic_cast<const SvxSnglPisaHitv1*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type SvxSnglPisaHitv1"
	   << endl;
      return 0;
    }

  return new((*mySvxPisaHit)[itrk]) SvxSnglPisaHitv1(*test);
}


void  SvxPisaHitv1::RemovePisaHit(const unsigned int itrk)
{
  mySvxPisaHit->RemoveAt(itrk);
  return;
}


