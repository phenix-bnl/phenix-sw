#include "MPCEXABSPisaHitv1.h"
#include "MPCEXABSSnglPisaHitv1.h"
#include <iostream>

ClassImp(MPCEXABSPisaHitv1)

using std::cout;
using std::cerr;
using std::endl;

#define PADNHIT 400

  // First we implement the "standard functions"...
MPCEXABSPisaHitv1::MPCEXABSPisaHitv1()
{
  nMPCEXABSPisaHit = 0;
  myMPCEXABSPisaHit  = new TClonesArray("MPCEXABSSnglPisaHitv1",PADNHIT);
}

MPCEXABSPisaHitv1::MPCEXABSPisaHitv1(const MPCEXABSPisaHitv1& rhs)
{
  myMPCEXABSPisaHit=0;
  rhs.copyto(*this);
}

MPCEXABSPisaHitv1& 
MPCEXABSPisaHitv1::operator=(const MPCEXABSPisaHitv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
MPCEXABSPisaHitv1::copyto(MPCEXABSPisaHitv1& dest) const
{
  delete dest.myMPCEXABSPisaHit;
  dest.myMPCEXABSPisaHit = new TClonesArray("MPCEXABSSnglPisaHitv1",nMPCEXABSPisaHit);
  dest.nMPCEXABSPisaHit = nMPCEXABSPisaHit;
  for ( unsigned int i = 0; i < nMPCEXABSPisaHit; ++i ) 
    {
      MPCEXABSSnglPisaHitv1* src = static_cast<MPCEXABSSnglPisaHitv1*>
	(GetHit(i));
      if ( src ) 
	{
	  dest.AddPisaHit(i);
	  MPCEXABSSnglPisaHitv1* d = static_cast<MPCEXABSSnglPisaHitv1*>
	    (dest.GetHit(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

MPCEXABSPisaHitv1* 
MPCEXABSPisaHitv1::clone() const
{
  return new MPCEXABSPisaHitv1(*this);
}

MPCEXABSPisaHitv1::~MPCEXABSPisaHitv1()
{
  myMPCEXABSPisaHit->Clear();
  return;
}

MPCEXABSSnglPisaHitv1* MPCEXABSPisaHitv1::GetHit (const unsigned int itrk) const {
  MPCEXABSSnglPisaHitv1 *Particle = (MPCEXABSSnglPisaHitv1 *) GetPisaHit()->UncheckedAt(itrk);
  return Particle;
}


void MPCEXABSPisaHitv1::identify(std::ostream& os) const
{
  os << "identify yourself: MPCEXABSPisaHitv1 Object\n"
     << "No of myMPCEXABSPisaHits: " << nMPCEXABSPisaHit << std::endl;
  return;
}

void MPCEXABSPisaHitv1::Reset()
{
 myMPCEXABSPisaHit->Clear();
 if (nMPCEXABSPisaHit>PADNHIT)
   {
     myMPCEXABSPisaHit->Expand(PADNHIT);
   }
 nMPCEXABSPisaHit = 0;
 return;
}

int MPCEXABSPisaHitv1::isValid() const
{
  return((nMPCEXABSPisaHit>0) ? 1 : 0);
}

int MPCEXABSPisaHitv1::SetTClonesArraySize(const unsigned int nHits)
{
  if (nHits > PADNHIT)
    {
      myMPCEXABSPisaHit->Expand(nHits);
     }
  return nHits;
}

void  MPCEXABSPisaHitv1::AddPisaHit(const unsigned int itrk)
{
  TClonesArray &Particle = *myMPCEXABSPisaHit;
  new(Particle[itrk]) MPCEXABSSnglPisaHitv1();
  return;
}

MPCEXABSSnglPisaHitv1* 
MPCEXABSPisaHitv1::AddPisaHit(const unsigned int itrk,
				const MPCEXABSSnglPisaHit& track)
{
  const MPCEXABSSnglPisaHitv1* test = dynamic_cast<const MPCEXABSSnglPisaHitv1*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type MPCEXABSSnglPisaHitv1"
	   << endl;
      return 0;
    }

  return new((*myMPCEXABSPisaHit)[itrk]) MPCEXABSSnglPisaHitv1(*test);
}


void  MPCEXABSPisaHitv1::RemovePisaHit(const unsigned int itrk)
{
  myMPCEXABSPisaHit->RemoveAt(itrk);
  return;
}


