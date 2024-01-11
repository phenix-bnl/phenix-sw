#include "MPCEXENTPisaHitv1.h"
#include "MPCEXENTSnglPisaHitv1.h"
#include <iostream>

ClassImp(MPCEXENTPisaHitv1)

using std::cout;
using std::cerr;
using std::endl;

#define PADNHIT 400

  // First we implement the "standard functions"...
MPCEXENTPisaHitv1::MPCEXENTPisaHitv1()
{
  nMPCEXENTPisaHit = 0;
  myMPCEXENTPisaHit  = new TClonesArray("MPCEXENTSnglPisaHitv1",PADNHIT);
}

MPCEXENTPisaHitv1::MPCEXENTPisaHitv1(const MPCEXENTPisaHitv1& rhs)
{
  myMPCEXENTPisaHit=0;
  rhs.copyto(*this);
}

MPCEXENTPisaHitv1& 
MPCEXENTPisaHitv1::operator=(const MPCEXENTPisaHitv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
MPCEXENTPisaHitv1::copyto(MPCEXENTPisaHitv1& dest) const
{
  delete dest.myMPCEXENTPisaHit;
  dest.myMPCEXENTPisaHit = new TClonesArray("MPCEXENTSnglPisaHitv1",nMPCEXENTPisaHit);
  dest.nMPCEXENTPisaHit = nMPCEXENTPisaHit;
  for ( unsigned int i = 0; i < nMPCEXENTPisaHit; ++i ) 
    {
      MPCEXENTSnglPisaHitv1* src = static_cast<MPCEXENTSnglPisaHitv1*>
	(GetHit(i));
      if ( src ) 
	{
	  dest.AddPisaHit(i);
	  MPCEXENTSnglPisaHitv1* d = static_cast<MPCEXENTSnglPisaHitv1*>
	    (dest.GetHit(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

MPCEXENTPisaHitv1* 
MPCEXENTPisaHitv1::clone() const
{
  return new MPCEXENTPisaHitv1(*this);
}

MPCEXENTPisaHitv1::~MPCEXENTPisaHitv1()
{
  myMPCEXENTPisaHit->Clear();
  return;
}

MPCEXENTSnglPisaHitv1* MPCEXENTPisaHitv1::GetHit (const unsigned int itrk) const {
  MPCEXENTSnglPisaHitv1 *Particle = (MPCEXENTSnglPisaHitv1 *) GetPisaHit()->UncheckedAt(itrk);
  return Particle;
}


void MPCEXENTPisaHitv1::identify(std::ostream& os) const
{
  os << "identify yourself: MPCEXENTPisaHitv1 Object\n"
     << "No of myMPCEXENTPisaHits: " << nMPCEXENTPisaHit << std::endl;
  return;
}

void MPCEXENTPisaHitv1::Reset()
{
 myMPCEXENTPisaHit->Clear();
 if (nMPCEXENTPisaHit>PADNHIT)
   {
     myMPCEXENTPisaHit->Expand(PADNHIT);
   }
 nMPCEXENTPisaHit = 0;
 return;
}

int MPCEXENTPisaHitv1::isValid() const
{
  return((nMPCEXENTPisaHit>0) ? 1 : 0);
}

int MPCEXENTPisaHitv1::SetTClonesArraySize(const unsigned int nHits)
{
  if (nHits > PADNHIT)
    {
      myMPCEXENTPisaHit->Expand(nHits);
     }
  return nHits;
}

void  MPCEXENTPisaHitv1::AddPisaHit(const unsigned int itrk)
{
  TClonesArray &Particle = *myMPCEXENTPisaHit;
  new(Particle[itrk]) MPCEXENTSnglPisaHitv1();
  return;
}

MPCEXENTSnglPisaHitv1* 
MPCEXENTPisaHitv1::AddPisaHit(const unsigned int itrk,
				const MPCEXENTSnglPisaHit& track)
{
  const MPCEXENTSnglPisaHitv1* test = dynamic_cast<const MPCEXENTSnglPisaHitv1*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type MPCEXENTSnglPisaHitv1"
	   << endl;
      return 0;
    }

  return new((*myMPCEXENTPisaHit)[itrk]) MPCEXENTSnglPisaHitv1(*test);
}


void  MPCEXENTPisaHitv1::RemovePisaHit(const unsigned int itrk)
{
  myMPCEXENTPisaHit->RemoveAt(itrk);
  return;
}


