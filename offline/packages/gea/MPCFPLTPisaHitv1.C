#include "MPCFPLTPisaHitv1.h"
#include "MPCFPLTSnglPisaHitv1.h"
#include <iostream>

ClassImp(MPCFPLTPisaHitv1)

using std::cout;
using std::cerr;
using std::endl;

#define PADNHIT 400

  // First we implement the "standard functions"...
MPCFPLTPisaHitv1::MPCFPLTPisaHitv1()
{
  nMPCFPLTPisaHit = 0;
  myMPCFPLTPisaHit  = new TClonesArray("MPCFPLTSnglPisaHitv1",PADNHIT);
}

MPCFPLTPisaHitv1::MPCFPLTPisaHitv1(const MPCFPLTPisaHitv1& rhs)
{
  myMPCFPLTPisaHit=0;
  rhs.copyto(*this);
}

MPCFPLTPisaHitv1& 
MPCFPLTPisaHitv1::operator=(const MPCFPLTPisaHitv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
MPCFPLTPisaHitv1::copyto(MPCFPLTPisaHitv1& dest) const
{
  delete dest.myMPCFPLTPisaHit;
  dest.myMPCFPLTPisaHit = new TClonesArray("MPCFPLTSnglPisaHitv1",nMPCFPLTPisaHit);
  dest.nMPCFPLTPisaHit = nMPCFPLTPisaHit;
  for ( unsigned int i = 0; i < nMPCFPLTPisaHit; ++i ) 
    {
      MPCFPLTSnglPisaHitv1* src = static_cast<MPCFPLTSnglPisaHitv1*>
	(GetHit(i));
      if ( src ) 
	{
	  dest.AddPisaHit(i);
	  MPCFPLTSnglPisaHitv1* d = static_cast<MPCFPLTSnglPisaHitv1*>
	    (dest.GetHit(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

MPCFPLTPisaHitv1* 
MPCFPLTPisaHitv1::clone() const
{
  return new MPCFPLTPisaHitv1(*this);
}

MPCFPLTPisaHitv1::~MPCFPLTPisaHitv1()
{
  myMPCFPLTPisaHit->Clear();
  return;
}

MPCFPLTSnglPisaHitv1* MPCFPLTPisaHitv1::GetHit (const unsigned int itrk) const {
  MPCFPLTSnglPisaHitv1 *Particle = (MPCFPLTSnglPisaHitv1 *) GetPisaHit()->UncheckedAt(itrk);
  return Particle;
}


void MPCFPLTPisaHitv1::identify(std::ostream& os) const
{
  os << "identify yourself: MPCFPLTPisaHitv1 Object\n"
     << "No of myMPCFPLTPisaHits: " << nMPCFPLTPisaHit << std::endl;
  return;
}

void MPCFPLTPisaHitv1::Reset()
{
 myMPCFPLTPisaHit->Clear();
 if (nMPCFPLTPisaHit>PADNHIT)
   {
     myMPCFPLTPisaHit->Expand(PADNHIT);
   }
 nMPCFPLTPisaHit = 0;
 return;
}

int MPCFPLTPisaHitv1::isValid() const
{
  return((nMPCFPLTPisaHit>0) ? 1 : 0);
}

int MPCFPLTPisaHitv1::SetTClonesArraySize(const unsigned int nHits)
{
  if (nHits > PADNHIT)
    {
      myMPCFPLTPisaHit->Expand(nHits);
     }
  return nHits;
}

void  MPCFPLTPisaHitv1::AddPisaHit(const unsigned int itrk)
{
  TClonesArray &Particle = *myMPCFPLTPisaHit;
  new(Particle[itrk]) MPCFPLTSnglPisaHitv1();
  return;
}

MPCFPLTSnglPisaHitv1* 
MPCFPLTPisaHitv1::AddPisaHit(const unsigned int itrk,
				const MPCFPLTSnglPisaHit& track)
{
  const MPCFPLTSnglPisaHitv1* test = dynamic_cast<const MPCFPLTSnglPisaHitv1*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type MPCFPLTSnglPisaHitv1"
	   << endl;
      return 0;
    }

  return new((*myMPCFPLTPisaHit)[itrk]) MPCFPLTSnglPisaHitv1(*test);
}


void  MPCFPLTPisaHitv1::RemovePisaHit(const unsigned int itrk)
{
  myMPCFPLTPisaHit->RemoveAt(itrk);
  return;
}


