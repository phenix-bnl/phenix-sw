#include "PhCglListv4_Run7a.h"
#include "phool.h"
#include "TClonesArray.h"

ClassImp(PhCglListv4_Run7a)

using namespace std;

static const unsigned int PHNTRACKS = 2000;

// First we implement the "standard functions"...
PhCglListv4_Run7a::PhCglListv4_Run7a()
{
  nCentral = 0;
  evtFilterTypecg = 0;
  Central = new TClonesArray("PhCglSnglv4_Run7a",PHNTRACKS);
}

PhCglListv4_Run7a::PhCglListv4_Run7a(const PhCglListv4_Run7a& rhs)
{
  Central=0;
  evtFilterTypecg = 0;
  rhs.copyto(*this);
}

PhCglListv4_Run7a&
PhCglListv4_Run7a::operator=(const PhCglListv4_Run7a& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PhCglListv4_Run7a::copyto(PhCglListv4_Run7a& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PhCglSnglv4_Run7a",nCentral);
  dest.nCentral = nCentral;
  dest.evtFilterTypecg = evtFilterTypecg;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PhCglSnglv4_Run7a* src = static_cast<PhCglSnglv4_Run7a*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PhCglSnglv4_Run7a* d = static_cast<PhCglSnglv4_Run7a*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PhCglListv4_Run7a*
PhCglListv4_Run7a::clone() const
{
  return new PhCglListv4_Run7a(*this);
}


PhCglListv4_Run7a::~PhCglListv4_Run7a()
{
  Central->Clear();
  delete Central;
}

void PhCglListv4_Run7a::identify(ostream& os) const
{
  os << "identify yourself: PhCglListv4_Run7a Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
}

void PhCglListv4_Run7a::Reset()
{
  Central->Clear();
  if (nCentral>PHNTRACKS)
    {
      Central->Expand(PHNTRACKS);
    }
  nCentral = 0;
  evtFilterTypecg = 0;
}

int PhCglListv4_Run7a::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PhCglListv4_Run7a::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  
PhCglListv4_Run7a::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PhCglSnglv4_Run7a();
  return;
}

PhCglSnglv4_Run7a* 
PhCglListv4_Run7a::AddPHParticle(const PHSnglCentralTrack &track)
{
  // First check if TC array exists (it does not for the base class
  if (!GetCentral())
    {
      return NULL;
    }
  // this is a bit ugly but GetLast returns the  index-1, so the argument
  // for the ExpandCreate is  GetLast() + 2
  int nnew;
  nnew = GetCentral()->GetLast() + 2;
  // this is a TCArray method, it creates a new Object of
  // the type which is stored in the TCArray. It uses the default ctor
  GetCentral()->ExpandCreate(nnew);
  PhCglSnglv4_Run7a *newtrack = static_cast<PhCglSnglv4_Run7a *> (GetCentral()->UncheckedAt(GetCentral()->GetLast()));
  // Since the ExpandCreate calls the default ctor we still need to copy
  // the actual values from the input particle
  newtrack->Copy(track);
  set_npart(nnew); // update track counter
  return newtrack;
}
