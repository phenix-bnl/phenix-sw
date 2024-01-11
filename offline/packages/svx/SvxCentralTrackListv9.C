// ========================
// FILE: SvxCentralTrackListv9.C
// ========================

#include "SvxCentralTrackListv9.h"
#include "SvxCentralTrackv9.h"
#include <iostream>

ClassImp(SvxCentralTrackListv9)

using namespace std;

#define SVXNCENTRALTRACK 4000

SvxCentralTrackListv9::SvxCentralTrackListv9()
{
  CentralTrack = new TClonesArray("SvxCentralTrackv9",SVXNCENTRALTRACK);
}

SvxCentralTrackListv9::SvxCentralTrackListv9(const SvxCentralTrackListv9& rhs)
{
  CentralTrack = 0;
  rhs.copyto(*this);
}

SvxCentralTrackListv9& 
SvxCentralTrackListv9::operator=(const SvxCentralTrackListv9& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxCentralTrackListv9::copyto(SvxCentralTrackListv9& dest) const
{
  delete dest.CentralTrack;
  dest.CentralTrack = new TClonesArray("SvxCentralTrackv9",get_nCentralTracks());
  for ( int i = 0; i < get_nCentralTracks(); ++i ) 
    {
      SvxCentralTrackv9* src = static_cast<SvxCentralTrackv9*>(getCentralTrack(i));
      if ( src ) 
	{
	  dest.addCentralTrack(i);
	  SvxCentralTrackv9* d = static_cast<SvxCentralTrackv9*>(dest.getCentralTrack(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxCentralTrackListv9* 
SvxCentralTrackListv9::clone() const
{
  return new SvxCentralTrackListv9(*this);
}

SvxCentralTrackListv9::~SvxCentralTrackListv9()
{
  CentralTrack->Delete();
  delete CentralTrack;
}

SvxCentralTrackv9* SvxCentralTrackListv9::getCentralTrack (const unsigned int iseg) const {
  SvxCentralTrackv9 *Particle = (SvxCentralTrackv9 *) getCentralTrackObject()->UncheckedAt(iseg);
  return Particle;
}

void SvxCentralTrackListv9::identify(ostream& os) const
{
  os << "identify yourself: SvxCentralTrackListv9 Object\n"
     << "No of CentralTracks: " << get_nCentralTracks() << std::endl;
  return;
}

void SvxCentralTrackListv9::Reset()
{
  CentralTrack->Clear();
  if (get_nCentralTracks()>SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(SVXNCENTRALTRACK);
    }

  return;
}

int SvxCentralTrackListv9::isValid() const
{
  return((get_nCentralTracks()>0) ? 1 : 0);
}

int SvxCentralTrackListv9::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(nseg);
    }
  return nseg;
}

void  SvxCentralTrackListv9::addCentralTrack(const unsigned int iseg)
{
  TClonesArray &Particle = *CentralTrack;
  new(Particle[iseg]) SvxCentralTrackv9();
  return;
}

SvxCentralTrackv9*
SvxCentralTrackListv9::addCentralTrack(const unsigned int iseg,
			     const SvxCentralTrack& seg)
{
  TClonesArray &Particle = *CentralTrack;
  SvxCentralTrackv9* pseg0 = new(Particle[iseg]) SvxCentralTrackv9();
  
  const SvxCentralTrackv9* pseg = dynamic_cast<const SvxCentralTrackv9*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxCentralTrackListv9::removeCentralTrack(const unsigned int iseg)
{
  CentralTrack->RemoveAt(iseg);
  return;
}

