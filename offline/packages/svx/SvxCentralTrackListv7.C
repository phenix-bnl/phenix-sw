// ========================
// FILE: SvxCentralTrackListv7.C
// ========================

#include "SvxCentralTrackListv7.h"
#include "SvxCentralTrackv7.h"
#include <iostream>

ClassImp(SvxCentralTrackListv7)

using namespace std;

#define SVXNCENTRALTRACK 4000

SvxCentralTrackListv7::SvxCentralTrackListv7()
{
  CentralTrack = new TClonesArray("SvxCentralTrackv7",SVXNCENTRALTRACK);
}

SvxCentralTrackListv7::SvxCentralTrackListv7(const SvxCentralTrackListv7& rhs)
{
  CentralTrack = 0;
  rhs.copyto(*this);
}

SvxCentralTrackListv7& 
SvxCentralTrackListv7::operator=(const SvxCentralTrackListv7& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxCentralTrackListv7::copyto(SvxCentralTrackListv7& dest) const
{
  delete dest.CentralTrack;
  dest.CentralTrack = new TClonesArray("SvxCentralTrackv7",get_nCentralTracks());
  for ( int i = 0; i < get_nCentralTracks(); ++i ) 
    {
      SvxCentralTrackv7* src = static_cast<SvxCentralTrackv7*>(getCentralTrack(i));
      if ( src ) 
	{
	  dest.addCentralTrack(i);
	  SvxCentralTrackv7* d = static_cast<SvxCentralTrackv7*>(dest.getCentralTrack(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxCentralTrackListv7* 
SvxCentralTrackListv7::clone() const
{
  return new SvxCentralTrackListv7(*this);
}

SvxCentralTrackListv7::~SvxCentralTrackListv7()
{
  CentralTrack->Delete();
  delete CentralTrack;
}

SvxCentralTrackv7* SvxCentralTrackListv7::getCentralTrack (const unsigned int iseg) const {
  SvxCentralTrackv7 *Particle = (SvxCentralTrackv7 *) getCentralTrackObject()->UncheckedAt(iseg);
  return Particle;
}

void SvxCentralTrackListv7::identify(ostream& os) const
{
  os << "identify yourself: SvxCentralTrackListv7 Object\n"
     << "No of CentralTracks: " << get_nCentralTracks() << std::endl;
  return;
}

void SvxCentralTrackListv7::Reset()
{
  CentralTrack->Clear();
  if (get_nCentralTracks()>SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(SVXNCENTRALTRACK);
    }

  return;
}

int SvxCentralTrackListv7::isValid() const
{
  return((get_nCentralTracks()>0) ? 1 : 0);
}

int SvxCentralTrackListv7::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(nseg);
    }
  return nseg;
}

void  SvxCentralTrackListv7::addCentralTrack(const unsigned int iseg)
{
  TClonesArray &Particle = *CentralTrack;
  new(Particle[iseg]) SvxCentralTrackv7();
  return;
}

SvxCentralTrackv7*
SvxCentralTrackListv7::addCentralTrack(const unsigned int iseg,
			     const SvxCentralTrack& seg)
{
  TClonesArray &Particle = *CentralTrack;
  SvxCentralTrackv7* pseg0 = new(Particle[iseg]) SvxCentralTrackv7();
  
  const SvxCentralTrackv7* pseg = dynamic_cast<const SvxCentralTrackv7*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxCentralTrackListv7::removeCentralTrack(const unsigned int iseg)
{
  CentralTrack->RemoveAt(iseg);
  return;
}

