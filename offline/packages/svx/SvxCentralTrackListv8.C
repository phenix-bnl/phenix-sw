// ========================
// FILE: SvxCentralTrackListv8.C
// ========================

#include "SvxCentralTrackListv8.h"
#include "SvxCentralTrackv8.h"
#include <iostream>

ClassImp(SvxCentralTrackListv8)

using namespace std;

#define SVXNCENTRALTRACK 4000

SvxCentralTrackListv8::SvxCentralTrackListv8()
{
  CentralTrack = new TClonesArray("SvxCentralTrackv8",SVXNCENTRALTRACK);
}

SvxCentralTrackListv8::SvxCentralTrackListv8(const SvxCentralTrackListv8& rhs)
{
  CentralTrack = 0;
  rhs.copyto(*this);
}

SvxCentralTrackListv8& 
SvxCentralTrackListv8::operator=(const SvxCentralTrackListv8& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxCentralTrackListv8::copyto(SvxCentralTrackListv8& dest) const
{
  delete dest.CentralTrack;
  dest.CentralTrack = new TClonesArray("SvxCentralTrackv8",get_nCentralTracks());
  for ( int i = 0; i < get_nCentralTracks(); ++i ) 
    {
      SvxCentralTrackv8* src = static_cast<SvxCentralTrackv8*>(getCentralTrack(i));
      if ( src ) 
	{
	  dest.addCentralTrack(i);
	  SvxCentralTrackv8* d = static_cast<SvxCentralTrackv8*>(dest.getCentralTrack(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxCentralTrackListv8* 
SvxCentralTrackListv8::clone() const
{
  return new SvxCentralTrackListv8(*this);
}

SvxCentralTrackListv8::~SvxCentralTrackListv8()
{
  CentralTrack->Delete();
  delete CentralTrack;
}

SvxCentralTrackv8* SvxCentralTrackListv8::getCentralTrack (const unsigned int iseg) const {
  SvxCentralTrackv8 *Particle = (SvxCentralTrackv8 *) getCentralTrackObject()->UncheckedAt(iseg);
  return Particle;
}

void SvxCentralTrackListv8::identify(ostream& os) const
{
  os << "identify yourself: SvxCentralTrackListv8 Object\n"
     << "No of CentralTracks: " << get_nCentralTracks() << std::endl;
  return;
}

void SvxCentralTrackListv8::Reset()
{
  CentralTrack->Clear();
  if (get_nCentralTracks()>SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(SVXNCENTRALTRACK);
    }

  return;
}

int SvxCentralTrackListv8::isValid() const
{
  return((get_nCentralTracks()>0) ? 1 : 0);
}

int SvxCentralTrackListv8::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(nseg);
    }
  return nseg;
}

void  SvxCentralTrackListv8::addCentralTrack(const unsigned int iseg)
{
  TClonesArray &Particle = *CentralTrack;
  new(Particle[iseg]) SvxCentralTrackv8();
  return;
}

SvxCentralTrackv8*
SvxCentralTrackListv8::addCentralTrack(const unsigned int iseg,
			     const SvxCentralTrack& seg)
{
  TClonesArray &Particle = *CentralTrack;
  SvxCentralTrackv8* pseg0 = new(Particle[iseg]) SvxCentralTrackv8();
  
  const SvxCentralTrackv8* pseg = dynamic_cast<const SvxCentralTrackv8*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxCentralTrackListv8::removeCentralTrack(const unsigned int iseg)
{
  CentralTrack->RemoveAt(iseg);
  return;
}

