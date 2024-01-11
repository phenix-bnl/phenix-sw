// ========================
// FILE: SvxCentralTrackListv4.C
// ========================

#include "SvxCentralTrackListv4.h"
#include "SvxCentralTrackv4.h"
#include <iostream>

ClassImp(SvxCentralTrackListv4)

using namespace std;

#define SVXNCENTRALTRACK 4000

SvxCentralTrackListv4::SvxCentralTrackListv4()
{
  CentralTrack = new TClonesArray("SvxCentralTrackv4",SVXNCENTRALTRACK);
}

SvxCentralTrackListv4::SvxCentralTrackListv4(const SvxCentralTrackListv4& rhs)
{
  CentralTrack = 0;
  rhs.copyto(*this);
}

SvxCentralTrackListv4& 
SvxCentralTrackListv4::operator=(const SvxCentralTrackListv4& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxCentralTrackListv4::copyto(SvxCentralTrackListv4& dest) const
{
  delete dest.CentralTrack;
  dest.CentralTrack = new TClonesArray("SvxCentralTrackv4",get_nCentralTracks());
  for ( int i = 0; i < get_nCentralTracks(); ++i ) 
    {
      SvxCentralTrackv4* src = static_cast<SvxCentralTrackv4*>(getCentralTrack(i));
      if ( src ) 
	{
	  dest.addCentralTrack(i);
	  SvxCentralTrackv4* d = static_cast<SvxCentralTrackv4*>(dest.getCentralTrack(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxCentralTrackListv4* 
SvxCentralTrackListv4::clone() const
{
  return new SvxCentralTrackListv4(*this);
}

SvxCentralTrackListv4::~SvxCentralTrackListv4()
{
  CentralTrack->Delete();
  delete CentralTrack;
}

SvxCentralTrackv4* SvxCentralTrackListv4::getCentralTrack (const unsigned int iseg) const {
  SvxCentralTrackv4 *Particle = (SvxCentralTrackv4 *) getCentralTrackObject()->UncheckedAt(iseg);
  return Particle;
}

void SvxCentralTrackListv4::identify(ostream& os) const
{
  os << "identify yourself: SvxCentralTrackListv4 Object\n"
     << "No of CentralTracks: " << get_nCentralTracks() << std::endl;
  return;
}

void SvxCentralTrackListv4::Reset()
{
  CentralTrack->Clear();
  if (get_nCentralTracks()>SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(SVXNCENTRALTRACK);
    }

  return;
}

int SvxCentralTrackListv4::isValid() const
{
  return((get_nCentralTracks()>0) ? 1 : 0);
}

int SvxCentralTrackListv4::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(nseg);
    }
  return nseg;
}

void  SvxCentralTrackListv4::addCentralTrack(const unsigned int iseg)
{
  TClonesArray &Particle = *CentralTrack;
  new(Particle[iseg]) SvxCentralTrackv4();
  return;
}

SvxCentralTrackv4*
SvxCentralTrackListv4::addCentralTrack(const unsigned int iseg,
			     const SvxCentralTrack& seg)
{
  TClonesArray &Particle = *CentralTrack;
  SvxCentralTrackv4* pseg0 = new(Particle[iseg]) SvxCentralTrackv4();
  
  const SvxCentralTrackv4* pseg = dynamic_cast<const SvxCentralTrackv4*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxCentralTrackListv4::removeCentralTrack(const unsigned int iseg)
{
  CentralTrack->RemoveAt(iseg);
  return;
}

