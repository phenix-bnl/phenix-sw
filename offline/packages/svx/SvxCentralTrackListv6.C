// ========================
// FILE: SvxCentralTrackListv6.C
// ========================

#include "SvxCentralTrackListv6.h"
#include "SvxCentralTrackv6.h"
#include <iostream>

ClassImp(SvxCentralTrackListv6)

using namespace std;

#define SVXNCENTRALTRACK 4000

SvxCentralTrackListv6::SvxCentralTrackListv6()
{
  CentralTrack = new TClonesArray("SvxCentralTrackv6",SVXNCENTRALTRACK);
}

SvxCentralTrackListv6::SvxCentralTrackListv6(const SvxCentralTrackListv6& rhs)
{
  CentralTrack = 0;
  rhs.copyto(*this);
}

SvxCentralTrackListv6& 
SvxCentralTrackListv6::operator=(const SvxCentralTrackListv6& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxCentralTrackListv6::copyto(SvxCentralTrackListv6& dest) const
{
  delete dest.CentralTrack;
  dest.CentralTrack = new TClonesArray("SvxCentralTrackv6",get_nCentralTracks());
  for ( int i = 0; i < get_nCentralTracks(); ++i ) 
    {
      SvxCentralTrackv6* src = static_cast<SvxCentralTrackv6*>(getCentralTrack(i));
      if ( src ) 
	{
	  dest.addCentralTrack(i);
	  SvxCentralTrackv6* d = static_cast<SvxCentralTrackv6*>(dest.getCentralTrack(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxCentralTrackListv6* 
SvxCentralTrackListv6::clone() const
{
  return new SvxCentralTrackListv6(*this);
}

SvxCentralTrackListv6::~SvxCentralTrackListv6()
{
  CentralTrack->Delete();
  delete CentralTrack;
}

SvxCentralTrackv6* SvxCentralTrackListv6::getCentralTrack (const unsigned int iseg) const {
  SvxCentralTrackv6 *Particle = (SvxCentralTrackv6 *) getCentralTrackObject()->UncheckedAt(iseg);
  return Particle;
}

void SvxCentralTrackListv6::identify(ostream& os) const
{
  os << "identify yourself: SvxCentralTrackListv6 Object\n"
     << "No of CentralTracks: " << get_nCentralTracks() << std::endl;
  return;
}

void SvxCentralTrackListv6::Reset()
{
  CentralTrack->Clear();
  if (get_nCentralTracks()>SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(SVXNCENTRALTRACK);
    }

  return;
}

int SvxCentralTrackListv6::isValid() const
{
  return((get_nCentralTracks()>0) ? 1 : 0);
}

int SvxCentralTrackListv6::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(nseg);
    }
  return nseg;
}

void  SvxCentralTrackListv6::addCentralTrack(const unsigned int iseg)
{
  TClonesArray &Particle = *CentralTrack;
  new(Particle[iseg]) SvxCentralTrackv6();
  return;
}

SvxCentralTrackv6*
SvxCentralTrackListv6::addCentralTrack(const unsigned int iseg,
			     const SvxCentralTrack& seg)
{
  TClonesArray &Particle = *CentralTrack;
  SvxCentralTrackv6* pseg0 = new(Particle[iseg]) SvxCentralTrackv6();
  
  const SvxCentralTrackv6* pseg = dynamic_cast<const SvxCentralTrackv6*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxCentralTrackListv6::removeCentralTrack(const unsigned int iseg)
{
  CentralTrack->RemoveAt(iseg);
  return;
}

