// ========================
// FILE: SvxCentralTrackListv2.C
// ========================

#include "SvxCentralTrackListv2.h"
#include "SvxCentralTrackv2.h"
#include <iostream>

ClassImp(SvxCentralTrackListv2)

using namespace std;

#define SVXNCENTRALTRACK 4000

SvxCentralTrackListv2::SvxCentralTrackListv2()
{
  CentralTrack = new TClonesArray("SvxCentralTrackv2",SVXNCENTRALTRACK);
}

SvxCentralTrackListv2::SvxCentralTrackListv2(const SvxCentralTrackListv2& rhs)
{
  CentralTrack = 0;
  rhs.copyto(*this);
}

SvxCentralTrackListv2& 
SvxCentralTrackListv2::operator=(const SvxCentralTrackListv2& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxCentralTrackListv2::copyto(SvxCentralTrackListv2& dest) const
{
  delete dest.CentralTrack;
  dest.CentralTrack = new TClonesArray("SvxCentralTrackv2",get_nCentralTracks());
  for ( int i = 0; i < get_nCentralTracks(); ++i ) 
    {
      SvxCentralTrackv2* src = static_cast<SvxCentralTrackv2*>(getCentralTrack(i));
      if ( src ) 
	{
	  dest.addCentralTrack(i);
	  SvxCentralTrackv2* d = static_cast<SvxCentralTrackv2*>(dest.getCentralTrack(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxCentralTrackListv2* 
SvxCentralTrackListv2::clone() const
{
  return new SvxCentralTrackListv2(*this);
}

SvxCentralTrackListv2::~SvxCentralTrackListv2()
{
  CentralTrack->Delete();
  delete CentralTrack;
}

SvxCentralTrackv2* SvxCentralTrackListv2::getCentralTrack (const unsigned int iseg) const {
  SvxCentralTrackv2 *Particle = (SvxCentralTrackv2 *) getCentralTrackObject()->UncheckedAt(iseg);
  return Particle;
}

void SvxCentralTrackListv2::identify(ostream& os) const
{
  os << "identify yourself: SvxCentralTrackListv2 Object\n"
     << "No of CentralTracks: " << get_nCentralTracks() << std::endl;
  return;
}

void SvxCentralTrackListv2::Reset()
{
  CentralTrack->Clear();
  if (get_nCentralTracks()>SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(SVXNCENTRALTRACK);
    }

  return;
}

int SvxCentralTrackListv2::isValid() const
{
  return((get_nCentralTracks()>0) ? 1 : 0);
}

int SvxCentralTrackListv2::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(nseg);
    }
  return nseg;
}

void  SvxCentralTrackListv2::addCentralTrack(const unsigned int iseg)
{
  TClonesArray &Particle = *CentralTrack;
  new(Particle[iseg]) SvxCentralTrackv2();
  return;
}

SvxCentralTrackv2*
SvxCentralTrackListv2::addCentralTrack(const unsigned int iseg,
			     const SvxCentralTrack& seg)
{
  TClonesArray &Particle = *CentralTrack;
  SvxCentralTrackv2* pseg0 = new(Particle[iseg]) SvxCentralTrackv2();
  
  const SvxCentralTrackv2* pseg = dynamic_cast<const SvxCentralTrackv2*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxCentralTrackListv2::removeCentralTrack(const unsigned int iseg)
{
  CentralTrack->RemoveAt(iseg);
  return;
}

