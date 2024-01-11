// ========================
// FILE: SvxCentralTrackListv3.C
// ========================

#include "SvxCentralTrackListv3.h"
#include "SvxCentralTrackv3.h"
#include <iostream>

ClassImp(SvxCentralTrackListv3)

using namespace std;

#define SVXNCENTRALTRACK 4000

SvxCentralTrackListv3::SvxCentralTrackListv3()
{
  CentralTrack = new TClonesArray("SvxCentralTrackv3",SVXNCENTRALTRACK);
}

SvxCentralTrackListv3::SvxCentralTrackListv3(const SvxCentralTrackListv3& rhs)
{
  CentralTrack = 0;
  rhs.copyto(*this);
}

SvxCentralTrackListv3& 
SvxCentralTrackListv3::operator=(const SvxCentralTrackListv3& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxCentralTrackListv3::copyto(SvxCentralTrackListv3& dest) const
{
  delete dest.CentralTrack;
  dest.CentralTrack = new TClonesArray("SvxCentralTrackv3",get_nCentralTracks());
  for ( int i = 0; i < get_nCentralTracks(); ++i ) 
    {
      SvxCentralTrackv3* src = static_cast<SvxCentralTrackv3*>(getCentralTrack(i));
      if ( src ) 
	{
	  dest.addCentralTrack(i);
	  SvxCentralTrackv3* d = static_cast<SvxCentralTrackv3*>(dest.getCentralTrack(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxCentralTrackListv3* 
SvxCentralTrackListv3::clone() const
{
  return new SvxCentralTrackListv3(*this);
}

SvxCentralTrackListv3::~SvxCentralTrackListv3()
{
  CentralTrack->Delete();
  delete CentralTrack;
}

SvxCentralTrackv3* SvxCentralTrackListv3::getCentralTrack (const unsigned int iseg) const {
  SvxCentralTrackv3 *Particle = (SvxCentralTrackv3 *) getCentralTrackObject()->UncheckedAt(iseg);
  return Particle;
}

void SvxCentralTrackListv3::identify(ostream& os) const
{
  os << "identify yourself: SvxCentralTrackListv3 Object\n"
     << "No of CentralTracks: " << get_nCentralTracks() << std::endl;
  return;
}

void SvxCentralTrackListv3::Reset()
{
  CentralTrack->Clear();
  if (get_nCentralTracks()>SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(SVXNCENTRALTRACK);
    }

  return;
}

int SvxCentralTrackListv3::isValid() const
{
  return((get_nCentralTracks()>0) ? 1 : 0);
}

int SvxCentralTrackListv3::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(nseg);
    }
  return nseg;
}

void  SvxCentralTrackListv3::addCentralTrack(const unsigned int iseg)
{
  TClonesArray &Particle = *CentralTrack;
  new(Particle[iseg]) SvxCentralTrackv3();
  return;
}

SvxCentralTrackv3*
SvxCentralTrackListv3::addCentralTrack(const unsigned int iseg,
			     const SvxCentralTrack& seg)
{
  TClonesArray &Particle = *CentralTrack;
  SvxCentralTrackv3* pseg0 = new(Particle[iseg]) SvxCentralTrackv3();
  
  const SvxCentralTrackv3* pseg = dynamic_cast<const SvxCentralTrackv3*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxCentralTrackListv3::removeCentralTrack(const unsigned int iseg)
{
  CentralTrack->RemoveAt(iseg);
  return;
}

