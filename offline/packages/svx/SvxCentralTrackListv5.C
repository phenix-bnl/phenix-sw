// ========================
// FILE: SvxCentralTrackListv5.C
// ========================

#include "SvxCentralTrackListv5.h"
#include "SvxCentralTrackv5.h"
#include <iostream>

ClassImp(SvxCentralTrackListv5)

using namespace std;

#define SVXNCENTRALTRACK 4000

SvxCentralTrackListv5::SvxCentralTrackListv5()
{
  CentralTrack = new TClonesArray("SvxCentralTrackv5",SVXNCENTRALTRACK);
}

SvxCentralTrackListv5::SvxCentralTrackListv5(const SvxCentralTrackListv5& rhs)
{
  CentralTrack = 0;
  rhs.copyto(*this);
}

SvxCentralTrackListv5& 
SvxCentralTrackListv5::operator=(const SvxCentralTrackListv5& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxCentralTrackListv5::copyto(SvxCentralTrackListv5& dest) const
{
  delete dest.CentralTrack;
  dest.CentralTrack = new TClonesArray("SvxCentralTrackv5",get_nCentralTracks());
  for ( int i = 0; i < get_nCentralTracks(); ++i ) 
    {
      SvxCentralTrackv5* src = static_cast<SvxCentralTrackv5*>(getCentralTrack(i));
      if ( src ) 
	{
	  dest.addCentralTrack(i);
	  SvxCentralTrackv5* d = static_cast<SvxCentralTrackv5*>(dest.getCentralTrack(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxCentralTrackListv5* 
SvxCentralTrackListv5::clone() const
{
  return new SvxCentralTrackListv5(*this);
}

SvxCentralTrackListv5::~SvxCentralTrackListv5()
{
  CentralTrack->Delete();
  delete CentralTrack;
}

SvxCentralTrackv5* SvxCentralTrackListv5::getCentralTrack (const unsigned int iseg) const {
  SvxCentralTrackv5 *Particle = (SvxCentralTrackv5 *) getCentralTrackObject()->UncheckedAt(iseg);
  return Particle;
}

void SvxCentralTrackListv5::identify(ostream& os) const
{
  os << "identify yourself: SvxCentralTrackListv5 Object\n"
     << "No of CentralTracks: " << get_nCentralTracks() << std::endl;
  return;
}

void SvxCentralTrackListv5::Reset()
{
  CentralTrack->Clear();
  if (get_nCentralTracks()>SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(SVXNCENTRALTRACK);
    }

  return;
}

int SvxCentralTrackListv5::isValid() const
{
  return((get_nCentralTracks()>0) ? 1 : 0);
}

int SvxCentralTrackListv5::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(nseg);
    }
  return nseg;
}

void  SvxCentralTrackListv5::addCentralTrack(const unsigned int iseg)
{
  TClonesArray &Particle = *CentralTrack;
  new(Particle[iseg]) SvxCentralTrackv5();
  return;
}

SvxCentralTrackv5*
SvxCentralTrackListv5::addCentralTrack(const unsigned int iseg,
			     const SvxCentralTrack& seg)
{
  TClonesArray &Particle = *CentralTrack;
  SvxCentralTrackv5* pseg0 = new(Particle[iseg]) SvxCentralTrackv5();
  
  const SvxCentralTrackv5* pseg = dynamic_cast<const SvxCentralTrackv5*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxCentralTrackListv5::removeCentralTrack(const unsigned int iseg)
{
  CentralTrack->RemoveAt(iseg);
  return;
}

