// ========================
// FILE: SvxCentralTrackListv1.C
// ========================

#include "SvxCentralTrackListv1.h"
#include "SvxCentralTrackv1.h"
#include <iostream>

ClassImp(SvxCentralTrackListv1)

using namespace std;

#define SVXNCENTRALTRACK 4000

SvxCentralTrackListv1::SvxCentralTrackListv1()
{
  CentralTrack = new TClonesArray("SvxCentralTrackv1",SVXNCENTRALTRACK);
}

SvxCentralTrackListv1::SvxCentralTrackListv1(const SvxCentralTrackListv1& rhs)
{
  CentralTrack = 0;
  rhs.copyto(*this);
}

SvxCentralTrackListv1& 
SvxCentralTrackListv1::operator=(const SvxCentralTrackListv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxCentralTrackListv1::copyto(SvxCentralTrackListv1& dest) const
{
  delete dest.CentralTrack;
  dest.CentralTrack = new TClonesArray("SvxCentralTrackv1",get_nCentralTracks());
  for ( int i = 0; i < get_nCentralTracks(); ++i ) 
    {
      SvxCentralTrackv1* src = static_cast<SvxCentralTrackv1*>(getCentralTrack(i));
      if ( src ) 
	{
	  dest.addCentralTrack(i);
	  SvxCentralTrackv1* d = static_cast<SvxCentralTrackv1*>(dest.getCentralTrack(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

SvxCentralTrackListv1* 
SvxCentralTrackListv1::clone() const
{
  return new SvxCentralTrackListv1(*this);
}

SvxCentralTrackListv1::~SvxCentralTrackListv1()
{
  CentralTrack->Delete();
  delete CentralTrack;
}

SvxCentralTrackv1* SvxCentralTrackListv1::getCentralTrack (const unsigned int iseg) const {
  SvxCentralTrackv1 *Particle = (SvxCentralTrackv1 *) getCentralTrackObject()->UncheckedAt(iseg);
  return Particle;
}

void SvxCentralTrackListv1::identify(ostream& os) const
{
  os << "identify yourself: SvxCentralTrackListv1 Object\n"
     << "No of CentralTracks: " << get_nCentralTracks() << std::endl;
  return;
}

void SvxCentralTrackListv1::Reset()
{
  CentralTrack->Clear();
  if (get_nCentralTracks()>SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(SVXNCENTRALTRACK);
    }

  return;
}

int SvxCentralTrackListv1::isValid() const
{
  return((get_nCentralTracks()>0) ? 1 : 0);
}

int SvxCentralTrackListv1::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNCENTRALTRACK)
    {
      CentralTrack->Expand(nseg);
    }
  return nseg;
}

void  SvxCentralTrackListv1::addCentralTrack(const unsigned int iseg)
{
  TClonesArray &Particle = *CentralTrack;
  new(Particle[iseg]) SvxCentralTrackv1();
  return;
}

SvxCentralTrackv1*
SvxCentralTrackListv1::addCentralTrack(const unsigned int iseg,
			     const SvxCentralTrack& seg)
{
  TClonesArray &Particle = *CentralTrack;
  SvxCentralTrackv1* pseg0 = new(Particle[iseg]) SvxCentralTrackv1();
  
  const SvxCentralTrackv1* pseg = dynamic_cast<const SvxCentralTrackv1*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxCentralTrackListv1::removeCentralTrack(const unsigned int iseg)
{
  CentralTrack->RemoveAt(iseg);
  return;
}

