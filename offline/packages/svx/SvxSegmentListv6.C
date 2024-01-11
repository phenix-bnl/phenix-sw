// ========================
// FILE: SvxSegmentListv6.C
// ========================

#include "SvxSegmentListv6.h"
#include "SvxSegmentv6.h"
#include <iostream>

ClassImp(SvxSegmentListv6)

using namespace std;

#define SVXNSEGMENT 4000

SvxSegmentListv6::SvxSegmentListv6()
{
  Segment = new TClonesArray("SvxSegmentv6",SVXNSEGMENT);
  vertex[0]=-9999.;
  vertex[1]=-9999.;
  vertex[2]=-9999.;
}

SvxSegmentListv6::SvxSegmentListv6(const SvxSegmentListv6& rhs)
{
  Segment = 0;
  rhs.copyto(*this);
}

SvxSegmentListv6& SvxSegmentListv6::operator=(const SvxSegmentListv6& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void SvxSegmentListv6::copyto(SvxSegmentListv6& dest) const
{
  delete dest.Segment;
  dest.Segment = new TClonesArray("SvxSegmentv6",get_nSegments());
  double vx = getVertex(0);
  double vy = getVertex(1);
  double vz = getVertex(2);
  dest.setVertex(vx,vy,vz);
  for ( int i = 0; i < get_nSegments(); ++i ) 
    {
      SvxSegmentv6* src = static_cast<SvxSegmentv6*>(get_segment(i));
      if ( src ) 
	{
	  dest.AddSegment(i);
	  SvxSegmentv6* d = static_cast<SvxSegmentv6*>(dest.get_segment(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl; /// \todo proper error handling
	}
    }
}

SvxSegmentListv6* SvxSegmentListv6::clone() const
{
  return new SvxSegmentListv6(*this);
}

SvxSegmentListv6::~SvxSegmentListv6()
{
  Segment->Delete();
  delete Segment;
}

SvxSegmentv6* SvxSegmentListv6::get_segment (const unsigned int iseg) const
{
  SvxSegmentv6 *Particle = (SvxSegmentv6 *) GetSegment()->UncheckedAt(iseg);
  return Particle;
}

void SvxSegmentListv6::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentListv6 Object\n"
     << "No of Segments: " << get_nSegments() << std::endl;
  return;
}

void SvxSegmentListv6::Reset()
{
  Segment->Clear();
  if (get_nSegments()>SVXNSEGMENT)
    {
      Segment->Expand(SVXNSEGMENT);
    }
  vertex[0]=-9999.;
  vertex[1]=-9999.;
  vertex[2]=-9999.;
  return;
}

int SvxSegmentListv6::isValid() const
{
  return((get_nSegments()>0) ? 1 : 0);
}

int SvxSegmentListv6::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNSEGMENT)
    {
      Segment->Expand(nseg);
    }
  return nseg;
}

void SvxSegmentListv6::AddSegment(const unsigned int iseg)
{
  TClonesArray &Particle = *Segment;
  new(Particle[iseg]) SvxSegmentv6();
  return;
}

SvxSegmentv6* SvxSegmentListv6::AddSegment(const unsigned int iseg,
					   const SvxSegment& seg)
{
  TClonesArray &Particle = *Segment;
  SvxSegmentv6* pseg0 = new(Particle[iseg]) SvxSegmentv6();
  
  const SvxSegmentv6* pseg = dynamic_cast<const SvxSegmentv6*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}

void  SvxSegmentListv6::RemoveSegment(const unsigned int iseg)
{
  Segment->RemoveAt(iseg);
  return;
}

