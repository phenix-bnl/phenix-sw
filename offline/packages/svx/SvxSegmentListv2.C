// ========================
// FILE: SvxSegmentListv2.C
// ========================

#include "SvxSegmentListv2.h"
#include "SvxSegmentv2.h"
#include <iostream>

ClassImp(SvxSegmentListv2)

using namespace std;

#define SVXNSEGMENT 4000

SvxSegmentListv2::SvxSegmentListv2()
{
  Segment = new TClonesArray("SvxSegmentv2",SVXNSEGMENT);
  vertex[0]=-9999.;
  vertex[1]=-9999.;
  vertex[2]=-9999.;
}

SvxSegmentListv2::SvxSegmentListv2(const SvxSegmentListv2& rhs)
{
  Segment = 0;
  rhs.copyto(*this);
}

SvxSegmentListv2& SvxSegmentListv2::operator=(const SvxSegmentListv2& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void SvxSegmentListv2::copyto(SvxSegmentListv2& dest) const
{
  delete dest.Segment;
  dest.Segment = new TClonesArray("SvxSegmentv2",get_nSegments());
  double vx = getVertex(0);
  double vy = getVertex(1);
  double vz = getVertex(2);
  dest.setVertex(vx,vy,vz);
  for ( int i = 0; i < get_nSegments(); ++i ) 
    {
      SvxSegmentv2* src = static_cast<SvxSegmentv2*>(get_segment(i));
      if ( src ) 
	{
	  dest.AddSegment(i);
	  SvxSegmentv2* d = static_cast<SvxSegmentv2*>(dest.get_segment(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl; /// \todo proper error handling
	}
    }
}

SvxSegmentListv2* SvxSegmentListv2::clone() const
{
  return new SvxSegmentListv2(*this);
}

SvxSegmentListv2::~SvxSegmentListv2()
{
  Segment->Delete();
  delete Segment;
}

SvxSegmentv2* SvxSegmentListv2::get_segment (const unsigned int iseg) const
{
  SvxSegmentv2 *Particle = (SvxSegmentv2 *) GetSegment()->UncheckedAt(iseg);
  return Particle;
}

void SvxSegmentListv2::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentListv2 Object\n"
     << "No of Segments: " << get_nSegments() << std::endl;
  return;
}

void SvxSegmentListv2::Reset()
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

int SvxSegmentListv2::isValid() const
{
  return((get_nSegments()>0) ? 1 : 0);
}

int SvxSegmentListv2::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNSEGMENT)
    {
      Segment->Expand(nseg);
    }
  return nseg;
}

void SvxSegmentListv2::AddSegment(const unsigned int iseg)
{
  TClonesArray &Particle = *Segment;
  new(Particle[iseg]) SvxSegmentv2();
  return;
}

SvxSegmentv2* SvxSegmentListv2::AddSegment(const unsigned int iseg,
					   const SvxSegment& seg)
{
  TClonesArray &Particle = *Segment;
  SvxSegmentv2* pseg0 = new(Particle[iseg]) SvxSegmentv2();
  
  const SvxSegmentv2* pseg = dynamic_cast<const SvxSegmentv2*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}

void  SvxSegmentListv2::RemoveSegment(const unsigned int iseg)
{
  Segment->RemoveAt(iseg);
  return;
}

