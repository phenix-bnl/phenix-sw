// ========================
// FILE: SvxSegmentListv3.C
// ========================

#include "SvxSegmentListv3.h"
#include "SvxSegmentv3.h"
#include <iostream>

ClassImp(SvxSegmentListv3)

using namespace std;

#define SVXNSEGMENT 4000

SvxSegmentListv3::SvxSegmentListv3()
{
  Segment = new TClonesArray("SvxSegmentv3",SVXNSEGMENT);
  vertex[0]=-9999.;
  vertex[1]=-9999.;
  vertex[2]=-9999.;
}

SvxSegmentListv3::SvxSegmentListv3(const SvxSegmentListv3& rhs)
{
  Segment = 0;
  rhs.copyto(*this);
}

SvxSegmentListv3& SvxSegmentListv3::operator=(const SvxSegmentListv3& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void SvxSegmentListv3::copyto(SvxSegmentListv3& dest) const
{
  delete dest.Segment;
  dest.Segment = new TClonesArray("SvxSegmentv3",get_nSegments());
  double vx = getVertex(0);
  double vy = getVertex(1);
  double vz = getVertex(2);
  dest.setVertex(vx,vy,vz);
  for ( int i = 0; i < get_nSegments(); ++i ) 
    {
      SvxSegmentv3* src = static_cast<SvxSegmentv3*>(get_segment(i));
      if ( src ) 
	{
	  dest.AddSegment(i);
	  SvxSegmentv3* d = static_cast<SvxSegmentv3*>(dest.get_segment(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl; /// \todo proper error handling
	}
    }
}

SvxSegmentListv3* SvxSegmentListv3::clone() const
{
  return new SvxSegmentListv3(*this);
}

SvxSegmentListv3::~SvxSegmentListv3()
{
  Segment->Delete();
  delete Segment;
}

SvxSegmentv3* SvxSegmentListv3::get_segment (const unsigned int iseg) const
{
  SvxSegmentv3 *Particle = (SvxSegmentv3 *) GetSegment()->UncheckedAt(iseg);
  return Particle;
}

void SvxSegmentListv3::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentListv3 Object\n"
     << "No of Segments: " << get_nSegments() << std::endl;
  return;
}

void SvxSegmentListv3::Reset()
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

int SvxSegmentListv3::isValid() const
{
  return((get_nSegments()>0) ? 1 : 0);
}

int SvxSegmentListv3::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNSEGMENT)
    {
      Segment->Expand(nseg);
    }
  return nseg;
}

void SvxSegmentListv3::AddSegment(const unsigned int iseg)
{
  TClonesArray &Particle = *Segment;
  new(Particle[iseg]) SvxSegmentv3();
  return;
}

SvxSegmentv3* SvxSegmentListv3::AddSegment(const unsigned int iseg,
					   const SvxSegment& seg)
{
  TClonesArray &Particle = *Segment;
  SvxSegmentv3* pseg0 = new(Particle[iseg]) SvxSegmentv3();
  
  const SvxSegmentv3* pseg = dynamic_cast<const SvxSegmentv3*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}

void  SvxSegmentListv3::RemoveSegment(const unsigned int iseg)
{
  Segment->RemoveAt(iseg);
  return;
}

