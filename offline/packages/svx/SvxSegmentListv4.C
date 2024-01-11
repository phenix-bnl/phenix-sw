// ========================
// FILE: SvxSegmentListv4.C
// ========================

#include "SvxSegmentListv4.h"
#include "SvxSegmentv4.h"
#include <iostream>

ClassImp(SvxSegmentListv4)

using namespace std;

#define SVXNSEGMENT 4000

SvxSegmentListv4::SvxSegmentListv4()
{
  Segment = new TClonesArray("SvxSegmentv4",SVXNSEGMENT);
  vertex[0]=-9999.;
  vertex[1]=-9999.;
  vertex[2]=-9999.;
  recomode = true;
}

SvxSegmentListv4::SvxSegmentListv4(const SvxSegmentListv4& rhs)
{
  Segment = 0;
  rhs.copyto(*this);
}

SvxSegmentListv4& SvxSegmentListv4::operator=(const SvxSegmentListv4& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void SvxSegmentListv4::copyto(SvxSegmentListv4& dest) const
{
  delete dest.Segment;
  dest.Segment = new TClonesArray("SvxSegmentv4",get_nSegments());
  double vx = getVertex(0);
  double vy = getVertex(1);
  double vz = getVertex(2);
  dest.setVertex(vx,vy,vz);
  for ( int i = 0; i < get_nSegments(); ++i ) 
    {
      SvxSegmentv4* src = static_cast<SvxSegmentv4*>(get_segment(i));
      if ( src ) 
	{
	  dest.AddSegment(i);
	  SvxSegmentv4* d = static_cast<SvxSegmentv4*>(dest.get_segment(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl; /// \todo proper error handling
	}
    }
}

SvxSegmentListv4* SvxSegmentListv4::clone() const
{
  return new SvxSegmentListv4(*this);
}

SvxSegmentListv4::~SvxSegmentListv4()
{
  Segment->Delete();
  delete Segment;
}

SvxSegmentv4* SvxSegmentListv4::get_segment (const unsigned int iseg) const
{
  SvxSegmentv4 *Particle = (SvxSegmentv4 *) GetSegment()->UncheckedAt(iseg);
  return Particle;
}

void SvxSegmentListv4::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentListv4 Object\n"
     << "No of Segments: " << get_nSegments() << std::endl;
  return;
}

void SvxSegmentListv4::Reset()
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

int SvxSegmentListv4::isValid() const
{
  return((get_nSegments()>0) ? 1 : 0);
}

int SvxSegmentListv4::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNSEGMENT)
    {
      Segment->Expand(nseg);
    }
  return nseg;
}

void SvxSegmentListv4::AddSegment(const unsigned int iseg)
{
  TClonesArray &Particle = *Segment;
  new(Particle[iseg]) SvxSegmentv4();
  return;
}

SvxSegmentv4* SvxSegmentListv4::AddSegment(const unsigned int iseg,
					   const SvxSegment& seg)
{
  TClonesArray &Particle = *Segment;
  SvxSegmentv4* pseg0 = new(Particle[iseg]) SvxSegmentv4();
  
  const SvxSegmentv4* pseg = dynamic_cast<const SvxSegmentv4*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}

void  SvxSegmentListv4::RemoveSegment(const unsigned int iseg)
{
  Segment->RemoveAt(iseg);
  return;
}

