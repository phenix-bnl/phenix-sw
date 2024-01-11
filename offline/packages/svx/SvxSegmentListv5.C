// ========================
// FILE: SvxSegmentListv5.C
// ========================

#include "SvxSegmentListv5.h"
#include "SvxSegmentv5.h"
#include <iostream>

ClassImp(SvxSegmentListv5)

using namespace std;

#define SVXNSEGMENT 4000

SvxSegmentListv5::SvxSegmentListv5()
{
  Segment = new TClonesArray("SvxSegmentv5",SVXNSEGMENT);
  vertex[0]=-9999.;
  vertex[1]=-9999.;
  vertex[2]=-9999.;
}

SvxSegmentListv5::SvxSegmentListv5(const SvxSegmentListv5& rhs)
{
  Segment = 0;
  rhs.copyto(*this);
}

SvxSegmentListv5& SvxSegmentListv5::operator=(const SvxSegmentListv5& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void SvxSegmentListv5::copyto(SvxSegmentListv5& dest) const
{
  delete dest.Segment;
  dest.Segment = new TClonesArray("SvxSegmentv5",get_nSegments());
  double vx = getVertex(0);
  double vy = getVertex(1);
  double vz = getVertex(2);
  dest.setVertex(vx,vy,vz);
  for ( int i = 0; i < get_nSegments(); ++i ) 
    {
      SvxSegmentv5* src = static_cast<SvxSegmentv5*>(get_segment(i));
      if ( src ) 
	{
	  dest.AddSegment(i);
	  SvxSegmentv5* d = static_cast<SvxSegmentv5*>(dest.get_segment(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl; /// \todo proper error handling
	}
    }
}

SvxSegmentListv5* SvxSegmentListv5::clone() const
{
  return new SvxSegmentListv5(*this);
}

SvxSegmentListv5::~SvxSegmentListv5()
{
  Segment->Delete();
  delete Segment;
}

SvxSegmentv5* SvxSegmentListv5::get_segment (const unsigned int iseg) const
{
  SvxSegmentv5 *Particle = (SvxSegmentv5 *) GetSegment()->UncheckedAt(iseg);
  return Particle;
}

void SvxSegmentListv5::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentListv5 Object\n"
     << "No of Segments: " << get_nSegments() << std::endl;
  return;
}

void SvxSegmentListv5::Reset()
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

int SvxSegmentListv5::isValid() const
{
  return((get_nSegments()>0) ? 1 : 0);
}

int SvxSegmentListv5::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNSEGMENT)
    {
      Segment->Expand(nseg);
    }
  return nseg;
}

void SvxSegmentListv5::AddSegment(const unsigned int iseg)
{
  TClonesArray &Particle = *Segment;
  new(Particle[iseg]) SvxSegmentv5();
  return;
}

SvxSegmentv5* SvxSegmentListv5::AddSegment(const unsigned int iseg,
					   const SvxSegment& seg)
{
  TClonesArray &Particle = *Segment;
  SvxSegmentv5* pseg0 = new(Particle[iseg]) SvxSegmentv5();
  
  const SvxSegmentv5* pseg = dynamic_cast<const SvxSegmentv5*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}

void  SvxSegmentListv5::RemoveSegment(const unsigned int iseg)
{
  Segment->RemoveAt(iseg);
  return;
}

