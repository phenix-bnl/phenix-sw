// ========================
// FILE: SvxSegmentListv1.C
// ========================

#include "SvxSegmentListv1.h"
#include "SvxSegmentv1.h"
#include <iostream>

ClassImp(SvxSegmentListv1)

using namespace std;

#define SVXNSEGMENT 4000

SvxSegmentListv1::SvxSegmentListv1()
{
  Segment = new TClonesArray("SvxSegmentv1",SVXNSEGMENT);
  vertex[0]=-9999.;
  vertex[1]=-9999.;
  vertex[2]=-9999.;
}

SvxSegmentListv1::SvxSegmentListv1(const SvxSegmentListv1& rhs)
{
  Segment = 0;
  rhs.copyto(*this);
}

SvxSegmentListv1& 
SvxSegmentListv1::operator=(const SvxSegmentListv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
SvxSegmentListv1::copyto(SvxSegmentListv1& dest) const
{
  delete dest.Segment;
  dest.Segment = new TClonesArray("SvxSegmentv1",get_nSegments());
  //dest.vertex[0] = static_cast<double>(getVertex(0));
  //dest.vertex[1] = static_cast<double>(getVertex(1));
  //dest.vertex[2] = static_cast<double>(getVertex(2));
  double vx = getVertex(0);
  double vy = getVertex(1);
  double vz = getVertex(2);
  dest.setVertex(vx,vy,vz);
  //dest.vertex[0] = this->getVertex(0);
  //dest.vertex[1] = this->getVertex(1);
  //dest.vertex[2] = this->getVertex(2);
  for ( int i = 0; i < get_nSegments(); ++i ) 
    {
      SvxSegmentv1* src = static_cast<SvxSegmentv1*>
	(get_segment(i));
      if ( src ) 
	{
	  dest.AddSegment(i);
	  SvxSegmentv1* d = static_cast<SvxSegmentv1*>
	    (dest.get_segment(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl; /// \todo proper error handling
	}
    }
}

SvxSegmentListv1* 
SvxSegmentListv1::clone() const
{
  return new SvxSegmentListv1(*this);
}

SvxSegmentListv1::~SvxSegmentListv1()
{
  Segment->Delete();
  delete Segment;
}

SvxSegmentv1* SvxSegmentListv1::get_segment (const unsigned int iseg) const {
  SvxSegmentv1 *Particle = (SvxSegmentv1 *) GetSegment()->UncheckedAt(iseg);
  return Particle;
}

void SvxSegmentListv1::identify(ostream& os) const
{
  os << "identify yourself: SvxSegmentListv1 Object\n"
     << "No of Segments: " << get_nSegments() << std::endl;
  return;
}

void SvxSegmentListv1::Reset()
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

int SvxSegmentListv1::isValid() const
{
  return((get_nSegments()>0) ? 1 : 0);
}

int SvxSegmentListv1::set_TClonesArraySize(const unsigned int nseg)
{
  if (nseg > SVXNSEGMENT)
    {
      Segment->Expand(nseg);
    }
  return nseg;
}

void  SvxSegmentListv1::AddSegment(const unsigned int iseg)
{
  TClonesArray &Particle = *Segment;
  new(Particle[iseg]) SvxSegmentv1();
  return;
}

/*
SvxSegmentv1* 
SvxSegmentListv1::AddSegment(const unsigned int iseg,
			     const SvxSegment& seg)
{
  const SvxSegmentv1* test = dynamic_cast<const SvxSegmentv1*>
    (&seg);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type SvxSegmentv1"
	   << endl;
      return 0;
    }

  return new((*Segment)[iseg]) SvxSegmentv1(*test);
}
*/

SvxSegmentv1*
SvxSegmentListv1::AddSegment(const unsigned int iseg,
			     const SvxSegment& seg)
{
  TClonesArray &Particle = *Segment;
  SvxSegmentv1* pseg0 = new(Particle[iseg]) SvxSegmentv1();
  
  const SvxSegmentv1* pseg = dynamic_cast<const SvxSegmentv1*>(&seg);
  *pseg0 = *pseg;
  
  return pseg0;
}


void  SvxSegmentListv1::RemoveSegment(const unsigned int iseg)
{
  Segment->RemoveAt(iseg);
  return;
}

