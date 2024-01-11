// ========================
// FILE: SvxSegmentListv1.h
// ========================

#ifndef __SVXSEGMENTLISTV1_H
#define __SVXSEGMENTLISTV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxSegmentList.h"
#include "SvxSegmentv1.h"

class SvxSegmentListv1 : public SvxSegmentList
{

 public:

  SvxSegmentListv1();
  SvxSegmentListv1(const SvxSegmentListv1&);
  SvxSegmentListv1& operator=(const SvxSegmentListv1&);
  virtual ~SvxSegmentListv1();

  SvxSegmentListv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Implementations of the set/get methods...
  int  get_nSegments () const {return Segment->GetEntries();}

  // Routines to manipulate the cluster array...
  int set_TClonesArraySize(const unsigned int nseg);
  void AddSegment          (const unsigned int iseg);
  void RemoveSegment       (const unsigned int iseg);
  SvxSegmentv1* AddSegment (const unsigned int iseg, 
			    const SvxSegment& seg);
  SvxSegmentv1* get_segment(const unsigned int iseg) const;

  void setVertex(const double vx, const double vy, const double vz) {vertex[0]=vx; vertex[1]=vy; vertex[2]=vz;}
  double getVertex(int coor) const {return vertex[coor];}

 protected:

  TClonesArray *GetSegment() const {return Segment;}
  TClonesArray *Segment;
  double vertex[3];

private:
  void copyto(SvxSegmentListv1& dest) const;

  ClassDef(SvxSegmentListv1,1)

};

#endif /* __SVXSEGMENTLISTV1_H */

