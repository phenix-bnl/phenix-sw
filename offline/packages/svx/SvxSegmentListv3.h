// ========================
// FILE: SvxSegmentListv3.h
// ========================

#ifndef __SVXSEGMENTLISTV3_H
#define __SVXSEGMENTLISTV3_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxSegmentList.h"
#include "SvxSegmentv3.h"

class SvxSegmentListv3 : public SvxSegmentList
{

 public:

  SvxSegmentListv3();
  SvxSegmentListv3(const SvxSegmentListv3&);
  SvxSegmentListv3& operator=(const SvxSegmentListv3&);
  virtual ~SvxSegmentListv3();

  SvxSegmentListv3* clone() const;

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
  SvxSegmentv3* AddSegment (const unsigned int iseg, 
			    const SvxSegment& seg);
  SvxSegmentv3* get_segment(const unsigned int iseg) const;

  void setVertex(const double vx, const double vy, const double vz) {
    vertex[0] = vx;
    vertex[1] = vy;
    vertex[2] = vz;
  }
  double getVertex(int coor) const {return vertex[coor];}

 protected:

  TClonesArray *GetSegment() const {return Segment;}
  TClonesArray *Segment;
  double vertex[3];

private:
  void copyto(SvxSegmentListv3& dest) const;

  ClassDef(SvxSegmentListv3,1)

};

#endif /* __SVXSEGMENTLISTV3_H */

