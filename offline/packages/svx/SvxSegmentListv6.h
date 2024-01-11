// ========================
// FILE: SvxSegmentListv6.h
// ========================
//
// v6 implemented by D. McGlinchey 10/7/2013
//
#ifndef __SVXSEGMENTLISTV6_H
#define __SVXSEGMENTLISTV6_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxSegmentList.h"
#include "SvxSegmentv6.h"

class SvxSegmentListv6 : public SvxSegmentList
{

 public:

  SvxSegmentListv6();
  SvxSegmentListv6(const SvxSegmentListv6&);
  SvxSegmentListv6& operator=(const SvxSegmentListv6&);
  virtual ~SvxSegmentListv6();

  SvxSegmentListv6* clone() const;

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
  SvxSegmentv6* AddSegment (const unsigned int iseg, 
			    const SvxSegment& seg);
  SvxSegmentv6* get_segment(const unsigned int iseg) const;

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
  void copyto(SvxSegmentListv6& dest) const;
  
  ClassDef(SvxSegmentListv6,1)
    
};

#endif /* __SVXSEGMENTLISTV6_H */

