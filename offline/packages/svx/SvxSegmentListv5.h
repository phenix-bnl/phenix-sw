// ========================
// FILE: SvxSegmentListv5.h
// ========================

#ifndef __SVXSEGMENTLISTV5_H
#define __SVXSEGMENTLISTV5_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxSegmentList.h"
#include "SvxSegmentv5.h"

class SvxSegmentListv5 : public SvxSegmentList
{

 public:

  SvxSegmentListv5();
  SvxSegmentListv5(const SvxSegmentListv5&);
  SvxSegmentListv5& operator=(const SvxSegmentListv5&);
  virtual ~SvxSegmentListv5();

  SvxSegmentListv5* clone() const;

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
  SvxSegmentv5* AddSegment (const unsigned int iseg, 
			    const SvxSegment& seg);
  SvxSegmentv5* get_segment(const unsigned int iseg) const;

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
  void copyto(SvxSegmentListv5& dest) const;
  
  ClassDef(SvxSegmentListv5,1)
    
};

#endif /* __SVXSEGMENTLISTV5_H */

