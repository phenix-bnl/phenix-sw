// ========================
// FILE: SvxSegmentListv4.h
// ========================

#ifndef __SVXSEGMENTLISTV4_H
#define __SVXSEGMENTLISTV4_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxSegmentList.h"
#include "SvxSegmentv4.h"

class SvxSegmentListv4 : public SvxSegmentList
{

 public:

  SvxSegmentListv4();
  SvxSegmentListv4(const SvxSegmentListv4&);
  SvxSegmentListv4& operator=(const SvxSegmentListv4&);
  virtual ~SvxSegmentListv4();

  SvxSegmentListv4* clone() const;

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
  SvxSegmentv4* AddSegment (const unsigned int iseg, 
			    const SvxSegment& seg);
  SvxSegmentv4* get_segment(const unsigned int iseg) const;

  void setVertex(const double vx, const double vy, const double vz) {
    vertex[0] = vx;
    vertex[1] = vy;
    vertex[2] = vz;
  }
  double getVertex(int coor) const {return vertex[coor];}
  void setRecoMode(const bool flag) {
    recomode = flag;
  }
  bool getRecoMode() const {return recomode;}

 protected:

  TClonesArray *GetSegment() const {return Segment;}
  TClonesArray *Segment;
  double vertex[3];
  bool recomode;
  // recomode=true  : default parameters are used for pattern recognition
  // recomode=false : loose parameters are used for pattern recognition

private:
  void copyto(SvxSegmentListv4& dest) const;

  ClassDef(SvxSegmentListv4,1)

};

#endif /* __SVXSEGMENTLISTV4_H */

