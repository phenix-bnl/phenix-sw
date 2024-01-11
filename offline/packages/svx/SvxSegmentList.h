// ======================
// FILE: SvxSegmentList.h
// ======================

#ifndef __SVXSEGMENTLIST_HH_
#define __SVXSEGMENTLIST_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

// Container for the SvxSegments.
// This contains a list of all SvxSegments in an event.
// 
// Created by Jeffery Mitchell on 9/11/03.
//


class SvxSegment;

class SvxSegmentList : public PHObject
{

 public:
  virtual ~SvxSegmentList() {}
  virtual int  get_nSegments () const 
    {
      std::cout << "SvxSegmentList::Error get_nSegments not overridden" << std::endl;
      return 0;
    }

  // Set data members
  virtual int  set_TClonesArraySize(const unsigned int nseg) {return 0;}
  virtual void AddSegment       (const unsigned int iseg) {return;}
  virtual void RemoveSegment    (const unsigned int iseg) {return;}
  virtual SvxSegment* AddSegment(const unsigned int iseg, 
				 const SvxSegment &segment) {return NULL;}

  // Get data members
  virtual SvxSegment* get_segment(const unsigned int iseg) const 
    {
      std::cout << "Single segment return not implemented for your version of segment list" << std::endl;
      return 0;
    }

  virtual SvxSegmentList* clone() const
    {
      std::cout << "Clone method not implemented for your version of SvxSegmentList" << std::endl;
      return 0;
    }


  // Standard functions of all inheritors of PHObject classes...
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual SvxSegmentList object" << std::endl;
    return;
  }

     virtual void setVertex(const double vx, const double vy, const double vz) {std::cout << PHWHERE << "ERROR: setVertex() not implemented by daughter function" << std::endl;}
     virtual double getVertex(int coor) const {std::cout << PHWHERE << "ERROR: getVertex() not implemented by daughter function" << std::endl; return -9999.;}
     virtual void setRecoMode(const bool flag) {std::cout << PHWHERE << "ERROR: setRecoMode() not implemented by daughter function" << std::endl;}
     virtual bool getRecoMode() const {std::cout << PHWHERE << "ERROR: setRecoMode() not implemented by daughter function" << std::endl; return false;}
  ClassDef(SvxSegmentList,1)

};
#endif /* __SVXSEGMENTLIST_HH_ */
