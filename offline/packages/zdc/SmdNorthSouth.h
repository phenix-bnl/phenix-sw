#ifndef __SMDNORTHSOUTH_H__
#define __SMDNORTHSOUTH_H__

#include <iostream>
#include "TObject.h"

class SmdNorthSouth : public TObject
{
public:
  SmdNorthSouth() { }
  SmdNorthSouth(const float xpos, const float ypos, const float energy);
  virtual ~SmdNorthSouth() { }
  void identify(std::ostream& os = std::cout) const;

  float get_Pos(const int xy) const {
    if (xy==0)      return Xpos;
    else if (xy==1) return Ypos;
    return -9999.;
  }
  float get_Xpos() const {return Xpos;}
  float get_Ypos() const {return Ypos;}

/*
  float get_Rms(const int xy) const {
    if (xy==0)      return Xrms;
    else if (xy==1) return Yrms;
    return -9999.;
  }
  float get_Xrms() const {return Xrms;}
  float get_Yrms() const {return Yrms;}
*/
  float get_Energy() const {return Energy;}

protected:
  float Xpos;	// x energy weighted centroid
  float Ypos;	// y energy weighted centroid
  float Energy; // total energy

  ClassDef(SmdNorthSouth,1)
};

#endif	// __SMDNORTHSOUTH_H__
