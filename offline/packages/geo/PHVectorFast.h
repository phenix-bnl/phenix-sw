#ifndef __PHVECTORFAST_H__
#define __PHVECTORFAST_H__

/*
  Creator : Tim Miller
  Purpose : A wrapper around PHVector to store the length and lengthSqr
            values rather than have to calculate them multiple times
	    during the course of event analysis
*/

#include "PHVector.h"

class PHVectorFast : public PHVector {

 public:

  PHVectorFast() : PHVector() {}
  PHVectorFast(const double x, const double y, const double z) : PHVector(x,y,z) {}
  PHVectorFast(const PHPoint &pnt) : PHVector(pnt) {}
  virtual ~PHVectorFast() {}

  /*  code I think will make everything transparent???  */
  PHVectorFast(const PHVector &vect) : PHVector(vect) {}

  /*  new specific code for this wrapper */  
    
  double length() const { return dl; }
  double lengthSqr() const { return dlSqr; }

  void setX(const double x) { PHVector::setX(x); calcLength(); }
  void setY(const double y) { PHVector::setY(y); calcLength(); }
  void setZ(const double z) { PHVector::setZ(z); calcLength(); }

  void setXYZ(const double x, const double y, const double z) { 
    PHVector::setXYZ(x,y,z); calcLength();
  }

 protected:
  double dl, dlSqr;

  void calcLength();

};

#endif /* __PHVECTORFAST_H__ */






