#ifndef __PHFRAME_H__
#define __PHFRAME_H__

// Created by:  Jane M. Burward-Hoy and Federica Messer
// Purpose: A reference frame.
// Adapted from Jeff Mitchell's luxFrame.hh and luxFrame.cc

#include "phool.h"
#include "PHPoint.h"
#include "PHVector.h"

class PHFrame
{
public:
  PHFrame();
  virtual ~PHFrame() {}

  PHFrame(const PHPoint&, const PHVector &, const PHVector &);
  PHFrame(const PHPoint&, const PHVector &, 
	  const PHVector &, const PHVector &);
  
  void print() const;
  
  PHPoint  getOrigin() const { return origin;} 
  PHVector getU() const { return u; }
  PHVector getV() const { return v; }
  PHVector getW() const { return w; }
 
  void setOrigin(PHPoint p) { origin = p; }

private:
  PHBoolean checkOrthogonal() const;

 protected:
  PHPoint origin;
  PHVector u, v, w;
};

#endif /* __PHFRAME_H__ */
