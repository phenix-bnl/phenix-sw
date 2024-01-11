#ifndef __EMC_GEAGEOMETRY_H__
#define __EMC_GEAGEOMETRY_H__





#include <vector>

#include <PHObject.h>

class emcGeaParams;

class emc_geometry_t {
public:
  int arm, sector, iy, iz, swkey; // indices
  float x, y, z;                  // x, y, z position of the center of the front face
  float theta, phi;               // theta, phi at the center of the front face
  float distance, flashtime;      // distance from vertex, tof for photon
  float sectheta, secphi;         // theta, phi in the sector (local) coordinate system.
  float ex, ey, ez;               // unit vector for the axis of the tower

  virtual ~emc_geometry_t(){}


  ClassDef(emc_geometry_t, 0)
};


class emcGeaGeometry: public PHObject, public std::vector< std::vector< std::vector< emc_geometry_t > > >{
public:
  emcGeaGeometry();
  int init(emcGeaParams * parm);
  virtual void Reset(){}


public:
  const static unsigned int wallmax = 8;
  const static unsigned int ymax = 48;
  const static unsigned int zmax = 96;

  ClassDef(emcGeaGeometry, 0)
};





#endif /* ! __EMC_GEAGEOMETRY_H__ */

