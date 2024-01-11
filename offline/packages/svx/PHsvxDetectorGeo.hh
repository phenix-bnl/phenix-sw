//--------------------------------------------------- 
// Class: PHsvxDetectorGeo
//--------------------------------------------------- 

#ifndef __PHSVXDETECTORGEO_HH__
#define __PHSVXDETECTORGEO_HH__

#include "PHObject.h"
#include "svxDetectorGeo.hh"

/**
 * @brief  [NOT USED YET] ROOT access for the svxDetectorGeo class.
 *
 * This class provides ROOT access functionality to data nodes
 * for the svxDetectorGeo class.
 *
 * @date  Last update: 3/28/00, Created by Jeffery T. Mitchell
 */
class PHsvxDetectorGeo : public PHObject, public svxDetectorGeo
{ 

public:
  PHsvxDetectorGeo();                             // constructor
  ~PHsvxDetectorGeo();                            // destructor
  
}; 

#endif /* __PHSVXDETECTORGEO_HH__ */
