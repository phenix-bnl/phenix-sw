#ifndef __DETECTORGEOMETRY_H__
#define __DETECTORGEOMETRY_H__

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class PHPanel;
class TClonesArray;
///
class DetectorGeometry: public PHObject
{
 public:

  /// dtor
  virtual ~DetectorGeometry() {}

  /// Clear Event
  virtual void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& os = std::cout) const;

  /// isValid returns non zero if object contains valid data
  virtual int isValid() const;

  virtual int AddPanel(const PHPanel *panel, const int iarm, const int ipanel, const char *det) {return -1;}

  virtual TClonesArray *GetPHPanelTCArray() const {return NULL;}

 private: // prevent doc++ from showing ClassDef
  ClassDef(DetectorGeometry,1)

};

#endif // __DETECTORGEOMETRY_H__
