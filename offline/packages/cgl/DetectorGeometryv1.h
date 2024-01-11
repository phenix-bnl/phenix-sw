#ifndef __DETECTORGEOMETRYV1_H__
#define __DETECTORGEOMETRYV1_H__

#include "DetectorGeometry.h"

#include <iostream>

class PHPanel;
class TClonesArray;
///
class DetectorGeometryv1: public DetectorGeometry
{
 public:

  DetectorGeometryv1();
  /// dtor
  virtual ~DetectorGeometryv1();

  /// Clear Event
  void Reset() {return;}

  /** identify Function from PHObject
      @param os Output Stream 
   */
  void identify(std::ostream& os = std::cout) const;

  /// isValid returns non zero if object contains valid data
  int isValid() const;

  int AddPanel(const PHPanel *panel, const int iarm, const int ipanel, const char *det);

  TClonesArray *GetPHPanelTCArray() const {return SvPHPanel;}

 protected: // prevent doc++ from showing ClassDef
  int nPanel;
  TClonesArray *SvPHPanel;

 private:
  ClassDef(DetectorGeometryv1,1)

};

#endif // __DETECTORGEOMETRYV1_H__
