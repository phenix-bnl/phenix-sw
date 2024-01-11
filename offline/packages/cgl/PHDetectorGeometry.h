#ifndef __PHDETECTORGEOMETRY_H__
#define __PHDETECTORGEOMETRY_H__


#include "PHPanel.h"

#include <vector>

class PHCompositeNode;

///
class PHDetectorGeometry
{
 public:

  static PHDetectorGeometry *instance();
  /// dtor
  virtual ~PHDetectorGeometry() {}

  int copycglDetectorGeo(PHCompositeNode *topNode);
  int readDetectorGeometry(PHCompositeNode *topNode);
  void Print() const;

  const std::vector<PHPanel> *GetGeo(const int iarm, const int idet) const {return &panel[iarm][idet];}

  enum {PC1,PC2,PC3,TECIN,TECOUT,TOF,PBGL,PBSC,ACC,TOFW,SVXPIXEL,SVXSTRIP1,SVXSTRIP2,SVXSTRIP3,HBD,LAST}; // leave LAST at the end, that determines the size of the phpanel array

 protected:
  PHDetectorGeometry();
  static PHDetectorGeometry *__instance;
  int initialized;

  std::vector <PHPanel> panel[2][LAST];

};

#endif // __PHDETECTORGEOMETRY_H__
