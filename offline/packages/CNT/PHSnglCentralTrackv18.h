#ifndef __PHSNGLCENTRALTRACKV18_H_
#define __PHSNGLCENTRALTRACKV18_H_

#include "PHSnglCentralTrackv16.h"

class PHSnglCentralTrackv18 : public PHSnglCentralTrackv16
{
 public:
  PHSnglCentralTrackv18();
  PHSnglCentralTrackv18(const PHSnglCentralTrack &track);  
  virtual ~PHSnglCentralTrackv18() {}

  void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_stecid       (const short val) {stecid       = val; return;}


  // Here are the very explicit "get" routines...
  short get_stecid      () const { return stecid ;}

 protected:
  short stecid   ;

  ClassDef(PHSnglCentralTrackv18,1)
};

#endif /* PHHSNGLCENTRALTRACKV18 */

