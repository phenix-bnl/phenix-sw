#ifndef __PHSNGLCENTRALTRACKV19_H_
#define __PHSNGLCENTRALTRACKV19_H_

#include "PHSnglCentralTrackv17.h"

class PHSnglCentralTrackv19 : public PHSnglCentralTrackv17
{
 public:
  PHSnglCentralTrackv19();
  PHSnglCentralTrackv19(const PHSnglCentralTrack &track);  
  virtual ~PHSnglCentralTrackv19() {}

  void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_stecid       (const short val) {stecid       = val; return;}


  // Here are the very explicit "get" routines...
  short get_stecid      () const { return stecid ;}

 protected:
  short stecid   ;

  ClassDef(PHSnglCentralTrackv19,1)
};

#endif /* PHHSNGLCENTRALTRACKV19 */

