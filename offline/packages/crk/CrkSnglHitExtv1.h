#ifndef __CRKSNGLHITEXTV1_H__
#define __CRKSNGLHITEXTV1_H__

#include "CrkSnglHitExt.h"

class CrkSnglHitExtv1 : public CrkSnglHitExt
{
 public:
  CrkSnglHitExtv1();  
  virtual ~CrkSnglHitExtv1() {}

  short get_pmt() const {return pmt;}
  void set_pmt(const short ival) {pmt = ival; return;}

  float get_npe() const {return npe;}
  void set_npe(const float rval) {npe = rval; return;}

  float get_time() const {return time;}
  void set_time(const float rval) {time = rval; return;}

  float get_posX() const {return posX;}
  void set_posX(const float rval) {posX = rval; return;}

  float get_posY() const {return posY;}
  void set_posY(const float rval) {posY = rval; return;}

  float get_posZ() const {return posZ;}
  void set_posZ(const float rval) {posZ = rval; return;}

 protected:
  short pmt;
  float npe;
  float time;
  float posX,posY,posZ;

  ClassDef(CrkSnglHitExtv1,1)
};

#endif /* __CRKSNGLHITEXTV1_H__ */
