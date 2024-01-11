/*
  virtual Zdc DCM class
*/
#ifndef __ZDCDCM_H
#define __ZDCDCM_H

#include <PHObject.h>
#include "ZdcReturncodes.h"

class ZdcDCM : public PHObject //++CINT
{
 public:
  ZdcDCM() { }
  virtual ~ZdcDCM(){ }

  virtual long get_nWord(void) const {return INVALID_LONG;}
  virtual long get_scheme(void) const {return INVALID_LONG;}
  virtual long get_packetID(void) const {return INVALID_LONG;}
  virtual long get_DCM(short iword) const {return INVALID_LONG;}
  virtual long get_adc(int ch) const {return INVALID_LONG;}
  virtual long get_tdc0(int ch) const {return INVALID_LONG;}
  virtual long get_tdc1(int ch) const {return INVALID_LONG;}

  virtual short set_nWord(long ival)  {return INVALID_SHORT;}
  virtual short set_scheme(long ival)  {return INVALID_SHORT;}
  virtual short set_packetID(long ival) {return INVALID_SHORT;}
  virtual short set_DCM(short iword, long ival) {return INVALID_SHORT;}
  virtual short set_DCM(long *ival) {return INVALID_SHORT;}

  ClassDef(ZdcDCM,1)
};


#endif
