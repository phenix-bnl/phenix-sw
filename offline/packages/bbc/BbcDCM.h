/*
  virtual Bbc DCM class
*/
#ifndef BBCDCM_H
#define BBCDCM_H

#include <iostream.h>
#include <PHObject.h>
#include "BbcReturncodes.h"

class BbcDCM : public PHObject //++CINT
{
 public:
  BbcDCM() { }
  virtual ~BbcDCM(){ }

  virtual unsigned long get_nWord(void) const {return INVALID_ULONG;}
  virtual unsigned long get_scheme(void) const {return INVALID_ULONG;}
  virtual unsigned long get_packetID(void) const {return INVALID_ULONG;}
  virtual unsigned long get_DCM(short iword) const {return INVALID_ULONG;}

  virtual short set_nWord(long ival)  {return INVALID_SHORT;}
  virtual short set_scheme(long ival)  {return INVALID_SHORT;}
  virtual short set_packetID(long ival) {return INVALID_SHORT;}
  virtual short set_DCM(short iword, long ival) {return INVALID_SHORT;}
  virtual short set_DCM(long *ival) {return INVALID_SHORT;}

  virtual void identify(ostream&  = cout) const = 0;


};


#endif
