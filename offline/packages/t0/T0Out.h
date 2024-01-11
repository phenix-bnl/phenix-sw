#ifndef __T0OUT_H
#define __T0OUT_H

#include <phool.h>
#include <PHObject.h>
#include <T0Returncodes.h>
#include <iostream>

#define T0_VIRTUAL_WARNING std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl

class T0Out: public PHObject
{
 public:
  virtual ~T0Out() {}
  
  virtual int isValid(void) const 
    {
      std::cout << "T0Out: isValid() not implemented" << std::endl;
      return 0;
    }

  virtual void identify(std::ostream& os = std::cout) const 
    {
      os << "virtual T0Out object" << std::endl;
      return;
    }

  virtual void FillFromClass(T0Out *t0out) {T0_VIRTUAL_WARNING; return;}

  virtual int set_BbcT0(const float t0, const float t0err) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual int set_BbcT0(const float t0) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual float get_BbcT0() const {T0_VIRTUAL_WARNING; return T0_INVALID_FLOAT;}

  virtual int set_NtcT0(const float t0, const float t0err) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual int set_NtcT0(const float t0) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual float get_NtcT0() const {T0_VIRTUAL_WARNING; return T0_INVALID_FLOAT;}

  virtual int set_NtcpT0(const float t0, const float t0err) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual int set_NtcpT0(const float t0) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual float get_NtcpT0() const {T0_VIRTUAL_WARNING; return T0_INVALID_FLOAT;}

  virtual int set_TzrT0(const float t0, const float t0err) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual int set_TzrT0(const float t0) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual float get_TzrT0() const {T0_VIRTUAL_WARNING; return T0_INVALID_FLOAT;}

  virtual int set_ZdcT0(const float t0, const float t0err) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual int set_ZdcT0(const float t0) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual float get_ZdcT0() const {T0_VIRTUAL_WARNING; return T0_INVALID_FLOAT;}

  virtual int set_FkeT0(const float t0, const float t0err) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual int set_FkeT0(const float t0) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual float get_FkeT0() const {T0_VIRTUAL_WARNING; return T0_INVALID_FLOAT;}

  virtual int set_T0(const float t0) {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}
  virtual float get_T0() const {T0_VIRTUAL_WARNING; return T0_INVALID_FLOAT;}
  virtual float get_T0Error() const {T0_VIRTUAL_WARNING; return T0_INVALID_FLOAT;}

  virtual int get_T0List() const {T0_VIRTUAL_WARNING; return T0_INVALID_INT;}

  virtual bool isBbcT0() const {T0_VIRTUAL_WARNING; return false;}
  virtual bool isZdcT0() const {T0_VIRTUAL_WARNING; return false;}
  virtual bool isTzrT0() const {T0_VIRTUAL_WARNING; return false;}
  virtual bool isNtcT0() const {T0_VIRTUAL_WARNING; return false;}
  virtual bool isNtcpT0() const {T0_VIRTUAL_WARNING; return false;}
  virtual bool isFkeT0() const {T0_VIRTUAL_WARNING; return false;}

  virtual const char *which_t0() const {T0_VIRTUAL_WARNING; return "NONE";} 

  ClassDef(T0Out,1)

};

#undef T0_VIRTUAL_WARNING

#endif
