
#ifndef __ACCSNGLRAW_H_
#define __ACCSNGLRAW_H_

#include <iostream>
#include "PHObject.h"
#include "phool.h"

class AccSnglRaw : public PHObject
{

 public:
  AccSnglRaw() {}
  virtual ~AccSnglRaw() {}  

  // set the values in the SnglRaw
  virtual void set_boxid(const int val)                {warning("boxid");}
  virtual void set_adc(const int i, const int val)     {warning("adc");}
  virtual void set_tdc(const int i, const int val)     {warning("tdc");}
  virtual void set_adcpost(const int i, const int val) {warning("adcpost");}
  virtual void set_adcpre(const int i, const int val)  {warning("adcpre");}

  // get the values from the SnglRaw
  virtual int get_boxid()          const     {warning("boxid"); return -9999;}
  virtual int get_adc(const int i) const     {warning("adc"); return -9999;}
  virtual int get_tdc(const int i) const     {warning("tdc"); return -9999;}
  virtual int get_adcpost(const int i) const {warning("adcpost"); return -9999;} 
  virtual int get_adcpre(const int i) const  {warning("adcpost"); return -9999;} 

 private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout <<"Single ACC RAW Offending field == " << field << std::endl;
  }

  ClassDef(AccSnglRaw,1)
};

#endif
