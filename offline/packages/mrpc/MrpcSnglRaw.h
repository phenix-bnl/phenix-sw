#ifndef __MRPCSNGLRAW_H_
#define __MRPCSNGLRAW_H_

#include <iostream>
#include <PHObject.h>
#include <phool.h>

class MrpcSnglRaw : public PHObject
{

 public:
  MrpcSnglRaw() {}
  virtual ~MrpcSnglRaw() {}  

  // set the values in the SnglRaw
  virtual void set_slatid(const int val)              {warning("slatid");}
  virtual void set_t3(const int i, const int val)     {warning("t3");}
  virtual void set_t4(const int i, const int val)     {warning("t4");}
  virtual void set_q1(const int i, const int val)     {warning("q1");}
  virtual void set_q3(const int i, const int val)     {warning("q3");}
  virtual void set_qvc(const int i, const int val)    {warning("qvc");}
  virtual void set_t3_dig(const int i, const int val)     {warning("t3_dig");}
  virtual void set_t4_dig(const int i, const int val)     {warning("t4_dig");}

  // get the values from the SnglRaw
  virtual int get_slatid()        const     {warning("slatid"); return -9999;}
  virtual int get_t3(const int i) const     {warning("t3"); return -9999;}
  virtual int get_t4(const int i) const     {warning("t4"); return -9999;}
  virtual int get_q1(const int i) const     {warning("q1"); return -9999;} 
  virtual int get_q3(const int i) const     {warning("q3"); return -9999;} 
  virtual int get_qvc(const int i) const    {warning("qvc"); return -9999;} 
  virtual int get_t3_dig(const int i) const     {warning("t3_dig"); return -9999;}
  virtual int get_t4_dig(const int i) const     {warning("t4_dig"); return -9999;}
 private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout <<"Single MRPC RAW Offending field == " << field << std::endl;
  }

  ClassDef(MrpcSnglRaw,1)
};

#endif
