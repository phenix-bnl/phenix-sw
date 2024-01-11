#ifndef __TOFWSNGLRAW_H_
#define __TOFWSNGLRAW_H_

#include <iostream>
#include <PHObject.h>
#include <phool.h>

class TofwSnglRaw : public PHObject
{
 public:
  virtual ~TofwSnglRaw() {}  

  // set the values in the SnglRaw
  virtual void set_stripid(const int val)               {warning("stripid");}
  virtual void set_chamberid(const int val)             {warning("chamberid"); }
  virtual void set_boxid(const int val)                 {warning("boxid"); }

  virtual void set_t3(const int i, const int val)       {warning("t3[i]"); }
  virtual void set_t4(const int i, const int val)       {warning("t4[i]"); }
  virtual void set_q1(const int i, const int val)       {warning("q1[i]"); }
  virtual void set_q3(const int i, const int val)       {warning("q3[i]"); }
  virtual void set_tvc(const int i, const float val)    {warning("tvc[i]"); }
  virtual void set_qvc(const int i, const float val)      {warning("qvc[i]"); }

  // get the values from the SnglRaw
  virtual int get_stripid() const           { warning("stripid"); return -9999;}
  virtual int get_chamberid() const         { warning("chamberid"); return -9999;}
  virtual int get_boxid() const             { warning("boxid"); return -9999;}

  virtual int get_t3(const int i) const     { warning("t3[i]"); return -9999;}
  virtual int get_t4(const int i) const     { warning("t4[i]"); return -9999;}
  virtual int get_q1(const int i) const     { warning("q1[i]"); return -9999;}
  virtual int get_q3(const int i) const     { warning("q3[i]"); return -9999;}
  virtual float get_tvc(const int i) const  { warning("tvc[i]"); return -9999;}
  virtual float get_qvc(const int i) const  { warning("qvc[i]"); return -9999;}

 private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout <<"Single TOFW RAW Offending field == " << field << std::endl;
  }

  ClassDef(TofwSnglRaw,1)
};

#endif
