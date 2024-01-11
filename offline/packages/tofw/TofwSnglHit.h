#ifndef __TOFWSNGLHIT_H_
#define __TOFWSNGLHIT_H_

#include <iostream>
#include <PHObject.h>
#include <phool.h>

class TofwSnglHit : public PHObject
{

 public:
  TofwSnglHit(){}
  virtual ~TofwSnglHit() {}

  // set the values in the SnglHit
  virtual void set_boxid(const int val)               {warning("boxid");}
  virtual void set_chamberid(const int val)           {warning("chamberid");}
  virtual void set_nstrip(const int val)              {warning("nstrip");} 
  virtual void set_max(const int val)              {warning("max_strip");}
  virtual void set_stripid(int istrip, const int val)             {warning("stripid[istrip]");}
  virtual void set_time(int istrip, const float val)              {warning("time[istrip]");}
  virtual void set_charge(int istrip, const float val)            {warning("charge[istrip]");}
  virtual void set_rawadc(int istrip, const int i, const float val)  {warning("rawadc[istrip][i]");}
  virtual void set_rawtdc(int istrip, const int i, const float val)  {warning("rawtdc[istrip][i]");}
  virtual void set_xyz(int istrip, const int i, const float val)  {warning("xyz[istrip][i]");}

  // get the values from the SnglHit
  virtual int   get_boxid()  const {warning("boxid"); return -9999;}
  virtual int   get_chamberid()  const {warning("chamberid"); return -9999;}
  virtual int   get_nstrip()  const {warning("nstrip"); return -9999;}
  virtual int   get_max()  const {warning("max_strip"); return -9999;}
  virtual int   get_stripid(int istrip)    const {warning("stripid[istrip]"); return -9999;}
  virtual float get_time(int istrip)      const {warning("time[istrip]"); return -9999;}
  virtual float get_charge(int istrip)    const {warning("charge[istrip]"); return -9999;}
  virtual float get_rawadc(int istrip, int i)  const {warning("rawadc[istrip][i]"); return -9999;}
  virtual float get_rawtdc(int istrip, int i)  const {warning("rawtdc[istrip][i]"); return -9999;}
  virtual float get_xyz(int istrip, int i)  const {warning("xyz[istrip][i]"); return -9999;}

 private:
  void warning(const char* field) const 
    {
      std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
      std::cout <<"Single TOFW HIT Offending field == " << field << std::endl;
    }

  ClassDef(TofwSnglHit,1)
};

#endif
