#ifndef __MRPCSNGLHIT_H_
#define __MRPCSNGLHIT_H_

#include <iostream>
#include <PHObject.h>
#include <phool.h>

class MrpcSnglHit : public PHObject
{

 public:
  MrpcSnglHit() {}
  virtual ~MrpcSnglHit() {}  

  // set the values in the SnglHit
  virtual void set_slatid(const int val)              {warning("slatid");}
  virtual void set_time(const float val)              {warning("time");}
  virtual void set_time_dig(const float val)          {warning("time_dig");}
  virtual void set_charge(const float val)            {warning("charge");}
  virtual void set_xyz(const int i, const float val)  {warning("xyz");}

  // get the values from the SnglHit
  virtual int get_slatid()            const {warning("slatid");  return -9999;}
  virtual float get_time()            const {warning("time");  return -9999;}
  virtual float get_time_dig()        const {warning("time_dig"); return -9999;}
  virtual float get_charge()          const {warning("charge"); return -9999;}
  virtual float get_xyz(const int i)  const {warning("xyz");  return -9999;}

 private:
  void warning(const char* field) const 
    {
      std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
      std::cout <<"Single MRPC HIT Offending field == " << field << std::endl;
    }
  
  ClassDef(MrpcSnglHit,1)
};

#endif
