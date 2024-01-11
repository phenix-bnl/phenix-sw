#ifndef __MRPCHIT_H_
#define __MRPCHIT_H_

#include <iostream>
#include <phool.h>
#include <PHObject.h>

class MrpcSnglHit;

class MrpcHit : public PHObject 
{
  
 public:
  virtual ~MrpcHit() {}
  
  //  Virtual methods should be over-ridden...
  virtual void set_nhit (const unsigned int NHIT) 
    {
      std::cout << "MrpcHit::Error set_nhit not overridden" << std::endl;
      return;
    }
  
  virtual int get_nhit () const 
    {
      std::cout << "MrpcHit::Error get_nhit not overridden" << std::endl;
      return 0;
    }
  
  // set functions add(remove) MrpcSnglHit objects to(from) the collection
  virtual int set_TClonesArraySize(const unsigned int nhit) {return 0;}
  virtual void AddHit        (const unsigned int ihit) {return;}
  virtual void RemoveHit     (const unsigned int ihit) {return;}
  virtual MrpcSnglHit* AddHit (const unsigned int ihit, const MrpcSnglHit& hit) {return NULL;}
  
  // get function retreives a pointer to any of the objects in the collection
  virtual MrpcSnglHit* get_hit (const unsigned int ihit) const 
    {
      std::cout << "Single Track return not implemented for your version of tracks" << std::endl;
      return 0;
    }
  
  // clone method allows to make additional containers based upon this one
  virtual MrpcHit* clone() const 
    {
      std::cout << "Clone method not implemented for your version of CentralTracks" << std::endl;
      return 0;
    }
  
  // standard functions of all inheritors of PHObject classes
  virtual void Reset() 
    {
      std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function " << std::endl;
      return;
    }
  
  virtual int isValid() const 
    {
      std::cout << PHWHERE << "isValid() not implemented by daughter function " << std::endl;
      return 0;
    }

  virtual void identify(std::ostream& os=std::cout) const
    {
      os << "identify yourself: virtual MrpcHit object " << std::endl;
      return;
    }

  virtual void set_slatid(const int ihit, const int val) { warning("slatid"); }
  virtual void set_time(const int ihit, const float val) { warning("time"); }
  virtual void set_time_dig(const int ihit, const float val) { warning("time_dig"); }
  virtual void set_charge(const int ihit, const float val) { warning("charge"); }
  virtual void set_xyz(const int ihit, int ixyz, const float val) { warning("xyz"); }
  
  virtual int get_slatid(const int ihit) const {return -9999;}
  virtual float get_time(const int ihit) const {return -9999;}
  virtual float get_time_dig(const int ihit) const {return -9999;}
  virtual float get_charge(const int ihit) const {return -9999;}
  virtual float get_xyz(const int ihit, const int ixyz) const {return -9999;}
  
 private:
  void warning(const char* field) const 
    {
      std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
      std::cout <<"MRPC HIT Offending field == " << field << std::endl;
    }
  
  ClassDef(MrpcHit,1)    
};
    
#endif
