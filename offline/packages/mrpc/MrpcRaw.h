#ifndef __MRPCRAW_H_
#define __MRPCRAW_H_

#include <iostream>
#include <phool.h>
#include <PHObject.h>

class MrpcSnglRaw;

class MrpcRaw : public PHObject 
{
  
 public:
  virtual ~MrpcRaw() {}
  
  //  Virtual methods should be over-ridden...
  virtual void set_nraw (const unsigned int MRPC_NCH_TOTAL) 
    {
      std::cout << "MrpcRaw::Error set_nraw not overridden" << std::endl;
      return;
    }
  
  virtual int get_nraw () const 
    {
      std::cout << "MrpcRaw::Error get_nraw not overridden" << std::endl;
      return 0;
    }
  
  // set functions add(remove) MrpcSnglRaw objects to(from) the collection
  virtual int set_TClonesArraySize(const unsigned int nch) {return 0;}
  virtual void AddRaw        (const unsigned int ich) {return;}
  virtual void RemoveRaw     (const unsigned int ich) {return;}
  virtual MrpcSnglRaw* AddRaw (const unsigned int ich, const MrpcSnglRaw& raw) {return NULL;}
  
  // get function retreives a pointer to any of the objects in the collection
  virtual MrpcSnglRaw* get_raw (const unsigned int ich) const 
    {
      std::cout << "Single Track return not implemented for your version of tracks" << std::endl;
      return 0;
    }
  
  // clone method allows to make additional containers based upon this one
  virtual MrpcRaw* clone() const 
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
      os << "identify yourself: virtual MrpcRaw object " << std::endl;
      return;
    }
  
  virtual void set_slatid(const int iraw, const int val) { warning("slatid"); }
  virtual void set_t3(const int iraw, const int ich, const int val) { warning("t3"); }
  virtual void set_t4(const int iraw, const int ich, const int val) { warning("t4"); }
  virtual void set_q1(const int iraw, const int ich, const int val) { warning("q1"); }
  virtual void set_q3(const int iraw, const int ich, const int val) { warning("q3"); }
  virtual void set_qvc(const int iraw, const int ich, const int val) { warning("qvc"); }
  virtual void set_t3_dig(const int iraw, const int ich, const int val) { warning("t3_dig"); }
  virtual void set_t4_dig(const int iraw, const int ich, const int val) { warning("t4_dig"); }

  virtual int get_slatid(const int iraw) const { return -9999; }
  virtual int get_t3(const int iraw, const int ich) const { return -9999; }
  virtual int get_t4(const int iraw, const int ich) const { return -9999; }
  virtual int get_q1(const int iraw, const int ich) const { return -9999; }
  virtual int get_q3(const int iraw, const int ich) const { return -9999; }
  virtual int get_qvc(const int iraw, const int ich) const { return -9999; }
  virtual int get_t3_dig(const int iraw, const int ich) const { return -9999; }
  virtual int get_t4_dig(const int iraw, const int ich) const { return -9999; }
 private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout <<"MRPC RAW Offending field == " << field << std::endl;
  }
 
  ClassDef(MrpcRaw,1)    
};

#endif
