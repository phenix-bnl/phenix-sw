#ifndef __ACCRAW_H_
#define __ACCRAW_H_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class AccSnglRaw;

class AccRaw : public PHObject 
{
  
public:
  virtual ~AccRaw() {}

  //  Virtual methods should be over-ridden...
  virtual void set_nraw (const unsigned int NPMT) {
    std::cout << "AccRaw::Error set_nraw not overridden" << std::endl;
    return;
  }

  virtual int get_nraw () const {
    std::cout << "AccRaw::Error get_nraw not overridden" << std::endl;
    return 0;
  }

  // set functions add(remove) AccSnglRaw objects to(from) the collection
  virtual int set_TClonesArraySize(const unsigned int nch) {return 0;}
  virtual void AddRaw        (const unsigned int ich) {return;}
  virtual void RemoveRaw     (const unsigned int ich) {return;}
  virtual AccSnglRaw* AddRaw (const unsigned int ich, const AccSnglRaw& raw) {return NULL;}

  // get function retreives a pointer to any of the objects in the collection
  virtual AccSnglRaw* get_raw (const unsigned int ich) const {
    std::cout << "Single Track return not implemented for your version of tracks" << std::endl;
    return 0;
  }

  // clone method allows to make additional containers based upon this one
  virtual AccRaw* clone() const {
    std::cout << "Clone method not implemented for your version of CentralTracks" << std::endl;
    return 0;
  }

  // standard functions of all inheritors of PHObject classes
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function " << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function " << std::endl;
    return 0;
  }

  virtual void identify(std::ostream& os=std::cout) const {
    os << "identify yourself: virtual AccRaw object " << std::endl;
    return;
  }

  virtual void set_boxid(const int iraw, const int val) { warning("boxid"); }
  virtual void set_adc(const int iraw, const int ipmt, const int val) { warning("adc"); }
  virtual void set_tdc(const int iraw, const int ipmt, const int val) { warning("tdc"); }
  virtual void set_adcpost(const int iraw, const int ipmt, const int val) { warning("adc_post"); }
  virtual void set_adcpre(const int iraw, const int ipmt, const int val) { warning("adc_pre"); }

  virtual int get_boxid(const int iraw) const { return -9999; }
  virtual int get_adc(const int iraw, const int ipmt) const { return -9999; }
  virtual int get_tdc(const int iraw, const int ipmt) const { return -9999; }
  virtual int get_adcpost(const int iraw, const int ipmt) const { return -9999; }
  virtual int get_adcpre(const int iraw, const int ipmt) const { return -9999; }

 private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout <<"ACC RAW Offending field == " << field << std::endl;
  }
 
  ClassDef(AccRaw,1)    
};

#endif
