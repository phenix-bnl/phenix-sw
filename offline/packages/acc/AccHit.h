#ifndef __ACCHIT_H_
#define __ACCHIT_H_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class AccSnglHit;

class AccHit : public PHObject 
{
  
public:
  virtual ~AccHit() {}

  //  Virtual methods should be over-ridden...
  virtual void set_nhit (const unsigned int NHIT) {
    std::cout << "AccHit::Error set_nhit not overridden" << std::endl;
    return;
  }

  virtual int get_nhit () const {
    std::cout << "AccHit::Error get_nhit not overridden" << std::endl;
    return 0;
  }

  // set functions add(remove) AccSnglHit objects to(from) the collection
  virtual int set_TClonesArraySize(const unsigned int nhit) {return 0;}
  virtual void AddHit        (const unsigned int ihit) {return;}
  virtual void RemoveHit     (const unsigned int ihit) {return;}
  virtual AccSnglHit* AddHit (const unsigned int ihit, const AccSnglHit& hit) {return NULL;}

  // get function retreives a pointer to any of the objects in the collection
  virtual AccSnglHit* get_hit (const unsigned int ihit) const {
    std::cout << "Single Track return not implemented for your version of tracks" << std::endl;
    return 0;
  }

  // clone method allows to make additional containers based upon this one
  virtual AccHit* clone() const {
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
    os << "identify yourself: virtual AccHit object " << std::endl;
    return;
  }

  virtual void set_boxid(const int ihit, const int val) { warning("boxid"); }
  virtual void set_npe(const int ihit, const float val) { warning("npe"); }
  virtual void set_tof(const int ihit, const float val) { warning("tof"); }
  virtual void set_tdiff(const int ihit, const float val) { warning("tdiff"); }
  virtual void set_xyz(const int ihit, int ixyz, const float val) { warning("xyz"); }

  virtual int get_boxid(const int ihit) const {return -9999;}
  virtual float get_npe(const int ihit) const {return -9999;}
  virtual float get_tof(const int ihit) const {return -9999;}
  virtual float get_tdiff(const int ihit) const {return -9999;}
  virtual float get_xyz(const int ihit, const int ixyz) const {return -9999;}

 private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout <<"ACC HIT Offending field == " << field << std::endl;
  }
 
  ClassDef(AccHit,1)    
};

#endif
