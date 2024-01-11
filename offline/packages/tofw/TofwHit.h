#ifndef __TOFWHIT_H_
#define __TOFWHIT_H_

#include <iostream>
#include <phool.h>
#include <PHObject.h>

class TofwSnglHit;

class TofwHit : public PHObject
{
 public:
  virtual ~TofwHit() {}

  virtual TofwHit* clone() const
    {
      std::cout << "Clone method not implemented for your version of CentralTracks" << std::endl;
      return 0;
    }
  
  // the standard PHObject response functions
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

  void identify(std::ostream& os=std::cout) const
    {
      os << "identify yourself: virtual TofwHit object " << std::endl;
    }

  // actual implementations of the set/get methods
  virtual void set_nhit (const unsigned int NHIT)
    {
      std::cout << "TofwHit::Error set_nhit not overridden" << std::endl;
      return;
      
    }
  
  virtual int  get_nhit () const
    {
      std::cout << "TofwHit::Error get_nhit not overridden" << std::endl;
      return 0;      
    }  

  // routines to manipulate the particle array
  virtual int set_TClonesArraySize (const unsigned int nhit){return 0;}
  virtual void AddHit              (const unsigned int ihit){return;}
  virtual void RemoveHit           (const unsigned int ihit){return;}
  virtual TofwSnglHit* AddHit     (const unsigned int ihit, const TofwSnglHit& sngl){return NULL;}
  virtual TofwSnglHit* get_hit    (const unsigned int ihit) const
    {
      std::cout << "Single Track return not implemented for your version of tracks" << std::endl;
      return 0;
    }
  
  virtual void set_boxid(const int ihit, const int val) {warning("boxid");}
  virtual void set_chamberid(const int ihit, const int val) {warning("charmberid");}
  virtual void set_nstrip(const int ihit, const int val) {warning("nstrip");}
  virtual void set_max(const int ihit, const int istrip) {warning("max");}
  virtual void set_stripid(const int ihit, const int istrip, const int val) {warning("stripid");}
  virtual void set_time(const int ihit, const int istrip, const float val) {warning("time");}
  virtual void set_charge(const int ihit, const int istrip, const float val) {warning("charge");}
  virtual void set_rawadc(const int ihit, const int istrip, int irawadc, const float val) {warning("rawadc");}
  virtual void set_rawtdc(const int ihit, const int istrip, int irawtdc, const float val) {warning("rawtdc");}
  virtual void set_xyz(const int ihit, const int istrip, int ixyz, const float val) {warning("xyz");}
 

  virtual int   get_boxid(const int ihit) const {return -9999;}
  virtual int   get_chamberid(const int ihit) const {return -9999;}
  virtual int   get_nstrip(const int ihit) const {return -9999;}
  virtual int   get_max(const int ihit) const {return -9999;}

  virtual int   get_stripid(const int ihit, const int istrip) const {return -9999;}
  virtual int   get_stripid(const int ihit) const {return -9999;}

  virtual float get_time(const int ihit, const int istrip) const {return -9999;}
  virtual float get_time(const int ihit) const {return -9999;}

  virtual float get_charge(const int ihit, const int istrip) const {return -9999;}
  virtual float get_charge(const int ihit) const {return -9999;}

  virtual float get_rawadc(const int ihit, const int istrip, const int irawadc) const {return -9999;}
  virtual float get_rawadc(const int ihit, const int irawadc) const {return -9999;}

  virtual float get_rawtdc(const int ihit, const int istrip, const int irawtdc) const {return -9999;}
  virtual float get_rawtdc(const int ihit, const int irawtdc) const {return -9999;}

  virtual float get_xyz(const int ihit, const int istrip, const int ixyz) const {return -9999;}
  virtual float get_xyz(const int ihit, const int ixyz) const {return -9999;}

 private:
  void warning(const char* field) const 
    {
      std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
      std::cout <<"TOFW HIT Offending field == " << field << std::endl;
    }
  
  ClassDef(TofwHit,1)
};
    
#endif
