#ifndef __PDBHBDMODULEGAIN_HH__
#define __PDBHBDMODULEGAIN_HH__

#include "PdbCalChan.hh"
#include <vector>

class PdbHbdModuleGain : public PdbCalChan 
{
public:
  PdbHbdModuleGain();
  PdbHbdModuleGain( const PdbHbdModuleGain &c);
  virtual ~PdbHbdModuleGain(){};

  PdbHbdModuleGain&  operator = (const PdbHbdModuleGain &c);
  
  // settters
  virtual int  setClockTick(const unsigned int, const unsigned int);
  virtual int  setGain(const unsigned int, const float);
  virtual int  setAllGains(const std::vector<float>);

  // getters
  virtual int  getClockTick(unsigned int&, unsigned int&) const;
  virtual int  getGain(const unsigned int, float&) const;
  virtual float getGain(const unsigned int) const;
  virtual int  getAllGains(std::vector<float>&) const;

  virtual void print() const;

private:

  static const unsigned int NHBDMODULES = 24;

  // Start of the validity range for a set of module by module gain constants
  // Need a ul64 to store a clock tick. Due to incompatibilities with DB, two 
  // unsigned ints are used for more or less significant halves of the clock tick. 
  // They are composed back to full size clock ticks in code
  unsigned int ctMoreSignif;  // more significant half of clock tick
  unsigned int ctLessSignif;  // less significant half of clock tick

  std::vector<float> gainConsts; // gain constants

  ClassDef(PdbHbdModuleGain,1);
};

#endif /* __PDBHBDMODULEGAIN_HH__ */
