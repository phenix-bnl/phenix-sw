#ifndef __TRIGLVL1_H
#define __TRIGLVL1_H

#include <iostream>
#include <PHObject.h>

class TrigLvl1: public PHObject
{
 public:
  virtual ~TrigLvl1() {}

  virtual int isValid() const;
  virtual void identify(std::ostream& os = std::cout) const;
  virtual void Reset();

  // add methods to get the bit information with an arguement
  // also add some specific methods for getting out specific
  // things -- how to value enumerated defined values

  virtual unsigned int get_lvl1_trigraw() const {return 0;}
  virtual void set_lvl1_trigraw(const unsigned int /*ival*/) {return;}
  virtual bool get_lvl1_trigraw_bit(const unsigned int /*i*/) const {return 0;}

  virtual unsigned int get_lvl1_triglive() const {return 0;}
  virtual void set_lvl1_triglive(const unsigned int /*ival*/) {return;}
  virtual bool get_lvl1_triglive_bit(const unsigned int /*i*/) const {return 0;}

  virtual unsigned int get_lvl1_trigscaled() const {return 0;}
  virtual void set_lvl1_trigscaled(const unsigned int /*ival*/) {return;}
  virtual bool get_lvl1_trigscaled_bit(const unsigned int /*i*/) const {return 0;}

  virtual unsigned int get_lvl1_clock_cross() const {return 0;}
  virtual void set_lvl1_clock_cross(const unsigned int /*ival*/) {return;}

  virtual unsigned int get_lvl1_rbits(const unsigned short /*i*/) const {return 0;}
  virtual void set_lvl1_rbits(const unsigned int /*ival*/, const unsigned short /*i*/) {return;}
  virtual bool get_lvl1_rbits_bit(const unsigned int /*i*/) const {return 0;}

  virtual void set_lvl1_beam_clk(const unsigned int /*clk*/, const int /*i*/) {return;}
  virtual unsigned int get_lvl1_beam_clk(const int /*i*/) const {return 0xFFFFFFFF;}
  ClassDef(TrigLvl1,1)

};

#endif /*  __TRIGLVL1_H */


