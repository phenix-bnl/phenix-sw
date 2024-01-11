#ifndef __TRIGLVL1V3_H
#define __TRIGLVL1V3_H

#include <TrigLvl1v2.h>
#include <iostream>

class TrigLvl1v3: public TrigLvl1v2
{
 public:
  TrigLvl1v3();
  virtual ~TrigLvl1v3() {}

  void identify(std::ostream& os = std::cout) const;
  TrigLvl1v3 * clone() const { return new TrigLvl1v3(*this); }
  void Reset();
  void set_lvl1_beam_clk(const unsigned int clk, const int i) {beamclk[i] = clk;}
  unsigned int get_lvl1_beam_clk(const int i) const {return beamclk[i];}

 protected:
   void init();
   unsigned int beamclk[2];
   ClassDef(TrigLvl1v3,1)

};

#endif /*  __TRIGLVL1V3_H */










