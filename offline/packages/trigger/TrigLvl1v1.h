#ifndef __TRIGLVL1V1_H
#define __TRIGLVL1V1_H

#include <iostream>
#include <TrigLvl1.h>

class TrigLvl1v1: public TrigLvl1
{
 public:
  TrigLvl1v1();
  virtual ~TrigLvl1v1() {}

  int isValid() const;
  void identify(std::ostream& os = std::cout) const;
  TrigLvl1v1 * clone() const { return new TrigLvl1v1(*this); }
  void Reset();

   unsigned int get_lvl1_trigraw() const {return lvl1_trigraw;}
   void set_lvl1_trigraw(const unsigned int ival) {lvl1_trigraw = ival; return;}

   unsigned int get_lvl1_triglive() const {return lvl1_triglive;}
   void set_lvl1_triglive(const unsigned int ival) {lvl1_triglive = ival; return;}

   unsigned int get_lvl1_trigscaled() const {return lvl1_trigscaled;}
   void set_lvl1_trigscaled(const unsigned int ival) {lvl1_trigscaled = ival; return;}

   unsigned int get_lvl1_clock_cross() const {return lvl1_clock_cross;}
   void set_lvl1_clock_cross(const unsigned int ival) {lvl1_clock_cross = ival; return;}

   unsigned int get_lvl1_rbits(const unsigned short i) const {return lvl1_rbits[i];}
   void set_lvl1_rbits(const unsigned int ival, const unsigned short i) {lvl1_rbits[i]=ival; return;}

 protected:
   void init();
   unsigned long lvl1_trigraw;     // 32 bits - one for each lvl1 trigger 
   unsigned long lvl1_triglive;
   unsigned long lvl1_trigscaled;  
   unsigned long lvl1_clock_cross; // the beam crossing clock (120 ticks)
   unsigned long lvl1_rbits[5];    // 130 bits - one for each reduced bit input described

   ClassDef(TrigLvl1v1,1)

};

#endif /*  __TRIGLVL1V1_H */


