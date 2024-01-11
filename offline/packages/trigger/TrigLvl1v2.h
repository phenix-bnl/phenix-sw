#ifndef __TRIGLVL1V2_H
#define __TRIGLVL1V2_H

#include <iostream>
#include <TrigLvl1.h>

/*! Stores event by event Level1 Trigger Information and allows user access */
class TrigLvl1v2: public TrigLvl1
{
 public:
  TrigLvl1v2();
  virtual ~TrigLvl1v2() {}

  int isValid() const;
  void identify(std::ostream& os = std::cout) const;
  TrigLvl1v2 * clone() const { return new TrigLvl1v2(*this); }
  void Reset();

   /*! retrieve 32 bit raw trigger word */
   unsigned int get_lvl1_trigraw() const {return lvl1_trigraw;}
   void set_lvl1_trigraw(const unsigned int ival) {lvl1_trigraw = ival; return;}
   /*! retrieve true or false if a given trigger bit is set 0-31 */
   bool get_lvl1_trigraw_bit(const unsigned int i) const {
      if (bounds_check(i,NBITS_SIZE)) {
        if (((lvl1_trigraw & (1<<i)) >> i) == 0) {
	  return false;
	} else{
	  return true;
	}
      } else {
	return false;  // if asking for a out of bounds bit then return false
      }
   }

   unsigned int get_lvl1_triglive() const {return lvl1_triglive;}
   void set_lvl1_triglive(const unsigned int ival) {lvl1_triglive = ival; return;}
   /*! retrieve true or false if a given trigger bit is set 0-31 */
   bool get_lvl1_triglive_bit(const unsigned int i) const {
      if (bounds_check(i,NBITS_SIZE)) {
        if (((lvl1_triglive & (1<<i)) >> i) == 0) {
	  return false;
	} else{
	  return true;
	}
      } else {
	return false;  // if asking for a out of bounds bit then return false
      }
   }

   unsigned int get_lvl1_trigscaled() const {return lvl1_trigscaled;}
   void set_lvl1_trigscaled(const unsigned int ival) {lvl1_trigscaled = ival; return;}
   /*! retrieve true or false if a given trigger bit is set 0-31 */
   bool get_lvl1_trigscaled_bit(const unsigned int i) const {
      if (bounds_check(i,NBITS_SIZE)) {
        if (((lvl1_trigscaled & (1<<i)) >> i) == 0) {
	  return false;
	} else{
	  return true;
	}
      } else {
	return false;  // if asking for a out of bounds bit then return false
      }
   }

   unsigned int get_lvl1_clock_cross() const {return lvl1_clock_cross;}
   void set_lvl1_clock_cross(const unsigned int ival) {lvl1_clock_cross = ival; return;}

   unsigned int get_lvl1_rbits(const unsigned short i) const {
     if(bounds_check(i,RBITS_WORD_SIZE)) {
       return lvl1_rbits[i];
     } else {
       return 0;
     }
   }
   void set_lvl1_rbits(const unsigned int ival,const unsigned short i) {
     if (bounds_check(i,RBITS_WORD_SIZE)) {
       lvl1_rbits[i]=ival; 
       return;
     } else {
       return;
     }
   }
   /*! retrieve true or false if a given trigger bit is set 0-129 */
   /*! currently not sure how to unpack 0-129 bits from 5 words - checking [ ] */
   bool get_lvl1_rbits_bit(const unsigned int i) const {
      if (bounds_check(i,RBITS_BITS_SIZE)) {	
	unsigned int nword =  (unsigned int) i/32;  // double check this (always floor?) [ ]
	unsigned int bit   =  i%32; 
        if (((lvl1_rbits[nword] & (1<<bit)) >> bit) == 0) {
	  return false;
	} else{
	  return true;
	}
      } else {
	return false;  // if asking for a out of bounds bit then return false
      }
   }

 protected:
   void init();
   void printval(std::ostream& os = std::cout) const;
   enum Size {RBITS_WORD_SIZE=5, RBITS_BITS_SIZE=130, NBITS_SIZE=32};

   unsigned int lvl1_trigraw;                // 32 bits - one for each lvl1 trigger 
   unsigned int lvl1_triglive;               // 32 bits - one for each lvl1 trigger 
   unsigned int lvl1_trigscaled;             // 32 bits - one for each lvl1 trigger 
   unsigned int lvl1_clock_cross;            // the beam crossing clock (120 ticks)
   unsigned int lvl1_rbits[RBITS_WORD_SIZE]; // 130 bits - one for each reduced bit input described

   bool bounds_check(unsigned int index, unsigned int size) const { 
     if (index>=size) {
       std::cout << "bounds_check failed in  " << "file: " 
		 << __FILE__ << "  line: " << __LINE__ << std::endl;
       return false;
     }
     return true;
   }


   ClassDef(TrigLvl1v2,1)

};

#endif /*  __TRIGLVL1V2_H */
