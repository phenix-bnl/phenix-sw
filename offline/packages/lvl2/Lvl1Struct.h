#ifndef __LVL1STRUCT_H__
#define __LVL1STRUCT_H__

#include <string>

typedef unsigned long ulong;

struct Lvl1Trig {
  std::string lvl1BitName;
  ulong lvl1Scaledown;
  ulong lvl1BitMask;
  ulong bitNumber;
  ulong partition;
  float rawScalerRate;
};

#endif // __LVL1STRUCT_H__









