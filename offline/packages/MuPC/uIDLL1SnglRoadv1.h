#ifndef __UIDLL1SNGLROADV1_H__
#define __UIDLL1SNGLROADV1_H__

#include "PHObject.h"
#include "uIDLL1SnglRoad.h"

class uIDLL1SnglRoadv1 : public uIDLL1SnglRoad
{
 public:
  uIDLL1SnglRoadv1();
  uIDLL1SnglRoadv1(uIDLL1SnglRoadv1*track);  
  virtual ~uIDLL1SnglRoadv1() {}

  // Here are the very explicit set routines...
  void set_arm    (const int val)   {arm = val;}
  void set_orient    (const int val)   {orient = val;}
  void set_symset (const int val)   {symset = val;}

  // Here are the very explicit "get" routines...

  int get_arm()  const  {return arm;}
  int get_orient() const   {return orient;}
  int get_symset() const   {return symset;}

 protected:
  int arm;
  int orient;
  int symset;

  ClassDef(uIDLL1SnglRoadv1,1)
};

#endif
