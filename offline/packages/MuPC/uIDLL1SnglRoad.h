#ifndef __UIDLL1SNGLROAD_HH__
#define __UIDLL1SNGLROAD_HH__

#include "PHObject.h"
#include <iostream>

class uIDLL1SnglRoad : public PHObject
{


 public:
  virtual ~uIDLL1SnglRoad() {}

  virtual void set_arm    (const int val)   {warning("arm   ");}
  virtual void set_orient    (const int val)   {warning("orient   ");}
  virtual void set_symset (const int val)   {warning("symset");}

  virtual int get_arm() const  {warning("arm   "); return -9999;}
  virtual int get_orient()const   {warning("orient   "); return -9999;}
  virtual int get_symset()const   {warning("symset"); return -9999;}

 private:
  void warning(const char* field) const;

  ClassDef(uIDLL1SnglRoad,1)
};
#endif
