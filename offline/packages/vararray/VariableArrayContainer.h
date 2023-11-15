#ifndef __VARIABLEARRAYCONTAINER_H_
#define __VARIABLEARRAYCONTAINER_H_

#include <PHObject.h>
#include <vector>

class TOjArray;
class VariableArray;

class VariableArrayContainer : public PHObject
{
 public:
  VariableArrayContainer();
  virtual ~VariableArrayContainer();

  void identify(std::ostream &os=std::cout) const;
  void AddVarArray(VariableArray *var);
  // Here are the very explicit set routines...
  void Reset();

 protected:
  TObjArray *arraycontainer;

  ClassDef(VariableArrayContainer,1)
};

#endif /* VARIABLEARRAYCONTAINER */

