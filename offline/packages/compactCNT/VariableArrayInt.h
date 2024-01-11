#ifndef __VARIABLEARRAYINT_H_
#define __VARIABLEARRAYINT_H_

#include <PHObject.h>
#include <vector>

class VariableArrayInt : public PHObject
{
 public:
  VariableArrayInt(const unsigned int idval = 0);
  virtual ~VariableArrayInt();

  void identify(std::ostream &os=std::cout) const;

  void set_val(const std::vector<int> &vec); 
  const int *get_array() const {return sval;}
  unsigned int get_array_size() const {return nVal;}
  int Id() const {return id;}
  //  void set_fval(const std::vector<float> &vec); 
  void Reset();

 protected:
  int id;
  unsigned int nVal;
  int *sval; //[nVal]

  ClassDef(VariableArrayInt,1)
};

#endif /* VARIABLEARRAYINT */

