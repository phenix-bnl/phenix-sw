//  Declaration of class PdbMuRecoPar
//  Author: rjnewby

#ifndef __PDBMURECOPAR_HH__
#define __PDBMURECOPAR_HH__

#include "PdbCalChan.hh"

class PdbMuRecoPar : public PdbCalChan 
{
public:
  PdbMuRecoPar();
  virtual ~PdbMuRecoPar();

  virtual void print() const;
  const char* ParName() const
  {
    return parname;
  }
  
  float ParValue() const
  {
    return parval;
  }
  void setParName(const char* newName);
  void setParValue(const float newVal)
  {
    parval = newVal;
    return;
  }

private:

  char parname[100];
  float parval;

  ClassDef(PdbMuRecoPar,1);
};

#endif /* __PDBMURECOPAR_HH__ */
