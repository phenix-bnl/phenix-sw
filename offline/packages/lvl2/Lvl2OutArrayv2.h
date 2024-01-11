#ifndef __LVL2OUTARRAYV2_H__
#define __LVL2OUTARRAYV2_H__

#include <iostream>
#include <TClonesArray.h>
#include <Lvl2OutArray.h>

class Lvl2OutArrayv2 : public Lvl2OutArray
{
public:
  Lvl2OutArrayv2();
  virtual ~Lvl2OutArrayv2();

  int get_npart() const { return nL2Prim; }
  void set_npart(const int val) { nL2Prim = val; }

  void fill(const unsigned int iprim, UINT n, PHDWORD *src);


  void dump_info();

  // Routine to unwrap the Lvl2Primitive
  int find(const char *name) const;
  int GetPrimitive(const char *name);

  // Routines to manipulate the particle array
  int set_TClonesArraySize(const unsigned int iprim);
  void AddPrimitive(const unsigned int iprim);
  void RemovePrimitive(const unsigned int iprim);

  // To replace the contents of an this object with 
  // copies of another's contents
  void ReplaceContents(Lvl2OutArray * replacer);


  // Standard functions of all virtual classes...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

protected:
  int nL2Prim;
  TClonesArray *GetL2Prim() const {return L2Prim;}
  TClonesArray *L2Prim;

  ClassDef(Lvl2OutArrayv2,1)
};

#endif	// __LVL2OUTARRAYV2_H__
