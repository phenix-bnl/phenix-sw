#ifndef __LVL2OUTARRAYV1_H__
#define __LVL2OUTARRAYV1_H__

#include <iostream>
#include <TClonesArray.h>
#include <Lvl2OutArray.h>

class Lvl2OutArrayv1 : public Lvl2OutArray
{
public:
  Lvl2OutArrayv1();
  virtual ~Lvl2OutArrayv1();

  int get_npart() const { return nL2Prim; }
  void set_npart(const int val) { nL2Prim = val; }

  const char *getname(const unsigned int iprim) const;
  short getversion(const unsigned int iprim) const;
  short getendianism(const unsigned int iprim) const;
  int getdatalength(const unsigned int iprim) const;
  PHDWORD *getdata(const unsigned int iprim) const;

  void setname(const unsigned int iprim, const char *src_ptr);
  void setversion(const unsigned int iprim, const short e);
  void setendianism(const unsigned int iprim, const short e);
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
  TClonesArray *L2Prim;

  ClassDef(Lvl2OutArrayv1,1)
};

#endif	// __LVL2OUTARRAYV1_H__
