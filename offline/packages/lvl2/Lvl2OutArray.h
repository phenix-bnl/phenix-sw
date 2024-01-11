#ifndef __LVL2OUTARRAY_H__
#define __LVL2OUTARRAY_H__

#include <iostream>
#include <PHObject.h>
#include <phenixTypes.h>

class Lvl2SnglOut;
class TClonesArray;

class Lvl2OutArray : public PHObject
{
public:
  Lvl2OutArray() {}
  virtual ~Lvl2OutArray() {}

  virtual int get_npart() const { return 0; }
  virtual void set_npart(const int val) {}

  virtual const char *getname(const unsigned int iprim) const;
  virtual short getversion(const unsigned int iprim) const;
  virtual short getendianism(const unsigned int iprim) const;
  virtual int getdatalength(const unsigned int iprim) const;
  virtual PHDWORD *getdata(const unsigned int iprim) const;

  virtual void setname(const unsigned int iprim, const char *src_ptr);
  virtual void setversion(const unsigned int iprim, const short e);
  virtual void setendianism(const unsigned int iprim, const short e);
  virtual void fill(const unsigned int iprim, UINT n, PHDWORD *src) {}


  virtual void dump_info() {} // returns the names of primitives

  // Routine to unwrap the Lvl2Primitive
  virtual int GetPrimitive(const char *name) = 0;

  // Routines to manipulate the particle array
  virtual int set_TClonesArraySize(const unsigned int iprim) {return -9999;}
  virtual void AddPrimitive(const unsigned int iprim) {}
  virtual void RemovePrimitive(const unsigned int iprim) {}

  // To replace the contents of an this object with 
  // copies of another's contents
  virtual void ReplaceContents(Lvl2OutArray * replacer) {}

  // Standard functions of all virtual classes...
  virtual void Reset();
  virtual int  isValid() const;
  virtual void identify(std::ostream &os=std::cout) const;

  // get single primitive
  virtual Lvl2SnglOut *getPrimitive(const unsigned int iprim) const;

 protected:
  virtual TClonesArray *GetL2Prim() const {return 0;}

  ClassDef(Lvl2OutArray,1)
};

#endif	// __LVL2OUTARRAY_H__



