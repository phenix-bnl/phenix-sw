#ifndef __LVL2SNGLOUTV2_H__
#define __LVL2SNGLOUTV2_H__

#include <Lvl2SnglOut.h>
#include <phenixTypes.h>

#include <iostream>
#include <string>

class Lvl2SnglOutv2 : public Lvl2SnglOut
{
public:
  Lvl2SnglOutv2();
  virtual ~Lvl2SnglOutv2();

  const char *getname() const { return name.c_str(); }
  short  getversion() const { return version; }
  short  getendianism() const { return endianism; }
  PHDWORD *getdata() const { return data; }

  void   setname(const char *src_ptr) { name = src_ptr; }
  void   setversion(const short val) { version = val; }
  void   setendianism(const short e) { endianism = e; }
  void   fill(UINT n, PHDWORD *src_ptr);
  int  getdatalength() const { return datalength; }
 
  void Reset();
  void Clear(Option_t *option = "");
  void identify(std::ostream& os = std::cout) const;

  enum { LitteEndian = 1, BigEndian };

protected:

  short endianism;	// big or little
  short version;	// primitve version
  std::string name;
  int   datalength;	// length of data
  PHDWORD *data;		//[datalength]

  ClassDef(Lvl2SnglOutv2,1)

};

#endif	// __LVL2SNGLOUTV2_H__
