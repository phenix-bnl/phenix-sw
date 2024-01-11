#ifndef __LVL2SNGLOUTV1_H__
#define __LVL2SNGLOUTV1_H__

#include <iostream>
#include <TString.h>
#include <TObject.h>
#include <phenixTypes.h>
#include <Lvl2SnglOut.h>

class Lvl2SnglOutv1 : public Lvl2SnglOut
{
public:
  Lvl2SnglOutv1();
  virtual ~Lvl2SnglOutv1();

  const char *getname() const { return name.Data(); }
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
  TString name;
  int   datalength;	// length of data
  PHDWORD *data;		//[datalength]

  ClassDef(Lvl2SnglOutv1,1)

};

#endif	// __LVL2SNGLOUTV1_H__
