#ifndef __LVL2OUTV1_H__
#define __LVL2OUTV1_H__

#include <iostream>
#include <phenixTypes.h>
#include <Lvl2Out.h>

class Lvl2Outv1 : public Lvl2Out
{
  void Clear(Option_t *option = "");

  enum { LitteEndian = 1, BigEndian };

  int   endianism;
  int   namelength;
  int   datalength;
  char  *name;		//[namelength]
  PHDWORD *data;		//[datalength]

public:
  Lvl2Outv1();
  virtual ~Lvl2Outv1();

  void   setname(char *src_ptr);
  char  *getname() const { return name; }
  void   fill(UINT n, PHDWORD *src_ptr);
  PHDWORD *getdata() const { return data; }
  Int_t  getdatalength() const { return datalength; }
  void   setendianism(int e) { endianism = e; }
  int    getendianism() const { return endianism; }
 
  void Reset();
  void identify(std::ostream& os = std::cout) const {
    os << "identify yourself: I am a Lvl2Outv1 object" << std::endl;
  }

  ClassDef(Lvl2Outv1,1)

};

#endif	// __LVL2OUTV1_H__
