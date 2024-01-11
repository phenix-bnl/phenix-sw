#ifndef __LVL2SNGLOUT_H__
#define __LVL2SNGLOUT_H__

#include <PHObject.h>
#include <phenixTypes.h>

#include <iosfwd>

class Lvl2SnglOut : public PHObject
{
public:
  Lvl2SnglOut() {}
  virtual ~Lvl2SnglOut() {}

  virtual const char *getname() const { return "ooooooo"; }
  virtual short  getversion() const { return -9999; }
  virtual short  getendianism() const { return -9999; }
  virtual PHDWORD *getdata() const { return 0; }

  virtual void   setname(const char *src_ptr) {}
  virtual void   setversion(short val) {}
  virtual void   setendianism(short e) {}
  virtual void   fill(UINT n, PHDWORD *src_ptr) {};
  virtual Int_t  getdatalength() const { return -9999; }
 
  virtual void Reset();
  virtual void Clear(Option_t *option = "");
  virtual void identify(std::ostream& os = std::cout) const;

  ClassDef(Lvl2SnglOut,1)

};

#endif	// __LVL2SNGLOUT_H__
