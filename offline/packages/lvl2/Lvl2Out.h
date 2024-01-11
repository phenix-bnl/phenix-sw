#ifndef __LVL2OUT_H__
#define __LVL2OUT_H__

#include <iostream>
#include <PHObject.h>
#include <phenixTypes.h>

class Lvl2Out : public PHObject
{
 public:
   virtual ~Lvl2Out() {}

   virtual void   setname(char *src_ptr) = 0;
   virtual char  *getname() const = 0;
   virtual void   fill(UINT n, PHDWORD *src) = 0;
   virtual PHDWORD *getdata() const = 0;
   virtual Int_t  getdatalength() const = 0;
   virtual void   setendianism(int e) = 0;
   virtual int    getendianism() const = 0;
   virtual void   identify(std::ostream& os = std::cout) const = 0;

   ClassDef(Lvl2Out,1)

};

#endif	// __LVL2OUT_H__
