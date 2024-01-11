#ifndef __HBDRAWLISTV1_H
#define __HBDRAWLISTV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "HbdRawList.h"
#include "HbdRawv1.h"

class HbdRawListv1 : public HbdRawList
{

 public:

  HbdRawListv1();
  HbdRawListv1(const HbdRawListv1&);
  HbdRawListv1& operator=(const HbdRawListv1&);
  virtual ~HbdRawListv1();

  HbdRawListv1* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Implementations of the set/get methods...
  void set_nRaws (const unsigned int nraw) 
    {nRaws = nraw; return;}
  int  get_nRaws () const {return nRaws;}

  // Routines to manipulate the raw array...
  int set_TClonesArraySize(const unsigned int nraw);
  void AddRaw          (const unsigned int iraw);
  void RemoveRaw       (const unsigned int iraw);
  HbdRawv1* AddRaw (const unsigned int iraw, 
			    const HbdRaw& raw);
  HbdRawv1* get_raw(const unsigned int iraw) const;

 protected:

  TClonesArray *GetRaw() const {return Raw;}
  unsigned int nRaws;
  TClonesArray *Raw;

private:
  void copyto(HbdRawListv1& dest) const;

  ClassDef(HbdRawListv1,1)

};

#endif /* __HBDRAWLISTV1_H */

