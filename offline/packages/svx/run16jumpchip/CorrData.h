#ifndef __CORRDATA_H__
#define __CORRDATA_H__

#include <TObject.h>
#include <iostream>
#include "phool.h"

class CorrData : public TObject
{
public:
  CorrData();
  virtual ~CorrData() {}

  virtual CorrData& operator=(const CorrData &rhs)
  { std::cout << "in here" << std::endl; PHOOL_VIRTUAL_WARNING; return *this; }

  virtual Float_t get_bbcq() const     { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual Short_t get_nhits(const int) const    { PHOOL_VIRTUAL_WARNING; return -9999; }

  //virtual void Copy( const unsigned int iclus ) { PHOOL_VIRTUAL_WARNING; }
  virtual void print(std::ostream&) { PHOOL_VIRTUAL_WARNING; }

protected:
  //
  // this is the base class for single crystals
  //

private:

  ClassDef(CorrData,1)
};

#endif /* __CORRDATA_H__ */

