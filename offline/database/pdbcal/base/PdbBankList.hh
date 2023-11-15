#ifndef __PDBBANKLIST_HH__
#define __PDBBANKLIST_HH__

#include "PHPointerList.h"
#include "PdbCalBank.hh"

class PdbBankList : public PHPointerList<PdbCalBank>
{
public:
  PdbBankList();
  virtual ~PdbBankList();
};

#endif /* __PDBBANKLIST_HH__ */
